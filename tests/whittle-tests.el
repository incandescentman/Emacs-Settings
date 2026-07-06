;;; whittle-tests.el --- ERT tests for whittle transcript cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused regression tests for transcript dry-run reports and duplicate-word
;; cleanup failures observed in STT transcript cleanup.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst whittle-test--source-file
  (expand-file-name
   "whittle.el"
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to `whittle.el` in this repo.")

(load-file whittle-test--source-file)

(defun whittle-test--with-text (text function)
  "Insert TEXT in a temp buffer and call FUNCTION there."
  (with-temp-buffer
    (insert text)
    (funcall function)))

(defun whittle-test--generate-report (text &optional source-path temp-file)
  "Return a transcript report result for TEXT.
SOURCE-PATH becomes the temp buffer's `buffer-file-name'. TEMP-FILE
is passed through to `whittle--transcript-report-generate'."
  (whittle-test--with-text
   text
   (lambda ()
     (let ((buffer-file-name
            (or source-path "/tmp/whittle-test-transcript.org")))
       (whittle--transcript-report-generate (point-min) (point-max) temp-file)))))

(defun whittle-test--count-report-changes (report)
  "Return the number of change headings in REPORT."
  (with-temp-buffer
    (insert report)
    (goto-char (point-min))
    (how-many "^\\*\\*\\* Change ")))

(ert-deftest whittle-duplicate-words-preserves-comma-function-repeats ()
  "Comma-separated function-word repeats should not be collapsed."
  (whittle-test--with-text
   (mapconcat
    #'identity
    '("What that is, is an automation step."
      "If you have AI write it for you, you have to stop."
      "The the easy repeat should still collapse.")
    "\n")
   (lambda ()
     (whittle--remove-duplicated-words (point-min) (point-max))
     (let ((output (buffer-string)))
       (should (string-match-p "What that is, is an automation step\\." output))
       (should (string-match-p "write it for you, you have to stop" output))
       (should (string-match-p "The easy repeat should still collapse" output))
       (should-not (string-match-p "The the easy repeat" output))))))

(ert-deftest whittle-report-does-not-capitalize-unit-starts ()
  "Report mode should not create spurious first-character case entries."
  (let* ((result (whittle-test--generate-report
                  "every morning. next sentence\n"
                  "/tmp/whittle-test-transcript.org"
                  "/tmp/whittle-test-report.org"))
         (entries (plist-get result :entries))
         (entry (car entries)))
    (should (= (length entries) 1))
    (should (member "case normalization" (plist-get entry :passes)))
    (should (string= (plist-get entry :after)
                     "every morning. Next sentence"))))

(ert-deftest whittle-report-leaves-source-buffer-unchanged ()
  "Generating the report should not edit the source buffer."
  (let ((input "I mean, this is is a draft.\n"))
    (whittle-test--with-text
     input
     (lambda ()
       (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
         (whittle--transcript-report-generate (point-min) (point-max)
                                             "/tmp/whittle-test-report.org")
         (should (string= (buffer-string) input)))))))

(ert-deftest whittle-report-coalesces-multiple-passes-per-unit ()
  "One paragraph touched by several passes should produce one entry."
  (let* ((result (whittle-test--generate-report
                  "I mean, this is is a draft.\n"
                  "/tmp/whittle-test-transcript.org"
                  "/tmp/whittle-test-report.org"))
         (report (plist-get result :report))
         (entries (plist-get result :entries))
         (entry (car entries))
         (passes (plist-get entry :passes)))
    (should (= (length entries) 1))
    (should (member "conservative filler removal" passes))
    (should (member "duplicate-word collapse" passes))
    (should (string-match-p "- Changed units :: 1" report))
    (should (= (whittle-test--count-report-changes report) 1))))

(ert-deftest whittle-report-respects-unit-boundaries-and-headings ()
  "Headings are excluded and line-joining stays within paragraph units."
  (let* ((text (mapconcat
                #'identity
                '("* Heading"
                  "no punctuation"
                  ""
                  "the next paragraph"
                  "continues here")
                "\n"))
         (result (whittle-test--generate-report
                  text
                  "/tmp/whittle-test-transcript.org"
                  "/tmp/whittle-test-report.org"))
         (report (plist-get result :report)))
    (should-not (string-match-p "#\\+begin_quote\n\\* Heading" report))
    (should-not (string-match-p "no punctuation the next paragraph" report))
    (should (string-match-p "the next paragraph continues here" report))))

(ert-deftest whittle-report-includes-source-and-codex-target ()
  "The report should tell Codex which source file to edit directly."
  (let* ((source "/tmp/whittle-test-transcript.org")
         (result (whittle-test--generate-report
                  "I mean, this is a draft.\n"
                  source
                  "/tmp/whittle-test-report.org"))
         (report (plist-get result :report)))
    (should (string-match-p (regexp-quote (file-truename source)) report))
    (should (string-match-p "Codex should edit the source file above directly" report))
    (should (string-match-p "this report is not a replacement transcript" report))))

(ert-deftest whittle-transcript-report-writes-temp-file-and-copies-report ()
  "Interactive report command should save the report and copy it."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied
         result)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                     (lambda (text)
                       (setq copied text))))
            (whittle-test--with-text
             "I mean, this is a draft.\n"
             (lambda ()
               (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
                 (setq result (whittle-transcript-report (point-min) (point-max)))))))
          (let ((report (plist-get result :report))
                (clipboard-report (plist-get result :clipboard-report))
                (temp-file (plist-get result :temp-file)))
            (should (string= copied clipboard-report))
            (should-not (string= copied report))
            (should (string-match-p "Full report ::" copied))
            (should (string-prefix-p temp-dir temp-file))
            (should (file-exists-p temp-file))
            (should (string= (with-temp-buffer
                               (insert-file-contents temp-file)
                               (buffer-string))
                             report))))
      (delete-directory temp-dir t))))

(ert-deftest whittle-applies-conservative-cleanup-and-copies-report ()
  "`whittle' should edit the buffer and copy a Codex report."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           "I mean, this is is a draft.\n"
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-conservative.org"))
               (setq result (whittle (point-min) (point-max)))
               (should (string= (buffer-string) "this is a draft.\n"))))))
      (delete-directory temp-dir t))
    (should result)
    (should copied)
    (should (string-match-p "- Command :: whittle" copied))
    (should (string-match-p "cleanup already applied; verify and repair" copied))
    (should (string-match-p "Full report ::" copied))
    (should (string-match-p
             (regexp-quote (file-truename "/tmp/whittle-test-conservative.org"))
             copied))
    (should (string-match-p "Before excerpt:" copied))
    (should (string-match-p "After excerpt:" copied))))

(ert-deftest whittle-transcript-applies-cleanup-and-copies-report ()
  "`whittle-transcript' should edit the buffer and copy a Codex report."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           "I mean, this is is a draft.\n"
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
               (setq result (whittle-transcript (point-min) (point-max)))
               (should (string= (buffer-string) "this is a draft.\n"))))))
      (delete-directory temp-dir t))
    (should result)
    (should copied)
    (should (string-match-p "- Command :: whittle-transcript" copied))
    (should (string-match-p "cleanup already applied; verify and repair" copied))
    (should (string-match-p "Full report ::" copied))
    (should (string-match-p
             (regexp-quote (file-truename "/tmp/whittle-test-transcript.org"))
             copied))
    (should (string-match-p "Before excerpt:" copied))
    (should (string-match-p "After excerpt:" copied))))

(ert-deftest whittle-clipboard-report-summarizes-low-risk ()
  "Clipboard reports should summarize low-risk changes without quotes."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           "Um, hello\n"
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-compact.org"))
               (setq result (whittle (point-min) (point-max)))
               (should (string= (buffer-string) "hello\n"))))))
      (delete-directory temp-dir t))
    (should result)
    (should copied)
    (should (string-match-p "Low-Risk Summary" copied))
    (should (string-match-p "line 1, low :: conservative filler removal" copied))
    (should-not (string-match-p "#\\+begin_quote" copied))
    (should (string-match-p "No high- or medium-risk changes" copied))))

(ert-deftest whittle-transcript-preserves-comma-function-repeats-end-to-end ()
  "The full pipeline must not collapse comma-separated function-word repeats.
Regression guard: `whittle--remove-false-starts' used to eat \"you, you\"
even after `whittle--remove-duplicated-words' was tightened."
  (dolist (case '(("It wants to just write it for you, you have to stop that behavior."
                   "you, you")
                  ("What that is, is an automation step."
                   "is, is")
                  ("And then it, it worked out fine."
                   "it, it")
                  ("So they, they decided to leave early."
                   "they, they")))
    (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
               (lambda (_text) nil)))
      (whittle-test--with-text
       (car case)
       (lambda ()
         (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
           (whittle-transcript (point-min) (point-max))
           (should (string-match-p (regexp-quote (cadr case)) (buffer-string)))))))))

(ert-deftest whittle-false-starts-still-collapse-genuine-restarts ()
  "Multi-word restarts and space-separated repeats must still collapse."
  ;; Multi-word restart across a comma: not a single function word.
  (whittle-test--with-text
   "I was, I was going to say something."
   (lambda ()
     (whittle--remove-false-starts (point-min) (point-max))
     (should (string-match-p "I was going to say something" (buffer-string)))
     (should-not (string-match-p "I was, I was" (buffer-string)))))
  ;; Space-separated function-word repeat (no comma) is still a stammer.
  (whittle-test--with-text
   "and you you have to stop."
   (lambda ()
     (whittle--remove-false-starts (point-min) (point-max))
     (should (string-match-p "and you have to stop" (buffer-string)))
     (should-not (string-match-p "you you" (buffer-string))))))

(ert-deftest whittle-report-ranks-comma-function-repeat-high ()
  "A unit with a comma function-word repeat and a false-start collapse ranks high.
The multi-word restart \"I was, I was\" fires false-start collapse; the
surviving \"you, you\" should push the entry to high, not medium."
  (let* ((result (whittle-test--generate-report
                  "I was, I was saying that you, you know the answer.\n"
                  "/tmp/whittle-test-transcript.org"
                  "/tmp/whittle-test-report.org"))
         (entries (plist-get result :entries))
         (entry (car entries)))
    (should entry)
    (should (member "false-start collapse" (plist-get entry :passes)))
    (should (string-match-p "you, you" (plist-get entry :after)))
    (should (equal (plist-get entry :risk) "high"))))

(provide 'whittle-tests)

;;; whittle-tests.el ends here
