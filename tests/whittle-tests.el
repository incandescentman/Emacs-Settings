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

(defun whittle-test--scar-tags (before after)
  "Return final-output scar tags for BEFORE to AFTER in transcript mode."
  (mapcar (lambda (scar) (plist-get scar :tag))
          (whittle--transcript-report-scars before after 'transcript)))

(defun whittle-test--command-output (text command)
  "Run COMMAND on TEXT in a temp buffer and return the buffer string."
  (whittle-test--with-text
   text
   (lambda ()
     (funcall command (point-min) (point-max))
     (buffer-string))))

(defun whittle-test--reported-command-output (text command &optional source-path)
  "Run report-copying COMMAND on TEXT and return (OUTPUT . CLIPBOARD)."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied
         output)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           text
           (lambda ()
             (let ((buffer-file-name
                    (or source-path "/tmp/whittle-test-transcript.org")))
               (funcall command (point-min) (point-max))
               (setq output (buffer-string))))))
      (delete-directory temp-dir t))
    (cons output copied)))

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

(ert-deftest whittle-duplicate-words-preserves-final-emphasis ()
  "Identity/emphasis repeats at sentence end should survive duplicate cleanup."
  (whittle-test--with-text
   (mapconcat
    #'identity
    '("Use AI for everything except the thing that makes you you."
      "But you you have to go.")
    "\n")
   (lambda ()
     (whittle--remove-duplicated-words (point-min) (point-max))
     (let ((output (buffer-string)))
       (should (string-match-p "makes you you\\." output))
       (should (string-match-p "But you have to go\\." output))
       (should-not (string-match-p "But you you have to go" output))))))

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
                  "Um, this is is a draft.\n"
                  "/tmp/whittle-test-transcript.org"
                  "/tmp/whittle-test-report.org"))
         (report (plist-get result :report))
         (entries (plist-get result :entries))
         (entry (car entries))
         (passes (plist-get entry :passes)))
    (should (= (length entries) 1))
    (should (member "conservative filler removal" passes))
    (should (member "duplicate-word collapse" passes))
    (should (string-match-p "- Reported units :: 1" report))
    (should (= (whittle-test--count-report-changes report) 1))))

(ert-deftest whittle-scar-scan-detects-known-output-scars ()
  "Final-output scar scan should catch known applied-output damage shapes."
  (dolist (case '(("thinking, I mean, I love it"
                   "thinking I love it"
                   "deleted-comma-before-pronoun")
                  ("having that social, interactive part. is really useful"
                   "having that social, interactive part. Is really useful"
                   "case-clause-fusion")
                  ("documents. that I've created for Claude"
                   "documents. That I've created for Claude"
                   "case-clause-fusion")
                  ("I mean, first of all, don't you think"
                   "first of all don't you think"
                   "deleted-comma-before-pronoun")
                  ("When I've worked for magazines, like, I was an editor"
                   "When I've worked for magazines I was an editor"
                   "like-clause-fusion")
                  ("It's gonna get published, right? So… My premise is simple"
                   "It's gonna get published, right? … My premise is simple"
                   "right-so-chain")
                  ("It's gonna get published, right? So… My premise is simple"
                   "It's gonna get published, My premise is simple"
                   "right-so-chain")))
    (should (member (nth 2 case)
                    (whittle-test--scar-tags (nth 0 case) (nth 1 case))))))

(ert-deftest whittle-residue-detectors-are-generalized ()
  "Residue detectors should flag shapes, not exact transcript strings."
  (let ((pronoun-hazards
         (whittle--transcript-report-hazards
          "However, I uh it takes a while."
          "However, I it takes a while."
          '("conservative filler removal")
          'transcript))
        (comma-hazards
         (whittle--transcript-report-hazards
          "They were, you know, um totally novices."
          "They were, totally novices."
          '("filler-chain collapse")
          'transcript))
        (legit-parenthetical
         (whittle--transcript-report-hazards
          "They were, honestly, exhausted."
          "They were, honestly, exhausted."
          nil
          'transcript)))
    (should (member "subject-pronoun-fusion" pronoun-hazards))
    (should (member "stranded-comma-after-copula" comma-hazards))
    (should-not (member "stranded-comma-after-copula" legit-parenthetical))
    (should (member "subject-pronoun-fusion"
                    (whittle-test--scar-tags
                     "However, I uh it takes a while."
                     "However, I it takes a while.")))))

(ert-deftest whittle-scar-scan-renders-in-compact-and-full-reports ()
  "Reports should surface advisory scars before normal change review."
  (let* ((entry (list :number 1
                      :line 39
                      :unit 7
                      :start 1
                      :end 42
                      :risk "high"
                      :passes '("conservative filler removal")
                      :hazards nil
                      :scars (list (list :tag "deleted-comma-before-pronoun"
                                          :span (cons 0 15)))
                      :held-back nil
                      :before "thinking, I mean, I love it"
                      :after "thinking I love it"
                      :full-after "thinking I love it"
                      :full-passes '("conservative filler removal")
                      :full-hazards nil))
         (entries (list entry))
         (full (whittle--transcript-report-format
                "/tmp/source.org" "/tmp/report.org" entries
                "whittle-transcript" 'transcript t))
         (compact (whittle--transcript-report-format-compact
                   "/tmp/source.org" "/tmp/report.org" entries
                   "whittle-transcript" 'transcript t)))
    (dolist (report (list full compact))
      (should (string-match-p "Suspicious Output Scar Scan" report))
      (should (string-match-p "Scar counts :: .*deleted-comma-before-pronoun 1" report))
      (should (string-match-p "advisory final-output scan" report))
      (should (string-match-p "Output scars :: deleted-comma-before-pronoun" report)))
    (should (< (string-match "Suspicious Output Scar Scan" compact)
               (string-match "Applied High/Medium-Risk Changes" compact)))))

(ert-deftest whittle-applied-reports-check-saved-state-conditionally ()
  "Applied reports should ask for a save only when disk state is unconfirmed."
  (let* ((entry (list :number 1
                      :line 1
                      :unit 1
                      :risk "low"
                      :passes '("duplicate-word collapse")
                      :hazards nil
                      :scars nil
                      :held-back nil
                      :before "This is is ready."
                      :after "This is ready."))
         (entries (list entry))
         (full (whittle--transcript-report-format
                "/tmp/source.org" "/tmp/report.org" entries
                "whittle-transcript" 'transcript t))
         (compact (whittle--transcript-report-format-compact
                   "/tmp/source.org" "/tmp/report.org" entries
                   "whittle-transcript" 'transcript t)))
    (dolist (report (list full compact))
      (should (string-match-p
               (regexp-quote
                "If they are present, continue the review without asking Jay to save again.")
               report))
      (should (string-match-p
               (regexp-quote
                "If they are absent or you cannot confirm them, tell Jay to save the buffer and wait for confirmation")
               report))
      (should (string-match-p
               (regexp-quote
                "Never tell him to discard, revert, or avoid saving the Whittle changes.")
               report)))))

(ert-deftest whittle-scarred-entries-rank-high ()
  "Any final-output scar should promote the applied entry to high risk."
  (should (equal
           (whittle--transcript-report-risk
            "Hello , world" "Hello, world" '("punctuation cleanup") nil
            (list (list :tag "deleted-comma-before-pronoun"
                        :span (cons 0 5))))
           "high")))

(ert-deftest whittle-transcript-applies-safe-dup-inside-i-mean-hazard ()
  "A sentence-initial `I mean,' hazard should not block safe duplicate cleanup."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           "I mean, this is is a draft.\n"
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
               (whittle-transcript (point-min) (point-max))
               (should (string= (buffer-string)
                                "I mean, this is a draft.\n"))))))
      (delete-directory temp-dir t))
    (should copied)
    (should (string-match-p "Held Back" copied))
    (should (string-match-p "sentence-initial-i-mean" copied))
    (should (string-match-p "accepted partial cleanup applied; hazardous span protected" copied))
    (should (string-match-p "duplicate-word collapse" copied))
    (should (string-match-p "I mean, this is is a draft" copied))
    (should (string-match-p "I mean, this is a draft" copied))))

(ert-deftest whittle-transcript-applies-safe-filler-inside-like-hazard ()
  "A risky comma-bounded `like' deletion should not block safe nearby filler cleanup."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           "This is, um, useful when I've worked for magazines, like, I was an editor.\n"
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
               (whittle-transcript (point-min) (point-max))
               (should (string= (buffer-string)
                                "This is useful when I've worked for magazines, like, I was an editor.\n"))))))
      (delete-directory temp-dir t))
    (should copied)
    (should (string-match-p "Held Back" copied))
    (should (string-match-p "like-clause-fusion" copied))
    (should (string-match-p "accepted partial cleanup applied; hazardous span protected" copied))
    (should (string-match-p "conservative filler removal" copied))
    (should (string-match-p "This is useful" copied))))

(ert-deftest whittle-transcript-protects-held-back-spans-from-later-scars ()
  "Held-back discourse spans should protect adjacent punctuation and case."
  (let ((cases
         '(("Writing can be solitary, sitting alone, thinking, I mean, I love it, I think I'm good at it, but sharing what I know and helping other people, and having that social, interactive part. is really the part that I love even more."
            ("thinking I love" "part\\. Is really")
            ("sentence-initial-i-mean" "deleted-comma-before-pronoun" "case-clause-fusion"))
           ("This is a set of custom instructions, documents. that I've created..."
            ("documents\\. That I've created")
            ("case-clause-fusion"))
           ("I mean, first of all, I mean, don't you think that people can tell when you're using AI?"
            ("^first" "first of all don't" "I mean first")
            ("sentence-initial-i-mean" "deleted-comma-before-pronoun"))
           ("When I've worked for magazines, like, I was an editor at Psychology Today..."
            ("magazines I was")
            ("like-clause-fusion" "deleted-comma-before-pronoun")))))
    (dolist (case cases)
      (let* ((input (nth 0 case))
             (forbidden (nth 1 case))
             (hazards (nth 2 case))
             (temp-dir (make-temp-file "whittle-report-test" t))
             (whittle/transcript-report-directory temp-dir)
             copied
             output)
        (unwind-protect
            (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                       (lambda (text)
                         (setq copied text))))
              (whittle-test--with-text
               input
               (lambda ()
                 (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
                   (whittle-transcript (point-min) (point-max))
                   (setq output (buffer-string))))))
          (delete-directory temp-dir t))
        (should copied)
        (let ((case-fold-search nil))
          (dolist (pattern forbidden)
            (should-not (string-match-p pattern output))))
        (dolist (hazard hazards)
          (should (string-match-p (regexp-quote hazard) copied)))
        (should (string-match-p "Outcome ::" copied))))))

(ert-deftest whittle-scar-scan-stays-quiet-on-protected-narratively-shapes ()
  "Held-back hazards should not become advisory scars when output is protected."
  (dolist (input '("Writing can be solitary, sitting alone, thinking, I mean, I love it, I think I'm good at it, but sharing what I know and helping other people, and having that social, interactive part. is really the part that I love even more."
                   "So… that's how I think about it."
                   "This is a set of custom instructions, documents. that I've created..."
                   "I mean, first of all, I mean, don't you think that people can tell when you're using AI?"
                   "When I've worked for magazines, like, I was an editor at Psychology Today..."
                   "It's gonna get published, right? So… My premise is that AI can help."))
    (let* ((result (whittle-test--generate-report
                    input
                    "/tmp/whittle-test-transcript.org"
                    "/tmp/whittle-test-report.org"))
           (entries (plist-get result :entries)))
      (should-not (whittle--transcript-report-scar-entries entries)))))

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

(ert-deftest whittle-report-excludes-org-structural-lines ()
  "Report units should skip org syntax while cleaning surrounding prose."
  (let* ((text (mapconcat
                #'identity
                '(":PROPERTIES:"
                  ":ID:       20260706T233409.419166"
                  ":END:"
                  "#+TITLE: Fable: Thought Leadership Strategy"
                  "#+FILETAGS: :socratic:"
                  "- Links ::"
                  "- Source ::"
                  "Um, this is is a draft."
                  "#+begin_src user"
                  "great. so give me a detailed assignment"
                  "#+end_src")
                "\n"))
         (units
          (whittle-test--with-text
           text
           (lambda ()
             (whittle--transcript-report-units (point-min) (point-max))))))
    (should (equal (mapcar (lambda (unit) (plist-get unit :text)) units)
                   '("Um, this is is a draft."
                     "great. so give me a detailed assignment")))))

(ert-deftest whittle-report-apply-preserves-org-syntax ()
  "`whittle' should not clobber org metadata while applying safe cleanup."
  (let* ((input (mapconcat
                 #'identity
                 '(":PROPERTIES:"
                   ":ID:       20260706T233409.419166"
                   ":END:"
                   "#+TITLE: Fable: Thought Leadership Strategy"
                   "#+FILETAGS: :socratic:"
                   "- Links ::"
                   "- Source ::"
                   "Um, this is is a draft.")
                 "\n"))
         (output
          (car (whittle-test--reported-command-output
                input #'whittle "/tmp/whittle-test-conservative.org"))))
    (should (string-match-p ":ID:       20260706T233409\\.419166" output))
    (should (string-match-p "#\\+FILETAGS: :socratic:" output))
    (should (string-match-p "- Links ::" output))
    (should (string-match-p "- Source ::" output))
    (should-not (string-match-p "#\\+FILETAGS::socratic:" output))
    (should-not (string-match-p "- Links::" output))
    (should (string-match-p "this is a draft\\." output))))

(ert-deftest whittle-raw-commands-preserve-org-syntax ()
  "Raw apply commands should share the org structural protection layer."
  (let ((input (mapconcat
                #'identity
                '(":PROPERTIES:"
                  ":ID:       20260706T233409.419166"
                  ":END:"
                  "#+TITLE: Fable: Thought Leadership Strategy"
                  "#+FILETAGS: :socratic:"
                  "- Links ::"
                  "- Source ::"
                  "Um, this is is a draft.")
                "\n")))
    (dolist (command '(whittle-apply
                       whittle-transcript-apply
                       whittle/remove-filler-words
                       whittle/remove-duplicated-words))
      (let ((output (whittle-test--command-output input command)))
        (should (string-match-p ":ID:       20260706T233409\\.419166" output))
        (should (string-match-p "#\\+FILETAGS: :socratic:" output))
        (should (string-match-p "- Links ::" output))
        (should (string-match-p "- Source ::" output))
        (should-not (string-match-p "#\\+FILETAGS::socratic:" output))
        (should-not (string-match-p "- Links::" output))))))

(ert-deftest whittle-raw-apply-caches-org-protection-per-pass ()
  "Apply cleanup should not rescan org protection per regex match."
  (let* ((line "Um, this is is a draft with punctuation , and i should change.")
         (input (mapconcat #'identity (make-list 80 line) "\n"))
         (scan-count 0)
         (original (symbol-function 'whittle--org-protected-regions)))
    (cl-letf (((symbol-function 'whittle--org-protected-regions)
               (lambda (beg end)
                 (setq scan-count (1+ scan-count))
                 (funcall original beg end))))
      (whittle-test--command-output input #'whittle-transcript-apply))
    ;; The input is one 80-line paragraph with hundreds of matches. The
    ;; guarded engine runs each pass twice per paragraph (gated and
    ;; ungated), so protection is computed at most twice per pass, not
    ;; once per match or line.
    (should (<= scan-count
                (* 2 (length (whittle--transcript-report-pass-specs
                              'transcript)))))))

(ert-deftest whittle-transcript-apply-preserves-src-delimiters ()
  "Line joining should not fuse org source delimiters with quoted prose."
  (let* ((input "#+begin_src user\ngreat. so give me a detailed assignment\n#+end_src\n")
         (output (whittle-test--command-output input #'whittle-transcript-apply)))
    (should (string-match-p "#\\+begin_src user\ngreat\\. So give me" output))
    (should-not (string-match-p "#\\+begin_src user great" output))
    (should (string-match-p "\n#\\+end_src\n" output))))

(ert-deftest whittle-case-normalization-respects-abbreviations-and-ellipses ()
  "Case normalization should not capitalize after abbreviations or ellipses."
  (let ((input (mapconcat
                #'identity
                '("That distinction... idea vs. brand is useful."
                  "Now, what \"creed vs. argument\" means."
                  "e.g. right now, i could post."
                  "Line break e.g."
                  "right now stays lower."
                  "The P.P.S. (\"please go\") is bad."
                  "Pause... and then continue."
                  "Pause… and then continue.")
                "\n")))
    (whittle-test--with-text
     input
     (lambda ()
       (whittle--normalize-case (point-min) (point-max) t)
       (let ((output (buffer-string)))
         (should (string-match-p "distinction\\.\\.\\. idea vs\\. brand" output))
         (should (string-match-p "creed vs\\. argument" output))
         (should (string-match-p "e\\.g\\. right now, I could post" output))
         (should (string-match-p "Line break e\\.g\\.\nright now stays lower" output))
         (should (string-match-p "P\\.P\\.S\\. (\"please go\")" output))
         (should (string-match-p "Pause\\.\\.\\. and then continue" output))
         (should (string-match-p "Pause… and then continue" output)))))))

(ert-deftest whittle-case-normalization-still-capitalizes-after-headings ()
  "Protecting org headings should not disable post-heading capitalization."
  (let ((output (whittle-test--command-output
                 "* Heading\nthis should cap.\n"
                 #'whittle-apply)))
    (should (string-match-p "\\* Heading\nThis should cap\\." output))))

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
               (should (string= (buffer-string) "This is a draft.\n"))))))
      (delete-directory temp-dir t))
    (should result)
    (should copied)
    (should (string-match-p "- Command :: whittle" copied))
    (should (string-match-p
             "cleanup applied to Emacs buffer; verify saved source"
             copied))
    (should (string-match-p
             (regexp-quote
              "If they are present, continue the review without asking Jay to save again.")
             copied))
    (should (string-match-p
             (regexp-quote
              "If they are absent or you cannot confirm them, tell Jay to save the buffer and wait for confirmation")
             copied))
    (should (string-match-p
             (regexp-quote
              "Never tell him to discard, revert, or avoid saving the Whittle changes.")
             copied))
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
           "This is is a draft.\n"
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
               (setq result (whittle-transcript (point-min) (point-max)))
               (should (string= (buffer-string) "This is a draft.\n"))))))
      (delete-directory temp-dir t))
    (should result)
    (should copied)
    (should (string-match-p "- Command :: whittle-transcript" copied))
    (should (string-match-p
             "cleanup applied to Emacs buffer; verify saved source"
             copied))
    (should (string-match-p
             (regexp-quote
              "If they are present, continue the review without asking Jay to save again.")
             copied))
    (should (string-match-p
             (regexp-quote
              "If they are absent or you cannot confirm them, tell Jay to save the buffer and wait for confirmation")
             copied))
    (should (string-match-p
             (regexp-quote
              "Never tell him to discard, revert, or avoid saving the Whittle changes.")
             copied))
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
               (should (string= (buffer-string) "Hello\n"))))))
      (delete-directory temp-dir t))
    (should result)
    (should copied)
    (should (string-match-p "Low-Risk Summary" copied))
    (should (string-match-p "line 1, low :: conservative filler removal" copied))
    (should-not (string-match-p "#\\+begin_quote" copied))
    (should (string-match-p "No applied high- or medium-risk changes" copied))))

(ert-deftest whittle-clipboard-report-trims-high-risk-excerpts ()
  "High-risk clipboard entries should quote only the local changed span."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         (input (string-join
                 '("This opening sentence has a lot of harmless setup before"
                   "the risky phrase so the compact report should not copy"
                   "all of it. The way the assistant works is, I mean,"
                   "getting advice from a friend who asks questions before"
                   "making suggestions. After that comparison, the speaker"
                   "continues with additional context that should be trimmed"
                   "away from the clipboard excerpt.")
                 " "))
         copied
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           input
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-compact-high.org"))
               (setq result (whittle (point-min) (point-max)))))))
      (delete-directory temp-dir t))
    (should result)
    (should copied)
    (should (string-match-p "High/Medium-Risk Changes" copied))
    (should (string-match-p "\\.\\.\\." copied))
    (should (string-match-p "I mean" copied))
    (should-not
     (string-match-p "This opening sentence has a lot of harmless setup"
                     copied))
    (should-not
     (string-match-p "additional context that should be trimmed away"
                     copied))))

	(ert-deftest whittle-transcript-preserves-discourse-markers ()
	  "Transcript cleanup should not delete meaning-bearing discourse markers."
	  (let ((cases '("That's how I use AI, right?"
	                 "Right? I tell people this all the time."
	                 "That's the power. Right? Is that the right way to put it?"
	                 "Like I said, this is useful."
	                 "Like, forget the essay for a second."
	                 "So I think the best next move is clear."
	                 "So the same moment, reframed:"
	                 "Well, this is the point."
	                 "OK, the shape is coming into focus."
	                 "You know, this might be useful.")))
    (dolist (input cases)
      (let* ((temp-dir (make-temp-file "whittle-report-test" t))
             (whittle/transcript-report-directory temp-dir))
        (unwind-protect
            (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                       (lambda (_text) nil)))
              (whittle-test--with-text
               input
               (lambda ()
                 (let ((buffer-file-name "/tmp/whittle-test-discourse.org"))
                   (whittle-transcript (point-min) (point-max))
                   (should (string= (buffer-string) input))))))
          (delete-directory temp-dir t))))))

(ert-deftest whittle-transcript-gates-i-mean-openers ()
  "Risky sentence-initial `I mean,' cleanup should be reported and held back."
  (dolist (case '(("I mean, he looks basically like how I look."
                   "sentence-initial-i-mean")))
    (let* ((input (car case))
           (hazard (cadr case))
           (temp-dir (make-temp-file "whittle-report-test" t))
           (whittle/transcript-report-directory temp-dir)
           copied)
      (unwind-protect
          (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                     (lambda (text)
                       (setq copied text))))
            (whittle-test--with-text
             input
             (lambda ()
               (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
                 (whittle-transcript (point-min) (point-max))
                 (should (string= (buffer-string) input))))))
        (delete-directory temp-dir t))
      (should copied)
      (should (string-match-p "Hazard-Tagged / Not Applied" copied))
      (should (string-match-p (regexp-quote hazard) copied)))))

(ert-deftest whittle-transcript-holds-back-subject-pronoun-residue ()
  "Filler removal that would leave adjacent subject pronouns should be held."
  (let* ((input "However, I uh it takes a while.\n")
         (result (whittle-test--reported-command-output
                  input #'whittle-transcript "/tmp/whittle-test-transcript.org"))
         (output (car result))
         (copied (cdr result)))
    (should (string= output input))
    (should (string-match-p "subject-pronoun-fusion" copied))
    (should (string-match-p "Hazard-Tagged / Not Applied" copied))
    (should (string-match-p "However, I it takes a while" copied))))

(ert-deftest whittle-transcript-gates-like-clause-fusions ()
  "Comma-bounded `like' should not fuse adjacent clause-like spans."
  (dolist (case '(("When I've worked for magazines, like, I was an editor."
                   "like-clause-fusion")
                  ("We need this thing tomorrow, like, we're going to press."
                   "deleted-comma-before-pronoun")
                  ("That's not what I was gonna say, like, you don't know."
                   "deleted-comma-before-pronoun")
                  ("I know that I've had really like, amazing stories, like, I'm not always thinking about them."
                   "deleted-comma-before-pronoun")
                  ("I, like, you defined it clearly."
                   "deleted-comma-before-pronoun")
                  ("Emily, like, you don't have those weekends."
                   "deleted-comma-before-pronoun")
                  ("Hey, like, outline a screenplay for me."
                   "imperative-fusion")
                  ("Ask for honest feedback, like, tell it what you want."
                   "imperative-fusion")))
    (let* ((input (car case))
           (hazard (cadr case))
           (temp-dir (make-temp-file "whittle-report-test" t))
           (whittle/transcript-report-directory temp-dir)
           copied)
      (unwind-protect
          (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                     (lambda (text)
                       (setq copied text))))
            (whittle-test--with-text
             input
             (lambda ()
               (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
                 (whittle-transcript (point-min) (point-max))
                 (should (string= (buffer-string) input))))))
        (delete-directory temp-dir t))
      (should copied)
      (should (string-match-p "Hazard-Tagged / Not Applied" copied))
      (should (string-match-p "like-clause-fusion" copied))
      (should (string-match-p (regexp-quote hazard) copied)))))

(ert-deftest whittle-transcript-preserves-right-so-filler-chain ()
  "A `right? So...' transition should be preserved by default."
  (let* ((input "It's gonna get published, right? So… My premise is simple.")
         (temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         copied)
    (unwind-protect
        (cl-letf (((symbol-function 'whittle--copy-string-to-pbcopy)
                   (lambda (text)
                     (setq copied text))))
          (whittle-test--with-text
           input
           (lambda ()
	             (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
	               (whittle-transcript (point-min) (point-max))
	               (should (string= (buffer-string) input))))))
	      (delete-directory temp-dir t))
	    (should copied)))

(ert-deftest whittle-report-flags-and-applies-partial-hazard-entries ()
  "Hazards are recorded, while apply writes the entry's safe partial result."
  (let* ((before "That's how I use AI, right?")
         (after "That's how I use AI?")
         (passes '("sentence-edge filler removal"))
         (hazards (whittle--transcript-report-hazards before after passes)))
    (should (member "semantic-question-flip" hazards))
    (should (equal (whittle--transcript-report-risk before after passes hazards)
                   "high"))
    (should (member
             "orphan-leading-punctuation"
             (whittle--transcript-report-hazards
              "Right? I tell people this."
              "? I tell people this."
              passes)))
    (should (member
             "quote-marker-deletion"
             (whittle--transcript-report-hazards
              "Like, forget the essay for a second."
              "forget the essay for a second."
              passes)))
    (whittle-test--with-text
     "This is is a draft."
     (lambda ()
       (whittle--transcript-report-apply-entries
        (list (list :start (point-min)
                    :end (point-max)
                    :after "This is a draft."
                    :hazards hazards)))
       (should (string= (buffer-string) "This is a draft."))))))

(ert-deftest whittle-transcript-preserves-comma-function-repeats-end-to-end ()
  "The full pipeline must not collapse comma-separated function-word repeats.
Regression guard: `whittle--remove-false-starts' used to eat \"you, you\"
even after `whittle--remove-duplicated-words' was tightened."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir))
    (unwind-protect
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
                 (should
                  (string-match-p
                   (regexp-quote (cadr case))
                   (buffer-string))))))))
      (delete-directory temp-dir t))))

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

;;; Regressions from the October 2026 emoji presentation transcript ------------
;; A dry-run report was followed by `whittle-transcript-apply', which then ran
;; the passes ungated over the whole buffer: it applied edits the report had
;; held back, fused three speaker headings into the paragraphs above them, and
;; deleted a company list, a date, and a qualification.

(defconst whittle-test--emoji-transcript
  (mapconcat
   #'identity
   '("*** Jenny:"
     "Of those, like, in— this is 2015, their membership. I was like, oh, it's like 9 of the 15, like, members or 12 members were US multinational tech companies. So you had Oracle, IBM, Microsoft, Adobe, Google, Apple— what was then called Facebook, now called Meta. Um, though, like, kind of a mistake in that name, right?"
     ""
     "They're like, oh, we're going to become like a VR company, and now we're not, like, we're doing AI."
     ""
     "I was like, I'm gonna do this. And then so I joined the email list, and, um, and like, so that was maybe around August. In October, you— I got the email."
     ""
     "There are not technically a language, but let's just— we don't have a better word for it right now."
     ""
     "So the Dumpling Emoji Project, and then—"
     ""
     "*** Video narration:"
     "China has potstickers, Nepal and Tibet have momos. Yet somehow, despite their popularity—"
     ""
     "*** Jenny:"
     "I cooked a lot of things for this video."
     ""
     "*** Video narration:"
     "Emoji are global. Isn't it time we brought them together? Oh yeah, I might have—"
     ""
     "*** Jenny:"
     "And yeah, that is— and I'm literally still 2 meetings a week."
     ""
     "*** Jay:"
     "Uh, can I— so I think I still don't know."
     "")
   "\n")
  "Passages from a real transcript that the ungated apply path damaged.")

(defun whittle-test--heading-lines (text)
  "Return the org heading lines in TEXT, in order."
  (seq-filter (lambda (line) (string-match-p "\\`\\*+ " line))
              (split-string text "\n")))

(ert-deftest whittle-apply-commands-change-what-their-reports-preview ()
  "An apply command must produce exactly the text its dry run proposes."
  (dolist (case '((whittle-apply . conservative)
                  (whittle-transcript-apply . transcript)))
    (let* ((applied (whittle-test--command-output
                     whittle-test--emoji-transcript (car case)))
           (previewed
            (whittle-test--with-text
             whittle-test--emoji-transcript
             (lambda ()
               (let* ((buffer-file-name "/tmp/whittle-test-transcript.org")
                      (result (whittle--transcript-report-generate
                               (point-min) (point-max)
                               "/tmp/whittle-test-report.org" (cdr case))))
                 ;; The dry run leaves the source alone...
                 (should (string= (buffer-string)
                                  whittle-test--emoji-transcript))
                 ;; ...and its proposed text is what an apply must produce.
                 (whittle--transcript-report-apply-entries
                  (plist-get result :entries))
                 (buffer-string))))))
      (should (string= applied previewed))
      (should-not (string= applied whittle-test--emoji-transcript))
      ;; An edit the report holds back stays held back when applying.
      (should (string-match-p "now we're not, like, we're doing AI" applied)))))

(ert-deftest whittle-apply-keeps-speaker-headings-on-their-own-lines ()
  "Cleanup must never pull a heading up onto the preceding paragraph."
  (let ((headings (whittle-test--heading-lines whittle-test--emoji-transcript)))
    (dolist (em-dash '(nil t))
      (let ((whittle/collapse-em-dash-false-starts em-dash))
        (dolist (command '(whittle-apply
                           whittle-transcript-apply
                           whittle--collapse-em-dash-false-starts))
          (let ((output (whittle-test--command-output
                         whittle-test--emoji-transcript command)))
            (should (equal (whittle-test--heading-lines output) headings))
            (should-not (string-match-p "[^\n]\\*\\*\\* " output))
            ;; A dash that ends a paragraph is an interruption, not a restart.
            (should (string-match-p
                     "and then—\n\n\\*\\*\\* Video narration:" output))
            (should (string-match-p
                     "despite their popularity—\n\n\\*\\*\\* Jenny:" output))))))))

(ert-deftest whittle-em-dash-false-start-collapse-is-opt-in ()
  "By default a comma and a dash must not delete a clause."
  (let ((default (whittle-test--command-output
                  whittle-test--emoji-transcript #'whittle-transcript-apply))
        (reported (car (whittle-test--reported-command-output
                        whittle-test--emoji-transcript #'whittle-transcript))))
    (should (string= default reported))
    (dolist (kept '("So you had Oracle, IBM, Microsoft, Adobe, Google, Apple—"
                    "In October,"
                    "There are not technically a language, but let's just—"
                    "And yeah, that is—"))
      (should (string-match-p (regexp-quote kept) default)))
    ;; Ordinary filler cleanup still happens.
    (should (string-match-p "Though kind of a mistake in that name" default))
    (should-not (string-match-p "\\<um\\>" default)))
  ;; Opting in still collapses a genuine restart.
  (let* ((whittle/collapse-em-dash-false-starts t)
         (opted-in (whittle-test--command-output
                    whittle-test--emoji-transcript #'whittle-transcript-apply))
         (case-fold-search nil))
    (should (string-match-p "^And I'm literally still 2 meetings a week"
                            opted-in))
    (should-not (string-match-p "that is—" opted-in))))

(ert-deftest whittle-cleanup-keeps-paragraph-start-capitalized ()
  "Removing a paragraph-opening filler must not leave a lowercase start."
  (dolist (case '(("Uh, can I— so I think I still don't know.\n"
                   . "\\`Can I— so I think")
                  ("Um, the dog ran.\n" . "\\`The dog ran\\.")
                  ;; A paragraph that began lowercase stays lowercase.
                  ("um, every morning. next sentence\n"
                   . "\\`every morning\\. Next sentence")))
    (dolist (command '(whittle-apply whittle-transcript-apply))
      (let ((output (whittle-test--command-output (car case) command))
            (case-fold-search nil))
        (should (string-match-p (cdr case) output))))))

(ert-deftest whittle-opening-case-preserves-mixed-case-words ()
  "Opening filler removal must preserve spelling such as iPhone and eBay."
  (dolist (case '(("Um, iPhone emoji differ." . "iPhone emoji differ.")
                  ("Uh, eBay changed it." . "eBay changed it.")
                  ("Um, iOS changed it." . "iOS changed it.")))
    (dolist (command '(whittle whittle-transcript
                       whittle-apply whittle-transcript-apply))
      (let ((output (car (whittle-test--reported-command-output
                         (car case) command))))
        (should (string= output (cdr case)))))))

(ert-deftest whittle-em-dash-preserves-line-end-with-trailing-blanks ()
  "Trailing spaces or tabs must not turn an interrupted turn into a restart."
  (let ((whittle/collapse-em-dash-false-starts t)
        (fragment "So the Dumpling Emoji Project, and then—"))
    (dolist (blanks '(" " "\t" " \t "))
      (dolist (ending '("" "\n" "\r\n"
                         "\n\n*** Video narration:\nDumplings are universal.\n"))
        (let ((input (concat fragment blanks ending)))
          (dolist (command '(whittle--collapse-em-dash-false-starts
                             whittle-transcript-apply whittle-transcript))
            (let ((output (car (whittle-test--reported-command-output
                               input command))))
              (should (string-prefix-p fragment output))
              (should (equal (whittle-test--heading-lines input)
                             (whittle-test--heading-lines output))))))))))

(ert-deftest whittle-fillers-preserve-clause-punctuation ()
  "Removing fillers must keep clause separators without stranding other commas."
  (dolist (case '(("You can, um, it's very good." . "You can, it's very good.")
                  ("You can, um, uh, it's very good." . "You can, it's very good.")
                  ("They're kind of like, I don't say compound characters."
                   . "They're kind of like, I don't say compound characters.")
                  ("We're actually usually kind of like— I always use the girl."
                   . "We're actually usually kind of like— I always use the girl.")
                  ("This is, um, useful." . "This is useful.")
                  ("Um, \"iPhone\" is the name." . "\"iPhone\" is the name.")))
    (dolist (command '(whittle whittle-transcript whittle-apply
                       whittle-transcript-apply whittle/remove-filler-words))
      (let ((output (car (whittle-test--reported-command-output
                         (car case) command))))
        (should (string= output (cdr case)))))))

(ert-deftest whittle-filler-chains-preserve-punctuation-and-line-breaks ()
  "A filler chain may consume local commas, but not other delimiters."
  (dolist (case '(("You can, um, uh, it's very good." . "You can, it's very good.")
                  ("You can um, uh, it's very good." . "You can , it's very good.")
                  ("Um, uh— it's very good." . "— it's very good.")
                  ("Um, uh, \"iPhone\" is the name." . "\"iPhone\" is the name.")
                  ("Um, uh\n\n*** Jenny:\nIt's very good."
                   . "\n\n*** Jenny:\nIt's very good.")
                  ("Um,\nuh, it's very good." . "Um,\nuh, it's very good.")))
    (should (string=
             (whittle-test--command-output (car case) #'whittle--remove-filler-chains)
             (cdr case)))))

(ert-deftest whittle-prose-playground-meaning-regressions ()
  "Preserve comparisons, grammatical repeats, emphasis, and dashed restarts."
  (dolist (input '("It's kind of like that."
                   "I'm sort of of the mind that it helps."
                   "I'm never gonna ever, ever, ever want to publish."
                   "The developing world blah blah blah, and my goal is clear."
                   "Is it or is it— It might be."
                   "It's kind of like, you know, a conversation."))
    (dolist (command '(whittle-apply whittle-transcript-apply))
      (should (equal input (whittle-test--command-output input command))))))

(ert-deftest whittle-prose-playground-punctuation-regressions ()
  "Keep boundaries for lists, speech introductions, and restarts."
  (dolist (case '(("Reddit, um, YouTube, and other sources."
                   . "Reddit, YouTube, and other sources.")
                  ("Just know, like, say to your beta readers."
                   . "Just know, say to your beta readers.")
                  ("And say, like, can I get some beta readers?"
                   . "And say, can I get some beta readers?")
                  ("Because, I mean, Oh, I think that's the creativity."
                   . "Because, Oh, I think that's the creativity.")))
    (dolist (command '(whittle-apply whittle-transcript-apply))
      (should (equal (cdr case)
                     (whittle-test--command-output (car case) command))))))

(provide 'whittle-tests)

;;; whittle-tests.el ends here
