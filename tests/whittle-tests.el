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
    (should (string-match-p "safe partial cleanup applied; hazardous span protected" copied))
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
    (should (string-match-p "safe partial cleanup applied; hazardous span protected" copied))
    (should (string-match-p "conservative filler removal" copied))
    (should (string-match-p "This is useful" copied))))

(ert-deftest whittle-transcript-protects-held-back-spans-from-later-scars ()
  "Held-back discourse spans should protect adjacent punctuation and case."
  (let ((cases
         '(("Writing can be solitary, sitting alone, thinking, I mean, I love it, I think I'm good at it, but sharing what I know and helping other people, and having that social, interactive part. is really the part that I love even more."
            ("thinking I love" "part\\. Is really")
            ("sentence-initial-i-mean" "deleted-comma-before-pronoun" "case-clause-fusion"))
           ("So… that's how I think about it."
            ("\\`…")
            ("orphan-ellipsis"))
           ("This is a set of custom instructions, documents. that I've created..."
            ("documents\\. That I've created")
            ("case-clause-fusion"))
           ("I mean, first of all, I mean, don't you think that people can tell when you're using AI?"
            ("^first" "first of all don't" "I mean first")
            ("sentence-initial-i-mean" "deleted-comma-before-pronoun"))
           ("When I've worked for magazines, like, I was an editor at Psychology Today..."
            ("magazines I was")
            ("like-clause-fusion" "deleted-comma-before-pronoun"))
           ("It's gonna get published, right? So… My premise is that AI can help."
            ("published, My premise" "right\\? … My premise")
            ("right-so-chain" "case-clause-fusion")))))
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
           "This is is a draft.\n"
           (lambda ()
             (let ((buffer-file-name "/tmp/whittle-test-transcript.org"))
               (setq result (whittle-transcript (point-min) (point-max)))
               (should (string= (buffer-string) "This is a draft.\n"))))))
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
    (should (string-match-p "No applied high- or medium-risk changes" copied))))

(ert-deftest whittle-clipboard-report-trims-high-risk-excerpts ()
  "High-risk clipboard entries should quote only the local changed span."
  (let* ((temp-dir (make-temp-file "whittle-report-test" t))
         (whittle/transcript-report-directory temp-dir)
         (input (string-join
                 '("This opening sentence has a lot of harmless setup before"
                   "the risky phrase so the compact report should not copy"
                   "all of it. The way the assistant works is kind of like"
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
    (should (string-match-p "kind of like" copied))
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
                 "Like, forget the essay for a second.")))
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

(ert-deftest whittle-transcript-gates-so-ellipsis-and-i-mean-openers ()
  "Risky transcript openers should be reported and held back."
  (dolist (case '(("So… that's how I think about it."
                   "orphan-ellipsis")
                  ("So... If you sign up for the full version."
                   "orphan-ellipsis")
                  ("I mean, he looks basically like how I look."
                   "sentence-initial-i-mean")
                  ("Okay, thanks."
                   "lowercase-start-after-opener")
                  ("So what do you do?"
                   "lowercase-start-after-opener")))
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

(ert-deftest whittle-transcript-gates-right-so-filler-chain ()
  "A filler-chain containing `right? So...' should not be auto-applied."
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
    (should copied)
    (should (string-match-p "right-so-chain" copied))
    (should (string-match-p "case-clause-fusion" copied))))

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

(provide 'whittle-tests)

;;; whittle-tests.el ends here
