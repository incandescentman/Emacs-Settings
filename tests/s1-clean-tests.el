;;; s1-clean-tests.el --- ERT tests for s1-clean transcript cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests for the pure helpers in `s1-clean.el'. Nothing here starts the
;; local model, shells out, or touches the filesystem: these cover only the
;; output-path derivation, which must stay in step with the wrapper's own
;; `output_stem', and the buffer guard that rejects a non-file buffer.
;;
;; Run with:
;;   emacs --batch -Q -l tests/s1-clean-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(defconst s1-clean-test--source-file
  (expand-file-name
   "s1-clean.el"
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to `s1-clean.el` in this repo.")

(load-file s1-clean-test--source-file)

(ert-deftest s1-clean-output-path-strips-diarized-suffix ()
  "A `_diarized' transcript loses only that suffix."
  (should (equal (s1-clean--output-file "/tmp/Call_2026-06-05_diarized.org")
                 "/tmp/Call_2026-06-05_s1mini_cleaned.org"))
  (should (equal (s1-clean--output-file
                  "/tmp/Call_2026-06-05_elevenlabs-scribe-v2_diarized.org")
                 "/tmp/Call_2026-06-05_elevenlabs-scribe-v2_s1mini_cleaned.org")))

(ert-deftest s1-clean-output-path-strips-transcript-suffix ()
  "A plain `_transcript' file loses only that suffix."
  (should (equal (s1-clean--output-file "/tmp/Call_2026-06-05_transcript.org")
                 "/tmp/Call_2026-06-05_s1mini_cleaned.org")))

(ert-deftest s1-clean-output-path-keeps-an-unsuffixed-stem ()
  "A stem with neither suffix, or an unrelated one, is kept whole."
  (should (equal (s1-clean--output-file "/tmp/Call_2026-06-05.org")
                 "/tmp/Call_2026-06-05_s1mini_cleaned.org"))
  (should (equal (s1-clean--output-file "/tmp/Call_2026-06-05_cleaned.org")
                 "/tmp/Call_2026-06-05_cleaned_s1mini_cleaned.org")))

(ert-deftest s1-clean-output-path-stays-in-the-source-directory ()
  "The derivative is written beside its source, not in `default-directory'."
  (let ((default-directory "/"))
    (should (equal (s1-clean--output-file
                    "/Users/jay/Dropbox/github/transcription/transcriptions/A_diarized.org")
                   (concat "/Users/jay/Dropbox/github/transcription/transcriptions/"
                           "A_s1mini_cleaned.org")))))

(ert-deftest s1-clean-diff-path-matches-the-output-stem ()
  "The diff sidecar shares the output's stem and differs only in extension."
  (should (equal (s1-clean--diff-file "/tmp/Call_2026-06-05_diarized.org")
                 "/tmp/Call_2026-06-05_s1mini_cleaned.diff"))
  (should (equal (s1-clean--diff-file "/tmp/Call.org")
                 (concat (file-name-sans-extension
                          (s1-clean--output-file "/tmp/Call.org"))
                         ".diff"))))

(ert-deftest s1-clean-rejects-a-buffer-with-no-file ()
  "A buffer visiting no file cannot be cleaned."
  (with-temp-buffer
    (should-error (s1-clean--current-file) :type 'user-error)))

(ert-deftest s1-clean-rejects-a-non-org-file ()
  "Only Org transcripts are accepted; the extension check is case-insensitive."
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/notes.txt"))
      (should-error (s1-clean--current-file) :type 'user-error)))
  (with-temp-buffer
    (let ((buffer-file-name "/tmp/Call_2026-06-05_diarized.ORG"))
      (should (equal (s1-clean--current-file) "/tmp/Call_2026-06-05_diarized.ORG")))))

(provide 's1-clean-tests)

;;; s1-clean-tests.el ends here
