;;; timeline-tests.el --- ERT coverage for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests that exercise diary insertion, cleanup, and cancel behaviour.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'timeline-diary)
(require 'timeline-cleanup)
(require 'timeline-search)
(require 'timeline-calendar)

(ert-deftest my-calendar-test-date-format ()
  "Ensure `my-calendar--diary-format-date` emits M/D/YYYY strings."
  (should (equal (my-calendar--diary-format-date 4 5 2024) "4/5/2024"))
  (should (equal (my-calendar--diary-format-date 12 31 1999) "12/31/1999")))

(ert-deftest my-calendar-test-ensure-blank-line-before ()
  "Ensure the blank-line helper inserts a clean separator."
  (with-temp-buffer
    (insert "10/21/2025\n  - Oct 21\n")
    (goto-char (point-max))
    (my-calendar--ensure-blank-line-before)
    (should (equal (buffer-string)
                   "10/21/2025\n  - Oct 21\n\n"))))

(ert-deftest my-calendar-test-diary-insert-entry-spacing ()
  "Verify inserting a later entry keeps headings separated."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-file diary-temp)
         content)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "# 2025\n\n## October 2025\n\n10/21/2025\n  - Oct 21\n\n")
            (write-region (point-min) (point-max) diary-temp nil 'silent))
          (my-calendar--diary-insert-entry '(10 24 2025) '("Oct 24 test"))
          (setq content
                (with-temp-buffer
                  (insert-file-contents diary-temp)
                  (buffer-string))))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))
    (should (string-prefix-p "# 2025\n\n## October 2025" content))
    (should (string-match-p
             "10/21/2025\n  - Oct 21\n\n10/24/2025\n  - Oct 24 test\n\n"
             content))))

(ert-deftest my-calendar-test-edit-diary-entry-creates-missing-date ()
  "Ensure editing a date without entries creates a new heading and bullet."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-buffer-name (file-name-nondirectory diary-temp))
         (diary-file diary-temp)
         (calendar-buffer-name calendar-buffer)
         content)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "# 2025\n\n## October 2025\n\n")
            (write-region (point-min) (point-max) diary-temp nil 'silent))
          (calendar)
          (with-current-buffer (get-buffer calendar-buffer-name)
            (calendar-goto-date '(10 22 2025))
            (my-calendar-edit-diary-entry))
          (setq content
                (with-current-buffer (find-file-noselect diary-temp)
                  (buffer-string))))
      (when (get-buffer calendar-buffer-name)
        (kill-buffer calendar-buffer-name))
      (dolist (buf '("*Diary Entries*" "*Fancy Diary Entries*"))
        (when (get-buffer buf)
          (kill-buffer buf)))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))
    (should (string-match-p
             "^# 2025\n\n## October 2025\n+10/22/2025\n  - \n\n+"
             content))))

(ert-deftest my-timeline-test-cleanup-removes-empty-entries ()
  "Ensure empty date stubs disappear during cleanup."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-buffer-name (file-name-nondirectory diary-temp))
         (diary-file diary-temp))
    (unwind-protect
        (with-current-buffer (find-file-noselect diary-temp)
          (erase-buffer)
          (insert "# 2025\n\n## October 2025\n\n10/22/2025\n  - \n\n10/23/2025\n  - Real note\n\n")
          (my-timeline--cleanup-empty-entries)
          (should (equal (buffer-string)
                         "# 2025\n\n## October 2025\n\n10/23/2025\n  - Real note\n\n")))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(ert-deftest my-timeline-test-cleanup-preserves-populated-entries ()
  "Ensure cleanup leaves non-empty entries intact."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-buffer-name (file-name-nondirectory diary-temp))
         (diary-file diary-temp))
    (unwind-protect
        (with-current-buffer (find-file-noselect diary-temp)
          (erase-buffer)
          (insert "# 2025\n\n## October 2025\n\n10/22/2025\n  - Morning meeting\n\n")
          (my-timeline--cleanup-empty-entries)
          (should (string-match-p "10/22/2025\n  - Morning meeting" (buffer-string))))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(ert-deftest my-calendar-test-cancel-current-empty-entry ()
  "Cancelling on an empty date block removes it without prompting."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-buffer-name (file-name-nondirectory diary-temp))
         (diary-file diary-temp))
    (unwind-protect
        (with-current-buffer (find-file-noselect diary-temp)
          (erase-buffer)
          (insert "# 2025\n\n## October 2025\n\n10/22/2025\n  - \n\n")
          (goto-char (point-min))
          (search-forward "10/22/2025")
          (beginning-of-line)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _) nil)))
            (my-calendar-cancel-current-entry))
          (should-not (string-match-p "10/22/2025" (buffer-string))))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(ert-deftest my-calendar-test-cancel-current-entry-respects-decline ()
  "Cancelling a populated entry should respect a negative confirmation."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-buffer-name (file-name-nondirectory diary-temp))
         (diary-file diary-temp))
    (unwind-protect
        (with-current-buffer (find-file-noselect diary-temp)
          (erase-buffer)
          (insert "# 2025\n\n## October 2025\n\n10/22/2025\n  - Keep me\n\n")
          (goto-char (point-min))
          (search-forward "10/22/2025")
          (beginning-of-line)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) nil))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _) nil)))
            (my-calendar-cancel-current-entry))
          (should (string-match-p "10/22/2025\n  - Keep me" (buffer-string))))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(provide 'timeline-tests)

;;; timeline-tests.el ends here
