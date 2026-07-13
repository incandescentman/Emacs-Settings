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
(require 'timeline-agenda)

(ert-deftest my-calendar-test-date-format ()
  "Ensure `my-calendar--diary-format-date` emits M/D/YYYY strings."
  (should (equal (my-calendar--diary-format-date 4 5 2024) "4/5/2024"))
  (should (equal (my-calendar--diary-format-date 12 31 1999) "12/31/1999")))

(ert-deftest my-timeline-test-custom-diary-file-updates-diary-file ()
  "Customizing `my-timeline-diary-file` should update `diary-file`."
  (let ((original-option (default-value 'my-timeline-diary-file))
        (original-diary diary-file)
        (target (expand-file-name "custom-timeline.md" temporary-file-directory)))
    (unwind-protect
        (progn
          (my-timeline--set-diary-file 'my-timeline-diary-file target)
          (should (equal my-timeline-diary-file target))
          (should (equal diary-file target)))
      (set-default 'my-timeline-diary-file original-option)
      (setq diary-file original-diary))))

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

(ert-deftest my-timeline-test-upcoming-filters-and-sorts ()
  "Upcoming returns only entries on/after today, sorted ascending."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-file diary-temp)
         (diary-buffer-name (file-name-nondirectory diary-temp)))
    (unwind-protect
        (progn
          (with-temp-buffer
            ;; Deliberately out of file order to exercise the sort.
            (insert "# 2026\n\n## October 2026\n\n"
                    "10/5/2026\n  - Later thing\n\n"
                    "10/2/2026\n  - Rumpus reunion\n\n"
                    "## September 2026\n\n"
                    "9/1/2026\n  - Past thing\n\n")
            (write-region (point-min) (point-max) diary-temp nil 'silent))
          (let ((up (my-timeline--upcoming
                     (calendar-absolute-from-gregorian '(10 3 2026)))))
            (should (equal (mapcar (lambda (e) (plist-get e :string)) up)
                           '("10/5/2026"))))
          (let ((up (my-timeline--upcoming
                     (calendar-absolute-from-gregorian '(9 1 2026)))))
            (should (equal (mapcar (lambda (e) (plist-get e :string)) up)
                           '("9/1/2026" "10/2/2026" "10/5/2026")))
            (should (equal (plist-get (car up) :bullets) '("Past thing")))))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(ert-deftest my-timeline-test-upcoming-count-limits ()
  "A COUNT argument caps the number of upcoming entries returned."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-file diary-temp)
         (diary-buffer-name (file-name-nondirectory diary-temp)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "# 2026\n\n## October 2026\n\n"
                    "10/2/2026\n  - a\n\n"
                    "10/5/2026\n  - b\n\n"
                    "10/9/2026\n  - c\n\n")
            (write-region (point-min) (point-max) diary-temp nil 'silent))
          (let ((up (my-timeline--upcoming
                     (calendar-absolute-from-gregorian '(1 1 2026)) 2)))
            (should (= (length up) 2))
            (should (equal (plist-get (car up) :string) "10/2/2026"))
            (should (equal (plist-get (cadr up) :string) "10/5/2026"))))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(ert-deftest my-timeline-test-agenda-collapses-active-range ()
  "A paired multi-day event should appear once with an ongoing status."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-file diary-temp)
         (diary-buffer-name (file-name-nondirectory diary-temp))
         (today-abs (calendar-absolute-from-gregorian '(10 3 2026))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "# 2026\n\n## October 2026\n\n"
                    "10/2/2026\n"
                    "  - FRIDAY - SUN, OCT 2-4. Rumpus 50th anniversary reunion. new haven\n\n"
                    "10/4/2026\n"
                    "  - Last day of Rumpus 50th anniversary reunion\n\n"
                    "10/5/2026\n"
                    "  - Dentist\n\n")
            (write-region (point-min) (point-max) diary-temp nil 'silent))
          (let* ((items (my-timeline--agenda-items today-abs))
                 (range (seq-find (lambda (item)
                                    (eq (plist-get item :kind) 'range))
                                  items)))
            (should (= (length items) 2))
            (should (equal (plist-get range :title)
                           "Rumpus 50th anniversary reunion"))
            (should (= (plist-get range :sort-abs) today-abs))
            (should (equal (plist-get range :date) '(10 2 2026)))
            (should (equal (my-timeline--range-status range today-abs)
                           "Ongoing · ends tomorrow"))
            (should-not
             (seq-some
              (lambda (item)
                (member "Last day of Rumpus 50th anniversary reunion"
                        (plist-get item :bullets)))
              items))))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(ert-deftest my-timeline-test-agenda-keeps-unpaired-last-day-marker ()
  "An unpaired final-boundary marker should remain a normal agenda entry."
  (let* ((diary-temp (make-temp-file "timeline-test" nil ".md"))
         (diary-file diary-temp)
         (diary-buffer-name (file-name-nondirectory diary-temp))
         (today-abs (calendar-absolute-from-gregorian '(10 3 2026))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "# 2026\n\n## October 2026\n\n"
                    "10/4/2026\n"
                    "  - Last day of Mystery Festival\n\n")
            (write-region (point-min) (point-max) diary-temp nil 'silent))
          (let ((items (my-timeline--agenda-items today-abs)))
            (should (= (length items) 1))
            (should (eq (plist-get (car items) :kind) 'entry))
            (should (equal (plist-get (car items) :bullets)
                           '("Last day of Mystery Festival")))))
      (when (get-buffer diary-buffer-name)
        (kill-buffer diary-buffer-name))
      (when (file-exists-p diary-temp)
        (delete-file diary-temp)))))

(ert-deftest my-timeline-test-agenda-sections-use-monday-first-week ()
  "Agenda items should split into Today, This week, and Later."
  (let* ((today-abs (calendar-absolute-from-gregorian '(10 5 2026)))
         (week-end-abs (my-timeline--week-end-absolute today-abs))
         (today-item (list :sort-abs today-abs))
         (week-item (list :sort-abs (+ today-abs 3)))
         (later-item (list :sort-abs (+ today-abs 8))))
    (should (equal (calendar-gregorian-from-absolute week-end-abs)
                   '(10 11 2026)))
    (should (eq (my-timeline--agenda-section
                 today-item today-abs week-end-abs)
                'today))
    (should (eq (my-timeline--agenda-section
                 week-item today-abs week-end-abs)
                'this-week))
    (should (eq (my-timeline--agenda-section
                 later-item today-abs week-end-abs)
                'later))))

(provide 'timeline-tests)

;;; timeline-tests.el ends here
