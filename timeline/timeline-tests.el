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

(defmacro my-timeline-test-with-real-diary-copy (&rest body)
  "Run BODY against an in-memory copy of the configured diary, never saving it."
  (declare (indent 0))
  `(let ((source-file diary-file)
         (source (generate-new-buffer " *Timeline test diary*")))
     (unwind-protect
         (progn
           (with-current-buffer source
             (insert-file-contents source-file)
             (set-buffer-modified-p nil))
           (cl-letf (((symbol-function 'find-file-noselect)
                      (lambda (file &rest _)
                        (unless (equal (expand-file-name file)
                                       (expand-file-name source-file))
                          (error "Unexpected test file: %s" file))
                        source)))
             ,@body))
       (kill-buffer source))))

(ert-deftest my-timeline-test-real-source-cache-and-narrowing ()
  "Caching preserves point/restriction and refreshes after unsaved character edits."
  (my-timeline-test-with-real-diary-copy
    (with-current-buffer source
      (goto-char (point-max))
      (let ((position (point))
            (raw (my-timeline--collect-entries)))
        (save-restriction
          (narrow-to-region (point) (point))
          (let ((first (my-timeline--data)))
            (should (equal (car first) raw))
            (should (eq first (my-timeline--data)))
            (should (= (point-min) position))
            (should (= (point) position))
            (widen)
            ;; Reuse a real source event; the copy is deliberately unsaved.
            (goto-char (point-min))
            (re-search-forward "^  - .+")
            (insert " [edited]")
            (should-not (eq first (my-timeline--data)))))))))

(ert-deftest my-timeline-test-real-itinerary-range-and-source-contract ()
  "Every visible ordinary bullet and paired range derives from the real diary."
  (my-timeline-test-with-real-diary-copy
    (let* ((data (my-timeline--data))
           (range (car (cadr data))))
      (should range)
      (let* ((start (plist-get (plist-get range :start-entry) :abs))
             (end (plist-get (plist-get range :end-entry) :abs))
             (selected (min end (1+ start)))
             (items (my-timeline--preview-items selected 7 21))
             (active (seq-find (lambda (item)
                                (equal (plist-get item :description)
                                       (plist-get range :start-bullet))) items)))
        (should active)
        (should (= (plist-get active :sort-abs) selected))
        (dolist (item items)
          (if (eq (plist-get item :kind) 'range)
              (should (seq-some (lambda (entry)
                                 (member (plist-get item :description)
                                         (plist-get entry :bullets))) (car data)))
            (should (<= (- selected 7) (plist-get item :sort-abs) (+ selected 21)))
            (dolist (bullet (plist-get item :bullets))
              (should (seq-some (lambda (entry)
                                 (and (equal (plist-get entry :date) (plist-get item :date))
                                      (member bullet (plist-get entry :bullets)))) (car data))))))
        (with-temp-buffer
          (my-timeline-preview-mode)
          (my-timeline--render-preview (calendar-gregorian-from-absolute selected))
          (should buffer-read-only)
          (should-not (buffer-modified-p))
          (should (string-match-p "ongoing on selected date\\|last day on selected date" (buffer-string)))
          (should (string-match-p (regexp-quote (plist-get range :start-bullet)) (buffer-string)))
          (should-not (string-match-p "^[─_]+$\\|^ *• " (buffer-string))))))))

(ert-deftest my-timeline-test-view-initializes-org-once ()
  "Refreshing either generated view must not repeatedly run Org's hook chain."
  (let ((calls 0) (name " *Timeline view test*"))
    (unwind-protect
        (let ((org-mode-hook (list (lambda () (cl-incf calls)))))
          (my-timeline--view-buffer name #'my-timeline-preview-mode)
          (my-timeline--view-buffer name #'my-timeline-preview-mode)
          (should (= calls 1))
          (with-current-buffer name
            (should buffer-read-only)
            (should disable-olivetti-auto-toggle)))
      (kill-buffer name))))

(ert-deftest my-timeline-test-natural-date-and-history ()
  "An absolute natural date is parsed; arrows do not populate deliberate history."
  (should (equal (my-timeline--read-date '(9 8 2026) "oct 12 2026") '(10 12 2026)))
  (let ((my-timeline--history-back nil) (my-timeline--history-forward nil))
    (save-window-excursion
      (calendar)
      (calendar-goto-date '(9 8 2026))
      (my-timeline-goto-date '(10 12 2026))
      (calendar-forward-day 1)
      (should (equal my-timeline--history-back '((9 8 2026))))
      (my-timeline-history-back)
      (should (equal (calendar-cursor-to-date t) '(9 8 2026)))
      (my-timeline-history-forward)
      (should (equal (calendar-cursor-to-date t) '(10 13 2026))))))

(ert-deftest my-timeline-test-capture-preserves-unsaved-work ()
  "Capture appends to a real date in memory without saving existing unsaved work."
  (my-timeline-test-with-real-diary-copy
    (let* ((entry (car (my-timeline--collect-entries)))
           (date (plist-get entry :date))
           (text (car (plist-get entry :bullets)))
           (original-window (selected-window)))
      (with-current-buffer source
        (goto-char (point-min))
        (set-buffer-modified-p t)
        (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) (ert-fail "Unexpected save"))))
          (my-timeline-capture date text))
        (should (= (point) (point-min)))
        (should (buffer-modified-p)))
      (should (eq original-window (selected-window)))
      (should (= 2 (cl-count text (plist-get (car (my-timeline--collect-entries)) :bullets)
                            :test #'equal))))))

(ert-deftest my-timeline-test-stacked-session-edit-and-quit ()
  "A real diary copy is untouched by following; editing pauses it; q restores layout."
  (my-timeline-test-with-real-diary-copy
    (let ((configuration (current-window-configuration))
          (my-timeline-follow-preview t)
          (initial (with-current-buffer source (buffer-string))))
      (unwind-protect
          (progn
            (set-frame-parameter nil 'my-timeline-session nil)
            (my-timeline-open)
            (let* ((session (my-timeline--session))
                   (cal (plist-get session :calendar-window))
                   (pane (plist-get session :content-window))
                   (entry (seq-find (lambda (e) (plist-get e :bullets))
                                    (my-timeline--collect-entries))))
              (should (= (car (window-edges cal)) (car (window-edges pane))))
              (should (< (nth 1 (window-edges cal)) (nth 1 (window-edges pane))))
              (calendar-goto-date (plist-get entry :date))
              (my-calendar-edit-diary-entry)
              (let ((position (point)))
                (select-window cal)
                (calendar-forward-day 1)
                (my-calendar-view-diary-entry)
                (should (eq (window-buffer pane) source))
                (should (= position (with-current-buffer source (point)))))
              (select-window pane)
              (my-diary-return-to-calendar)
              (should (eq (window-buffer pane) (get-buffer "*Timeline Itinerary*")))
              (calendar-exit)
              (should-not (my-timeline--session))
              (should (compare-window-configurations configuration (current-window-configuration)))
              (should (equal initial (with-current-buffer source (buffer-string))))
              (should-not (buffer-modified-p source))))
        (set-frame-parameter nil 'my-timeline-session nil)
        (set-window-configuration configuration)))))

(ert-deftest my-timeline-test-follow-off-manual-preview-and-editing-quit ()
  "Following can pause without stale entry setup; quitting never hides an editor."
  (my-timeline-test-with-real-diary-copy
    (let ((configuration (current-window-configuration))
          (my-timeline-follow-preview nil))
      (unwind-protect
          (progn
            (set-frame-parameter nil 'my-timeline-session nil)
            (my-timeline-open)
            (let* ((pane (plist-get (my-timeline--session) :content-window))
                   (cal (plist-get (my-timeline--session) :calendar-window))
                   (initial-date (with-current-buffer (window-buffer pane) my-timeline-preview--date))
                   (entry (seq-find (lambda (e) (plist-get e :bullets)) (my-timeline--collect-entries))))
              (calendar-forward-day 1)
              (should (equal initial-date (with-current-buffer (window-buffer pane) my-timeline-preview--date)))
              (my-calendar-view-diary-entry)
              (should (equal (calendar-cursor-to-date t)
                             (with-current-buffer (window-buffer pane) my-timeline-preview--date)))
              (calendar-goto-date (plist-get entry :date))
              (my-calendar-edit-diary-entry)
              (select-window cal)
              (calendar-exit)
              (should-not (my-timeline--session))
              (should (get-buffer-window source))
              (should-not (get-buffer-window calendar-buffer))))
        (set-frame-parameter nil 'my-timeline-session nil)
        (set-window-configuration configuration)))))

(ert-deftest my-timeline-test-empty-day-next-and-extension ()
  "Empty days have an absolute Next date; extension keeps the selected date."
  (my-timeline-test-with-real-diary-copy
    (let* ((entry (car (sort (copy-sequence (my-timeline--collect-entries))
                             (lambda (a b) (< (plist-get a :abs) (plist-get b :abs))))))
           (date (calendar-gregorian-from-absolute (1- (plist-get entry :abs)))))
      (with-temp-buffer
        (my-timeline-preview-mode)
        (setq my-timeline-preview--date date)
        (my-timeline--render-preview date)
        (should (string-match-p "No entries. Press i to add one." (buffer-string)))
        (should (string-match-p (regexp-quote (concat "Next: " (my-timeline--date-label (plist-get entry :date)))) (buffer-string)))
        (my-timeline-preview-later)
        (should (= my-timeline-preview--after 49))
        (my-timeline-preview-earlier)
        (should (= my-timeline-preview--before 35))
        (should (equal my-timeline-preview--date date))))))

(ert-deftest my-timeline-test-private-date-prompt-preserves-layout ()
  "An inhibited or programmatic stock calendar does not start a Timeline session."
  (let ((my-timeline--inhibit-layout t)
        called)
    (my-timeline--around-calendar (lambda (&rest _) (setq called t)))
    (should called))
  (let ((my-timeline--open-requested nil)
        called)
    (my-timeline--around-calendar (lambda (&rest _) (setq called t)))
    (should called)))

(provide 'timeline-tests)

;;; timeline-tests.el ends here
