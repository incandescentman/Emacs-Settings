;;; timeline-agenda.el --- Upcoming-events view for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; A read-only "what's next" agenda over the Markdown timeline: lists the
;; upcoming dated entries on or after today, sorted chronologically, and lets
;; you jump from any line back to that date in the Calendar.

;;; Code:

(require 'seq)
(require 'timeline-core)
(require 'timeline-diary)

(defvar my-timeline-upcoming-count 15
  "Default number of upcoming diary entries shown by `my-timeline-upcoming'.")

(defun my-timeline--collect-entries ()
  "Scan `diary-file' and return every dated entry.
Each element is a plist with :abs (absolute day number), :date
\(list MONTH DAY YEAR), :string (\"M/D/YYYY\") and :bullets (list of
non-empty bullet strings)."
  (let ((buffer (find-file-noselect diary-file))
        entries)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" nil t)
          (let* ((month (string-to-number (match-string 1)))
                 (day   (string-to-number (match-string 2)))
                 (year  (string-to-number (match-string 3)))
                 (date  (list month day year))
                 (abs   (calendar-absolute-from-gregorian date))
                 (bullets nil))
            (forward-line 1)
            (while (and (not (eobp))
                        (not (looking-at "^[0-9]+/[0-9]+/[0-9]+$"))
                        (not (looking-at "^#")))
              (when (looking-at "^  - ?\\(.*\\)$")
                (let ((text (string-trim (match-string 1))))
                  (unless (string-empty-p text)
                    (push text bullets))))
              (forward-line 1))
            (push (list :abs abs :date date
                        :string (my-calendar--diary-format-date month day year)
                        :bullets (nreverse bullets))
                  entries)))))
    (nreverse entries)))

(defun my-timeline--upcoming (today-abs &optional count)
  "Return entries on or after TODAY-ABS, sorted ascending.
When COUNT is non-nil, return at most that many."
  (let ((future (seq-filter (lambda (e)
                              (and (>= (plist-get e :abs) today-abs)
                                   (plist-get e :bullets)))
                            (my-timeline--collect-entries))))
    (setq future (sort future (lambda (a b) (< (plist-get a :abs) (plist-get b :abs)))))
    (if count (seq-take future count) future)))

(defun my-timeline--relative-day-string (delta)
  "Return a human phrase for DELTA days from today."
  (cond ((= delta 0) "today")
        ((= delta 1) "tomorrow")
        (t (format "in %d days" delta))))

(defvar my-timeline-upcoming-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my-timeline-upcoming-visit)
    (define-key map (kbd "o")   #'my-timeline-upcoming-visit)
    (define-key map (kbd "g")   #'my-timeline-upcoming-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `my-timeline-upcoming-mode'.")

(define-derived-mode my-timeline-upcoming-mode special-mode "Timeline-Upcoming"
  "Major mode for the upcoming-events listing.")

(defun my-timeline-upcoming (&optional count)
  "Show upcoming timeline entries in the *Timeline Upcoming* buffer.
COUNT defaults to `my-timeline-upcoming-count'.  A numeric prefix
sets the count; a plain \\[universal-argument] shows all future entries."
  (interactive
   (list (cond ((consp current-prefix-arg) nil)
               (current-prefix-arg (prefix-numeric-value current-prefix-arg))
               (t my-timeline-upcoming-count))))
  (let* ((today (calendar-current-date))
         (today-abs (calendar-absolute-from-gregorian today))
         (entries (my-timeline--upcoming today-abs count))
         (buf (get-buffer-create "*Timeline Upcoming*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Upcoming timeline events — as of %s\n"
                        (my-calendar--describe-date today)))
        (insert "RET/o jump to date · g refresh · q quit\n\n")
        (if (null entries)
            (insert "No upcoming entries found.\n")
          (dolist (e entries)
            (let* ((date (plist-get e :date))
                   (delta (- (plist-get e :abs) today-abs))
                   (start (point)))
              (insert (format "%s %s  (%s)\n"
                              (calendar-day-name date)
                              (my-calendar--describe-date date)
                              (my-timeline--relative-day-string delta)))
              (dolist (b (plist-get e :bullets))
                (insert (format "    • %s\n" b)))
              (insert "\n")
              (put-text-property start (point) 'my-timeline-date date))))
        (goto-char (point-min)))
      (my-timeline-upcoming-mode))
    (pop-to-buffer buf)))

(defun my-timeline-upcoming-refresh ()
  "Rebuild the upcoming-events listing."
  (interactive)
  (my-timeline-upcoming my-timeline-upcoming-count))

(defun my-timeline-upcoming-visit ()
  "Open the entry on the current line in the Calendar and edit it."
  (interactive)
  (let ((date (get-text-property (point) 'my-timeline-date)))
    (unless date
      (user-error "No timeline entry on this line"))
    (calendar)
    (calendar-goto-date date)
    (when (fboundp 'my-calendar-edit-diary-entry)
      (my-calendar-edit-diary-entry))))

(provide 'timeline-agenda)

;;; timeline-agenda.el ends here
