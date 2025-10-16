;;; timeline-core.el --- Core helpers for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared variables and utilities used across the timeline modules.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'cal-move)
(require 'rx)

(defvar my-diary--origin-date nil
  "Buffer-local variable storing the calendar date from which the diary buffer was opened.")

(defvar my-calendar--current-date-string ""
  "String describing the currently highlighted date in the calendar.")

(defvar my-calendar--last-date nil
  "Stack of the last visited calendar date for toggling with today.")

(defvar my-timeline--suspend-cleanup nil
  "When non-nil, skip auto-cleanup of empty diary entries.")

(defvar my-timeline--cleanup-skip-date nil
  "When non-nil, date string that cleanup should ignore.")

(defun my-calendar--diary-format-date (month day year)
  "Return the canonical diary date string for MONTH, DAY and YEAR."
  (format "%d/%d/%d" month day year))

(defun my-calendar--diary-insert-year (year)
  "Insert a new YEAR heading in `diary-file' in chronological order."
  (let* ((pattern "^# \\([0-9]+\\)$")
         (insert-point (point-max)))
    (goto-char (point-min))
    (cl-loop while (re-search-forward pattern nil t)
             for existing = (string-to-number (match-string 1))
             when (> existing year)
             do (setq insert-point (match-beginning 0))
             finally (goto-char insert-point))
    (unless (or (bobp) (looking-back "\n" 1))
      (insert "\n"))
    (unless (looking-at "\n")
      (insert "\n"))
    (insert (format "# %d\n\n" year))))

(defun my-calendar--diary-year-region (year)
  "Ensure YEAR heading exists and return its region as (START END)."
  (let ((year-regexp (format "^# %d$" year)))
    (goto-char (point-min))
    (unless (re-search-forward year-regexp nil t)
      (my-calendar--diary-insert-year year)
      (goto-char (point-min))
      (re-search-forward year-regexp nil t))
    (let ((start (match-beginning 0))
          (end (save-excursion
                 (goto-char (match-end 0))
                 (if (re-search-forward "^# [0-9]+$" nil t)
                     (match-beginning 0)
                   (point-max)))))
      (list start end))))

(defun my-calendar--diary-insert-month (month year year-start year-end)
  "Insert MONTH heading for YEAR between YEAR-START and YEAR-END."
  (let* ((month-name (calendar-month-name month))
         (heading (format "## %s %d" month-name year))
         (pattern "^## \\([[:alpha:]]+\\) [0-9]+$"))
    (goto-char year-start)
    (forward-line 1)
    (let ((insert-point year-end))
      (cl-loop while (re-search-forward pattern year-end t)
               for existing-name = (match-string 1)
               for existing-month = (cdr (assoc-string existing-name calendar-month-alist t))
               when (and existing-month (> existing-month month))
               do (setq insert-point (match-beginning 0))
               finally (goto-char insert-point)))
    (unless (or (bobp) (looking-back "\n" 1))
      (insert "\n"))
    (unless (looking-at "\n")
      (insert "\n"))
    (insert heading)
    (insert "\n\n")))

(defun my-calendar--diary-month-region (month year)
  "Return the narrowed region containing MONTH in YEAR."
  (save-excursion
    (pcase-let ((`(,year-start ,year-end) (my-calendar--diary-year-region year))
                (month-regexp (format "^## %s %d$" (calendar-month-name month) year)))
      (save-restriction
        (narrow-to-region year-start year-end)
        (goto-char (point-min))
        (unless (re-search-forward month-regexp nil t)
          (goto-char (point-min))
          (my-calendar--diary-insert-month month year year-start year-end)
          (widen)
          (pcase-let ((`(,year-start ,year-end) (my-calendar--diary-year-region year)))
            (narrow-to-region year-start year-end)
            (goto-char (point-min))
            (re-search-forward month-regexp nil t)))
        (goto-char (match-end 0))
        (forward-line 1)
        (let ((month-start (point))
              (month-end
               (save-excursion
                 (if (re-search-forward "^\\(## \\|# \\)" nil t)
                     (match-beginning 0)
                   (point-max)))))
          (list month-start month-end))))))

(defun my-calendar--diary-normalize-lines (text)
  "Split TEXT into trimmed bullet lines, dropping blanks."
  (let (lines)
    (dolist (line (split-string text "\n"))
      (setq line (string-trim line))
      (unless (string-empty-p line)
        (push line lines)))
    (nreverse lines)))

(defun my-calendar--describe-date (date)
  "Return a readable string for DATE (list MONTH DAY YEAR)."
  (let ((month (nth 0 date))
        (day   (nth 1 date))
        (year  (nth 2 date)))
    (format "%s %d, %d" (calendar-month-name month) day year)))

(defun my-calendar--diary-entry-exists-p (date)
  "Return non-nil if an entry for DATE (list MONTH DAY YEAR) exists."
  (let* ((month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (date-line (my-calendar--diary-format-date month day year))
         (buffer (find-file-noselect diary-file)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward
         (concat "^" (regexp-quote date-line) "$") nil t)))))

(defun my-calendar--ensure-blank-line-before ()
  "Ensure exactly one blank line precedes point.
Collapses any existing run of newlines and inserts two."
  (let ((start (point)))
    (while (and (> start (point-min))
                (eq (char-before start) ?\n))
      (setq start (1- start)))
    (delete-region start (point))
    (insert "\n\n")))

(defun my-calendar--ensure-blank-line-after ()
  "Ensure exactly one blank line follows point.
Removes extra blank lines and inserts two newlines."
  (save-excursion
    (let ((start (point)))
      (skip-chars-forward "\n")
      (let ((end (point)))
        (delete-region start end)
        (insert "\n\n")))))

(provide 'timeline-core)

;;; timeline-core.el ends here
