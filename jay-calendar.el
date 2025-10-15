;;; jay-calendar.el --- Calendar diary helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom calendar and diary commands shared across the Emacs configuration.

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'subr-x)

;; Disable calendar highlighting and configure diary defaults
(setq calendar-mark-holidays-flag nil
      diary-file "/Users/jay/Dropbox/github/timeless/jay-diary.md"
      calendar-mark-diary-entries-flag t
      calendar-view-diary-initially-flag t
      diary-display-function 'diary-fancy-display
      diary-comment-start "#"
      diary-comment-end ""
      calendar-week-start-day 1)

(setq calendar-month-header
      '(propertize
        (format "%s %d" (calendar-month-name month) year)
        'font-lock-face 'calendar-month-header))

(defvar my-calendar--last-window nil
  "Remember the last window showing the calendar buffer.")

(defun my-calendar--remember-window (&rest _args)
  "Record the current window as the active calendar window."
  (when (and (string= (buffer-name) calendar-buffer)
             (window-live-p (selected-window)))
    (setq my-calendar--last-window (selected-window))))

(add-hook 'calendar-mode-hook #'my-calendar--remember-window)
(add-hook 'calendar-move-hook #'my-calendar--remember-window)

(defun my-calendar-jump-to-diary-entry (&optional date keep-calendar-selected)
  "Open `diary-file` at DATE (list MONTH DAY YEAR).
When KEEP-CALENDAR-SELECTED is nil, restore focus to the calendar window."
  (interactive)
  (let* ((calendar-window (selected-window))
         (date (or date (calendar-cursor-to-date t)))
         (month (number-to-string (nth 0 date)))
         (day   (number-to-string (nth 1 date)))
         (year  (number-to-string (nth 2 date)))
         (date-str (format "%s/%s/%s" month day year)))
    (setq my-calendar--last-window calendar-window)
    (find-file-other-window diary-file)
    (goto-char (point-min))
    (if (search-forward date-str nil t)
        (beginning-of-line)
      (message "🟡 Could not find entry for %s" date-str))
    (unless keep-calendar-selected
      (select-window calendar-window))))

(defun my-calendar-view-and-edit-entry ()
  "Show the fancy diary listing and open the Markdown entry for the date at point."
  (interactive)
  (let* ((calendar-window (selected-window))
         (date (calendar-cursor-to-date t)))
    (diary-view-entries nil)
    (select-window calendar-window)
    (my-calendar-jump-to-diary-entry date)))

(defun my-calendar-focus-calendar-window ()
  "Return focus to the live calendar window without changing its date."
  (interactive)
  (let* ((calendar-buffer (get-buffer calendar-buffer))
         (window (and (window-live-p my-calendar--last-window)
                      (eq (window-buffer my-calendar--last-window) calendar-buffer)
                      my-calendar--last-window)))
    (unless (and (window-live-p window)
                 (eq (window-buffer window) calendar-buffer))
      (setq window (get-buffer-window calendar-buffer t)))
    (if (and window (window-live-p window))
        (select-window window)
      (user-error "No active calendar window to focus"))))

(defun my-calendar-edit-diary-entry ()
  "Open the diary entry for the date at point and leave focus in the diary buffer."
  (interactive)
  (my-calendar-jump-to-diary-entry (calendar-cursor-to-date t) t))

(defvar my-calendar-diary-history nil
  "Minibuffer history for `my-calendar-insert-diary-entry'.")

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
    (unless (bolp)
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
    (unless (bolp)
      (insert "\n"))
    (insert heading "\n")))

(defun my-calendar--diary-month-region (month year)
  "Ensure MONTH heading in YEAR exists and return region as (START END)."
  (pcase-let ((`(,year-start ,year-end) (my-calendar--diary-year-region year)))
    (let* ((month-name (calendar-month-name month))
           (heading (format "## %s %d" month-name year))
           (month-regexp (concat "^" (regexp-quote heading) "$")))
      (goto-char year-start)
      (forward-line 1)
      (unless (re-search-forward month-regexp year-end t)
        (my-calendar--diary-insert-month month year year-start year-end)
        (pcase-let ((`(,year-start* ,year-end*) (my-calendar--diary-year-region year)))
          (setq year-start year-start*
                year-end year-end*)
          (goto-char year-start)
          (forward-line 1)
          (re-search-forward month-regexp year-end t)))
      (forward-line 1)
      (let ((month-start (point))
            (month-end (save-excursion
                         (if (re-search-forward "^\\(## \\|# \\)" year-end t)
                             (match-beginning 0)
                           year-end))))
        (list month-start month-end)))))

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

(defun my-calendar--diary-insert-entry (date lines)
  "Insert diary entry for DATE (MONTH DAY YEAR) using LINES."
  (let* ((month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (date-line (my-calendar--diary-format-date month day year))
         (buffer (find-file-noselect diary-file)))
    (with-current-buffer buffer
      (save-excursion
        (pcase-let ((`(,month-start ,month-end) (my-calendar--diary-month-region month year)))
          (save-restriction
            (narrow-to-region month-start month-end)
            (goto-char (point-min))
            (let ((insert-point nil)
                  (duplicate nil))
              (while (and (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" nil t)
                          (not insert-point))
                (let ((existing-month (string-to-number (match-string 1)))
                      (existing-day (string-to-number (match-string 2)))
                      (existing-year (string-to-number (match-string 3))))
                  (cond
                   ((and (= existing-year year)
                         (= existing-month month)
                         (= existing-day day))
                    (setq duplicate t)
                    (forward-line 1)
                    (while (and (not (eobp))
                                (looking-at "  - "))
                      (forward-line 1))
                    (setq insert-point (point)))
                   ((> existing-day day)
                    (setq insert-point (match-beginning 0))))))
              (unless insert-point
                (setq insert-point (point-max)))
              (goto-char insert-point)
              (unless duplicate
                (unless (bolp)
                  (insert "\n"))
                (insert date-line "\n"))
              (dolist (line lines)
                (insert "  - " line "\n"))
              (unless (looking-at "\n")
                (insert "\n"))))
          (widen)))
      (save-buffer))))

(defun my-calendar-insert-diary-entry (&optional date)
  "Prompt for a diary entry and insert it for DATE.
DATE defaults to the date at point in the calendar."
  (interactive)
  (let* ((date (or date (calendar-cursor-to-date t)))
         (month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (prompt (format "Entry for %s: "
                         (my-calendar--describe-date date)))
         (text (read-from-minibuffer prompt nil nil nil 'my-calendar-diary-history)))
    (let ((lines (my-calendar--diary-normalize-lines text)))
      (when (null lines)
        (user-error "Diary entry cannot be empty"))
      (my-calendar--diary-insert-entry date lines)
      (my-calendar-jump-to-diary-entry date t)
      (message "Added diary entry for %s"
               (format "%d/%d/%d" month day year)))))

(with-eval-after-load 'calendar
  ;; Make "RET" show diary entries
  (define-key calendar-mode-map (kbd "RET") #'my-calendar-view-and-edit-entry)
  ;; Alternate binding that jumps to the diary and leaves point there
  (define-key calendar-mode-map (kbd "e") #'my-calendar-edit-diary-entry)

  ;; Month / year navigation shortcuts
  (define-key calendar-mode-map (kbd "n")
    (lambda ()
      (interactive)
      (calendar-forward-month 1)))
  (define-key calendar-mode-map (kbd "M-<right>")
    (lambda ()
      (interactive)
      (calendar-forward-month 1)))
  (define-key calendar-mode-map (kbd "p")
    (lambda ()
      (interactive)
      (calendar-backward-month 1)))
  (define-key calendar-mode-map (kbd "M-<left>")
    (lambda ()
      (interactive)
      (calendar-backward-month 1)))
  (define-key calendar-mode-map (kbd "N")
    (lambda ()
      (interactive)
      (calendar-forward-year 1)))
  (define-key calendar-mode-map (kbd "P")
    (lambda ()
      (interactive)
      (calendar-backward-year 1)))

  ;; Add smarter diary entry insertion that keeps Markdown chronologically sorted
  (define-key calendar-mode-map (kbd "i") #'my-calendar-insert-diary-entry)
  ;; Keep the original command available if needed
  (define-key calendar-mode-map (kbd "I") #'calendar-insert-diary-entry)

  ;; Make "o" jump to the date under cursor
  (define-key calendar-mode-map (kbd "o") #'my-calendar-jump-to-diary-entry)

  ;; Make "t" jump to today, then jump to diary entry
  (define-key calendar-mode-map (kbd "t")
    (lambda ()
      (interactive)
      (calendar-goto-today)
      (my-calendar-jump-to-diary-entry))))

(defun my-calendar--setup-diary-shortcuts ()
  "Install diary navigation shortcuts when editing the diary file."
  (when (and buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name diary-file)))
    (local-set-key (kbd "s-.") #'my-calendar-focus-calendar-window)))

(add-hook 'markdown-mode-hook #'my-calendar--setup-diary-shortcuts)

(provide 'jay-calendar)

;;; jay-calendar.el ends here
