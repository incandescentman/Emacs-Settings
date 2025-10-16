;;; timeline-calendar.el --- Calendar integration for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Calendar-facing helpers, hooks, and window management for the timeline.

;;; Code:

(require 'timeline-core)

(defun my-calendar-toggle-last-date ()
  "Toggle between today and the last visited calendar date.
If already on today, jump back to the previous date.
Otherwise, save the current date and jump to today."
  (interactive)
  (let ((current-date (calendar-cursor-to-date t)))
    (if (equal current-date (calendar-current-date))
        (if my-calendar--last-date
            (progn
              (calendar-goto-date my-calendar--last-date)
              (setq my-calendar--last-date nil)
              (message "Jumped to previous date."))
          (message "No previous date stored."))
      (setq my-calendar--last-date current-date)
      (calendar-goto-today)
      (message "Jumped to today (M-t to return)."))))

(defun my-calendar-help ()
  "Display a popup buffer with a cheat sheet of custom Calendar keybindings."
  (interactive)
  (let ((buf (get-buffer-create "*Calendar Help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (concat
          "# 📅 Custom Calendar Keybindings Cheat Sheet\n\n"
          "| Key         | Action                                    |\n"
          "|-------------|-------------------------------------------|\n"
          "| i, c        | Insert new diary entry                     |\n"
          "| C           | Insert diary entry (with last as default)  |\n"
          "| I           | Original `calendar-insert-diary-entry`     |\n"
          "| RET, e      | Edit diary entry at point                  |\n"
          "| v, o, SPC   | View diary entry at point                  |\n"
          "| O           | Show fancy diary listing for date          |\n"
          "| t           | Jump to today and view diary entry         |\n"
          "| M-t         | Toggle between today and previous date     |\n"
          "| n, M-→      | Next month                                 |\n"
          "| p, M-←      | Previous month                             |\n"
          "| N           | Next year                                  |\n"
          "| P           | Previous year                              |\n"
          "| s-.         | (In diary) Focus calendar window           |\n"
          "| ?           | Show this help popup                       |\n"
          "\n"
          "Press `q` to quit this help.\n"))
        (goto-char (point-min))
        (view-mode 1)
        (local-set-key (kbd "q") #'quit-window)))
    (pop-to-buffer buf)))

;; Ensure calendar-month-alist exists (Emacs 29+ doesn't always provide it)
(unless (boundp 'calendar-month-alist)
  (setq calendar-month-alist
        '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
          ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
          ("September" . 9) ("October" . 10) ("November" . 11)
          ("December" . 12))))

;; Calendar defaults used by the timeline workflow.
(setq calendar-mark-holidays-flag nil
      calendar-holiday-marker 'default
      diary-file "/Users/jay/Dropbox/github/timeless/data/timeline.md"
      calendar-mark-diary-entries-flag t
      calendar-view-diary-initially-flag nil
      diary-display-function 'diary-fancy-display
      diary-comment-start "#"
      diary-comment-end ""
      calendar-week-start-day 1)

(defun my-calendar--disable-holiday-highlighting ()
  "Remove calendar holiday colors so dates render like regular days."
  (when (facep 'holiday)
    (set-face-attribute 'holiday nil
                        :inherit 'default
                        :foreground nil
                        :background nil
                        :weight 'normal)))

;; Apply immediately and after theme changes.
(my-calendar--disable-holiday-highlighting)
(add-hook 'after-load-theme-hook #'my-calendar--disable-holiday-highlighting)

(setq calendar-month-header
      '(propertize
        (format "%s %d" (calendar-month-name month) year)
        'font-lock-face 'calendar-month-header))

(defvar my-calendar--last-window nil
  "Remember the last window showing the calendar buffer.")

(defun my-calendar--close-diary-display ()
  "Remove any fancy diary display windows so layouts stay stable."
  (dolist (buffer '("*Fancy Diary Entries*" "*Diary Entries*"))
    (when-let ((buf (get-buffer buffer)))
      (dolist (win (get-buffer-window-list buf nil t))
        (when (window-live-p win)
          (condition-case nil
              (delete-window win)
            (error nil)))))))

(defun my-calendar--remember-window (&rest _args)
  "Record the current window as the active calendar window."
  (when (and (string= (buffer-name) calendar-buffer)
             (window-live-p (selected-window)))
    (setq my-calendar--last-window (selected-window))))

(add-hook 'calendar-mode-hook #'my-calendar--remember-window)
(add-hook 'calendar-move-hook #'my-calendar--remember-window)

(defun my-calendar-jump-to-diary-entry (&optional date keep-calendar-selected)
  "Open `diary-file` at DATE (list MONTH DAY YEAR) and return its position.
When KEEP-CALENDAR-SELECTED is nil, restore focus to the calendar window.
If the date is missing, return nil after displaying a warning.
Also set buffer-local `my-diary--origin-date` in the diary buffer."
  (interactive)
  (let* ((calendar-window (selected-window))
         (date (or date (calendar-cursor-to-date t)))
         (month (number-to-string (nth 0 date)))
         (day   (number-to-string (nth 1 date)))
         (year  (number-to-string (nth 2 date)))
         (date-str (format "%s/%s/%s" month day year))
         entry-pos
         (diary-buf (find-file-other-window diary-file)))
    (setq my-calendar--last-window calendar-window)
    (with-current-buffer diary-buf
      (setq-local my-diary--origin-date date))
    (setq entry-pos
          (with-current-buffer diary-buf
            (when (progn (goto-char (point-min))
                         (search-forward date-str nil t))
              (beginning-of-line)
              (point))))
    (unless entry-pos
      (message "🟡 Could not find entry for %s" date-str))
    (unless keep-calendar-selected
      (select-window calendar-window))
    entry-pos))

(defun my-diary-return-to-calendar ()
  "Return to the calendar and jump to the date from which this diary buffer was opened."
  (interactive)
  (if (not my-diary--origin-date)
      (message "No origin date stored for this diary buffer.")
    (let ((date my-diary--origin-date))
      (calendar)
      (calendar-goto-date date)
      (message "Returned to calendar at %s" (my-calendar--describe-date date)))))

(defun my-calendar-view-diary-entry ()
  "Display the diary entry for the date at point while staying in the calendar."
  (interactive)
  (my-calendar-jump-to-diary-entry (calendar-cursor-to-date t) nil)
  (my-calendar--close-diary-display))

(defun my-calendar-show-fancy-diary-listing ()
  "Show the fancy diary listing for the date at point, then reselect the calendar window.
After showing the listing, jump to the Markdown diary entry for that date."
  (interactive)
  (let* ((calendar-window (selected-window))
         (date (calendar-cursor-to-date t)))
    (diary-view-entries nil)
    (select-window calendar-window)
    (my-calendar-jump-to-diary-entry date)))

(defun my-calendar-focus-calendar-window ()
  "Return focus to the live calendar window without changing its date."
  (interactive)
  (my-calendar--close-diary-display)
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

(defun my-calendar--setup-diary-shortcuts ()
  "Install diary navigation shortcuts when editing the diary file."
  (when (and buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name diary-file)))
    (local-set-key (kbd "s-.") #'my-calendar-focus-calendar-window)
    (local-set-key (kbd "C-SPC") #'my-diary-return-to-calendar)
    (local-set-key (kbd "C-c C-c") #'my-diary-return-to-calendar)))

(add-hook 'markdown-mode-hook #'my-calendar--setup-diary-shortcuts)

(defun my-calendar--update-date-display ()
  "Update `my-calendar--current-date-string` and display it in the echo area."
  (let* ((date (calendar-cursor-to-date t))
         (dayname (calendar-day-name date))
         (monthname (calendar-month-name (nth 0 date)))
         (day (nth 1 date))
         (year (nth 2 date))
         (str (format "%s — %d %s %d" dayname day monthname year)))
    (setq my-calendar--current-date-string str)
    (message "%s" str)))

(add-hook 'calendar-move-hook #'my-calendar--update-date-display)

(provide 'timeline-calendar)

;;; timeline-calendar.el ends here
