(defvar my-diary--origin-date nil
  "Buffer-local variable storing the calendar date from which the diary buffer was opened.")

(defvar my-calendar--current-date-string ""
  "String describing the currently highlighted date in the calendar.")
(defvar my-calendar--last-date nil
  "Stack of the last visited calendar date for toggling with today.")

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
;;; markdown-diary.el --- Calendar diary helpers -*- lexical-binding: t; -*-

;;; Author: Jay Dixit

;;; Commentary:
;; Custom calendar and diary commands shared across the Emacs configuration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'cal-move)   ;; for `calendar-month-alist` and navigation helpers

;; Ensure calendar-month-alist exists (Emacs 29+ doesn't always provide it)
(unless (boundp 'calendar-month-alist)
  (setq calendar-month-alist
        '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
          ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
          ("September" . 9) ("October" . 10) ("November" . 11)
          ("December" . 12))))


;; Disable calendar highlighting and configure diary defaults
(setq calendar-mark-holidays-flag nil
      diary-file "/Users/jay/Dropbox/github/timeless/data/jay-diary.md"
      calendar-mark-diary-entries-flag t
      calendar-view-diary-initially-flag nil
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


;; Command to return to calendar at the origin date
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

;; Renamed and clarified docstring
(defun my-calendar-show-fancy-diary-listing ()
  "Show the fancy diary listing for the date at point, then reselect the calendar window and open the Markdown diary entry for that date."
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

(defun my-calendar-edit-diary-entry ()
  "Open the diary entry for the date at point and leave focus in the diary buffer.
After jumping, move point to the end of the last bullet item for that date."
  (interactive)
  (let* ((date (calendar-cursor-to-date t))
         (entry-pos (my-calendar-jump-to-diary-entry date t)))
    (when entry-pos
      (goto-char entry-pos)
      (let ((date-line-end (line-end-position))
            (last-bullet-pos nil))
        (forward-line 1)
        ;; Skip blank lines immediately following the heading.
        (while (and (not (eobp)) (looking-at "^\\s-*$"))
          (forward-line 1))
        ;; Track the end of each bullet line.
        (while (looking-at "^  - ")
          (setq last-bullet-pos (line-end-position))
          (forward-line 1)
          ;; Allow optional blank separators inside the block without losing the last bullet.
          (while (and (not (eobp)) (looking-at "^\\s-*$"))
            (forward-line 1)))
        (goto-char (or last-bullet-pos date-line-end))))))

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
    (insert heading "\n")))

;; Improved: always guarantee a valid month region even if heading doesn't exist.
(defun my-calendar--diary-month-region (month year)
  "Ensure MONTH heading in YEAR exists and return region as (START END).
This always returns a valid region, even if the month heading did not exist before."
  (pcase-let ((`(,year-start ,year-end) (my-calendar--diary-year-region year)))
    (let* ((month-name (calendar-month-name month))
           (heading (format "## %s %d" month-name year))
           (month-regexp (concat "^" (regexp-quote heading) "$"))
           heading-pos)
      (goto-char year-start)
      (forward-line 1)
      (setq heading-pos (re-search-forward month-regexp year-end t))
      (unless heading-pos
        ;; Heading does not exist, so insert it and set heading-pos accordingly.
        (my-calendar--diary-insert-month month year year-start year-end)
        ;; After insertion, the heading will be just before the point.
        ;; Find the heading we just inserted.
        (goto-char year-start)
        (forward-line 1)
        (setq heading-pos (re-search-forward month-regexp year-end t)))
      ;; Now heading-pos is at the end of the heading line.
      ;; Start region at line after heading.
      (goto-char (match-end 0))
      (forward-line 1)
      (let ((month-start (point))
            (month-end
             (save-excursion
               (let ((limit year-end))
                 (if (re-search-forward "^\\(## \\|# \\)" limit t)
                     (match-beginning 0)
                   limit)))))
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

(defun my-calendar--ensure-blank-line-before ()
  "Ensure exactly one blank line precedes point.
Collapses any existing run of newlines and inserts two."
  (save-excursion
    (let ((end (point)))
      (skip-chars-backward "\n")
      (let ((start (point)))
        (delete-region start end)
        (insert "\n\n")))))

(defun my-calendar--ensure-blank-line-after ()
  "Ensure exactly one blank line follows point.
Removes extra blank lines and inserts two newlines."
  (save-excursion
    (let ((start (point)))
      (skip-chars-forward "\n")
      (let ((end (point)))
        (delete-region start end)
        (insert "\n\n")))))

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
                (let ((existing-day (string-to-number (match-string 2))))
                  (cond
                   ((= existing-day day)
                    (setq duplicate t)
                    (forward-line 1)
                    (while (and (not (eobp))
                                (looking-at "  - "))
                      (forward-line 1))
                    (unless (or (eobp) (looking-at "^\\s-*$"))
                      (insert "\n"))
                    (setq insert-point (point)))
                   ((> existing-day day)
                    (setq insert-point (match-beginning 0))))))
              (unless insert-point
                (setq insert-point (point-max)))
              (goto-char insert-point)
              (unless duplicate
                (my-calendar--ensure-blank-line-before)
                (insert date-line "\n"))
              (dolist (line lines)
                (insert "  - " line "\n"))
              (my-calendar--ensure-blank-line-after)))
          (widen)))
      (save-buffer))))

(defun my-calendar--insert-diary-entry (date stay-in-diary initial)
  "Insert a diary entry for DATE, seeding the minibuffer with INITIAL.
DATE defaults to the calendar date at point; STAY-IN-DIARY matches
the interactive prefix argument behaviour from the public commands."
  (let* ((stay-in-diary (not (null stay-in-diary)))
         (date (or date (calendar-cursor-to-date t)))
         (month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (prompt (format "Entry for %s: "
                         (my-calendar--describe-date date)))
         (text (read-from-minibuffer prompt initial nil nil 'my-calendar-diary-history)))
    (let ((lines (my-calendar--diary-normalize-lines text)))
      (when (null lines)
        (user-error "Diary entry cannot be empty"))
      (my-calendar--diary-insert-entry date lines)
      (my-calendar-jump-to-diary-entry date stay-in-diary)
      (message "Added diary entry for %s"
               (format "%d/%d/%d" month day year)))))

(defun my-calendar-insert-diary-entry (&optional date stay-in-diary)
  "Prompt for a diary entry and insert it for DATE with an empty minibuffer."
  (interactive (list nil current-prefix-arg))
  (my-calendar--insert-diary-entry date stay-in-diary ""))

(defun my-calendar-insert-diary-entry-and-autopopulate (&optional date stay-in-diary)
  "Prompt for a diary entry using the most recent history entry as default text."
  (interactive (list nil current-prefix-arg))
  (my-calendar--insert-diary-entry
   date stay-in-diary (or (car my-calendar-diary-history) "")))

(with-eval-after-load 'calendar
  ;; Bind both "RET" and "e" to edit the diary entry and place point at the end
  (define-key calendar-mode-map (kbd "RET") #'my-calendar-edit-diary-entry)
  (define-key calendar-mode-map (kbd "e") #'my-calendar-edit-diary-entry)
  ;; "v"/"o" display the diary entry but keep focus in the calendar
  (define-key calendar-mode-map (kbd "v") #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "o") #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "SPC") #'my-calendar-view-diary-entry)
  ;; "O" shows the fancy diary listing and then reselects the calendar
  (define-key calendar-mode-map (kbd "O") #'my-calendar-show-fancy-diary-listing)

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
  (define-key calendar-mode-map (kbd "c") #'my-calendar-insert-diary-entry)
  (define-key calendar-mode-map (kbd "C") #'my-calendar-insert-diary-entry-and-autopopulate)
  ;; Keep the original command available if needed
  (define-key calendar-mode-map (kbd "I") #'calendar-insert-diary-entry)

  ;; Make "t" jump to today, then jump to diary entry
  (define-key calendar-mode-map (kbd "t")
              (lambda ()
                (interactive)
                (calendar-goto-today)
                (my-calendar-jump-to-diary-entry))))

  ;; Bind "?" to calendar help
  (define-key calendar-mode-map (kbd "?") #'my-calendar-help)
  ;; Bind "M-t" to toggle today/last date
  (define-key calendar-mode-map (kbd "M-t") #'my-calendar-toggle-last-date)
)

(with-eval-after-load 'which-key
  (with-eval-after-load 'calendar
    (when (fboundp 'which-key-add-key-based-replacements)
      (which-key-add-key-based-replacements
        "i" "Insert diary entry"
        "c" "Insert diary entry"
        "C" "Insert diary (with default)"
        "I" "Original insert diary"
        "RET" "Edit diary entry"
        "e" "Edit diary entry"
        "v" "View diary entry"
        "o" "View diary entry"
        "SPC" "View diary entry"
        "O" "Fancy diary listing"
        "t" "Jump to today + view entry"
        "M-t" "Toggle today/last date"
        "n" "Next month"
        "M-<right>" "Next month"
        "p" "Prev month"
        "M-<left>" "Prev month"
        "N" "Next year"
        "P" "Prev year"
        "?" "Show calendar help"))))

(defun my-calendar--setup-diary-shortcuts ()
  "Install diary navigation shortcuts when editing the diary file."
  (when (and buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name diary-file)))
    (local-set-key (kbd "s-.") #'my-calendar-focus-calendar-window)
    (local-set-key (kbd "C-SPC") #'my-diary-return-to-calendar)))

(add-hook 'markdown-mode-hook #'my-calendar--setup-diary-shortcuts)


;; --- Calendar date string display in echo area ---
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

(provide 'markdown-diary)

;;; markdown-diary.el ends here
