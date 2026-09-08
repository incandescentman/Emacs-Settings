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
              (my-timeline-goto-date my-calendar--last-date)
              (setq my-calendar--last-date nil)
              (message "Jumped to previous date."))
            (message "No previous date stored."))
        (setq my-calendar--last-date current-date)
        (my-timeline-goto-date (calendar-current-date))
        (message "Jumped to today (M-t to return)."))))

(defconst my-calendar--help-commands
  '((my-timeline-jump . "Jump to a natural-language date")
    (my-calendar-edit-diary-entry . "Edit the selected day in the lower pane")
    (my-calendar-view-diary-entry . "Show the itinerary; keep focus in the grid")
    (my-timeline-toggle-preview . "Toggle automatic following (remembered)")
    (my-calendar-insert-diary-entry . "Add an entry; prefix opens the editor afterward")
    (my-calendar-insert-diary-entry-and-autopopulate . "Add an entry using the last text as default")
    (my-timeline-today-edit . "Jump to today and edit")
    (my-calendar-toggle-last-date . "Toggle today and the previous date")
    (my-timeline-history-back . "Go back through deliberate date jumps")
    (my-timeline-history-forward . "Go forward through date jumps")
    (my-timeline-upcoming . "Upcoming events from today; prefix changes the count")
    (my-diary-search . "Search the diary")
    (calendar-forward-month . "Next month")
    (calendar-backward-month . "Previous month")
    (calendar-forward-year . "Next year")
    (calendar-backward-year . "Previous year")
    (my-calendar-show-fancy-diary-listing . "Show the stock fancy diary listing")
    (diary-insert-entry . "Stock diary insertion")
    (calendar-exit . "Quit; restore the writing layout unless an editor is active")
    (my-calendar-help . "Show this guide"))
  "Command descriptions; displayed keys are resolved from the current keymap.")

(defun my-calendar-help ()
  "Show an Org cheat sheet whose keys come from the current keymaps."
  (interactive)
  (require 'timeline-agenda)
  (let ((buffer (my-timeline--view-buffer "*Calendar Help*" #'my-timeline-help-mode)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: Timeline calendar keys\n\n* Calendar\n\n")
        (dolist (entry my-calendar--help-commands)
          (when-let ((keys (where-is-internal (car entry) calendar-mode-map)))
            (insert "- " (mapconcat #'key-description keys ", ") " — " (cdr entry) "\n")))
        (insert "\n* Diary editor\n\n")
        (dolist (entry '((my-calendar-focus-calendar-window . "Focus the grid; keep editing paused")
                         (my-diary-return-to-calendar . "Return to the origin date and resume the itinerary")
                         (my-calendar-cancel-current-entry . "Cancel the date block; confirm if populated")
                         (my-diary-search . "Search the diary")))
          (insert "- " (mapconcat #'key-description (where-is-internal (car entry) my-diary-mode-map) ", ")
                  " — " (cdr entry) "\n"))
        (insert "\n* Capture from anywhere\n\n- M-x my-timeline-capture — date, then event; return to your current work.\n\nq closes this guide.\n")
        (goto-char (point-min))
        (org-show-all)
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)))

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
      calendar-holidays nil
      diary-file (expand-file-name my-timeline-diary-file)
      calendar-mark-diary-entries-flag t
      calendar-view-diary-initially-flag nil
      diary-display-function 'diary-fancy-display
      diary-comment-start "#"
      diary-comment-end ""
      calendar-week-start-day 1)

(defun my-calendar--disable-holiday-highlighting (&rest _args)
  "Remove calendar holiday colors so dates render like regular days."
  (when (facep 'holiday)
    (set-face-attribute 'holiday nil
                        :inherit 'default
                        :foreground nil
                        :background nil
                        :weight 'normal)))

(defun my-calendar--style-diary-face (&rest _args)
  "Render diary dates with a mint-green foreground."
  (when (facep 'diary)
    (set-face-attribute 'diary nil
                        :inherit 'default
                        :foreground "#6FCFA6"
                        :background nil
                        :weight 'normal)))

;; Apply immediately and after theme changes.
(my-calendar--disable-holiday-highlighting)
(remove-hook 'after-load-theme-hook #'my-calendar--disable-holiday-highlighting)
(add-hook 'enable-theme-functions #'my-calendar--disable-holiday-highlighting)
(add-hook 'disable-theme-functions #'my-calendar--disable-holiday-highlighting)
(my-calendar--style-diary-face)
(remove-hook 'after-load-theme-hook #'my-calendar--style-diary-face)
(add-hook 'enable-theme-functions #'my-calendar--style-diary-face)
(add-hook 'disable-theme-functions #'my-calendar--style-diary-face)

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
         (diary-buf (find-file-noselect diary-file))
         (content-window (plist-get (my-timeline--session) :content-window)))
    (if (window-live-p content-window)
        (progn
          (my-timeline--session-put :editing t)
          (set-window-buffer content-window diary-buf)
          (select-window content-window))
      (pop-to-buffer diary-buf))
    (setq my-calendar--last-window calendar-window)
    (with-current-buffer diary-buf
      (setq-local my-diary--origin-date date))
    (setq entry-pos
          (with-current-buffer diary-buf
            (widen)
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "^" (regexp-quote date-str) "$") nil t)
              (beginning-of-line)
              (point))))
    (unless entry-pos
      (message "🟡 Could not find entry for %s" date-str))
    (unless keep-calendar-selected
      (select-window calendar-window))
    entry-pos))

(defun my-diary-return-to-calendar ()
  "Return from editing to the originating date and its itinerary."
  (interactive)
  (if (not my-diary--origin-date)
      (message "No origin date stored for this diary buffer.")
    (let ((date my-diary--origin-date))
      (my-timeline-open)
      (my-timeline--session-put :editing nil)
      (calendar-goto-date date)
      (my-timeline-preview-show date))))

(defun my-calendar-view-diary-entry ()
  "Show the selected date's itinerary without disturbing a diary edit."
  (interactive)
  (my-timeline-preview-show (calendar-cursor-to-date t)))

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

;; Diary keys now belong to my-diary-mode; also retire the hook on reload.
(remove-hook 'markdown-mode-hook #'my-calendar--setup-diary-shortcuts)

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

;; A session belongs to the frame whose writing layout it temporarily replaces.
(defvar my-timeline--open-requested nil)
(defvar my-timeline--inhibit-layout nil)
(defcustom my-timeline-follow-preview t
  "Whether calendar movement updates the itinerary.  The toggle persists in Custom."
  :type 'boolean :group 'my-timeline)
(defvar my-timeline--history-back nil)
(defvar my-timeline--history-forward nil)

(defun my-timeline--session ()
  "Return the current frame's Timeline browsing session."
  (frame-parameter nil 'my-timeline-session))

(defun my-timeline--session-put (key value)
  "Set KEY to VALUE in this frame's session."
  (set-frame-parameter nil 'my-timeline-session
                       (plist-put (my-timeline--session) key value)))

(defun my-timeline-open ()
  "Open the stacked Timeline calendar, preserving the current writing layout."
  (interactive)
  (let ((my-timeline--open-requested t)) (calendar)))

(defun my-timeline--around-calendar (original &rest args)
  "Give an explicitly opened calendar a stacked itinerary and reversible layout."
  (if (or my-timeline--inhibit-layout
          (> (minibuffer-depth) 0)
          (not (or my-timeline--open-requested
                   (called-interactively-p 'any))))
      (apply original args)
    (let* ((session (my-timeline--session))
           (window (plist-get session :calendar-window)))
      (if (and (window-live-p window)
               (eq (window-buffer window) (get-buffer calendar-buffer)))
          (select-window window)
        (let ((configuration (current-window-configuration))
              (my-timeline--inhibit-layout t))
          (condition-case err
              (progn
                (apply original args)
                (delete-other-windows)
                (let* ((cal-window (selected-window))
                       (height (max 10 (+ 2 (count-lines (point-min) (point-max)))))
                       (content-window (split-window cal-window height 'below)))
                  (set-frame-parameter
                   nil 'my-timeline-session
                   (list :configuration configuration :calendar-window cal-window
                         :content-window content-window :editing nil))
                  (setq my-calendar--last-window cal-window)
                  (set-window-buffer content-window
                                     (my-timeline--view-buffer
                                      "*Timeline Itinerary*" #'my-timeline-preview-mode))
                  (set-window-margins cal-window 0 0)
                  (set-window-margins content-window 0 0)))
            (error
             (set-frame-parameter nil 'my-timeline-session nil)
             (set-window-configuration configuration)
             (signal (car err) (cdr err)))))
        ;; Opening establishes the initial view even if automatic following
        ;; was previously disabled; subsequent movement honors the toggle.
        (my-timeline-preview-show)))))

(defun my-timeline--around-calendar-exit (original &rest args)
  "Restore the pre-calendar layout, or leave an active diary editor visible."
  (let* ((session (my-timeline--session))
         (configuration (plist-get session :configuration))
         (calendar-window (plist-get session :calendar-window)))
    (if (not session)
        (apply original args)
      (set-frame-parameter nil 'my-timeline-session nil)
      (if (plist-get session :editing)
          ;; Explicitly returning from the editor ends this exception.  Merely
          ;; focusing the grid and quitting must not displace work in progress.
          (when (and (window-live-p calendar-window)
                     (not (one-window-p t)))
            (delete-window calendar-window))
        (when (window-configuration-p configuration)
          (set-window-configuration configuration))))))

(defun my-timeline--read-date (&optional date input)
  "Read an Org natural-language date, defaulting to DATE.
INPUT supplies a date string noninteractively, useful for checking parsing."
  (require 'org)
  (let* ((date (or date (calendar-current-date)))
         (org-popup-calendar-for-date-prompt nil)
         (my-timeline--inhibit-layout t)
         (time (org-read-date nil t input "Date: "
                              (encode-time 0 0 12 (nth 1 date)
                                           (car date) (nth 2 date))))
         (decoded (decode-time time)))
    (list (nth 4 decoded) (nth 3 decoded) (nth 5 decoded))))

(defun my-timeline-goto-date (date &optional history-navigation)
  "Jump deliberately to DATE, remembering the departure for Back.
HISTORY-NAVIGATION suppresses recording when traversing the history itself."
  (let ((current (calendar-cursor-to-date t)))
    (unless (or history-navigation (equal date current))
      (unless (equal current (car my-timeline--history-back))
        (push current my-timeline--history-back))
      (setq my-timeline--history-forward nil))
    (calendar-goto-date date)
    (my-timeline--follow)))

(defun my-timeline-today-edit ()
  "Jump deliberately to today and open its diary entry."
  (interactive)
  (my-timeline-goto-date (calendar-current-date))
  (my-calendar-edit-diary-entry))

(defun my-timeline-jump ()
  "Jump to an Org natural-language date, preserving the other g-prefix commands."
  (interactive)
  (my-timeline-goto-date
   (my-timeline--read-date (calendar-cursor-to-date t))))

(defun my-timeline-history-back ()
  "Return to the departure date of the last deliberate jump."
  (interactive)
  (unless my-timeline--history-back (user-error "No earlier date jump"))
  (push (calendar-cursor-to-date t) my-timeline--history-forward)
  (my-timeline-goto-date (pop my-timeline--history-back) t))

(defun my-timeline-history-forward ()
  "Move forward after returning through date-jump history."
  (interactive)
  (unless my-timeline--history-forward (user-error "No later date jump"))
  (push (calendar-cursor-to-date t) my-timeline--history-back)
  (my-timeline-goto-date (pop my-timeline--history-forward) t))

(defun my-timeline--follow ()
  "Refresh the itinerary when following, unless its window is editing the diary."
  (when (and (not my-timeline--inhibit-layout)
             my-timeline-follow-preview (my-timeline--session)
             (not (plist-get (my-timeline--session) :editing))
             (eq (current-buffer) (get-buffer calendar-buffer)))
    (my-timeline-preview-show)))

(defun my-timeline-toggle-preview ()
  "Toggle automatic following and remember the preference across restarts."
  (interactive)
  (customize-save-variable 'my-timeline-follow-preview
                           (not my-timeline-follow-preview))
  (when my-timeline-follow-preview (my-timeline--follow))
  (message "Itinerary follow %s" (if my-timeline-follow-preview "on" "off")))

(advice-add 'calendar :around #'my-timeline--around-calendar)
(advice-add 'calendar-exit :around #'my-timeline--around-calendar-exit)
(add-hook 'calendar-move-hook #'my-timeline--follow t)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today t)

(provide 'timeline-calendar)

;;; timeline-calendar.el ends here
