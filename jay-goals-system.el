;;; jay-goals-system.el --- Goals system support for Jay's workflow -*- lexical-binding: t; -*-

;; Rep Counter dashboard for weekly input metrics.
;; Based on Jay's three-track strategy system

;;; Code:

;;;; ============================================================
;;;; Rep Counter Dashboard
;;;; ============================================================

(defgroup jay-goals nil
  "Jay's goals system customization."
  :group 'convenience
  :prefix "jay-goals-")

(defcustom jay-goals-reps-file
  "/Users/jay/Dropbox/roam/velocity/tracking/weekly-reps.org"
  "Path to store weekly rep counts."
  :type 'string
  :group 'jay-goals)

(defcustom jay-goals-prefix-key "s-M-g"
  "Prefix key for goals commands.
`s-g' is reserved for `isearch-repeat-forward'."
  :type 'string
  :group 'jay-goals)

;; Weekly targets
(defcustom jay-goals-targets
  '((outreach . 10)
    (followups . 15)
    (calls . 5)
    (posts . 2))
  "Weekly rep targets for each category."
  :type '(alist :key-type symbol :value-type integer)
  :group 'jay-goals)

;; Current rep counts (reset weekly)
(defvar jay-goals--reps
  '((outreach . 0)
    (followups . 0)
    (calls . 0)
    (posts . 0))
  "Current week's rep counts.")

(defvar jay-goals--week-start nil
  "ISO week number when current counts started.")

(defun jay-goals--current-week ()
  "Return current ISO week number."
  (string-to-number (format-time-string "%V")))

(defun jay-goals--ensure-week-current ()
  "Reset counts if we're in a new week."
  (let ((current-week (jay-goals--current-week)))
    (unless (equal jay-goals--week-start current-week)
      (setq jay-goals--week-start current-week)
      (setq jay-goals--reps
            '((outreach . 0)
              (followups . 0)
              (calls . 0)
              (posts . 0)))
      (jay-goals--save-reps)
      (message "New week! Rep counters reset."))))

(defun jay-goals--save-reps ()
  "Save rep counts to file."
  (let ((dir (file-name-directory jay-goals-reps-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file jay-goals-reps-file
    (insert ":PROPERTIES:\n:ID: weekly-reps-tracking\n:END:\n")
    (insert "#+TITLE: Weekly Reps Tracking\n\n")
    (insert (format "* Week %d (%s)\n\n"
                    jay-goals--week-start
                    (format-time-string "%Y")))
    (insert "| Category   | Current | Target | Progress |\n")
    (insert "|------------+---------+--------+----------|\n")
    (dolist (category '(outreach followups calls posts))
      (let ((current (alist-get category jay-goals--reps))
            (target (alist-get category jay-goals-targets)))
        (insert (format "| %-10s | %7d | %6d | %6d%% |\n"
                        (capitalize (symbol-name category))
                        current
                        target
                        (if (> target 0)
                            (min 100 (/ (* 100 current) target))
                          0)))))
    (insert "\n")))

(defun jay-goals--load-reps ()
  "Load rep counts from file if it exists and is current week."
  (when (file-exists-p jay-goals-reps-file)
    (with-temp-buffer
      (insert-file-contents jay-goals-reps-file)
      (goto-char (point-min))
      ;; Extract week number from "* Week NN"
      (when (re-search-forward "^\\* Week \\([0-9]+\\)" nil t)
        (let ((file-week (string-to-number (match-string 1))))
          (when (equal file-week (jay-goals--current-week))
            (setq jay-goals--week-start file-week)
            ;; Parse the table
            (while (re-search-forward "^| \\([A-Za-z]+\\) +| +\\([0-9]+\\)" nil t)
              (let ((category (intern (downcase (match-string 1))))
                    (count (string-to-number (match-string 2))))
                (when (assq category jay-goals--reps)
                  (setf (alist-get category jay-goals--reps) count))))))))))

(defun jay-goals-increment-rep (category &optional n)
  "Increment rep count for CATEGORY by N (default 1)."
  (jay-goals--ensure-week-current)
  (let ((current (alist-get category jay-goals--reps 0)))
    (setf (alist-get category jay-goals--reps) (+ current (or n 1))))
  (jay-goals--save-reps)
  (jay-goals--update-mode-line)
  (let* ((current (alist-get category jay-goals--reps))
         (target (alist-get category jay-goals-targets)))
    (message "%s: %d/%d %s"
             (capitalize (symbol-name category))
             current
             target
             (if (>= current target) "✓ Target reached!" ""))))

(defun jay-goals-increment-outreach ()
  "Log an outreach email."
  (interactive)
  (jay-goals-increment-rep 'outreach))

(defun jay-goals-increment-followup ()
  "Log a follow-up."
  (interactive)
  (jay-goals-increment-rep 'followups))

(defun jay-goals-increment-call ()
  "Log a call."
  (interactive)
  (jay-goals-increment-rep 'calls))

(defun jay-goals-increment-post ()
  "Log a post (LinkedIn/Substack)."
  (interactive)
  (jay-goals-increment-rep 'posts))

(defun jay-goals-show-scoreboard ()
  "Show current rep scoreboard in minibuffer."
  (interactive)
  (jay-goals--ensure-week-current)
  (message "Week %d: Outreach %d/%d | Follow-ups %d/%d | Calls %d/%d | Posts %d/%d"
           jay-goals--week-start
           (alist-get 'outreach jay-goals--reps)
           (alist-get 'outreach jay-goals-targets)
           (alist-get 'followups jay-goals--reps)
           (alist-get 'followups jay-goals-targets)
           (alist-get 'calls jay-goals--reps)
           (alist-get 'calls jay-goals-targets)
           (alist-get 'posts jay-goals--reps)
           (alist-get 'posts jay-goals-targets)))

(defun jay-goals-open-scoreboard-file ()
  "Open the weekly reps tracking file."
  (interactive)
  (jay-goals--ensure-week-current)
  (jay-goals--save-reps)
  (find-file jay-goals-reps-file))

(defun jay-goals-reset-scoreboard ()
  "Manually reset all rep counters (for new week)."
  (interactive)
  (when (yes-or-no-p "Reset all rep counters to zero? ")
    (setq jay-goals--week-start (jay-goals--current-week))
    (setq jay-goals--reps
          '((outreach . 0)
            (followups . 0)
            (calls . 0)
            (posts . 0)))
    (jay-goals--save-reps)
    (jay-goals--update-mode-line)
    (message "Rep counters reset for week %d." jay-goals--week-start)))

;;; Mode-line integration

(defcustom jay-goals-intentions-file
  "/Users/jay/Dropbox/roam/velocity/goals/20261113191152-weekly-intentions.org"
  "Org source of current weekly intentions."
  :type 'file :group 'jay-goals)

(defvar jay-goals-weekly-intentions nil
  "Cached compact labels and full wording from the intentions file.")
(defvar jay-goals-intentions-reviewed nil)
(defvar jay-goals-intentions-reminder-timer nil)
(defvar jay-goals-intentions-prompt-file
  (expand-file-name "weekly-intentions-last-prompt.txt" user-emacs-directory)
  "Persisted local calendar date of the last reminder, shared with agents.")

(defun jay-goals--read-intentions ()
  "Read the saved source, preserving the last good display on errors."
  (require 'org)
  (with-temp-buffer
    (insert-file-contents jay-goals-intentions-file)
    (let ((org-mode-hook nil)) (org-mode))
    (let* ((reviewed (cadar (org-collect-keywords '("REVIEWED"))))
           (tree (org-element-parse-buffer))
           (root (org-element-map tree 'headline
                   (lambda (h)
                     (when (and (= (org-element-property :level h) 1)
                                (equal (org-element-property :raw-value h)
                                       "Current intentions"))
                       h)) nil t))
           items)
      (unless (and reviewed
                   (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" reviewed)
                   (equal reviewed (format-time-string
                                    "%Y-%m-%d" (date-to-time (concat reviewed " 12:00:00"))))
                   root)
        (error "Weekly intentions need a valid REVIEWED date and Current intentions heading"))
      (org-element-map root 'headline
        (lambda (h)
          (when (= (org-element-property :level h) 2)
            (let ((begin (org-element-property :contents-begin h))
                  (end (org-element-property :contents-end h)))
              (unless (and begin end) (error "An intention has no full wording"))
              (push (cons (org-element-property :raw-value h)
                          (string-trim (buffer-substring-no-properties begin end))) items)))))
      (unless items (error "No current weekly intentions found"))
      (setq jay-goals-weekly-intentions (nreverse items)
            jay-goals-intentions-reviewed reviewed))))

(defun jay-goals--intention-age (&optional today)
  "Return calendar days since review, using TODAY or the local date."
  (require 'calendar)
  (let ((now (or today (format-time-string "%Y-%m-%d"))))
    (- (time-to-days (date-to-time (concat now " 12:00:00")))
       (time-to-days (date-to-time (concat jay-goals-intentions-reviewed " 12:00:00"))))))

(defun jay-goals--intentions-reminder-due-p (&optional today)
  "Whether more than seven days have passed without today's reminder."
  (let ((date (or today (format-time-string "%Y-%m-%d"))))
    (and jay-goals-intentions-reviewed
         (> (jay-goals--intention-age date) 7)
         (not (equal date
                     (when (file-readable-p jay-goals-intentions-prompt-file)
                       (with-temp-buffer
                         (insert-file-contents jay-goals-intentions-prompt-file)
                         (string-trim (buffer-string)))))))))

(defun jay-goals-open-intentions ()
  "Open the canonical weekly intentions."
  (interactive)
  (find-file jay-goals-intentions-file))

(defun jay-goals-review-intentions ()
  "Show current intentions and a nonblocking invitation to review them."
  (interactive)
  (jay-goals--read-intentions)
  (with-current-buffer (get-buffer-create "*Weekly intentions*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Weekly intentions\n\n")
      (insert (format "Last reviewed: %s\n\n" jay-goals-intentions-reviewed))
      (dolist (item jay-goals-weekly-intentions)
        (insert (cdr item) "\n\n"))
      (insert "What would you like to carry forward, change, or add?\nTell Codex, or ")
      (insert-text-button "edit the intentions file"
                          'action (lambda (_) (jay-goals-open-intentions)))
      (insert ".\n\nIf these still fit, ")
      (insert-text-button "keep these intentions for another week"
                          'action (lambda (_) (jay-goals-confirm-intentions)))
      (insert ".\n")
      (special-mode))
    (display-buffer (current-buffer)))
  (with-temp-file jay-goals-intentions-prompt-file
    (insert (format-time-string "%Y-%m-%d") "\n")))

(defun jay-goals-confirm-intentions ()
  "Explicitly reaffirm the current saved intentions and reset their age."
  (interactive)
  (let ((buf (find-file-noselect jay-goals-intentions-file)))
    (with-current-buffer buf
      (when (or (buffer-modified-p) (not (verify-visited-file-modtime buf)))
        (user-error "Save or reconcile the intentions file before confirming"))
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^#\\+REVIEWED: .*" nil t)
          (user-error "Missing REVIEWED date"))
        (replace-match (concat "#+REVIEWED: " (format-time-string "%Y-%m-%d")) t t)
        (save-buffer))))
  (jay-goals--update-mode-line)
  (message "Weekly intentions reaffirmed for another week."))

(defun jay-goals--maybe-remind-intentions ()
  "Prompt once per local day when overdue and Emacs has focus."
  (when (and (frame-focus-state) (not (active-minibuffer-window)))
    (condition-case err
        (progn
          (jay-goals--read-intentions)
          (when (jay-goals--intentions-reminder-due-p)
            (jay-goals-review-intentions)))
      (error (message "Weekly intentions: %s" (error-message-string err))))))

(defun jay-goals--intentions-after-save ()
  "Refresh the mode line when the source is saved in Emacs."
  (when (and buffer-file-name
             (file-equal-p buffer-file-name jay-goals-intentions-file))
    (jay-goals--update-mode-line)))

(defvar jay-goals-mode-line-string ""
  "Mode-line string showing weekly intentions.")

(defun jay-goals--format-mode-line ()
  "Format weekly intentions with their full wording on hover."
  (if jay-goals-weekly-intentions
      (propertize
       (concat " [" (mapconcat #'car jay-goals-weekly-intentions " · ") "]")
       'help-echo
       (concat "Weekly intentions\n\n"
               (mapconcat #'cdr jay-goals-weekly-intentions "\n\n")))
    ""))

(defun jay-goals--update-mode-line ()
  "Update the mode-line weekly intentions string."
  (condition-case err
      (jay-goals--read-intentions)
    (error (message "Weekly intentions: %s" (error-message-string err))))
  (setq jay-goals-mode-line-string (jay-goals--format-mode-line))
  (force-mode-line-update t))

(defvar jay-goals-mode-line-timer nil
  "Timer for periodic mode-line updates.")

(defun jay-goals-enable-mode-line ()
  "Add weekly intentions to the mode-line."
  (interactive)
  (jay-goals--update-mode-line)
  ;; Add to global-mode-string if not present
  (unless (member 'jay-goals-mode-line-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(jay-goals-mode-line-string))))
  ;; Refresh periodically after intention settings change.
  (when jay-goals-mode-line-timer
    (cancel-timer jay-goals-mode-line-timer))
  (setq jay-goals-mode-line-timer
        (run-with-timer 300 300 #'jay-goals--update-mode-line))
  (when jay-goals-intentions-reminder-timer
    (cancel-timer jay-goals-intentions-reminder-timer))
  (setq jay-goals-intentions-reminder-timer
        (run-with-idle-timer 60 t #'jay-goals--maybe-remind-intentions))
  (add-hook 'after-save-hook #'jay-goals--intentions-after-save)
  (message "Weekly intentions added to mode-line."))

(defun jay-goals-disable-mode-line ()
  "Remove weekly intentions from the mode-line."
  (interactive)
  (setq global-mode-string
        (delete 'jay-goals-mode-line-string global-mode-string))
  (when jay-goals-mode-line-timer
    (cancel-timer jay-goals-mode-line-timer)
    (setq jay-goals-mode-line-timer nil))
  (force-mode-line-update t)
  (when jay-goals-intentions-reminder-timer
    (cancel-timer jay-goals-intentions-reminder-timer)
    (setq jay-goals-intentions-reminder-timer nil))
  (remove-hook 'after-save-hook #'jay-goals--intentions-after-save)
  (message "Weekly intentions removed from mode-line."))


;;;; ============================================================
;;;; Keybindings
;;;; ============================================================

(defvar jay-goals-command-map
  (let ((map (make-sparse-keymap)))
    ;; Rep counter
    (define-key map "1" #'jay-goals-increment-outreach)
    (define-key map "2" #'jay-goals-increment-followup)
    (define-key map "3" #'jay-goals-increment-call)
    (define-key map "4" #'jay-goals-increment-post)
    (define-key map "s" #'jay-goals-show-scoreboard)
    (define-key map "S" #'jay-goals-open-scoreboard-file)
    (define-key map "r" #'jay-goals-reset-scoreboard)
    map)
  "Keymap for goals rep counter commands.")

;; Which-key descriptions
(with-eval-after-load 'which-key
  (let ((p jay-goals-prefix-key))
    (which-key-add-key-based-replacements
      p                 "goals"
      (concat p " 1")   "outreach +1"
      (concat p " 2")   "follow-up +1"
      (concat p " 3")   "call +1"
      (concat p " 4")   "post +1"
      (concat p " s")   "show scoreboard"
      (concat p " S")   "open scoreboard file"
      (concat p " r")   "reset scoreboard")))


;;;; ============================================================
;;;; Initialization
;;;; ============================================================

(defun jay-goals-setup ()
  "Set up the goals system."
  (interactive)
  ;; Load saved reps
  (jay-goals--load-reps)
  (jay-goals--ensure-week-current)
  ;; Enable mode-line
  (jay-goals-enable-mode-line)
  ;; Bind keymap
  (when (boundp 'key-minor-mode-map)
    ;; Preserve legacy search muscle memory.
    (define-key key-minor-mode-map (kbd "s-g") #'isearch-repeat-forward)
    (define-key key-minor-mode-map (kbd jay-goals-prefix-key) jay-goals-command-map))
  (message "Goals rep counter ready. Use %s for rep commands." jay-goals-prefix-key))

(provide 'jay-goals-system)

;;; jay-goals-system.el ends here
