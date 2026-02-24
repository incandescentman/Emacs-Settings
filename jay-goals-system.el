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

(defvar jay-goals-mode-line-string ""
  "Mode-line string showing rep progress.")

(defun jay-goals--format-mode-line ()
  "Format the rep counter for mode-line display."
  (jay-goals--ensure-week-current)
  (let ((o (alist-get 'outreach jay-goals--reps))
        (ot (alist-get 'outreach jay-goals-targets))
        (f (alist-get 'followups jay-goals--reps))
        (ft (alist-get 'followups jay-goals-targets))
        (c (alist-get 'calls jay-goals--reps))
        (ct (alist-get 'calls jay-goals-targets))
        (p (alist-get 'posts jay-goals--reps))
        (pt (alist-get 'posts jay-goals-targets)))
    (format " [O:%d/%d F:%d/%d C:%d/%d P:%d/%d]"
            o ot f ft c ct p pt)))

(defun jay-goals--update-mode-line ()
  "Update the mode-line rep counter string."
  (setq jay-goals-mode-line-string (jay-goals--format-mode-line))
  (force-mode-line-update t))

(defvar jay-goals-mode-line-timer nil
  "Timer for periodic mode-line updates.")

(defun jay-goals-enable-mode-line ()
  "Add rep counter to mode-line."
  (interactive)
  (jay-goals--load-reps)
  (jay-goals--ensure-week-current)
  (jay-goals--update-mode-line)
  ;; Add to global-mode-string if not present
  (unless (member 'jay-goals-mode-line-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(jay-goals-mode-line-string))))
  ;; Update every 5 minutes to catch week changes
  (when jay-goals-mode-line-timer
    (cancel-timer jay-goals-mode-line-timer))
  (setq jay-goals-mode-line-timer
        (run-with-timer 300 300 #'jay-goals--update-mode-line))
  (message "Rep counter added to mode-line."))

(defun jay-goals-disable-mode-line ()
  "Remove rep counter from mode-line."
  (interactive)
  (setq global-mode-string
        (delete 'jay-goals-mode-line-string global-mode-string))
  (when jay-goals-mode-line-timer
    (cancel-timer jay-goals-mode-line-timer)
    (setq jay-goals-mode-line-timer nil))
  (force-mode-line-update t)
  (message "Rep counter removed from mode-line."))


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
