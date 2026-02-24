;;; jay-org-roam-core.el --- Org-roam core: resilient, lazy, and fast  -*- lexical-binding: t; -*-

;;; Commentary:
;; Core module (no templates). Pairs with `jay-org-roam-profiles.el`.
;; - Maximally lazy: no blocking (require 'org-roam) at init.
;; - Two-phase idle setup, safe DB advice, leader-key, helpers, bindings.
;; - Profile system integration for multi-database support.
;; - Templates live in profiles module.

;;; Code:

;; Optional debug switch for org-roam startup diagnostics.
;; Keep this nil for normal use to avoid noisy startup logs.
(defvar jay/org-roam-debug nil
  "When non-nil, emit extra org-roam startup diagnostics.")

;; CRITICAL: Set org-roam-directory FIRST, before anything else
;; This prevents org-roam from ever seeing the CloudStorage File Provider path
(defvar org-roam-directory)  ; Forward declaration
(setq org-roam-directory "/Users/jay/Dropbox/roam")

;; Prevent file-truename from resolving Dropbox paths to CloudStorage
;; MUST be installed before org-roam loads
(defun jay/prevent-dropbox-cloudstorage-resolution (orig-fn filename &rest args)
  "Prevent file-truename from resolving Dropbox paths to CloudStorage.
If FILENAME starts with /Users/jay/Dropbox, return it as-is without resolution."
  (if (and filename
           (stringp filename)
           (string-prefix-p "/Users/jay/Dropbox" (expand-file-name filename)))
      ;; Return the path as-is without calling file-truename
      (expand-file-name filename)
    ;; For other paths, call the original function
    (apply orig-fn filename args)))

(advice-add 'file-truename :around #'jay/prevent-dropbox-cloudstorage-resolution)

;; Keep any other filesystem helpers away from the macOS File Provider path.
(defconst jay/dropbox-direct-root "/Users/jay/Dropbox"
  "Direct filesystem mount for Dropbox.")

(defconst jay/dropbox-cloudstorage-root "/Users/jay/Library/CloudStorage/Dropbox"
  "macOS File Provider path for Dropbox that times out when accessed from Emacs.")

(defun jay/dropbox--rewrite-cloudstorage-path (path)
  "Return PATH rewritten to the direct Dropbox mount when needed."
  (let ((expanded (and path (expand-file-name path))))
    (if (and expanded
             (string-prefix-p jay/dropbox-cloudstorage-root expanded))
        (concat jay/dropbox-direct-root
                (substring expanded (length jay/dropbox-cloudstorage-root)))
      expanded)))

(defun jay/dropbox--make-directory-wrapper (orig-fn directory &rest args)
  "Reroute `make-directory' away from iCloud/Dropbox File Provider paths."
  (let* ((expanded (and directory (expand-file-name directory)))
         (rewritten (jay/dropbox--rewrite-cloudstorage-path expanded)))
    (condition-case err
        (apply orig-fn rewritten args)
      (file-error
       (if (and (not (equal expanded rewritten))
                (file-directory-p rewritten))
           ;; Directory already exists via the direct mount—treat as success.
           nil
         (signal (car err) (cdr err)))))))

(advice-add 'make-directory :around #'jay/dropbox--make-directory-wrapper)

;; Set org-roam-directory directly here, before any profile loading
(setq org-roam-directory "/Users/jay/Dropbox/roam")

;; Optional local fixes (loaded only if present).
;; During migration, fixes may live either beside this file (suite dir)
;; or one level up in emacs-settings/.
(defun jay/org-roam--load-optional-fix (filename)
  "Load optional org-roam fix FILENAME from suite dir or parent dir."
  (when load-file-name
    (let* ((suite-dir (file-name-directory load-file-name))
           (parent-dir (expand-file-name ".." suite-dir))
           (suite-path (expand-file-name filename suite-dir))
           (parent-path (expand-file-name filename parent-dir))
           (candidate (cond
                       ((file-exists-p suite-path) suite-path)
                       ((file-exists-p parent-path) parent-path)
                       (t nil))))
      (if candidate
          (load candidate t t)
        (message "org-roam: optional fix missing: %s (checked %s and %s)"
                 filename suite-path parent-path)))))

(when load-file-name
  (jay/org-roam--load-optional-fix "org-roam-id-fix.el")
  (jay/org-roam--load-optional-fix "org-roam-db-fix.el"))

;; Load the profile system
(require 'jay-org-roam-profiles)

;; Lazy-safe wrappers -----------------------------------------------------------
(defmacro jay/with-org-roam (&rest body)
  "Require `org-roam' lazily, then eval BODY."
  (declare (indent 0) (debug t))
  `(progn (unless (featurep 'org-roam) (require 'org-roam)) ,@body))

(defmacro jay/bind-roam (key command &optional no-wrap)
  "Bind KEY to COMMAND in `jay/super-u-map`. Wrap in lazy require unless NO-WRAP."
  `(define-key jay/super-u-map (kbd ,key)
               (lambda () (interactive)
                 ,(if no-wrap
                      `(call-interactively #',command)
                      `(jay/with-org-roam (call-interactively #',command))))))

;; Leader key ------------------------------------------------------------------
(define-prefix-command 'jay/super-u-map)
(global-set-key (kbd "s-u") 'jay/super-u-map)

;; Core org-roam settings (set before package load) ----------------------------
(with-eval-after-load 'org
  (setq org-roam-db-autosync-mode nil
        ;; Use direct path to avoid resolving Dropbox symlink to CloudStorage
        org-roam-directory "/Users/jay/Dropbox/roam"
        org-roam-database-connector 'sqlite-builtin
        org-roam-directory-exclude-regexp "^documents/"
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag))
        org-roam-dailies-directory "journal/"
        org-roam-file-exclude-regexp "\\.git/\\|attachments/\\|\\.org~$\\|#.*#$"
        org-roam-db-location (expand-file-name "org-roam.db" (xdg-cache-home))
        org-roam-db-update-method 'idle
        org-roam-dailies-capture-templates (copy-tree jay/org-roam-dailies-template-default))
  (message "DEBUG core: org-roam-directory set to %s" org-roam-directory)

  ;; staged setup for speed
  (run-with-idle-timer
   1 nil
   (lambda ()
     (condition-case err
         (jay/with-org-roam
          (require 'ol)
          ;; Initialize profile system and load saved profile FIRST
          ;; This ensures org-roam-directory is set correctly before org-roam-setup
          (jay/org-roam-profiles-init)
          ;; Now setup org-roam with the correct directory
          (org-roam-setup))
       (error (message "Initial org-roam setup error: %s" (error-message-string err))))))

  (run-with-idle-timer
   5 nil
   (lambda ()
     (condition-case err
         (jay/with-org-roam
          (let ((cache-warm (jay/org-roam-db-cache-healthy-p)))
            (setq jay/org-roam--skip-next-sync cache-warm)
            (when jay/org-roam-debug
              (message "⮡ enabling org-roam autosync … (cache warm: %s)" cache-warm)))
          (when (and (boundp 'org-roam-db) (not (emacsql-live-p org-roam-db)))
            (setq org-roam-db nil)
            (org-roam-db))
          (org-roam-db-autosync-mode 1))
       (error (message "Failed to enable autosync: %s" (error-message-string err)))))))

;; Safety wrappers for DB -------------------------------------------------------
(defun jay/org-roam--safe-update (orig-fn &rest args)
  (condition-case err (apply orig-fn args)
    (error (message "org-roam skipped %s (%s)" (or (car args) "<buffer>") (error-message-string err)))))
(advice-add 'org-roam-db-update-file :around #'jay/org-roam--safe-update)

(defun jay/org-roam--safe-query (orig-fn &rest args)
  (condition-case err (apply orig-fn args)
    (error (message "org-roam query skipped: %s" (error-message-string err)) nil)))
(with-eval-after-load 'org-roam
  (advice-add 'org-roam-db-query :around #'jay/org-roam--safe-query))

(defvar jay/org-roam--skip-next-sync nil
  "Internal flag to skip the next org-roam-db-sync call when cache is fresh.")

(defconst jay/org-roam-db-min-rows 100
  "Minimum number of indexed files required to consider the cache healthy.")

(defun jay/org-roam-db-cache-healthy-p ()
  "Return non-nil when the org-roam DB already has indexed files."
  (when (and (boundp 'org-roam-db-location)
             org-roam-db-location)
    (let ((db-file (expand-file-name org-roam-db-location)))
      (when (file-exists-p db-file)
        (condition-case err
            (progn
              (org-roam-db) ; ensure connection exists
              (let* ((rows (org-roam-db-query [:select (funcall count *) :from files]))
                     (count (and rows (caar rows))))
                (when jay/org-roam-debug
                  (message "org-roam cache probe: %s indexed files" count))
                (and count (>= count jay/org-roam-db-min-rows))))
          (error
           (message "org-roam cache probe failed: %s" (error-message-string err))
           nil))))))

(defun jay/org-roam--skip-initial-sync (orig-fn &optional force)
  "Skip the next automatic `org-roam-db-sync' when the cache is healthy.
Only effective when `jay/org-roam--skip-next-sync' is non-nil and FORCE is nil."
  (if (and jay/org-roam--skip-next-sync
           (not force))
      (progn
        (setq jay/org-roam--skip-next-sync nil)
        (message "org-roam: skipped initial db sync (cache already warm)"))
    (prog1 (funcall orig-fn force)
      (setq jay/org-roam--skip-next-sync nil))))

(with-eval-after-load 'org-roam-db
  (advice-add 'org-roam-db-sync :around #'jay/org-roam--skip-initial-sync))

(defun jay/patch-emacsql-close (connection &rest _)
  "Prevent `emacsql-close' if CONNECTION handle is nil."
  (when (ignore-errors (oref connection handle)) t))
(advice-add 'emacsql-close :before-while #'jay/patch-emacsql-close)

;; Custom refile (region-or-subtree) -------------------------------------------
(defun org-roam-refile-region-or-subtree ()
  "Refile region if active, else subtree, into an Org-roam node."
  (interactive)
  (jay/with-org-roam
   (let* ((regionp (org-region-active-p))
          (region-start (and regionp (region-beginning)))
          (region-end   (and regionp (region-end)))
          (node (org-roam-node-read nil nil nil 'require-match))
          (file (org-roam-node-file node))
          (nbuf (or (find-buffer-visiting file) (find-file-noselect file)))
          level reversed)
     (if (equal (org-roam-node-at-point) node)
         (user-error "Target is the same as current node")
         (if regionp
             (progn (kill-new (buffer-substring-no-properties region-start region-end))
                    (org-save-markers-in-region region-start region-end))
             (when (org-before-first-heading-p) (org-roam-demote-entire-buffer))
             (org-copy-subtree 1 nil t))
         (with-current-buffer nbuf
           (org-with-wide-buffer
            (goto-char (org-roam-node-point node))
            (setq level (org-get-valid-level (funcall outline-level) 1)
                  reversed (org-notes-order-reversed-p))
            (goto-char (if reversed (or (outline-next-heading) (point-max))
                           (or (save-excursion (org-get-next-sibling)) (org-end-of-subtree t t) (point-max))))
            (unless (bolp) (newline))
            (if regionp (insert (current-kill 0)) (org-paste-subtree level nil nil t))
            (when (bound-and-true-p org-auto-align-tags)
              (let ((org-loop-over-headlines-in-active-region nil)) (org-align-tags)))
            (when (fboundp 'deactivate-mark) (deactivate-mark))))
         (if regionp (progn (goto-char region-end) (delete-region region-start region-end))
             (org-preserve-local-variables
              (delete-region (and (org-back-to-heading t) (point))
                             (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
         (when (eq (buffer-size) 0)
           (when (buffer-file-name) (delete-file (buffer-file-name)))
           (set-buffer-modified-p nil)
           (when (and (bound-and-true-p org-capture-mode) (buffer-base-buffer (current-buffer)))
             (org-capture-kill))
           (kill-buffer (current-buffer)))))))

;; Helpers ---------------------------------------------------------------------
(defun jay/org-roam-yesterday ()
  (interactive)
  (condition-case nil
      (progn (when (fboundp 'jay/save-some-buffers) (jay/save-some-buffers))
             (jay/with-org-roam (org-roam-dailies-goto-previous-note)))
    (error (message "Failed to go to yesterday's note"))))

(defun jay/org-roam-backlinks-buffer ()
  (interactive)
  (jay/with-org-roam (org-roam-buffer-toggle))
  (other-window 1))

(defun jay/org-roam-search-nodes ()
  (interactive)
  (let ((dir (if (boundp 'org-roam-directory) org-roam-directory "/Users/jay/Dropbox/roam")))
    (cond ((fboundp 'consult-ripgrep) (consult-ripgrep dir))
          ((fboundp 'counsel-rg) (counsel-rg nil dir nil nil))
          (t (user-error "Need consult or counsel for ripgrep")))))

(defun jay/org-roam-create-sequence-next ()
  (interactive) (insert "- Next :: ") (jay/with-org-roam (org-roam-node-insert)))
(defun jay/org-roam-create-sequence-previous ()
  (interactive) (insert "- Previous :: ") (jay/with-org-roam (org-roam-node-insert)))

(defun insert-colon () (interactive) (insert ":"))

;; Goals workflow helpers ------------------------------------------------------
(defconst jay/goals-hub-id "20251201T155922.490384"
  "Org-roam ID for the goals navigation hub.")

(defconst jay/goals-execution-dashboard-id "20260114-socratic-ai-execution"
  "Org-roam ID for the execution dashboard.")

(defconst jay/goals-avoiding-id "20251201T155810.623762"
  "Org-roam ID for the anti-stall avoidance list.")

(defconst jay/goals-daily-pipeline-template-lines
  '("* Daily Pipeline"
    "- [ ] OBT :: "
    "- [ ] MIT 1 :: "
    "- [ ] MIT 2 :: "
    "- [ ] Reach-out 1 (Track 1) :: "
    "- [ ] Reach-out 2 (Track 1) :: "
    "- [ ] Visibility Move (Track 2) :: "
    "- [ ] Health Action :: ")
  "Template inserted into a daily note by `jay/goals-start-day'.")

(defvar jay/anti-stall-default-minutes 2
  "Default timer length for `jay/anti-stall-now'.")

(defvar jay/anti-stall--timer nil
  "Internal timer object for active anti-stall sprint.")

(defcustom jay/prolific-root
  "/Users/jay/Dropbox/github/writing-trackers/prolific"
  "Path to the prolific workspace."
  :type 'string
  :group 'applications)

(defcustom jay/prolific-pomo-command
  "bin/pomo"
  "Path to prolific pom command, relative to `jay/prolific-root'."
  :type 'string
  :group 'applications)

(defvar jay/prolific-pomo-process nil
  "Active prolific pom process, when running.")

(defvar jay/prolific-pomo-buffer-name "*jay-prolific-pomo*"
  "Buffer used for prolific pom command output.")

(defvar jay/revenue-block-default-sprints 3
  "Default number of prolific sprints for `jay/revenue-block-start'.")

(defun jay/goals--find-node-by-id (id)
  "Return org-roam node for ID, or nil when not found."
  (jay/with-org-roam (org-roam-node-from-id id)))

(defun jay/goals--display-id (id)
  "Display Org-roam note for ID without stealing focus."
  (let ((node (jay/goals--find-node-by-id id)))
    (if node
        (let ((buf (find-file-noselect (org-roam-node-file node))))
          (with-current-buffer buf
            (goto-char (org-roam-node-point node)))
          (display-buffer buf)
          t)
      (message "Goals helper: missing note id %s" id)
      nil)))

(defun jay/goals--open-id (id &optional other-window)
  "Visit Org-roam note for ID.
When OTHER-WINDOW is non-nil, open in another window."
  (let ((node (jay/goals--find-node-by-id id)))
    (unless node
      (user-error "Could not find Org-roam note for id %s" id))
    (jay/with-org-roam
     (org-roam-node-visit node other-window))))

(defun jay/goals--today-daily-buffer ()
  "Return today's org-roam daily buffer."
  (save-window-excursion
    (jay/with-org-roam (org-roam-dailies-goto-today))
    (current-buffer)))

(defun jay/goals--goto-or-create-heading (heading)
  "Move point to HEADING in current org buffer, creating it when absent."
  (goto-char (point-min))
  (if (re-search-forward (format "^\\*+ %s\\b" (regexp-quote heading)) nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "\n* %s\n" heading))
    (forward-line -1)
    (beginning-of-line)))

(defun jay/goals--append-daily-log (heading text)
  "Append TEXT under HEADING in today's daily note."
  (let ((daily-buffer (jay/goals--today-daily-buffer)))
    (with-current-buffer daily-buffer
      (save-excursion
        (jay/goals--goto-or-create-heading heading)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert text "\n"))
      (save-buffer))))

(defun jay/goals--ensure-daily-pipeline ()
  "Ensure today's daily note contains a Daily Pipeline section.
Return non-nil when the section was inserted."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\*+ Daily Pipeline\\b" nil t)
        nil
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n" (mapconcat #'identity jay/goals-daily-pipeline-template-lines "\n") "\n")
      (save-buffer)
      t)))

(defun jay/goals-start-day ()
  "Open the three key goals buffers and seed today's Daily Pipeline."
  (interactive)
  (jay/with-org-roam (org-roam-dailies-goto-today))
  (let* ((daily-buffer (current-buffer))
         (inserted (jay/goals--ensure-daily-pipeline)))
    (jay/goals--display-id jay/goals-execution-dashboard-id)
    (jay/goals--display-id jay/goals-hub-id)
    (switch-to-buffer daily-buffer)
    (message "Goals day setup complete%s."
             (if inserted " (inserted Daily Pipeline template)" ""))))

(defun jay/anti-stall--timer-finished (minutes)
  "Handle completion of anti-stall timer for MINUTES."
  (setq jay/anti-stall--timer nil)
  (jay/goals--append-daily-log
   "Anti-Stall Log"
   (format "- [%s] Completed %d-minute anti-stall sprint."
           (format-time-string "%H:%M")
           minutes))
  (beep)
  (message "Anti-stall sprint complete. Keep momentum."))

(defun jay/anti-stall-now (&optional arg)
  "Start anti-stall sprint from the avoiding list.
With prefix ARG, prompt for minutes; with numeric ARG, use that duration."
  (interactive "P")
  (let ((minutes (cond
                  ((numberp arg) arg)
                  (arg (read-number "Anti-stall minutes: " jay/anti-stall-default-minutes))
                  (t jay/anti-stall-default-minutes))))
    (when (timerp jay/anti-stall--timer)
      (cancel-timer jay/anti-stall--timer)
      (setq jay/anti-stall--timer nil))
    (jay/goals--open-id jay/goals-avoiding-id)
    (when (derived-mode-p 'org-mode)
      (goto-char (point-min))
      (unless (re-search-forward "^\\s-*[-+] \\[ \\] " nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "- [ ] "))
      (beginning-of-line))
    (jay/goals--append-daily-log
     "Anti-Stall Log"
     (format "- [%s] Started %d-minute anti-stall sprint."
             (format-time-string "%H:%M")
             minutes))
    (setq jay/anti-stall--timer
          (run-at-time (* minutes 60) nil #'jay/anti-stall--timer-finished minutes))
    (message "Anti-stall sprint started for %d minutes." minutes)))

(defun jay/prolific-pomo-running-p ()
  "Return non-nil when prolific pom process is active."
  (and (processp jay/prolific-pomo-process)
       (process-live-p jay/prolific-pomo-process)))

(defun jay/prolific-pomo--sentinel (process event)
  "Handle lifecycle EVENT updates from prolific PROCESS."
  (when (memq (process-status process) '(exit signal))
    (setq jay/prolific-pomo-process nil)
    (message "Prolific pom finished: %s"
             (replace-regexp-in-string "[\n\r]+\\'" "" event))))

(defun jay/prolific-pomo-start (&optional sprints)
  "Start prolific pom with SPRINTS (default 3)."
  (interactive "P")
  (let* ((count (cond
                 ((numberp sprints) sprints)
                 (sprints (prefix-numeric-value sprints))
                 (t jay/revenue-block-default-sprints)))
         (default-directory (file-name-as-directory (expand-file-name jay/prolific-root)))
         (program (expand-file-name jay/prolific-pomo-command default-directory))
         (buffer (get-buffer-create jay/prolific-pomo-buffer-name)))
    (unless (file-directory-p default-directory)
      (user-error "Missing prolific workspace: %s" default-directory))
    (unless (file-executable-p program)
      (user-error "Missing executable prolific command: %s" program))
    (when (jay/prolific-pomo-running-p)
      (user-error "Prolific pom is already running"))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer))
    (setq jay/prolific-pomo-process
          (start-file-process "jay-prolific-pomo"
                              buffer
                              program
                              (number-to-string (max 1 count))))
    (set-process-query-on-exit-flag jay/prolific-pomo-process nil)
    (set-process-sentinel jay/prolific-pomo-process #'jay/prolific-pomo--sentinel)
    (pop-to-buffer buffer)
    (message "Started prolific pom: %d sprint(s)." (max 1 count))))

(defun jay/prolific-pomo-stop ()
  "Stop active prolific pom process."
  (interactive)
  (unless (jay/prolific-pomo-running-p)
    (user-error "No active prolific pom process"))
  (delete-process jay/prolific-pomo-process)
  (setq jay/prolific-pomo-process nil)
  (message "Stopped prolific pom process."))

(defun jay/revenue-block-start (&optional arg)
  "Start Track 1 revenue block via prolific pom.
Default is `jay/revenue-block-default-sprints' (3 sprints ~= 90 minutes)."
  (interactive "P")
  (let ((sprints (cond
                  ((numberp arg) arg)
                  (arg (read-number "Revenue block sprints: " jay/revenue-block-default-sprints))
                  (t jay/revenue-block-default-sprints))))
    (jay/prolific-pomo-start sprints)
    (jay/goals--append-daily-log
     "Revenue Block Log"
     (format "- [%s] Started prolific block (%d sprint%s)."
             (format-time-string "%H:%M")
             (max 1 sprints)
             (if (= 1 (max 1 sprints)) "" "s")))))

(defun jay/revenue-block-stop ()
  "Stop current revenue block (prolific pom)."
  (interactive)
  (jay/prolific-pomo-stop)
  (jay/goals--append-daily-log
   "Revenue Block Log"
   (format "- [%s] Stopped prolific block early."
           (format-time-string "%H:%M"))))

;; Keybindings -----------------------------------------------------------------
(jay/bind-roam "f" org-roam-node-find)
(jay/bind-roam "l" org-roam-buffer-toggle)
(jay/bind-roam "i" org-roam-node-insert)
(jay/bind-roam "c" org-roam-capture)
(jay/bind-roam "a" org-roam-alias-add)
(jay/bind-roam "s" org-roam-db-sync)
(define-key jay/super-u-map (kbd "g") #'jay/goals-start-day)
(define-key jay/super-u-map (kbd "A") #'jay/anti-stall-now)
(define-key jay/super-u-map (kbd "R") #'jay/revenue-block-start)
(define-key jay/super-u-map (kbd "C-r") #'jay/revenue-block-stop)

;; Profile switching
(define-key jay/super-u-map (kbd "P") #'jay/org-roam-switch-profile)
(define-key jay/super-u-map (kbd "C-p") #'jay/org-roam-show-current-profile)
(global-set-key (kbd "s-u 1") #'jay/org-roam-switch-to-default)
(global-set-key (kbd "s-u 2") #'jay/org-roam-switch-to-mylife)
(global-set-key (kbd "s-u 3") #'jay/org-roam-switch-to-social)
(global-set-key (kbd "s-u 4") #'jay/org-roam-switch-to-parents)

(jay/bind-roam "o" org-roam-dailies-find-date)
(jay/bind-roam "." org-roam-dailies-goto-date)
(jay/bind-roam "p" org-roam-dailies-goto-previous-note)
(jay/bind-roam "n" org-roam-dailies-goto-next-note)
(jay/bind-roam "T" org-roam-dailies-goto-tomorrow)
(jay/bind-roam "k" org-roam-dailies-capture-date)
(define-key jay/super-u-map (kbd "y") #'jay/org-roam-yesterday)
(jay/bind-roam "Y" org-roam-dailies-yesterday)
(define-key jay/super-u-map (kbd "r") #'org-roam-refile-region-or-subtree)

;; Non-org-roam (no lazy wrapper)
(jay/bind-roam "h" org-id-get-create t)
(jay/bind-roam "t" org-transclusion-make-from-link t)

(global-set-key (kbd "S-s-<up>")    #'jay/org-roam-backlinks-buffer)
(global-set-key (kbd "S-s-<left>")  (lambda () (interactive) (jay/with-org-roam (call-interactively #'org-roam-node-insert))))
(global-set-key (kbd "S-s-<right>") (lambda () (interactive) (jay/with-org-roam (call-interactively #'org-roam-node-find))))
(global-set-key (kbd "s-/ sn")      #'jay/org-roam-search-nodes)
(global-set-key (kbd "s-:")         (lambda () (interactive) (jay/with-org-roam (org-roam-dailies-goto-today))))
(global-set-key (kbd "C-S-d")       (lambda () (interactive) (jay/with-org-roam (org-roam-dailies-goto-today))))
(global-set-key (kbd ":")           #'insert-colon)

;; Agenda custom command -------------------------------------------------------
(add-to-list 'org-agenda-custom-commands
             '("r" "Review items" agenda ""
               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "::review::"))
                (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                (org-agenda-overriding-header "Items to review"))))

;; consult-org-roam (deferred) -------------------------------------------------
(with-eval-after-load 'consult-org-roam
  (setq consult-org-roam-buffer-after-buffers t))

;; Embark integration ----------------------------------------------------------
(defun jay/embark-org-roam--target-node (target)
  "Return org-roam node object from Embark TARGET."
  (cond
   ((and (fboundp 'org-roam-node-p) (org-roam-node-p target)) target)
   ((stringp target)
    (or (get-text-property 0 'node target)
        (when (fboundp 'org-roam-node-from-title-or-alias)
          (org-roam-node-from-title-or-alias target))))
   (t nil)))

(defun jay/embark-org-roam-visit (target)
  "Visit org-roam TARGET node."
  (interactive)
  (let ((node (jay/embark-org-roam--target-node target)))
    (unless node
      (user-error "Could not resolve org-roam node from target"))
    (jay/with-org-roam (org-roam-node-visit node))))

(defun jay/embark-org-roam-insert-link (target)
  "Insert link to org-roam TARGET node at point."
  (interactive)
  (let ((node (jay/embark-org-roam--target-node target)))
    (unless node
      (user-error "Could not resolve org-roam node from target"))
    (insert
     (org-link-make-string
      (concat "id:" (org-roam-node-id node))
      (org-roam-node-title node)))))

(defun jay/embark-org-roam-refile (target)
  "Refile region/subtree to org-roam TARGET node."
  (interactive)
  (let ((node (jay/embark-org-roam--target-node target)))
    (unless node
      (user-error "Could not resolve org-roam node from target"))
    (jay/with-org-roam (org-roam-refile node))))

(defun jay/embark-org-roam-buffer-toggle (&optional _target)
  "Toggle org-roam backlinks buffer.
_TARGET is ignored so this can be used as an Embark action."
  (interactive)
  (jay/with-org-roam (org-roam-buffer-toggle)))

(defun jay/embark-org-roam-alias-add (target)
  "Add alias to org-roam TARGET node."
  (interactive)
  (let ((node (jay/embark-org-roam--target-node target)))
    (unless node
      (user-error "Could not resolve org-roam node from target"))
    (with-current-buffer (find-file-noselect (org-roam-node-file node))
      (save-excursion
        (goto-char (org-roam-node-point node))
        (call-interactively #'org-roam-alias-add)))))

(defun jay/embark-url-copy (url)
  "Copy URL target from Embark."
  (interactive "sURL: ")
  (kill-new url)
  (message "Copied URL: %s" url))

(defun jay/embark-url-preview (&optional _url)
  "Preview links/images in current Org buffer.
_URL is ignored so this can be used as an Embark action."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Preview action is only available in org-mode buffers"))
  (cond
   ((fboundp 'jay/org-preview-inline-images)
    (jay/org-preview-inline-images))
   ((fboundp 'org-link-preview)
    (org-link-preview '(16)))
   ((fboundp 'org-display-inline-images)
    (org-display-inline-images))
   (t
    (user-error "No Org link preview command available"))))

(with-eval-after-load 'embark
  (with-eval-after-load 'org-roam
    (defvar jay/embark-org-roam-node-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map embark-general-map)
        (define-key map (kbd "i") #'jay/embark-org-roam-insert-link)
        (define-key map (kbd "v") #'jay/embark-org-roam-visit)
        (define-key map (kbd "r") #'jay/embark-org-roam-refile)
        (define-key map (kbd "b") #'jay/embark-org-roam-buffer-toggle)
        (define-key map (kbd "a") #'jay/embark-org-roam-alias-add)
        map)
      "Embark actions for org-roam node targets.")
    (setq embark-keymap-alist (assq-delete-all 'org-roam-node embark-keymap-alist))
    (add-to-list 'embark-keymap-alist '(org-roam-node . jay/embark-org-roam-node-map)))
  (define-key embark-url-map (kbd "y") #'jay/embark-url-copy)
  (define-key embark-url-map (kbd "p") #'jay/embark-url-preview))

;; Which-key descriptions for org-roam bindings --------------------------------
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    ;; Profile shortcuts
    "s-u 1"   "profile: Work"
    "s-u 2"   "profile: My Life"
    "s-u 3"   "profile: Social"
    "s-u 4"   "profile: Parents"
    "s-u P"   "switch profile"
    "s-u C-p" "show profile"
    ;; Dailies
    "s-u d"   "today"
    "s-u y"   "yesterday"
    "s-u Y"   "yesterday (roam)"
    "s-u T"   "tomorrow"
    "s-u ."   "goto date"
    "s-u p"   "prev daily"
    "s-u n"   "next daily"
    "s-u k"   "capture date"
    ;; Core
    "s-u f"   "find node"
    "s-u i"   "insert node"
    "s-u c"   "capture"
    "s-u b"   "buffer toggle"
    "s-u a"   "add alias"
    "s-u s"   "db sync"
    "s-u g"   "goals start day"
    "s-u A"   "anti-stall now"
    "s-u R"   "start prolific block"
    "s-u C-r" "stop prolific block"
    "s-u r"   "refile"
    "s-u h"   "add ID"
    "s-u t"   "transclusion"))

(provide 'jay-org-roam-core)
;;; jay-org-roam-core.el ends here
