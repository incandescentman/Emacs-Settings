;;; jay-org-roam-lazy.el --- Org-roam: resilient, lazy, and fast  -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete drop-in replacement for your previous org-roam.el with the same behavior,
;; but optimized for *lazy loading* and startup speed.
;;
;; Now integrated with jay-org-roam-profiles.el for multi-database support.
;;
;; Enhancements:
;; - Unified keybinding helper macro for clarity.
;; - Removed unnecessary lazy wrappers for non-org-roam functions.
;; - All helper functions explicitly defined (fully self-contained).
;; - Maintains your two-phase idle startup while reducing repetition.
;; - Profile system integration for switching between work/personal databases.

;;; Code:

;; -----------------------------------------------------------------------------
;; Dependencies (optional local fixes)
;; -----------------------------------------------------------------------------
(when load-file-name
  (load (expand-file-name "org-roam-id-fix.el" (file-name-directory load-file-name)) t)
  (load (expand-file-name "org-roam-db-fix.el" (file-name-directory load-file-name)) t))

;; Load the profile system
(require 'jay-org-roam-profiles)

;; -----------------------------------------------------------------------------
;; Lazy-safe load utilities
;; -----------------------------------------------------------------------------
(defmacro jay/with-org-roam (&rest body)
  "Require `org-roam' lazily, then eval BODY."
  (declare (indent 0) (debug t))
  `(progn (unless (featurep 'org-roam) (require 'org-roam)) ,@body))

(defmacro jay/bind-roam (key command &optional no-wrap)
  "Bind KEY to COMMAND in `jay/super-u-map'.
Wrap in lazy require unless NO-WRAP is non-nil."
  `(define-key jay/super-u-map (kbd ,key)
     (lambda () (interactive)
       ,(if no-wrap
            `(call-interactively #',command)
          `(jay/with-org-roam (call-interactively #',command))))))

;; -----------------------------------------------------------------------------
;; Prefix key definition
;; -----------------------------------------------------------------------------
(define-prefix-command 'jay/super-u-map)
(global-set-key (kbd "s-u") 'jay/super-u-map)

;; -----------------------------------------------------------------------------
;; Core org-roam configuration (lazy init)
;; -----------------------------------------------------------------------------
(with-eval-after-load 'org
  ;; Base settings (profiles will override directory/db/templates)
  (setq org-roam-db-autosync-mode nil
        org-roam-database-connector 'sqlite-builtin
        org-roam-directory-exclude-regexp "^documents/"
        org-roam-node-display-template
          (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag))
        org-roam-file-exclude-regexp "\\.git/\\|attachments/\\|\\.org~$\\|#.*#$"
        org-roam-db-update-method 'idle)

  ;; Dailies capture templates (same for all profiles)
  (setq org-roam-dailies-capture-templates
        '(("j" "Journal" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: :journal:\n\n- Links ::\n\n* %<%A, %B %d, %Y>\n\n** Today [0/1]\n"))))

  ;; Staged setup for speed
  (run-with-idle-timer
   1 nil
   (lambda ()
     (condition-case err
         (jay/with-org-roam
           (require 'ol)
           (org-roam-setup)
           ;; Initialize profile system and load saved profile
           (jay/org-roam-profiles-init)
           ;; Sync after profile is loaded
           (org-roam-db-sync))
       (error (message "Initial org-roam setup error: %s" (error-message-string err))))))

  (run-with-idle-timer
   5 nil
   (lambda ()
     (condition-case err
         (jay/with-org-roam
           (message "⮡ enabling org-roam autosync …")
           (when (and (boundp 'org-roam-db) (not (emacsql-live-p org-roam-db)))
             (setq org-roam-db nil)
             (org-roam-db))
           (org-roam-db-autosync-mode 1))
       (error (message "Failed to enable autosync: %s" (error-message-string err)))))))

;; -----------------------------------------------------------------------------
;; Safety wrappers for DB
;; -----------------------------------------------------------------------------
(defun jay/org-roam--safe-update (orig-fn &rest args)
  "Wrap org-roam DB updates to avoid crashing on errors."
  (condition-case err (apply orig-fn args)
    (error (message "org-roam skipped %s (%s)" 
                    (or (car args) "<buffer>") 
                    (error-message-string err)))))

(advice-add 'org-roam-db-update-file :around #'jay/org-roam--safe-update)

(defun jay/org-roam--safe-query (orig-fn &rest args)
  "Wrap org-roam DB queries to avoid crashing on errors."
  (condition-case err (apply orig-fn args)
    (error (message "org-roam query skipped: %s" (error-message-string err)) 
           nil)))

(with-eval-after-load 'org-roam
  (advice-add 'org-roam-db-query :around #'jay/org-roam--safe-query))

(defun jay/patch-emacsql-close (connection &rest _)
  "Prevent `emacsql-close' if CONNECTION handle is nil."
  (when (ignore-errors (oref connection handle)) t))

(advice-add 'emacsql-close :before-while #'jay/patch-emacsql-close)

;; -----------------------------------------------------------------------------
;; Custom refile function (fixes region-refile issue)
;; -----------------------------------------------------------------------------
(defun org-roam-refile-region-or-subtree ()
  "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point.
Fixes the 'not a tree' error when refiling regions."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (node (org-roam-node-read nil nil nil 'require-match))
         (file (org-roam-node-file node))
         (nbuf (or (find-buffer-visiting file) (find-file-noselect file)))
         level reversed)
    (if (equal (org-roam-node-at-point) node)
        (user-error "Target is the same as current node")
      (if regionp
          (progn
            (kill-new (buffer-substring-no-properties region-start region-end))
            (org-save-markers-in-region region-start region-end))
        (progn
          (when (org-before-first-heading-p)
            (org-roam-demote-entire-buffer))
          (org-copy-subtree 1 nil t)))
      (with-current-buffer nbuf
        (org-with-wide-buffer
          (goto-char (org-roam-node-point node))
          (setq level (org-get-valid-level (funcall outline-level) 1)
                reversed (org-notes-order-reversed-p))
          (goto-char (if reversed
                         (or (outline-next-heading) (point-max))
                       (or (save-excursion (org-get-next-sibling))
                           (org-end-of-subtree t t)
                           (point-max))))
          (unless (bolp) (newline))
          (if regionp
              (insert (current-kill 0))
            (org-paste-subtree level nil nil t))
          (when (bound-and-true-p org-auto-align-tags)
            (let ((org-loop-over-headlines-in-active-region nil))
              (org-align-tags)))
          (when (fboundp 'deactivate-mark) (deactivate-mark))))
      (if regionp
          (progn (goto-char region-end) 
                 (delete-region region-start region-end))
        (org-preserve-local-variables
          (delete-region (and (org-back-to-heading t) (point))
                         (min (1+ (buffer-size)) 
                              (org-end-of-subtree t t) 
                              (point)))))
      (when (eq (buffer-size) 0)
        (when (buffer-file-name) 
          (delete-file (buffer-file-name)))
        (set-buffer-modified-p nil)
        (when (and (bound-and-true-p org-capture-mode)
                   (buffer-base-buffer (current-buffer)))
          (org-capture-kill))
        (kill-buffer (current-buffer))))))

;; -----------------------------------------------------------------------------
;; Helper functions
;; -----------------------------------------------------------------------------
(defun jay/org-roam-yesterday ()
  "Go to yesterday's daily note, saving buffers first if possible."
  (interactive)
  (condition-case nil
      (progn
        (when (fboundp 'jay/save-some-buffers) 
          (jay/save-some-buffers))
        (jay/with-org-roam (org-roam-dailies-goto-previous-note)))
    (error (message "Failed to go to yesterday's note"))))

(defun jay/org-roam-backlinks-buffer ()
  "Toggle org-roam backlinks buffer and switch to it."
  (interactive)
  (jay/with-org-roam (org-roam-buffer-toggle))
  (other-window 1))

(defun jay/org-roam-search-nodes ()
  "Search org-roam directory using ripgrep (via consult or counsel)."
  (interactive)
  (let ((dir (if (boundp 'org-roam-directory) 
                 org-roam-directory 
               (file-truename "~/Dropbox/roam"))))
    (cond ((fboundp 'consult-ripgrep) 
           (consult-ripgrep dir))
          ((fboundp 'counsel-rg) 
           (counsel-rg nil dir nil nil))
          (t (user-error "Need consult or counsel for ripgrep")))))

(defun jay/org-roam-create-sequence-next ()
  "Insert '- Next :: ' then prompt to insert an org-roam node link."
  (interactive) 
  (insert "- Next :: ") 
  (jay/with-org-roam (org-roam-node-insert)))

(defun jay/org-roam-create-sequence-previous ()
  "Insert '- Previous :: ' then prompt to insert an org-roam node link."
  (interactive) 
  (insert "- Previous :: ") 
  (jay/with-org-roam (org-roam-node-insert)))

(defun insert-colon ()
  "Insert a colon character."
  (interactive)
  (insert ":"))

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------
;; Core org-roam commands
(jay/bind-roam "f" org-roam-node-find)
(jay/bind-roam "l" org-roam-buffer-toggle)
(jay/bind-roam "i" org-roam-node-insert)
(jay/bind-roam "c" org-roam-capture)
(jay/bind-roam "a" org-roam-alias-add)
(jay/bind-roam "s" org-roam-db-sync)

;; Profile switching (NEW!)
(define-key jay/super-u-map (kbd "P") #'jay/org-roam-switch-profile)
(define-key jay/super-u-map (kbd "C-p") #'jay/org-roam-show-current-profile)

;; Quick profile switches
(global-set-key (kbd "s-u 1") #'jay/org-roam-switch-to-default)
(global-set-key (kbd "s-u 2") #'jay/org-roam-switch-to-mylife)

;; Dailies
(jay/bind-roam "o" org-roam-dailies-find-date)
(jay/bind-roam "." org-roam-dailies-goto-date)
(jay/bind-roam "p" org-roam-dailies-goto-previous-note)
(jay/bind-roam "n" org-roam-dailies-goto-next-note)
(jay/bind-roam "T" org-roam-dailies-goto-tomorrow)
(jay/bind-roam "k" org-roam-dailies-capture-date)
(define-key jay/super-u-map (kbd "y") #'jay/org-roam-yesterday)
(define-key jay/super-u-map (kbd "Y") 
  (lambda () (interactive) (jay/with-org-roam (org-roam-dailies-yesterday))))

;; Refile (special handling, wraps org-roam internally)
(define-key jay/super-u-map (kbd "r") #'org-roam-refile-region-or-subtree)

;; Non-org-roam commands (no lazy wrapper needed)
(jay/bind-roam "h" org-id-get-create t)
(jay/bind-roam "t" org-transclusion-make-from-link t)

;; Global keys
(global-set-key (kbd "S-s-<up>") #'jay/org-roam-backlinks-buffer)
(global-set-key (kbd "S-s-<left>") 
  (lambda () (interactive) (jay/with-org-roam (call-interactively #'org-roam-node-insert))))
(global-set-key (kbd "S-s-<right>") 
  (lambda () (interactive) (jay/with-org-roam (call-interactively #'org-roam-node-find))))
(global-set-key (kbd "s-/ sn") #'jay/org-roam-search-nodes)
(global-set-key (kbd "s-:") 
  (lambda () (interactive) (jay/with-org-roam (org-roam-dailies-goto-today))))
(global-set-key (kbd "C-S-d") 
  (lambda () (interactive) (jay/with-org-roam (org-roam-dailies-goto-today))))
(global-set-key (kbd ":") #'insert-colon)

;; -----------------------------------------------------------------------------
;; Agenda: custom review command
;; -----------------------------------------------------------------------------
(add-to-list 'org-agenda-custom-commands
             '("r" "Review items" agenda ""
               ((org-agenda-skip-function 
                 '(org-agenda-skip-entry-if 'notregexp "::review::"))
                (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                (org-agenda-overriding-header "Items to review"))))

;; -----------------------------------------------------------------------------
;; consult-org-roam: defer configuration until loaded
;; -----------------------------------------------------------------------------
(with-eval-after-load 'consult-org-roam
  (setq consult-org-roam-buffer-after-buffers t))

;; -----------------------------------------------------------------------------
;; Ispell dictionary configuration
;; -----------------------------------------------------------------------------
(defun jay/fix-ispell-contraction ()
  "Fix ispell handling of contractions like shouldn't."
  (add-to-list 'ispell-dictionary-alist
               '("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil 
                 ("-d" "en_GB") nil utf-8)))

(with-eval-after-load 'ispell
  (jay/fix-ispell-contraction)
  (add-to-list 'ispell-dictionary-alist
               '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil 
                 ("-d" "en_US") nil utf-8))
  (add-to-list 'ispell-dictionary-alist
               '("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil 
                 ("-d" "en_US-large") nil utf-8))
  (add-to-list 'ispell-dictionary-alist
               '("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil 
                 ("-d" "en_US-large") nil utf-8)))

;; -----------------------------------------------------------------------------
;; Shell and export environment tweaks
;; -----------------------------------------------------------------------------
(setq shell-file-name "/bin/bash")
(let* ((login-path (replace-regexp-in-string
                    "[ \t\n\r]+\\'" ""
                    (shell-command-to-string
                     "bash -l -c 'printf %s \"$PATH\"'")))
       (login-path-list (split-string login-path ":" t))
       (preferred-front '("/usr/local/bin" "/opt/homebrew/bin"))
       (preferred-tail '("/usr/local/texlive/2024/bin/universal-darwin"))
       (combined-paths (delete-dups 
                        (append preferred-front 
                                login-path-list 
                                preferred-tail 
                                (copy-sequence exec-path))))
       (combined-string (mapconcat #'identity combined-paths ":")))
  (setenv "PATH" combined-string)
  (setenv "SHELL" "/bin/bash")
  (setq exec-path combined-paths))

;; Configure XeLaTeX for org-mode exports
(setq org-latex-pdf-process 
      '("xelatex -shell-escape -interaction nonstopmode %f"))

;; captain predicate hooks
(add-hook 'text-mode-hook (lambda () (setq captain-predicate (lambda () t))))
(add-hook 'org-mode-hook (lambda () (setq captain-predicate (lambda () t))))

;; -----------------------------------------------------------------------------
(provide 'jay-org-roam-lazy)
;;; jay-org-roam-lazy.el ends here
