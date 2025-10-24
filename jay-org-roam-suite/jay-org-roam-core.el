;;; jay-org-roam-core.el --- Org-roam core: resilient, lazy, and fast  -*- lexical-binding: t; -*-

;;; Commentary:
;; Core module (no templates). Pairs with `jay-org-roam-profiles.el`.
;; - Maximally lazy: no blocking (require 'org-roam) at init.
;; - Two-phase idle setup, safe DB advice, leader-key, helpers, bindings.
;; - Profile system integration for multi-database support.
;; - Templates live in profiles module.

;;; Code:

;; Optional local fixes (loaded only if present)
(when load-file-name
  (load (expand-file-name "org-roam-id-fix.el" (file-name-directory load-file-name)) t)
  (load (expand-file-name "org-roam-db-fix.el" (file-name-directory load-file-name)) t))

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
        org-roam-directory (file-truename "~/Dropbox/roam")
        org-roam-database-connector 'sqlite-builtin
        org-roam-directory-exclude-regexp "^documents/"
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag))
        org-roam-dailies-directory "journal/"
        org-roam-file-exclude-regexp "\\.git/\\|attachments/\\|\\.org~$\\|#.*#$"
        org-roam-db-location (expand-file-name "org-roam.db" (xdg-cache-home))
        org-roam-db-update-method 'idle
        org-roam-dailies-capture-templates (copy-tree jay/org-roam-dailies-template-default))

  ;; staged setup for speed
  (run-with-idle-timer
   1 nil
   (lambda ()
     (condition-case err
         (jay/with-org-roam
          (require 'ol)
          (org-roam-setup)
          ;; Initialize profile system and load saved profile
          ;; (profile init handles its own sync when needed)
          (jay/org-roam-profiles-init))
       (error (message "Initial org-roam setup error: %s" (error-message-string err))))))

  (run-with-idle-timer
   5 nil
   (lambda ()
     (condition-case err
         (jay/with-org-roam
          (when init-file-debug (message "⮡ enabling org-roam autosync …"))
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
  (let ((dir (if (boundp 'org-roam-directory) org-roam-directory (file-truename "~/Dropbox/roam"))))
    (cond ((fboundp 'consult-ripgrep) (consult-ripgrep dir))
          ((fboundp 'counsel-rg) (counsel-rg nil dir nil nil))
          (t (user-error "Need consult or counsel for ripgrep")))))

(defun jay/org-roam-create-sequence-next ()
  (interactive) (insert "- Next :: ") (jay/with-org-roam (org-roam-node-insert)))
(defun jay/org-roam-create-sequence-previous ()
  (interactive) (insert "- Previous :: ") (jay/with-org-roam (org-roam-node-insert)))

(defun insert-colon () (interactive) (insert ":"))

;; Keybindings -----------------------------------------------------------------
(jay/bind-roam "f" org-roam-node-find)
(jay/bind-roam "l" org-roam-buffer-toggle)
(jay/bind-roam "i" org-roam-node-insert)
(jay/bind-roam "c" org-roam-capture)
(jay/bind-roam "a" org-roam-alias-add)
(jay/bind-roam "s" org-roam-db-sync)

;; Profile switching
(define-key jay/super-u-map (kbd "P") #'jay/org-roam-switch-profile)
(define-key jay/super-u-map (kbd "C-p") #'jay/org-roam-show-current-profile)
(global-set-key (kbd "s-u 1") #'jay/org-roam-switch-to-default)
(global-set-key (kbd "s-u 2") #'jay/org-roam-switch-to-mylife)

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

(provide 'jay-org-roam-core)
;;; jay-org-roam-core.el ends here
