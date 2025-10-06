;;; org-roam.el --- resilient startup  -*- lexical-binding: t; -*-

;;----------------------------------------------------------------
;;  org-roam  — resilient + idle-sync setup
;;----------------------------------------------------------------

;; Fix org-id-locations corruption issue with symlinks
(load (expand-file-name "org-roam-id-fix.el"
                        (file-name-directory load-file-name)) t)

;; Make Super-u behave like a leader key
(define-prefix-command 'jay/super-u-map)
(global-set-key (kbd "s-u") 'jay/super-u-map)

;; Include org-roam-protocol and org-roam-export after org-roam
;; (use-package org-roam-protocol
;;   :after org-roam)

;; (use-package org-roam-export
;;   :after org-roam)





;; ────────────────────────────────────────────────────────────────
;;  org-roam – load fast, work later
;; ────────────────────────────────────────────────────────────────
(use-package org-roam
  :defer t                            ; only load when explicitly needed
  :after org
  :delight org-roam-mode              ; hide lighter

  ;; ---------- options that must be set *before* loading -----------
  :init
  ;; keep autosync off while init is still running
  (setq org-roam-db-autosync-mode nil)

  ;; ---------- user variables --------------------------------------
  :custom
  (org-roam-directory             (file-truename "~/Dropbox/roam"))
  (org-roam-database-connector    'sqlite-builtin)
  (org-roam-directory-exclude-regexp "^documents/")
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:15}" 'face 'org-tag)))
  (org-roam-dailies-directory     "journal/")
  (org-roam-file-exclude-regexp   "\\.git/\\|attachments/\\|\\.org~$\\|#.*#$")
  (org-roam-db-location           (expand-file-name "org-roam.db" (xdg-cache-home)))
  (org-roam-db-update-method      'idle)      ; update only when idle

  ;; ---------- postpone the heavy stuff ----------------------------
  :config
  ;; Load database fixes
  (load (expand-file-name "org-roam-db-fix.el"
                          (file-name-directory load-file-name)) t)

  ;; 1) Make every single-file refresh resilient (never raise an error)
  (defun my/org-roam--safe-update (orig-fn &rest args)
    (condition-case err
        (apply orig-fn args)
      (error (message "org-roam: skipped %s (%s)"
                      (or (car args) "<buffer>")
                      (error-message-string err)))))

  (advice-add 'org-roam-db-update-file :around #'my/org-roam--safe-update)

  ;; 2) Full setup + first sync after 1 s of *user* idleness
  (run-with-idle-timer
   1  nil
   (lambda ()
     (condition-case err
         (progn
           (require 'org-roam)            ; autoloaded anyway, but explicit is clear
           (require 'ol)                  ; org-link helpers
           (org-roam-setup)
           ;; First full scan (runs async)
           (org-roam-db-sync))
       (error (message "Initial org-roam setup error: %s" (error-message-string err))))))

  ;; 3) Turn *auto* sync on only after we have been idle for 5 s
  (run-with-idle-timer
   5 nil
   (lambda ()
     (condition-case err
         (progn
           (message "⮡ enabling org-roam autosync …")
           ;; Ensure database connection before enabling autosync
           (when (and (boundp 'org-roam-db)
                      (not (emacsql-live-p org-roam-db)))
             (setq org-roam-db nil)
             (org-roam-db))
           (org-roam-db-autosync-mode 1))
       (error (message "Failed to enable autosync: %s" (error-message-string err))))))

  ;; 4) Capture templates (light-weight, keep them here)
  (setq org-roam-dailies-capture-templates
        '(("j" "Journal" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: :journal:\n\n- Links ::\n\n* %<%A, %B %d, %Y>\n\n** Today [0/1]\n")))))









;; ------------------------------------------------------------
;; Org-roam global key bindings
;; (defined *after* org-roam is on the load-path)
;; ------------------------------------------------------------
(with-eval-after-load 'org-roam
  ;; ­— core —
  (global-set-key (kbd "s-u f") #'org-roam-node-find)
  (global-set-key (kbd "s-u l") #'org-roam-buffer-toggle)
  (global-set-key (kbd "s-u i") #'org-roam-node-insert)
  (global-set-key (kbd "s-u c") #'org-roam-capture)
  (global-set-key (kbd "s-u r") #'org-roam-refile)
  (global-set-key (kbd "s-u h") #'org-roam-heading-add)
  (global-set-key (kbd "s-u a") #'org-roam-alias-add)
  (global-set-key (kbd "s-u t") #'org-transclusion-make-from-link)

  ;; ­— backlinks & navigation —
  (global-set-key (kbd "S-s-<up>")    #'org-roam-backlinks-buffer)
  (global-set-key (kbd "S-s-<left>")  #'org-roam-node-insert)
  (global-set-key (kbd "S-s-<right>") #'org-roam-node-find)

  ;; ­— dailies (goto) —
  (global-set-key (kbd "s-u o") #'org-roam-dailies-find-date)
  (global-set-key (kbd "s-u .") #'org-roam-dailies-goto-date)
  (global-set-key (kbd "s-u p") #'org-roam-dailies-goto-previous-note)
  (global-set-key (kbd "s-u n") #'org-roam-dailies-goto-next-note)
  (global-set-key (kbd "s-:")   #'org-roam-dailies-goto-today)
  (global-set-key (kbd "C-S-d") #'org-roam-dailies-goto-today)
  (global-set-key (kbd "s-u y") #'org-roam-dailies-goto-yesterday)
  (global-set-key (kbd "s-u Y") #'org-roam-dailies-yesterday)
  (global-set-key (kbd "s-u T") #'org-roam-dailies-goto-tomorrow)

  ;; ­— dailies (capture) —
  (global-set-key (kbd "s-u k") #'org-roam-dailies-capture-date)

  ;; ­— search —
  (global-set-key (kbd "s-/ sn") #'org-roam-search-nodes)

  ;; ­— misc —
  (global-set-key (kbd ":") #'insert-colon))








(add-to-list 'org-agenda-custom-commands
             '("r" "Review items"
               agenda ""
               (
                ;; (org-agenda-files '("~/path/to/your/org/files/"))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "::review::"))
                (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                (org-agenda-overriding-header "Items to review"))))

;; Fix org-roam-refile to avoid this error:
;; user-error: The kill is not a (set of) tree(s). Use 'C-y' to yank anyway

;; Problem: seems to be related to the org-paste-subtree function, which expects the content in the kill ring to be a valid Org subtree or a set of subtrees. If the content you're trying to refile as a region is not in this format, the function raises an error.
;; Solution: Modify the org-roam-refile function to copy the region as plain text instead of using org-copy-subtree
(defun org-roam-refile-region-or-subtree ()
  "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (node (org-roam-node-read nil nil nil 'require-match))
         (file (org-roam-node-file node))
         (nbuf (or (find-buffer-visiting file)
                   (find-file-noselect file)))
         level reversed)
    (if (equal (org-roam-node-at-point) node)
        (user-error "Target is the same as current node")
        (if regionp
            (progn
              (kill-new (buffer-substring-no-properties region-start region-end))
              (org-save-markers-in-region region-start region-end))
            (progn
              (if (org-before-first-heading-p)
                  (org-roam-demote-entire-buffer))
              (org-copy-subtree 1 nil t)))
        (with-current-buffer nbuf
          (org-with-wide-buffer
           (goto-char (org-roam-node-point node))
           (setq level (org-get-valid-level (funcall outline-level) 1)
                 reversed (org-notes-order-reversed-p))
           (goto-char
            (if reversed
                (or (outline-next-heading) (point-max))
                (or (save-excursion (org-get-next-sibling))
                    (org-end-of-subtree t t)
                    (point-max))))
           (unless (bolp) (newline))
           (if regionp
               (insert (current-kill 0))
               (org-paste-subtree level nil nil t))
           (and org-auto-align-tags
                (let ((org-loop-over-headlines-in-active-region nil))
                  (org-align-tags)))
           (when (fboundp 'deactivate-mark) (deactivate-mark))))
        (if regionp
            (progn
              (goto-char region-end)
              (delete-region region-start region-end))
            (org-preserve-local-variables
             (delete-region
              (and (org-back-to-heading t) (point))
              (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
        ;; If the buffer end-up empty after the refile, kill it and delete its
        ;; associated file.
        (when (eq (buffer-size) 0)
          (if (buffer-file-name)
              (delete-file (buffer-file-name)))
          (set-buffer-modified-p nil)
          ;; If this was done during capture, abort the capture process.
          (when (and org-capture-mode
                     (buffer-base-buffer (current-buffer)))
            (org-capture-kill))
          (kill-buffer (current-buffer))))))

;; TODO learn how to use this
(use-package consult-org-roam)
;; https://github.com/jgru/consult-org-roam

(setq consult-org-roam-buffer-after-buffers t)

;; maybe not OP but I tried this package a few days ago and gave up, it slowed considerably consult-buffer. I couldn't identify yet why.
;; https://www.reddit.com/r/emacs/comments/yy79pn/how_to_hideignore_orgroam_buffersfiles_when_using/
;; exclude certain tags
;; (add-to-list 'recentf-exclude ".*roam.org$")
;; (add-to-list 'consult-buffer-filter ".*roam.org$")


(defun org-roam-create-sequence-next ()
  "Insert '- Next :: ' and then run org-roam-node-insert."
  (interactive)
  (insert "- Next :: ")
  (org-roam-node-insert))

(defun org-roam-create-sequence-previous ()
  "Insert '- Previous :: ' and then run org-roam-node-insert."
  (interactive)
  (insert "- Previous :: ")
  (org-roam-node-insert))

(define-key key-minor-mode-map (kbd "s-u P") 'org-roam-create-sequence-previous)
(define-key key-minor-mode-map (kbd "s-u N") 'org-roam-create-sequence-next)





(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            'org-roam-unlinked-references-section
            ))

(defun org-roam-yesterday ()
  (interactive)
  (condition-case nil
      (progn
        (jay/save-some-buffers)
        (org-roam-dailies-goto-previous-note))
    (error (message "Failed to go to yesterday's note"))))


(defun org-roam-backlinks-buffer ()
  (interactive)
  (org-roam-buffer-toggle)
  (other-window 1)
  )

(defun org-roam-search-nodes ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (counsel-rg nil org-roam-directory nil nil))

;; rename some org-roam functions
(defalias 'org-roam-heading-add 'org-id-get-create)
(defalias 'org-roam-find-node 'org-roam-node-find)
(defalias 'org-roam-insert-node 'org-roam-node-insert)


(defun patch/emacsql-close (connection &rest args)
  "Prevent calling emacsql-close if connection handle is nil."
  (when (oref connection handle)
    t))

(advice-add 'emacsql-close :before-while #'patch/emacsql-close)
