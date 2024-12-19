(use-package org-roam
  :defer
  :after org
  :delight
  :config
  (require 'ol)
  :hook
  (after-init . org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (file-truename "/Users/jay/Dropbox/roam"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))
  (org-roam-dailies-directory "journal/")


  ;; Capture templates

  ;; %A, %B %d, %Y
  (org-roam-dailies-capture-templates
   '(("j" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: :journal:
- Links :: \n
* %<%A, %B %d, %Y>
\n
** Today [0/1]
"))))
  ;; - %<Week %w, day %j>\n
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  ;; (advice-add #'org-roam-buffer-persistent-set :after #'org-roam-buffer-redisplay-h) ;; need to define this or remove
  (setq org-roam-db-location "/Users/jay/dropbox/roam/org-roam.db")


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

  ;; Add custom functions and advice
  ;; (global-page-break-lines-mode 0)
  (advice-add #'org-roam-fontify-like-in-org-mode :around (lambda (fn &rest args) (save-excursion (apply fn args))))

  (setq org-roam-completion-everywhere t)
  ;; doesn't work for some reason
  ;; so that org-roam links can be followed
  ;; source: [[https://github.com/org-roam/org-roam/issues/1732][clicking on any link within *org-roam* buffer fails with an error message · Issue #1732 · org-roam/org-roam]]
  ;; if necessary, consider using org-roam-buffer-refresh

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


  :bind
  (
   ("s-u f" . org-roam-find-node)
   ("S-s-<up>" . org-roam-backlinks-buffer)
   ;; ("S-s-<down>" . projectile-ripgrep)
   ;;   ("S-s-<down>" . deadgrep)
   ("s-u l" . org-roam-buffer-toggle)
   ("s-u i" . org-roam-node-insert)
   ("s-u c" . org-roam-capture)
   ("S-s-<left>" . org-roam-node-insert)
   ("S-s-<right>" . org-roam-node-find)
   ("s-u r" . org-roam-refile)

   ;; goto
   ("s-u o" . org-roam-dailies-find-date)
   ("s-u ." . org-roam-dailies-goto-date)
   ("s-u p" . org-roam-yesterday)
   ("s-u n" . org-roam-dailies-goto-next-note)
   ("s-j"   . org-roam-dailies-goto-today)
   ("C-S-d" . org-roam-dailies-goto-today)
   ("s-u y" . org-roam-dailies-goto-yesterday)
   ("s-u t" . org-transclusion-make-from-link)
   ("s-u T" . org-roam-dailies-goto-tomorrow)
   ("s-u Y" . org-roam-dailies-yesterday)

   ;; capture
   ("s-u k" . org-roam-dailies-capture-date)

   ;; search
   ("s-/ sn" . org-roam-search-nodes)
   (":" . insert-colon)

   ("s-u h" . org-roam-heading-add) ;; org-roam create heading
   ("s-u a" . org-roam-alias-add)

   ;;   ("s-T" . org-roam-tags)

   ;; ("C-c r d a" . org-agenda)
   ;; ("C-c r d s" . org-schedule)

   ))




;; Include org-roam-protocol and org-roam-export after org-roam
(use-package org-roam-protocol
  :after org-roam)

(use-package org-roam-export
  :after org-roam)


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


(setq org-roam-db-update-method 'immediate)

;; Setting org-roam-db-update-method to 'immediate ensures real-time updates but can impact performance for large note sets. Monitor performance and consider alternative methods if necessary.
