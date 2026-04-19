;;; jay-org-roam-extras.el --- User-facing Org-roam extras  -*- lexical-binding: t; -*-

;;; Commentary:
;; Sidecar module for interactive Org-roam helpers that do not belong in the
;; suite's core/profile/template plumbing. This keeps the suite architecture
;; clean while moving user-facing commands out of `shared-functions.org'.

;;; Code:

(require 'dash)

(use-package org-roam-ui
  :defer t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defvar refile-or-roam-refile-mode nil)

(defun select-refile-mode ()
  (interactive)
  (setq refile-or-roam-refile-mode
        (completing-read
         "Would you like to refile this region or subtree to Org file or Org-roam file? (org-refile or org-roam-refile): "
         '("org-refile" "org-roam-refile"))))

(defun choose-refile-method-and-refile ()
  "Choose the refile method and refile the current subtree or region."
  (interactive)
  (unless refile-or-roam-refile-mode
    (select-refile-mode))
  (if (use-region-p)
      (save-excursion
        (if (string= refile-or-roam-refile-mode "org-refile")
            (progn
              (refile-region-or-subtree)
              (message "Refiled to Org file"))
          (progn
            (org-roam-refile-region-or-subtree)
            (message "Refiled to Org-roam file"))))
    (let ((current-heading (org-get-heading t t t t)))
      (save-excursion
        (if (string= refile-or-roam-refile-mode "org-refile")
            (progn
              (refile-region-or-subtree)
              (message "Refiled to Org file %s" current-heading))
          (progn
            (org-roam-refile-region-or-subtree)
            (message "Refiled to Org-roam file %s" current-heading)))))))

(defun org-roam-auto-link-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((word (thing-at-point 'word 'no-properties))
             (node (car (org-roam-db-query [:select [id] :from nodes :where (= title $s1)] word))))
        (if node
            (progn
              (push-mark)
              (forward-word)
              (let ((region (list (mark) (point))))
                (org-roam-link-set-region (car region) (cadr region) node))))))))

(defun kb/find-node-backlink (arg &optional node choices)
  "Navigate notes by link.
With universal ARG try to use only to navigate the tags of the current note.
Optionally takes a selected NODE and filepaths CHOICES."
  (interactive "P")
  (let* ((depth (if (numberp arg) arg 1))
         (choices
          (or choices
              (when arg
                (-map #'org-roam-backlink-target-node
                      (org-roam-backlinks-get
                       (org-roam-node-from-id
                        (or (ignore-errors (org-roam-node-id node))
                            (org-id-get-create))))))))
         (all-notes (org-roam-node--completions))
         (completions
          (or (--filter (-contains-p choices (cdr it)) all-notes) all-notes))
         (next-node
          (let* ((nodes completions)
                 (node (completing-read
                        "Node: "
                        (lambda (string pred action)
                          (if (eq action 'metadata)
                              '(metadata
                                (annotation-function
                                 . (lambda (title)
                                     (funcall org-roam-node-annotation-function
                                              (get-text-property 0 'node title))))
                                (category . org-roam-node))
                            (complete-with-action action nodes string pred))))))
            (or (cdr (assoc node nodes))
                (org-roam-node-create :title node)))))
    (ignore depth)
    (if (equal node next-node)
        (org-roam-node-visit node)
      (kb/find-node-backlink
       nil
       next-node
       (cons next-node
             (-map #'org-roam-backlink-source-node
                   (org-roam-backlinks-get next-node)))))))

;; Ad-hoc parser diagnostics for Dropbox-synced Org trees.
;; These stay in the sidecar because they are interactive troubleshooting
;; tools, not core suite plumbing.
(defun jay/debug-find-corrupted-file (&optional file)
  "Prompt for an Org file and try to parse it.

If `org-element-parse-buffer' signals an error, print the error.
Otherwise report success and, on confirmation, move the file to
~/Dropbox/roam/notes/ (directory must already exist)."
  (interactive
   (list
    (read-file-name
     "Org file to test: "
     "~/Downloads/"
     nil t
     nil
     (lambda (f) (string-match-p "\\.org\\'" f)))))
  (let* ((debug-on-error nil)
         (result
          (with-temp-buffer
            (insert-file-contents file)
            (condition-case err
                (progn (org-element-parse-buffer) 'ok)
              (error err)))))
    (cond
     ((eq result 'ok)
      (message "✅  %s parsed with no errors" file)
      (when (yes-or-no-p "Move it to ~/Dropbox/roam/notes/? ")
        (let* ((dest-dir "~/Dropbox/roam/notes/")
               (dest-file (expand-file-name (file-name-nondirectory file)
                                            dest-dir)))
          (rename-file file dest-file 1)
          (message "Moved to %s" dest-file))))
     (t
      (message "❌  %s --- %S" file result)))))

(defun jay/debug-scan-org-tree (root)
  "Recursively scan ROOT for *.org files that choke `org-element-parse-buffer'."
  (interactive "DDirectory to scan: ")
  (require 'org)
  (let ((bad nil)
        (total 0))
    (dolist (file (directory-files-recursively root "\\.org\\'"))
      (setq total (1+ total))
      (with-temp-buffer
        (insert-file-contents file nil nil nil 'replace)
        (condition-case err
            (org-element-parse-buffer)
          (error
           (push (cons file err) bad)))))
    (if bad
        (progn
          (message "❌  %d / %d Org files failed to parse:" (length bad) total)
          (dolist (p (reverse bad))
            (message "    %s  ---  %S" (car p) (cdr p))))
      (message "✅  All %d Org files parsed cleanly" total))
    bad))

(defun jay/debug-current-buffer ()
  "Parse the current Org buffer and echo success or first error."
  (interactive)
  (condition-case err
      (progn (org-element-parse-buffer) (message "✅ no parse errors"))
    (error (message "❌ %S" err))))

(defun jay/find-suspect-lines (&optional max-indent max-len)
  "Echo line numbers with very deep bullet indent or very long length."
  (interactive)
  (let* ((max-indent (or max-indent 40))
         (max-len (or max-len 800))
         (re-bullet "^[ \t]*[-+*]\\|^[ \t]*[0-9]+[.)]"))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bol (point))
               (eol (line-end-position))
               (len (- eol bol))
               (indent (progn
                         (skip-chars-forward " \t")
                         (current-column))))
          (when (or (and (looking-at re-bullet) (>= indent max-indent))
                    (> len max-len))
            (message "⚠️  line %d: indent %d, length %d"
                     (line-number-at-pos) indent len)))
        (forward-line 1)))))

(provide 'jay-org-roam-extras)
;;; jay-org-roam-extras.el ends here
