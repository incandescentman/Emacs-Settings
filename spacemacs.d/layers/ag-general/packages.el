;;; packages.el --- ag-general layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-general-packages '(
                                ;; adding magithub explicitly here, until this issue is fixed:
                                ;; see: https://github.com/syl20bnr/spacemacs/issues/9288 and:
                                ;; https://github.com/magit/magit/issues/3154#issuecomment-325648623
                                ;; and wait when they enable this:
                                ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+source-control/github/packages.el#L19
                                ;; magithub
                                helpful
                                rainbow-mode
                                ;; atomic-chrome
                                helm-pages
                                evil-mc
                                edit-indirect))

;; (defun ag-general/init-magithub ()
;;   (use-package magithub
;;     :after magit
;;     :init
;;     (setq magithub-dir (concat spacemacs-cache-directory "magithub/"))
;;     :config
;;     (progn
;;       (magithub-feature-autoinject t)
;;       (define-key magit-status-mode-map "H" #'magithub-dispatch-popup))))

(defun ag-general/init-helpful ()
  (use-package helpful
    :config
    (unbind-key "h" help-map)
    (bind-key "hh" 'helpful-symbol help-map)
    (bind-key "ha" 'helpful-at-point help-map)))

(defun ag-general/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    ;; remove rectangles from around the colors
    (setq css-fontify-colors nil)))

(defun ag-general/init-atomic-chrome ()
  (use-package atomic-chrome
    :init
    (atomic-chrome-start-server)
    (define-key atomic-chrome-edit-mode-map (kbd "C-c C-c") 'atomic-chrome-close-current-buffer)
    :config
    (setq atomic-chrome-default-major-mode 'markdown-mode
          atomic-chrome-enable-bidirectional-edit t
          atomic-chrome-extension-type-list '(atomic-chrome))
    (add-hook 'atomic-chrome-edit-mode-hook #'ag/atomic-edit-start)
    (add-hook 'atomic-chrome-edit-done-hook #'ag/atomic-edit-done)))

(defun ag-general/init-helm-pages ()
  (use-package helm-pages
    :defer t))

(with-eval-after-load 'flycheck
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (defun flycheck-mode-off () (flycheck-mode -1))
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode-off)))

(with-eval-after-load 'ibuf-ext
  (setq
    ibuffer-old-time 8 ;; buffer considered old after that many hours
    ibuffer-group-buffers-by 'projects
    ibuffer-expert t
    ibuffer-show-empty-filter-groups nil)

  (define-ibuffer-filter unsaved-file-buffers
    "Toggle current view to buffers whose file is unsaved."
  (:description "file is unsaved")
  (ignore qualifier)
  (and (buffer-local-value 'buffer-file-name buf)
       (buffer-modified-p buf)))

  (define-ibuffer-filter file-buffers
      "Only show buffers backed by a file."
    (:description "file buffers")
    (ignore qualifier)
    (buffer-local-value 'buffer-file-name buf))

  (define-key ibuffer-mode-map (kbd "/ u") #'ibuffer-filter-by-unsaved-file-buffers)
  (define-key ibuffer-mode-map (kbd "/ F") #'ibuffer-filter-by-file-buffers))

;; Taken from the discussion here: https://github.com/syl20bnr/spacemacs/issues/2669
;; TODO: remove this when official method is implemented in Spacmacs
(defun ag-general/init-evil-mc ()
  (use-package evil-mc
    :init
    (unbind-key "C-t")
    :config
    (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
    (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)

    (defun evil--mc-make-cursor-at-col (startcol _endcol orig-line)
      (move-to-column startcol)
      (unless (= (line-number-at-pos) orig-line)
        (evil-mc-make-cursor-here)))
    (defun evil-mc-make-vertical-cursors (beg end)
      (interactive (list (region-beginning) (region-end)))
      (evil-mc-pause-cursors)
      (apply-on-rectangle #'evil--mc-make-cursor-at-col
                          beg end (line-number-at-pos (point)))
      (evil-mc-resume-cursors)
      (evil-normal-state)
      (move-to-column (evil-mc-column-number (if (> end beg)
                                                 beg
                                               end))))))
(defun ag-general/init-edit-indirect ()
  (use-package edit-indirect
    :config
    (defun ag/edit-indirect-guess-mode (parent-buffer beg end)
      (let ((major (with-current-buffer parent-buffer major-mode)))
        (cond ((eq major 'clojurescript-mode) (org-mode))
              (t (funcall major)))))
    (setq edit-indirect-guess-mode-function 'ag/edit-indirect-guess-mode)))

;; Add `git log ORIG_HEAD..HEAD` to magit
;; that lets you log only changes since the last pull
(with-eval-after-load 'magit
  (defun magit-log-orig_head--head (&optional args files)
    "Compare log since the last pull. i.e.: show only commits between last pull and head"
    (interactive (magit-log-arguments))
    (magit-log (list "ORIG_HEAD..HEAD") args files))

  (magit-define-popup-action 'magit-log-popup
    ?p "orig_head..head" 'magit-log-orig_head--head)

  (defun magit-log-other--current (revision)
    "Compare log between branches à la GitHub style.
i.e.: show only commits that differ between selected (other branch) and current branch"
    (interactive (list (magit-read-other-branch-or-commit "Log compare")))
    (magit-log (list (concat revision ".." (magit-get-current-branch)))))

  (magit-define-popup-action 'magit-log-popup
    ?R "other..current" 'magit-log-other--current)

  (defun magit-diff-range-reversed (rev-or-range &optional args files)
    "Diff between two branches. Unlike `diff-range` works in opposite order i.e.: `base..current`"
    (interactive (list (magit-read-other-branch-or-commit "Diff range")))
    (magit-diff (concat rev-or-range ".." (magit-get-current-branch)) args files))

  (magit-define-popup-action 'magit-diff-popup
    ?R "Diff range (reversed)" 'magit-diff-range-reversed)

  (add-hook 'magit-hook 'turn-off-evil-mc-mode)
  ;; who cares about tags to be displayed in magit-refs buffer?
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))


;;; packages.el ends here
