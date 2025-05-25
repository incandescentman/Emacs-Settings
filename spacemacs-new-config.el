;; debugging steps. Commenting these out now that it seems to be working.

;; (message "DEBUG: About to load helpers...")
;; (defvar te nil)

;; (advice-add 'load :before (lambda (f &rest _) (message ">>> LOADING %s" f)))


;; (defun trace-load (file &rest _)
;;   (message ">>> LOADING %s" file))
;; (advice-add 'load :before #'trace-load)


;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Scratch-12.org

;; --------------------
;; Performance optimizations
;; --------------------




;; 2) --- use GCMH via use-package -----------------------
(use-package gcmh
  :ensure t
  :demand t
  :init (gcmh-mode 1))

(setq message-log-max t)
;; (use-package benchmark-init
;;   :ensure t

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; --------------------
;; Don't give org-assert-version error
;; --------------------
;; (require 'org-macs)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)

                                        ; Source: https://ambrevar.xyz/emacs2/



(setq find-file-visit-truename nil)
;; speed optimizations from
;; https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/

(setq frame-inhibit-implied-resize t)
;; (setq initial-major-mode 'fundamental-mode)

(let ((directory "~/emacs/emacs-settings/elpa-supplement/"))
  (dolist (file (directory-files directory t "\\.el$"))
    (load file)))



;; (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))




(setq vc-follow-symlinks t)



(setq default-frame-alist
      '(
        (width . 160) ; character
        (height . 42) ; lines
        ))

(setq yas-snippet-dirs '("/Users/jay/emacs/interesting-snippets/" "~/emacs/snippets"))


(use-package wc-goal-mode
  :defer
  :ensure t
  :defer
  :load-path "/Users/jay/emacs/emacs-settings/")

(use-package counsel
  :defer)


(autoload 'whittle "whittle" nil t)


(load "/Users/jay/emacs/emacs-settings/jay-osx.el")
(org-babel-load-file "~/emacs/emacs-settings/gnu-emacs-startup.org")
(org-babel-load-file "~/emacs/emacs-settings/shared-functions.org")
(org-babel-load-file "~/emacs/emacs-settings/spacecraft-mode.org")
(org-babel-load-file "~/emacs/emacs-settings/pasteboard-copy-and-paste-functions.org")
(org-babel-load-file "/Users/jay/emacs/emacs-settings/search-commands.org")
(org-babel-load-file "/Users/jay/emacs/emacs-settings/fonts-and-themes.org")
;; (load "/Users/jay/emacs/archive/email.el")
;; (org-babel-load-file "/Users/jay/emacs/external-packages/org-mime-stuff/org-mime-stuff.org")
(load "/Users/jay/emacs/external-packages/prelude/core/prelude-core.el")
(load "/Users/jay/emacs/emacs-settings/skeletons.el")
(load "/Users/jay/emacs/emacs-settings/prelude-key-chord.el")
;; (load "/Users/jay/gnulisp/book-functions.el")
(load "/Users/jay/emacs/emacs-settings/poetry_JD.el")
;; (load "/Users/jay/emacs/emacs-settings/define-word.el")
;; (load "/Users/jay/emacs/emacs-settings/searchlink/searchlink-new.el")
;; (load "/Users/jay/emacs/emacs-settings/ivy-smex.el")
;; (load "/Users/jay/emacs/emacs-settings/emacs_friends.el")
;; (load "/Users/jay/gnulisp/org-image.el")
(load "/Users/jay/emacs/emacs-settings/org-roam-config.el")
;;(load "/Users/jay/emacs/emacs-settings/org-roam-review.el")

;; (monaco-font)

;; (load "/Users/jay/emacs/org-mime.el")

;; (load "/Users/jay/tramp-settings.el")

;; automatically display any prefix
(setq guide-key/recursive-key-sequence-flag t)

;; use OSX standard keybindings for navigating word-by-word and selecting
;; whole words at a time
;; I've been wanting to do this for so long. :-)
;; this works correctly!!
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<M-S-left>") nil)
     (define-key org-mode-map (kbd "<M-S-right>") nil)
     (define-key org-mode-map (kbd "<M-S-up>") nil)
     (define-key org-mode-map (kbd "<M-S-down>") nil)
     (define-key org-mode-map (kbd "<M-left>") nil)
     (define-key org-mode-map (kbd "<M-right>") nil)
     (define-key org-mode-map (kbd "<M-right>") nil)
     (define-key org-mode-map [C-S-right] 'org-shiftmetaright)
     (define-key org-mode-map [C-S-left] 'org-shiftmetaleft)
     (define-key org-mode-map [C-right] 'org-metaright)
     (define-key org-mode-map [C-left] 'org-metaleft)
     (define-key org-mode-map [C-up] 'org-metaup)
     (define-key org-mode-map [C-down] 'org-metadown)
     (define-key org-mode-map (kbd "<C-return>") 'return-insert-blank-line-before)
     (define-key org-mode-map (kbd "<C-S-return>") 'smart-org-insert-todo-heading-dwim)
     ;; (define-key key-minor-mode-map (kbd "<C-M-right>") 'org-shiftright)
     ;; (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-shiftleft)
     ;;     (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-backward-sentence)
     (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-outdent-or-promote)
     ;;     (define-key key-minor-mode-map (kbd "<C-M-right>") 'smart-forward-sentence)
     (define-key key-minor-mode-map (kbd "<C-M-right>") 'org-indent-or-demote)


     (define-key org-mode-map [C-S-return] 'org-insert-todo-heading)
     (define-key org-mode-map (kbd "<C-return>") 'return-insert-blank-line-before)
     (define-key org-mode-map (kbd "<C-S-return>") 'smart-org-insert-todo-heading-dwim)

     (define-key key-minor-mode-map (kbd "<M-S-up>") 'org-shiftup)
     (define-key key-minor-mode-map (kbd "<M-S-down>") 'org-shiftdown)
     (define-key org-mode-map (kbd "<M-up>") 'up-by-degrees)
     (define-key org-mode-map (kbd "<M-down>") 'down-by-degrees)
     (define-key key-minor-mode-map (kbd "<M-down>") 'down-by-degrees)
     (define-key key-minor-mode-map (kbd "<M-up>") 'up-by-degrees)

     ;; (define-key org-mode-map (kbd "needs a binding") 'org-insert-heading-respect-content)
     ;; formerly bound to C-return
     ))


;;  (setq helm-echo-input-in-header-line nil)
;; (add-hook 'helm-after-initialize-hook
;;           #'(lambda () (setq helm-echo-input-in-header-line nil)))



(setq org-bullets-bullet-list '("• "))

;; (use-package transient)
;; (use-package rg)

(use-package reveal-in-finder
  :defer
  )

;; (recenter-top-bottom)
(setq case-fold-search t)

(setq company-global-modes '(not org-mode))

(toggle-fullscreen)
(menu-bar-mode -1)
(toggle-menu-bar-mode-from-frame)
(setq org-hide-leading-stars t)

;; (electric-pair-mode 1)
(smartparens-mode 1)
(smartparens-global-mode 1)

;; (add-hook 'ido-setup-hook (lambda ()
;;                            (define-key ido-completion-map (kbd "<left>") 'ido-prev-match)
;;                            (define-key ido-completion-map (kbd "<right>") 'ido-next-match)
;;                            ) t)

;; (defadvice load-theme (before theme-dont-propagate activate)
;;   (mapcar #'disable-theme custom-enabled-themes))

;; if Emacs is running in terminal
(if (is-in-terminal)
    (iterm-mode)
    ;; (load-theme 'zenburn)
    (org-mode)
    )

;; (iterm-mode)

(setq org-emphasis-alist
      (quote
       (("*" bold)
        ("/" italic)
        ("_" underline)
        ("~" org-code verbatim)
        ("=" flyspell-incorrect)
        ("+"
         (:strike-through t)))))

(setq org-adapt-indentation nil)

;; disable smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;;



(setq package-archive-priorities nil)

(setq global-auto-complete-mode -1)

(setq ido-save-directory-list-file "/Users/jay/emacs/emacs-settings/spacemacs.d/.savefile/ido.hist")

(display-time)

;; (setq evil-emacs-state-cursor '("red" (hbar . 2))) ; for horizontal cursor
(setq evil-emacs-state-cursor '("red")) ; ;; for box cursor


(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;;(horizontal-cursor)
(box-cursor)
(incarnadine-cursor)
(scroll-bar-mode 1)
(defun package--save-selected-packages (&rest opt) nil)

;; (load "/Users/jay/emacs/emacs-settings/mu4e-send-delay.el")



;; To permanently enable mode line display of org clock, add this snippet to your dotspacemacs/user-config function:
;; (setq spaceline-org-clock-p t)

(setq user-init-file "/Users/jay/emacs/emacs-settings/spacemacs.d/init.el")

(setq case-fold-search t)

(setq global-hl-line-mode nil)
(setq hl-line-mode nil)

(setq org-twbs-link-home "http://jaydixit.com")
(setq org-twbs-postamble nil)
(setq org-twbs-postamble-format nil)
(setq org-twbs-preamble nil)

(load "/Users/jay/emacs/emacs-settings/jay-org-in-item-p.el")

;; (load "/Users/jay/emacs/emacs-settings/pdf-continuous-scroll.el")

;; (server-reflash)
;;  (triplicate-code)
;; (embiggen-text)
(scrollbar-init)

;; (setq max-lisp-eval-depth 10000)
;;; When opening a file that is a symbolic link, don't ask whether I
;;; want to follow the link. Just do it
;; (setq find-file-visit-truename t)
;; this seems to break in Emacs 28, so I commented it



;; (define-key org-ai-mode-map (kbd "C-c r") 'eval-region)

(global-fasd-mode 1)




(use-package server
  :defer)
(when (server-running-p)
  (server-force-delete))
;; (server-start)

(redbold)
