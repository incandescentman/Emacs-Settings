  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Scratch-12.org
  ;; The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))


  (setq message-log-max t)


  (require 'org-macs)

  (use-package benchmark-init
    :ensure t
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
  ;; (defun ambrevar/reset-gc-cons-threshold ()
  ;; (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
  ;; (setq gc-cons-threshold (* 64 1024 1024))
  ;; (add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)


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
  (setq initial-major-mode 'fundamental-mode)

  (use-package auto-capitalize)
  (use-package recentf)
  (setq recentf-save-file "/Users/jay/emacs/emacs-settings/.savefile/recentf")

  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (use-package mu4e)

  ;; (use-package tiny)
  ;; (use-package re-builder)


  ;; I downloaded the updated version of org from GNU Elpa here and it seemed to work: https://elpa.gnu.org/packages/org.html
  (use-package org
    :load-path "/Users/jay/emacs/org-9.6.1/"
    :config
    (org-reload))
  ;; Removing the above causes the error: Symbol’s function definition is void: org-assert-version



  (use-package org-contrib)


  (setq vc-follow-symlinks t)
  (setq global-flyspell-mode t)

  (setq default-frame-alist
        '(
          (width . 160) ; character
          (height . 42) ; lines
          ))

  (flyspell-mode)

  (setq yas-snippet-dirs '("/Users/jay/emacs/interesting-snippets/" "~/emacs/snippets"))

  ;; ORG-BABEL: enable python, ruby, perl, sh, emacs-lisp
  ;; (org-babel-do-load-languages
  ;; 'org-babel-load-languages
  ;; '( (perl . t)
  ;;    (ruby . t)
  ;;    (shell . t)
  ;;    (python . t)
  ;;    (emacs-lisp . t)
  ;;    (ditaa . t)
  ;;    ))

  ;; (load "/Users/jay/emacs/emacs-settings/zones.el")
  ;; (use-package zones)

  (load "/Users/jay/emacs/emacs-settings/org-panes.el")
  (load "/Users/jay/emacs/emacs-settings/wc-goal-mode.el")

  ;; (load "/Users/jay/emacs/emacs-settings/pastebin-credentials.el")

  ;; (load "/Users/jay/emacs/emacs-settings/latex-templates/jay-latex-yosemite-setup.el")
  ;; (load "/Users/jay/emacs/emacs-settings/tufte-org-mode-master/ox-tufte-latex.el")
  ;; (load "/Users/jay/emacs/emacs-settings/latex-templates/beautiful-racket.el")

  ;; (load "/Users/jay/emacs/emacs-settings/latex-templates/elegant.el")

  ;; (load "/Users/jay/Dropbox/github/emacs-grammarly/emacs-grammarly.el")


  (load "/Users/jay/emacs/emacs-settings/aide.el")

  (load "/Users/jay/emacs/emacs-settings/jay-osx.el")
  (org-babel-load-file "~/emacs/emacs-settings/gnu-emacs-startup.org")
  (org-babel-load-file "~/emacs/emacs-settings/shared-functions.org")
  (org-babel-load-file "~/emacs/emacs-settings/spacemacs-config.org")
  (org-babel-load-file "/Users/jay/emacs/emacs-settings/fonts-and-themes.org")
  (load "/Users/jay/emacs/prelude/core/prelude-core.el")
;;  (load "/Users/jay/emacs/emacs-settings/skeletons.el")
  (load "/Users/jay/emacs/emacs-settings/prelude-key-chord.el")
  (load "/Users/jay/gnulisp/book-functions.el")
  ;; (load "/Users/jay/gnulisp/reveal-in-finder.el")
  (load "/Users/jay/emacs/emacs-settings/poetry_JD.el")
  ;; (load "/Users/jay/emacs/emacs-settings/define-word.el")
  (load "/Users/jay/emacs/emacs-settings/searchlink/searchlink-new.el")
  ;; (load "/Users/jay/emacs/emacs-settings/ivy-smex.el")
  ;; (load "/Users/jay/emacs/emacs-settings/emacs_friends.el")
  ;; (load "/Users/jay/emacs/emacs-settings/email.el")
  ;; (load "/Users/jay/gnulisp/org-image.el")
  (load "/Users/jay/emacs/emacs-settings/org-roam-config.el")


  (jay-load-latex)


  (load "/Users/jay/emacs/org-mime.el")

  (load "/Users/jay/tramp-settings.el")

  ;; (use-package org-mime)

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
       (define-key key-minor-mode-map (kbd "<C-M-right>") 'org-shiftright)
       (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-shiftleft)
       (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-backward-sentence)
       (define-key key-minor-mode-map (kbd "<C-M-right>") 'smart-forward-sentence)


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
       ;;    (imenu-list-minor-mode)
       (menu-bar-mode -1)
       ))

  ;; (spaceline-spacemacs-theme 'narrow)


;;  (setq helm-echo-input-in-header-line nil)

  ;; (add-hook 'helm-after-initialize-hook
  ;;           #'(lambda () (setq helm-echo-input-in-header-line nil)))



  (setq org-bullets-bullet-list '("• "))

  (use-package reveal-in-finder)

  ;; (recenter-top-bottom)
  (setq case-fold-search t)

  (setq company-global-modes '(not org-mode))

  (toggle-fullscreen)
  (menu-bar-mode -1)

  (toggle-menu-bar-mode-from-frame)

  (setq org-hide-leading-stars t)

  (electric-pair-mode 1)

  (add-hook 'ido-setup-hook (lambda ()
                              (define-key ido-completion-map (kbd "<left>") 'ido-prev-match)
                              (define-key ido-completion-map (kbd "<right>") 'ido-next-match)
                              ) t)

  (defadvice load-theme (before theme-dont-propagate activate)
    (mapcar #'disable-theme custom-enabled-themes))

  ;; if Emacs is running in terminal
  (if (is-in-terminal)
      (iterm-mode)
    ;; (load-theme 'zenburn)
    (org-mode)
    )

  (iterm-mode)

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


  (menu-bar-mode -1)

  ;; If non-nil activate `clean-aindent-mode' which tries to correct
  ;; virtual indentation of simple modes. This can interfere with mode specific
  ;; indent handling like has been reported for `go-mode'.
  ;; If it does deactivate it here.
  ;; (default t)
  dotspacemacs-use-clean-aindent-mode t

  ;; Accept SPC as y for prompts if non-nil. (default nil)
  dotspacemacs-use-SPC-as-y nil

  ;; If non-nil shift your number row to match the entered keyboard layout
  ;; (only in insert state). Currently supported keyboard layouts are:
  ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
  ;; New layouts can be added in `spacemacs-editing' layer.
  ;; (default nil)
  dotspacemacs-swap-number-row nil

  ;; Either nil or a number of seconds. If non-nil zone out after the specified
  ;; number of seconds. (default nil)
  dotspacemacs-zone-out-when-idle nil

  ;; Run `spacemacs/prettify-org-buffer' when
  ;; visiting README.org files of Spacemacs.
  ;; (default nil)
  dotspacemacs-pretty-docs nil

  ;; If nil the home buffer shows the full path of agenda items
  ;; and todos. If non-nil only the file name is shown.
  dotspacemacs-home-shorten-agenda-source nil

  ;; If non-nil then byte-compile some of Spacemacs files.
dotspacemacs-byte-compile nil

  ;; ))

  (defun dotspacemacs/user-env ()
    "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
    (spacemacs/load-spacemacs-env)
    )

  (defun dotspacemacs/user-init ()
    "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
    )


  ;; (setq default-tab-width 4) ; for graphviz

  ;; disable smooth scrolling
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil)
  ;;


  (defun dotspacemacs/user-load ()
    "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
    )

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
       (define-key org-mode-map [C-S-return] 'org-insert-todo-heading)
       (define-key org-mode-map (kbd "<C-return>") 'return-insert-blank-line-before)
       (define-key org-mode-map (kbd "<C-S-return>") 'smart-org-insert-todo-heading-dwim)
       (define-key key-minor-mode-map (kbd "<C-M-right>") 'org-shiftright)
       (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-shiftleft)
       (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-backward-sentence)
       (define-key key-minor-mode-map (kbd "<C-M-right>") 'smart-forward-sentence)
       ))

  (defun dotspacemacs/user-config ()
    "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded.")


  (setq package-archive-priorities nil)

  (setq global-auto-complete-mode -1)

  (setq ido-save-directory-list-file "/Users/jay/emacs/emacs-settings/spacemacs.d/.savefile/ido.hist")

  ()

  (display-time)

  ;; (setq evil-emacs-state-cursor '("red" (hbar . 2))) ; for horizontal cursor
  (setq evil-emacs-state-cursor '("red")) ; for box cursor


  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  ;; (horizontal-cursor)
  (box-cursor)
  (incarnadine-cursor)
  (scroll-bar-mode 1)
  (defun package--save-selected-packages (&rest opt) nil)

  ;; (load "/Users/jay/emacs/emacs-settings/mu4e-send-delay.el")



  ;; (use-package mu4e-send-delay)

  ;; To permanently enable mode line display of org clock, add this snippet to your dotspacemacs/user-config function:
  ;; (setq spaceline-org-clock-p t)

  (setq user-init-file "/Users/jay/emacs/emacs-settings/spacemacs.d/init.el")

  (setq case-fold-search t)

                                        ; (cyberpunk-jay)

;;  (counsel-mode 1)


  (setq global-hl-line-mode nil)
  (setq hl-line-mode nil)

  (setq org-latex-default-class "elegant")
  (setq org-twbs-link-home "http://jaydixit.com")
  (setq org-twbs-postamble nil)
  (setq org-twbs-postamble-format nil)
  (setq org-twbs-preamble nil)

  (load "/Users/jay/emacs/emacs-settings/jay-org-in-item-p.el")

  ;; (load "/Users/jay/emacs/emacs-settings/pdf-continuous-scroll.el")

  ;; (server-reflash)
  (triplicate-code)
  ;; (embiggen-text)
  (scrollbar-init)

;; (setq max-lisp-eval-depth 10000)
;;; When opening a file that is a symbolic link, don't ask whether I
;;; want to follow the link. Just do it
;; (setq find-file-visit-truename t)
;; this seems to break in Emacs 28, so I commented it


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

