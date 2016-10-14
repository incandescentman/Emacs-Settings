;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(setq user-init-file "/Users/jay/emacs/emacs-settings/spacemacs.d/init.el")

(setq recentf-save-file "/Users/jay/emacs/emacs-settings/spacemacs.d/.savefile/recentf")

(setq org-agenda-files (quote ("~/Dropbox/writing/notationaldata/accountability.org"
;; "/Users/jay/Dropbox/writing/book/feb-18/narcs.txt"
"/Users/jay/Dropbox/writing/book/feb-18/50-shades.txt"
;; "/Users/jay/Dropbox/writing/book/feb-18/ovulatory-shift.txt" "/Users/jay/Dropbox/writing/book/feb-18/0-why-I-wrote-this-book.txt"
)))


(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/emacs/spacemacs/private/jay/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
tramp
     helm
     ;; spacemacs-ivy
     auto-completion
     ;; better-defaults
;;      emacs-lisp
     ;; git
     ;; markdown
org
;;     osx
     ;;   xkcd
smex
evil
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
  ;;   spell-checking
  ;;   syntax-checking
     ;; version-control
    ;;  html
     mu4e
  ;;   shell
  ;;   shell-scripts
     jay
;;     speed-reading
;;     emoji
;;     typography
     ;;    javascript

     ;;     floobits
     ;;    deft
;;     chrome
     themes-megapack
 ;;    ibuffer
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(
                                    company
                ;;                    ws-butler
                                    adaptive-wrap
				    vi-tilde-fringe
            exec-path-from-shell

evil-mode evil-escape evil-local evil-org
evil-search-highlight-persist evil-surround eyebrowse global-undo-tree-mode
evil-unimpaired
auto-revert


;; didn't work
auto-encryption-mode
clean-aiindent-mode
diff-auto-refine-mode
electric-indent-mode
evil-mode
hl-line-mode
hs-mode
volatile-highlights
winner-mode-enable
xterm-mouse-mode

highlight-indentation
)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 25
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
leuven
;;                         zenburn
  ;;                       spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "]"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (use-package mu4e)

  ;; (require 'package)
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; (package-initialize)
  ;; -----------------------------THIS EXISTS IN DEFAULT SPACEMACS config

  (setq vc-follow-symlinks t)
  (setq global-flyspell-mode t)

  (setq default-frame-alist
        '(
          (width . 160) ; character
          (height . 42) ; lines
          ))

  (setq yas-snippet-dirs '("/Users/jay/emacs/interesting-snippets/" "~/emacs/snippets"))

  ;; ORG-BABEL: enable python, ruby, perl, sh, emacs-lisp
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (perl . t)
      (ruby . t)
      (sh . t)
      (python . t)
      (emacs-lisp . t)
      ))

  (org-babel-load-file "~/emacs/emacs-settings/gnu-emacs-startup.org")
  (org-babel-load-file "~/emacs/emacs-settings/shared-functions.org")
  (org-babel-load-file "~/emacs/emacs-settings/spacemacs-config.org")
  (org-babel-load-file "/Users/jay/emacs/emacs-settings/fonts-and-themes.org")
  (load "/Users/jay/emacs/prelude/core/prelude-core.el")
  (load "/Users/jay/emacs/emacs-settings/skeletons.el")
  (load "/Users/jay/emacs/emacs-settings/prelude-key-chord.el")
  (load "/Users/jay/gnulisp/book-functions.el")
  (load "/Users/jay/gnulisp/reveal-in-finder.el")
  (load "/Users/jay/emacs/emacs-settings/jay-osx.el")
  (load "/Users/jay/emacs/emacs-settings/poetry_JD.el")
  (load "/Users/jay/emacs/emacs-settings/define-word.el")
  (load "/Users/jay/emacs/emacs-settings/searchlink/searchlink.el")
  ;; (load "/Users/jay/emacs/emacs-settings/ivy-smex.el")
  (load "/Users/jay/emacs/emacs-settings/emacs_friends.el")
;;  (load "/Users/jay/emacs/emacs-settings/email.el")

  ;; automatically display any prefix
  (setq guide-key/recursive-key-sequence-flag t)

  ;; use OSX standard keybindings for navigating word-by-word and selecting whole words at a time
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
       (find-file "~/nd/warm.org")
;;       (imenu-list-minor-mode)
       (menu-bar-mode -1)
       ))



  ;;  (define-key key-minor-mode-map (kbd "C-c d") 'prelude-duplicate-current-line-or-region)

  (setq helm-echo-input-in-header-line nil)

  (add-hook 'helm-after-initialize-hook
            #'(lambda () (setq helm-echo-input-in-header-line nil)))


  (setq org-bullets-bullet-list '("◉" "◉" "○" "○" "✸" "✸" "✿" "✿")) ; for oddlevelsonly mode
  ;;  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")) ; for SHOWSTARS:evenodd

  ;; (load "/Users/jay/emacs/emacs-settings/gnugol.el")
  ;; (use-package gnugol)

(use-package reveal-in-finder)

  (recenter-top-bottom)
  (setq case-fold-search t)

  (setq company-global-modes '(not org-mode))

  (toggle-fullscreen)
  (menu-bar-mode -1)

  (toggle-menu-bar-mode-from-frame)

  (setq auto-revert-interval 1)

  (setq org-hide-leading-stars nil)

  (smartparens-global-mode 1)

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

  (setq global-auto-revert-mode 1)


  (setq org-emphasis-alist
        (quote
         (("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-code verbatim)
          ("~" flyspell-incorrect)

          ("+"
           (:strike-through t)))))

  (setq org-adapt-indentation nil)

  (menu-bar-mode -1)

  (setq org-hide-leading-stars nil)

(custom-set-faces
 '(bold ((t (:family "Sans Serif" :weight bold :height 1.1))))
 '(italic ((t (:family "Garamond" :slant italic :height 1.3))))
 '(flyspell-incorrect ((t (:underline (:style wave :color "red")))))
 '(flyspell-duplicate ((t (:underline (:style wave :color "red")))))
 '(org-code ((t (:family "Courier" :height 1.3))))
 '(org-link ((t (:underline nil)))))



  (setq org-bullets-bullet-list '("◉" "◉" "○" "○" "✸" "✸" "✿" "✿")) ; for oddlevelsonly mode
  ;;  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")) ; for SHOWSTARS:evenodd

  (find-file "~/nd/warm.org")



;; disable smooth scrolling
  (setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;;



  ;; use OSX standard keybindings for navigating word-by-word and selecting whole words at a time
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


;; (zenburn)
(leuven)
;; (spacemacs-light)


(setq global-auto-complete-mode -1)

(setq ido-save-directory-list-file "/Users/jay/emacs/emacs-settings/spacemacs.d/.savefile/ido.hist")
(setq recentf-save-file "/Users/jay/emacs/emacs-settings/spacemacs.d/.savefile/recentf")


(setq evil-emacs-state-cursor '("red" (hbar . 2))) 


(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(horizontal-cursor)
(scroll-bar-mode 1)
(defun package--save-selected-packages (&rest opt) nil)

)

;; theend
;; 
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
;;
;;

