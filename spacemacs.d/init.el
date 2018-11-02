;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("/Users/jay/starship/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     ;; better-defaults
     emacs-lisp
     ;;   git
     ;;   markdown
     ;;    python

     (org
      :variables
      org-enable-bootstrap-support t
      org-enable-github-support t
      ;; org-enable-reveal-js-support t

      :location built-in
      )

     ;;   osx
     ;;   xkcd
;;     ag-general
;;     ag-lang-tools
     smex
       (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;;   spell-checking
     syntax-checking ; this is the layer with flycheck
     ;;   version-control
     ;;   html
     mu4e
     ;;   shell
     ;;   shell-scripts
     ;;     jay
     ;;     speed-reading
     ;;     emoji
     ;;     typography
     javascript

     ;;     floobits
     ;;    deft
     ;;     chrome
     ;; themes-megapack
     ;;    ibuffer
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(


                                      ;;  auto-complete
                                      ;;  dired-hacks-utils
                                      ;;  dired-single
                                      ;;  frame-restore
                                      ;;  helm-projectile
                                      ;;  markdown-mode
                                      ;;  notmuch
                                      ;;  projectile
                                      ;; graphviz-dot-mode
                                      ;; js2
                                      ;; magit
                                      ;; smex
                                      solarized-theme
                                      ;; sublime-themes
                                      ;; xml-rpc
                                      zenburn-theme
                                      ;;                                      ag
                                      auto-capitalize
                                      beacon
                                      blimp
                                      bongo
                                      buffer-stack
                                      caps-lock
                                      change-inner
                                      cheatsheet
                                      command-log-mode
                                      counsel
                                      spotify
                                      crux
                                      cyberpunk-theme
                                      dired+
                                      dired-details+
                                      dired-quick-sort
                                      dired-sort-menu
                                      discover-my-major
                                      emms
                                      expand-region
                                      fancy-narrow
                                      fastdef
                                      flyspell-lazy
                                      fountain-mode
                                      frame-cmds
                                      fuzzy
                                      gist
                                      helm
                                      helm-cmd-t
                                      ido-hacks
                                      imenu-list
                                      key-chord
                                      key-seq
                                      maxframe
                                      mpv
                                      multicolumn
                                      multiple-cursors
                                      nm
                                      olivetti
                                      openwith
                                      org-bookmark-heading
                                      org-fstree
                                      org-mime
                                      org-projectile
                                      org-projectile-helm
                                      org-sticky-header
                                      ox-clip
                                      ox-gfm
                                      ox-twbs
                                      ox-tufte
                                      palimpsest
                                      paredit
                                      peg
                                      point-stack
                                      polymode
                                      popup
                                      project-explorer
                                      rainbow-mode
                                      re-builder
                                      recentf
                                      regex-tool
                                      rspec-mode
                                      scratch
                                      scratch-message
                                      stripe-buffer
                                      tabbar
                                      tiny
                                      tldr
                                      transcribe
                                      unfill
                                      visible-mark
                                      wc-mode
                                      web-mode
                                      wrap-region
                                      writeroom-mode
                                      wttrin
                                      xah-replace-pairs
                                      yahoo-weather
                                      zone
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    adaptive-wrap
                                    auto-encryption-mode
                                    clean-aiindent-mode
                                    company
                                    diff-auto-refine-mode
                                    electric-indent-mode
                                    evil-mode
                                    evil-mode
                                    evil-escape
                                    evil-local
                                    evil-org
                                    evil-search-highlight-persist
                                    evil-surround
                                    eyebrowse
                                    global-undo-tree-mode
                                    evil-unimpaired
                                    exec-path-from-shell
                                    global-git-commit
                                    highlight-indentation
                                    hs-mode
                                    persp-mode
                                    popwin
                                    spaceline
                                    vi-tilde-fringe
                                    volatile-highlights
                                    winner
                                    winner-mode-enable
                                    xterm-mouse
                                    xterm-mouse-mode
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'nil

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         whiteboard
                         leuven
                         zenburn
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         monokai
                         zenburn)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "]"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
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

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis 'all
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."



;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)

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



  (require 'auto-capitalize)
  (require 'recentf)
  (setq recentf-save-file "/Users/jay/emacs/emacs-settings/.savefile/recentf")

  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))
  (use-package mu4e)

  ;; (require 'package)
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; (package-initialize)
  ;; -----------------------------THIS EXISTS IN DEFAULT SPACEMACS config



(require 'helm)
(require 'ivy)
(require 'tiny)
(require 're-builder)
(require 'multiple-cursors)


  
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
      (ditaa . t)
      ))

(load "/Users/jay/emacs/emacs-settings/zones.el")
(require 'zones)

(load "/Users/jay/emacs/emacs-settings/pastebin-credentials.el")
  (load "/Users/jay/emacs/emacs-settings/tufte-org-mode-master/ox-tufte-latex.el")
  (load "/Users/jay/emacs/emacs-settings/jay-osx.el")
  (org-babel-load-file "~/emacs/emacs-settings/gnu-emacs-startup.org")
  (org-babel-load-file "~/emacs/emacs-settings/shared-functions.org")
  (org-babel-load-file "~/emacs/emacs-settings/spacemacs-config.org")
  (org-babel-load-file "/Users/jay/emacs/emacs-settings/fonts-and-themes.org")
  (load "/Users/jay/emacs/prelude/core/prelude-core.el")
;;  (load "/Users/jay/emacs/emacs-settings/skeletons.el")
  (load "/Users/jay/emacs/emacs-settings/prelude-key-chord.el")
  (load "/Users/jay/gnulisp/book-functions.el")
  (load "/Users/jay/gnulisp/reveal-in-finder.el")
  (load "/Users/jay/emacs/emacs-settings/poetry_JD.el")
  (load "/Users/jay/emacs/emacs-settings/define-word.el")
  (load "/Users/jay/emacs/emacs-settings/searchlink/searchlink.el")
  ;; (load "/Users/jay/emacs/emacs-settings/ivy-smex.el")
  (load "/Users/jay/emacs/emacs-settings/emacs_friends.el")
  (load "/Users/jay/emacs/emacs-settings/email.el")
(load "/Users/jay/gnulisp/org-image.el")


(load "/Users/jay/emacs/org-mime.el")
(require 'org-mime)

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
       (find-file "~/nd/top-performer.org")
       ;;       (imenu-list-minor-mode)
       (menu-bar-mode -1)
       ))



;; (spaceline-spacemacs-theme 'narrow)


(setq helm-echo-input-in-header-line nil)

  (add-hook 'helm-after-initialize-hook
            #'(lambda () (setq helm-echo-input-in-header-line nil)))


  ;;  (setq org-bullets-bullet-list '("◉" "◉" "○" "○" "✸" "✸" "✿" "✿")) ; for oddlevelsonly mode
  ;;  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")) ; for SHOWSTARS:evenodd

  (setq org-bullets-bullet-list (quote ("• ")))

  ;; (load "/Users/jay/emacs/emacs-settings/gnugol.el")
  ;; (use-package gnugol)

  (use-package reveal-in-finder)

  (recenter-top-bottom)
  (setq case-fold-search t)

  (setq company-global-modes '(not org-mode))

  (toggle-fullscreen)
  (menu-bar-mode -1)

  (toggle-menu-bar-mode-from-frame)

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

  (setq org-hide-leading-stars nil)


  (add-hook 'org-mode-hook (lambda ()
                             (setq-local global-hl-line-mode
                                         nil)))


   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

  (setq org-bullets-bullet-list '("◉" "◉" "○" "○" "✸" "✸" "✿" "✿")) ; for oddlevelsonly mode
  ;;  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")) ; for SHOWSTARS:evenodd

  (find-file "~/nd/top-performer.org")



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


;;(leuven)
  ;; (spacemacs-light)
;; (zenburn)


  (setq global-auto-complete-mode -1)

  (setq ido-save-directory-list-file "/Users/jay/emacs/emacs-settings/spacemacs.d/.savefile/ido.hist")


  (display-time)

  ;; (setq evil-emacs-state-cursor '("red" (hbar . 2))) ; for horizontal cursor
  (setq evil-emacs-state-cursor '("red")) ; for box cursor


  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  ;; (horizontal-cursor)
  (box-cursor)
  (incarnadine-cursor)
  (scroll-bar-mode 1)
  (defun package--save-selected-packages (&rest opt) nil)

  (load "/Users/jay/emacs/emacs-settings/mu4e-send-delay.el")
  (require 'mu4e-send-delay)

  ;; To permanently enable mode line display of org clock, add this snippet to your dotspacemacs/user-config function:
  ;; (setq spaceline-org-clock-p t)

  (setq user-init-file "/Users/jay/emacs/emacs-settings/spacemacs.d/init.el")

  (setq org-agenda-files (quote ("~/Dropbox/writing/notationaldata/accountability.org"
                                 ;; "/Users/jay/Dropbox/writing/book/feb-18/narcs.txt"
                                 ;; "/Users/jay/Dropbox/writing/book/feb-18/50-shades.txt"
                                 ;;"/Users/jay/Dropbox/writing/book/feb-18/ovulatory-shift.txt"
                                 "/Users/jay/Dropbox/writing/book/feb-18/0-why-I-wrote-this-book.txt"
                                 )))

(setq case-fold-search t)

; (cyberpunk-jay) 

(counsel-mode 1)


(setq global-hl-line-mode nil)


  )

;; theend
;;
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
;;
;;


(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-mime doom-modeline async goto-chg marshal yasnippet projectile f expand-region pythonic evil w3m avy ht alert notmuch swiper bind-key smartparens ivy flycheck company helm helm-core js2-mode org-projectile org-plus-contrib imenu-list diminish shell-pop multi-term eshell-z eshell-prompt-extras esh-help zenburn-theme yapfify yahoo-weather xah-replace-pairs wttrin ws-butler writeroom-mode wrap-region winum which-key web-mode web-beautify wc-mode uuidgen use-package unfill toc-org tldr tiny tabbar symon stripe-buffer string-inflection solarized-theme smex scratch-message scratch rspec-mode restart-emacs request regex-tool rainbow-mode rainbow-delimiters pyvenv pytest pyenv-mode py-isort project-explorer polymode point-stack pip-requirements pcre2el password-generator paredit paradox palimpsest ox-twbs ox-tufte ox-gfm ox-clip org-sticky-header org-projectilec-helm org-present org-pomodoro org-mac-link org-fstree org-download org-bullets org-brain org-bookmark-heading openwith open-junk-file olivetti nm neotree multicolumn mu4e-maildirs-extension mu4e-alert move-text monokai-theme maxframe macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint key-chord json-mode js2-refactor js-doc info+ indent-guide ido-hacks hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-company helm-cmd-t helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gist fuzzy frame-cmds fountain-mode flyspell-lazy flycheck-pos-tip flx-ido fill-column-indicator fastdef fancy-narrow fancy-battery evil-visualstar evil-visual-mark-mode evil-tutor evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav editorconfig dumb-jump discover-my-major dired-sort-menu dired-quick-sort dired-details+ dired+ define-word cython-mode cyberpunk-theme crux counsel company-statistics command-log-mode column-enforce-mode coffee-mode clean-aindent-mode cheatsheet change-inner caps-lock buffer-stack bongo beacon auto-yasnippet auto-highlight-symbol auto-compile auto-capitalize anaconda-mode aggressive-indent ag ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((org-inline-image-overlays)
     (org-latex-caption-above)
     (org-hide-emphasis-markers . t)
     (org-hide-macro-markers . t)
     (org-fontify-quote-and-verse-blocks . t)
     (eval org-sbe "latex-link")
     (eval org-sbe "latex-opt-link")
     (eval org-sbe "jk-keywords")
     (eval org-sbe "pdf-process-bibtex")
     (eval org-sbe "ngz-nbsp")
     (eval org-sbe "latex-filter-footcites")
     (eval org-sbe "biblatex-cite-link")
     (eval org-sbe "biblatex-textcite-link")
     (eval org-sbe "biblatex-parencite-link")
     (eval org-sbe "biblatex-sidecite-link")
     (eval org-sbe "biblatex-multicite-link")
     (eval org-sbe "biblatex-footcite-link")
     (eval org-sbe "tufte-ebib-setup")
     (eval org-sbe "tufte-handout")
     (eval org-sbe "tufte-book")
     (eval org-sbe "user-entities")
     (eval ox-extras-activate
           (quote
            (ignore-headlines)))
     (eval require
           (quote ox-gfm))
     (eval require
           (quote ox-extra))
     (eval require
           (quote ox-tufte-latex))
     (eval define-key key-minor-mode-map
           (kbd "SPC")
           (quote insert-space))
     (eval define-key org-mode-map
           (kbd ":")
           (quote insert-colon))
     (org-html-head-include-scripts)
     (org-export-allow-bind-keywords . t)
     (eval define-key key-minor-mode-map
           (kbd "SPC")
           (quote insert-space))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon ((t (:inherit highlight))))
 '(bold ((t (:inherit font-lock-warning-face :foreground "Red" :weight bold :height 1.1 :family "Sans Serif"))))
 '(flyspell-duplicate ((t (:underline (:style wave :color "red")))))
 '(flyspell-incorrect ((t (:underline (:style wave :color "red")))))
 '(italic ((t (:foreground "DarkViolet" :slant italic :height 1.4 :family "Garamond"))))
 '(org-drill-visible-cloze-face ((t (:background "#FFFFD7" :foreground "black"))))
 '(org-level-1 ((t (:family "Gotham Narrow" :weight bold))))
 '(org-link ((t (:underline nil :inherit link))))
 '(org-quote ((t (:inherit default :background "#FFFFE0" :foreground "dim gray" :slant normal))))
 '(zz-fringe-for-narrowing ((t (:box (:line-width 2 :color "grey75" :style released-button))))))
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (async goto-chg marshal yasnippet projectile f expand-region pythonic evil w3m avy ht alert notmuch swiper bind-key smartparens ivy flycheck company helm helm-core js2-mode org-projectile org-plus-contrib imenu-list diminish shell-pop multi-term eshell-z eshell-prompt-extras esh-help zenburn-theme yapfify yahoo-weather xah-replace-pairs wttrin ws-butler writeroom-mode wrap-region winum which-key web-mode web-beautify wc-mode uuidgen use-package unfill toc-org tldr tiny tabbar symon stripe-buffer string-inflection solarized-theme smex scratch-message scratch rspec-mode restart-emacs request regex-tool rainbow-mode rainbow-delimiters pyvenv pytest pyenv-mode py-isort project-explorer polymode point-stack pip-requirements pcre2el password-generator paredit paradox palimpsest ox-twbs ox-tufte ox-gfm ox-clip org-sticky-header org-projectilec-helm org-present org-pomodoro org-mac-link org-fstree org-download org-bullets org-brain org-bookmark-heading openwith open-junk-file olivetti nm neotree multicolumn mu4e-maildirs-extension mu4e-alert move-text monokai-theme maxframe macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint key-chord json-mode js2-refactor js-doc info+ indent-guide ido-hacks hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-company helm-cmd-t helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gist fuzzy frame-cmds fountain-mode flyspell-lazy flycheck-pos-tip flx-ido fill-column-indicator fastdef fancy-narrow fancy-battery evil-visualstar evil-visual-mark-mode evil-tutor evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav editorconfig dumb-jump discover-my-major dired-sort-menu dired-quick-sort dired-details+ dired+ define-word cython-mode cyberpunk-theme crux counsel company-statistics command-log-mode column-enforce-mode coffee-mode clean-aindent-mode cheatsheet change-inner caps-lock buffer-stack bongo beacon auto-yasnippet auto-highlight-symbol auto-compile auto-capitalize anaconda-mode aggressive-indent ag ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon ((t (:inherit highlight))))
 '(bold ((t (:inherit font-lock-warning-face :foreground "Red" :weight bold :height 1.1 :family "Sans Serif"))))
 '(flyspell-duplicate ((t (:underline (:style wave :color "red")))))
 '(flyspell-incorrect ((t (:underline (:style wave :color "red")))))
 '(italic ((t (:foreground "DarkViolet" :slant italic :height 1.4 :family "Garamond"))))
 '(org-drill-visible-cloze-face ((t (:background "#FFFFD7" :foreground "black"))))
 '(org-level-1 ((t (:family "Gotham Narrow" :weight bold))))
 '(org-link ((t (:underline nil :inherit link))))
 '(org-quote ((t (:inherit default :background "#FFFFE0" :foreground "dim gray" :slant normal)))))

