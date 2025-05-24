;; -*- Mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.



(defmacro org-assert-version ()
  "Assert compile time and runtime version match."
  ;; We intentionally use a more permissive `org-release' instead of
  ;; `org-git-version' to work around deficiencies in Elisp
  ;; compilation after pulling latest changes.  Unchanged files will
  ;; not be re-compiled and thus their macro-expanded
  ;; `org-assert-version' calls would fail using strict
  ;; `org-git-version' check because the generated Org version strings
  ;; will not match.
  `(unless (equal (org-release) ,(org-release))
     (warn "Org version mismatch.  Make sure that correct `load-path' is set early in init.el
This warning usually appears when a built-in Org version is loaded
prior to the more recent Org version.

Version mismatch is commonly encountered in the following situations:

1. Emacs is loaded using literate Org config and more recent Org
   version is loaded inside the file loaded by `org-babel-load-file'.
   `org-babel-load-file' triggers the built-in Org version clashing
   the newer Org version attempt to be loaded later.

   It is recommended to move the Org loading code before the
   `org-babel-load-file' call.

2. New Org version is loaded manually by setting `load-path', but some
   other package depending on Org is loaded before the `load-path' is
   configured.
   This \"other package\" is triggering built-in Org version, again
   causing the version mismatch.

   It is recommended to set `load-path' as early in the config as
   possible.

3. New Org version is loaded using straight.el package manager and
   other package depending on Org is loaded before straight triggers
   loading of the newer Org version.

   It is recommended to put

    %s

   early in the config.  Ideally, right after the straight.el
   bootstrap.  Moving `use-package' :straight declaration may not be
   sufficient if the corresponding `use-package' statement is
   deferring the loading."
           ;; Avoid `warn' replacing "'" with "’" (see `format-message').
           "(straight-use-package 'org)")
     (error "Org version mismatch.  Make sure that correct `load-path' is set early in init.el")))

;; We rely on org-macs when generating Org version.  Checking Org
;; version here will interfere with Org build process.
;; (org-assert-version)



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
   dotspacemacs-enable-lazy-installation 'nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("/Users/jay/emacs/emacs-settings/spacemacs.d")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(python
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; ag-general
     ;; ag-lang-tools
     auto-completion
     ;; better-defaults
     compleseus
     ;; chrome
     ;; deft
     ;; emoji
     ;; floobits
     ;; helm
     ;; html
     ;; ivy
     ;; jay
     ;; markdown
     ;; mu4e
     emacs-lisp
     ;;     eww
     ;; (multiple-cursors :variables multiple-cursors-backend 'mc)
     (org
      :defer
      :variables
      org-enable-roam-protocol t
      org-enable-roam-support t
      org-export-backends '(ascii html icalendar latex md)
                                        ; org-enable-hugo-support to

      :config
      (require 'ox-extra)
      (ox-extra-activate '(ignore-headlines))
      )
     osx
     ;; pdf
     ;; python
     ;; shell
     ;; shell-scripts
                                        ; smex
     ;; speed-reading
     spell-checking
     ;; syntax-checking ; this is the layer with flycheck
     ;; treemacs
     ;; typography
     ;; xkcd

     )
   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(

                                      org-transclusion
                                      ctrlf
                                      web-mode
                                      ox-twbs
                                      mwim
                                      affe
                                      beacon
                                      bind-key
                                      bui
                                      ;; buffer-stack
                                      caps-lock
                                      captain
                                      change-inner
                                      ;; cheatsheet
                                      company
                                      consult-dir
                                      ;; consult-notes
                                      consult-ag
                                      consult-org-roam
                                      ;; benchmark-init
                                      consult-projectile ;; searches filenames
                                      counsel
                                      counsel-fd
                                      counsel-projectile
                                      crux
                                      deadgrep
                                      define-word
                                      dired-quick-sort
                                      ;; dired-sort-menu
                                      discover-my-major
                                      emacsql
                                      ;; embark
                                      ;; epc
                                      ;; eww
                                      exec-path-from-shell
                                      expand-region
                                      f
                                      fancy-narrow
                                      fasd
                                      flyspell-lazy
                                      ;; frame-cmds
                                      fuzzy
                                      gcmh ; The Garbage Collector Magic Hack
                                      google-this
                                      ;; gptel
                                      ;; helpful
                                      ;; ido-hacks
                                      ;; jinx
                                      key-seq
                                      ;; lister ;; (for delve)
                                      ;; magit
                                        ; markdown-mode
                                      maxframe
                                      multiple-cursors
                                      mw-thesaurus
                                      olivetti
                                      openwith
                                      orderless
                                      org-ai
                                      org-autolist
                                      org-bookmark-heading
                                      ;; org-contrib
                                      ;; org-drill
                                      ;; org-fstree
                                      org-mac-link
                                      org-pomodoro
                                      org-ql
                                      org-roam
                                      org-roam-ui
                                      ;; org-superstar
                                      ox-clip
                                      ;; ox-gfm
                                      ox-timeline
                                      ox-tufte
                                      palimpsest
                                      paredit
                                      peg
                                      persist
                                      ;; persp-mode
                                      point-stack
                                      popup
                                      ;; project-explorer
                                      projectile-ripgrep
                                      quelpa-use-package
                                      rainbow-mode
                                      re-builder
                                      recentf
                                      regex-tool
                                      rg
                                      ripgrep
                                      rspec-mode
                                      ;; scratch
                                      ;; scratch-message
                                      sdcv
                                      ;; simple-httpd
                                      ;; stripe-buffer
                                      sudo-edit
                                      sync-recentf
                                        ; tabbar
                                      tiny
                                      titlecase
                                      ts
                                      unfill
                                      visible-mark
                                      wc-goal-mode
                                      wc-mode
                                      ;; web-mode
                                      with-editor
                                      wrap-region
                                      ;; xah-replace-pairs

                                      ;; ag
                                      auto-complete
                                      ;; blimp
                                      ;; bongo
                                      ;; command-log-mode
                                      ;; cyberpunk-theme
                                      ;; dired-hacks-utils
                                      ;; dired-single
                                      ;; early-init
                                      ;; emms
                                      ;; fastdef
                                      ;; focus
                                      ;; fountain-mode
                                      ;; frame-restore
                                      ;; gist
                                      ;; graphviz-dot-mode
                                      ;; imenu-list
                                      ;; key-chord
                                      ;; magit-section
                                      ;; mpv
                                      ;; multicolumn
                                      ;; nm
                                      ;; org-download
                                      ;; org-mime
                                      ;; org-noter
                                      ;; org-noter-pdftools
                                      ;; org-pdftools
                                      ;; org-sidebar
                                      ;; ox-epub
                                      ;; ox-twbs
                                      ;; plain-org-wiki
                                      ;; polymode
                                      ;; solarized-theme
                                      ;; spotify
                                      ;; sublime-themes
                                      ;; tldr
                                      ;; transcribe
                                      ;; websocket
                                      ;; writeroom-mode
                                      ;; wttrin
                                      ;; xml-rpc
                                      ;; yahoo-weather
                                      ;; zenburn-theme
                                      ;; zone
                                      smex
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    vim-powerline
                                    adaptive-wrap
                                    auto-encryption-mode
                                    clean-aindent-mode
                                    company
                                    diff-auto-refine-mode
                                    diff-hl
                                    eldoc
                                    elisa
                                    electric-indent-mode
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
                                    gnus
                                    helm
                                    highlight-indentation
                                    hl-line
                                    hs-mode
                                    ;; ivy
                                    notmuch
                                    org-download
                                    org-superstar
                                    org-timeblock
                                    popwin
                                    spaceline
                                    treemacs-icons-dired
                                    vterm
                                    vi-tilde-fringe
                                    volatile-highlights
                                    winner
                                    winner-mode-enable
                                    xterm-mouse
                                    xterm-mouse-mode

                                    savehist
                                    magit
                                    persp-mode
                                    popup
                                    polymide
                                    markdown-mode

                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))
;;   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory nil

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'org-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   ;; dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   ;; dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         ;; whiteboard
                         ;; cyberpunk
                         leuven
                         zenburn
                         spacemacs-dark
                         spacemacs-light
                         monokai
                         zenburn)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Monaco"
                               :size 14.0
                               :weight normal
                               :width normal)

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

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
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-printscreen>" "C-M-m")

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;; :visual nil
   ;; :disabled-for-modes dired-mode
   ;;                    doc-view-mode
   ;;                    markdown-mode
   ;;                    org-mode
   ;;                    pdf-view-mode
   ;;                    text-mode
   ;; :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis 't
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

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

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

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
   dotspacemacs-byte-compile nil))

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

  ;; (setq debug-on-error t)



  (setq native-comp-enable-subr-trampolines nil)

  ;; Temporary band‑aid so Emacs launches even if smart‑quotes isn't ready
  (defvar smart-quotes-replacement-pairs nil
    "Alist of smart‑quote glyphs and their ASCII replacements.")

  ;; --- tell Flyspell / Ispell to use Hunspell --------------------
  (setq-default ispell-program-name "/opt/homebrew/bin/hunspell"
                ispell-really-hunspell t
                ispell-dictionary      "en_US-large"
                ispell-personal-dictionary
                (expand-file-name "~/Library/Spelling/personal.dic"))

  (setenv "DICPATH"   (expand-file-name "~/Library/Spelling"))
  (setenv "DICTIONARY" "en_US")


  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq gc-cons-threshold 100000000) ; 32mb, or 64mb, or *maybe* 128mb, BUT NOT 512mb
  (setq read-process-output-max (* 1024 1024))
                                        ; Set (setq gc-cons-threshold 100000000) and (setq read-process-output-max (* 1024 1024)) early in your config.


  ;; Define the minor mode so it's loaded on startup.
  (define-minor-mode org-config-files-local-mode
    "Minor mode for editing configuration files in Org mode."
    :init-value nil
    :lighter " OrgCfg"
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "<SPC>") 'insert-space)
              map)
    (message "org-config-files-local-mode is now %s" org-config-files-local-mode)
    (if org-config-files-local-mode
        (setq-local abbrev-mode nil)
      (kill-local-variable 'abbrev-mode)))



  (setq Info-default-directory-list '("/opt/homebrew/share/info"))
  (setq Info-directory-list '("/opt/homebrew/share/info"))

  (with-eval-after-load 'info
    (setq Info-directory-list '("/opt/homebrew/share/info")))

  (add-hook 'after-init-hook
            (lambda ()
              (setq Info-directory-list '("/opt/homebrew/share/info"))))

  (setq Info-additional-directory-list nil)


  ;; 1. ordinary “safe local variables”
  (add-to-list 'safe-local-variable-values '(lexical-binding . t))
  (put 'org-config-files-local-mode 'safe-local-variable #'booleanp)
  (put 'enable-local-eval        'safe-local-variable #'booleanp)

  ;; 2. eval forms you’re willing to trust (Emacs 29+; see note below)
  (when (boundp 'safe-local-eval-forms)
    (add-to-list 'safe-local-eval-forms
                 '(org-config-files-local-mode 1)))

  (load "/Users/jay/emacs/emacs-settings/spacemacs-new-config.el")
  (load "/Users/jay/gnulisp/smart-return.el")
  (load "/Users/jay/emacs/emacs-settings/elpa-supplement/buffer-stack.el")
  (load "/Users/jay/emacs/emacs-settings/elpa-supplement/frame-cmds.el")
  (load "/Users/jay/emacs/local-config.el")
  ;;  (load "/Users/jay/emacs/emacs-settings/aibo-config.el")
  ;; (load "/Users/jay/emacs/emacs-settings/aibo-power-pack.el")


  ;; Define a list of additional paths
  (defvar my-additional-paths
    '("/usr/local/bin"
      "/opt/homebrew/bin"
      "/Applications/Firefox.app/Contents/MacOS"))

  ;; Update PATH environment variable
  (setenv "PATH"
          (concat (getenv "PATH") ":"
                  (mapconcat 'identity my-additional-paths ":")))

  ;; Update exec-path
  (dolist (path my-additional-paths)
    (add-to-list 'exec-path path))



  ;; Rest of your configuration
  (message "Final PATH: %s" (getenv "PATH"))



  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.


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
   '(amx-history-length 199)
   '(calendar-week-start-day 1)
   '(counsel-search-engine 'google)
   '(deft-directory
     "/Users/jay/Library/Mobile Documents/27N4MQEA55~pro~writer/Documents/" t)
   '(dired-preview-delay 0.2)
   '(evil-want-Y-yank-to-eol nil)
   '(eww-search-prefix "https://www.google.com/search?q=jay+dixit/?q=")
   '(flycheck-disabled-checkers '(proselint))
   '(gptel-model "gpt-4")
   '(jinx-include-modes '(text-mode prog-mode conf-mode org-mode))
   '(line-number-mode t)
   '(marginalia-align 'left nil nil "Customized with use-package marginalia")
   '(mode-line-compact 'long)
   '(org-attach-expert nil)
   '(org-attach-id-dir "/Users/jay/Dropbox/roam/attachments")
   '(org-ellipsis " 🪜 ")
   '(org-export-preserve-breaks nil)
   '(org-html-prefer-user-labels t)
   '(org-noter-auto-save-last-location t)
   '(org-noter-insert-note-no-questions t)
   '(org-sidebar-jump-indirect t)
   '(org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source)
   '(org-superstar-headline-bullets-list '(42 42 42 42))
   '(org-superstar-item-bullet-alist '((42 . 183) (43 . 183) (45 . 183)))
   '(org-superstar-remove-leading-stars t)
   '(org-tempo-keywords-alist
     '(("l " . "src emacs-lisp ")
       ("H" . "html")
       ("A" . "ascii")
       ("i" . "index")))
   '(org-transclusion-exclude-elements '(property-drawer))
   '(org-transclusion-extensions
     '(org-transclusion-src-lines org-transclusion-font-lock org-transclusion-indent-mode))
   '(org-transclusion-include-first-section nil)
   '(org-twbs-head-include-default-style nil)
   '(org-twbs-head-include-scripts nil)
   '(org-twbs-htmlize-output-type 'css)
   '(package-selected-packages
     '(smex emacsql-sqlite s mw-thesaurus org-bookmark-heading org-mac-link org-pomodoro org-roam-ui org-sticky-header ox-clip ox-tufte palimpsest paredit point-stack popup quelpa-use-package rainbow-mode re-builder recentf regex-tool rspec-mode sudo-edit tiny titlecase unfill visible-mark wc-mode web-mode wrap-region xah-replace-pairs))
   '(paradox-github-token t)
   '(smex-save-file "/Users/jay/emacs/local-emacs-config/smex-items")
   '(sp-escape-wrapped-region nil)
   '(tab-bar-close-button-show nil)
   '(tab-bar-new-button-show nil)
   '(warning-suppress-log-types '((use-package))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(beacon ((t (:inherit highlight))))
   '(bold ((t (:inherit font-lock-warning-face :weight bold))))
   '(flyspell-duplicate ((t (:underline (:style wave :color "red")))))
   '(flyspell-incorrect ((t (:underline (:style wave :color "red")))))
   '(font-lock-comment-face ((t (:foreground "red" :slant italic))))
   '(italic ((t (:foreground "DarkViolet" :slant italic :height 1.2 :family "Garamond"))))
   '(marginalia-documentation ((t (:inherit marginalia-key :foreground "dark red"))))
   '(org-code ((t (:inherit shadow :box (:line-width (2 . 2) :color "grey75" :style released-button) :underline nil))))
   '(org-done ((t (:background "light green"))))
   '(org-drill-visible-cloze-face ((t (:background "#FFFFD7" :foreground "black"))))
   '(org-ellipsis ((t (:family "Iosevka Nerd Font" :foreground "LightGray" :underline nil))))
   '(org-headline-done ((t (:strike-through t))))
   '(org-level-6 ((t (:extend nil :foreground "#0077CC" :slant normal :weight bold :height 1.0))))
   '(org-link ((t (:inherit link :underline nil))))
   '(org-list-dt ((t (:box nil :weight bold :height 1.4))))
   '(org-quote ((t (:inherit default :background "#f5f7ff" :foreground "Charcoal" :slant normal :height 0.8 :family "Consolas"))))
   '(org-transclusion ((t (:extend t :background "light steel blue" :foreground "black"))))
   '(org-transclusion-fringe ((t (:background "yellow" :foreground "yellow"))))
   '(org-transclusion-source ((t (:background "#ebf6fa"))))
   '(org-transclusion-source-edit ((t (:background "#fff3da"))))
   '(org-transclusion-source-fringe ((t nil)))
   '(tab-bar ((t (:inherit (default variable-pitch) :background "black" :foreground "black" :height 2.0))))
   '(tab-bar-tab ((t (:inherit tab-bar :box 1))))
   '(tab-bar-tab-group-inactive ((t (:inherit (shadow tab-bar-tab-inactive) :foreground "black"))))
   '(variable-pitch ((t (:family "\"Courier\" "))))
   '(zz-fringe-for-narrowing ((t (:background "#darkgrey")))))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-disabled-checkers '(proselint))
 ;; '(org-agenda-files '("/Users/jay/Dropbox/roam/notes/accountability.org"))
 '(org-export-preserve-breaks nil)
 '(org-noter-auto-save-last-location t)
 '(org-noter-insert-note-no-questions t)
 '(org-sidebar-jump-indirect t)
 '(org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source)
 '(package-selected-packages
   '(
     emacsql-sqlite
     s
     mw-thesaurus
     org-bookmark-heading
     ;; org-download
     ;; org-drill
     ;;         org-fstree
     org-mac-link
     ;; org-mime
     org-pomodoro
     ;; org-roam
     ;;                   org-roam-ui
     org-sticky-header
     ox-clip
     ;; ox-epub
     ;; ox-gfm
     ;; org-noter
     ;; org-noter-pdftools
     ;; org-pdftools
     ;; org-sidebar
     ;; ox-twbs
     ox-tufte
     palimpsest
     paredit
     ;; peg
     ;; persp-mode
     ;; plain-org-wiki
     point-stack
     ;; polymode
     ;; popup
     ;; project-explorer
     quelpa-use-package
     rainbow-mode
     re-builder
     recentf
     regex-tool
     rspec-mode
     ;;                 scratch
     ;;               scratch-message
     ;;             stripe-buffer
     sudo-edit
     ;; tabbar
     ;;           tiny
     titlecase
     ;; tldr
     ;; transcribe
     unfill
     visible-mark
     wc-mode
     ;; web-mode
     wrap-region
     ;; writeroom-mode
     xah-replace-pairs
     )
   ))
