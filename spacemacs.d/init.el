;; -*- mode: emacs-lisp; lexical-binding: t -*-
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
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

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
     auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     org
     ;; helm
     compleseus
     ;; lsp
     ;; markdown
     multiple-cursors
     ;; python

     ;; i commented out the part below so that spacemacs
     ;; wouldn't download its outdated Elpa version of org
     ;;  (org
     ;;    :ensure org-plus-contrib
     ;;    :mode (("\\.org$" . org-mode))
     ;; :variables
     ;; org-enable-bootstrap-support t
     ;; org-enable-github-support t
     ;; ;; org-enable-reveal-js-support t
     ;; :location built-in
     ;; )
     osx
     ;; xkcd
     ;; ag-general
     ;; ag-lang-tools
     ;; pdf
     smex
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking ; this is the layer with flycheck
     ;; version-control
     ;; html
     ;; mu4e
     ;; shell
     ;; shell-scripts
     ;; jay
     ;; speed-reading
     ;; emoji
     ;; typography
     ;; javascript
     ;; floobits
     ;; deft
     ;; chrome
     ;; themes-megapack
     ;; ibuffer
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
                                      ;; auto-complete
                                      ;; dired-hacks-utils
                                      ;; dired-single
                                      ;; frame-restore
                                      ;; helm-projectile
                                      ;; markdown-mode
                                      ;; notmuch
                                      ;; projectile
                                      ;; graphviz-dot-mode
                                      ;; js2
                                      ;; magit
                                      ;; smex
                                      ;; solarized-theme
                                      ;; sublime-themes
                                      ;; xml-rpc
                                      ;; zenburn-theme
                                      ;; ag
                                      auto-capitalize
                                      beacon
                                      benchmark-init
                                      ;; blimp
                                      ;; bongo
                                      buffer-stack
                                      caps-lock
                                      change-inner
                                      chatgpt
                                      cheatsheet
                                      command-log-mode
                                      ;; counsel
                                      ;; spotify
                                      crux
                                      cyberpunk-theme
                                      ;; cyberpunk-theme
                                      dired+
                                      dired-details+
                                      dired-quick-sort
                                      dired-sort-menu
                                      discover-my-major
                                      ;; early-init
                                      emacsql
                                      magit-section
                                      embark
                                      ;; emms
                                      epc
                                      expand-region
                                      fancy-narrow
                                      fastdef
                                      flyspell-lazy
                                      ;; focus
                                      ;; fountain-mode
                                      frame-cmds
                                      fuzzy
                                      gcmh
                                      ;; gist
                                      ;; helm
                                      ;; helm-cmd-t
                                      ;; hyperbole
                                      ido-hacks
                                      ;; imenu-list
                                      ;; key-chord
                                      key-seq
                                      markdown-mode
                                      maxframe
                                      mpv
                                      multicolumn
                                      multiple-cursors
                                      mw-thesaurus
                                      ;; nm
                                      olivetti
                                      openwith
                                      ;;  orgalist
                                      orderless
                                      org-contrib
                                      org-bookmark-heading
				                              ;; org-download
                                      ;; org-drill
                                      org-fstree
                                      org-mac-link
                                      ;; org-mime
                                      org-pomodoro
                                      org-roam
                                      org-roam-ui
                                      websocket
                                      simple-httpd
                                      f
                                      org-sticky-header
                                      ox-clip
                                      ;; ox-epub
                                      ox-gfm
                                      org-modern
                                      ;; org-noter
                                      ;; org-noter-pdftools
                                      ;; org-pdftools
                                      ;; org-sidebar
                                      ;; ox-twbs
                                      ox-tufte
                                      palimpsest
                                      paredit
                                      peg
                                      persp-mode
                                      plain-org-wiki
                                      point-stack
                                      polymode
                                      popup
                                      project-explorer
				                              quelpa-use-package
                                      rainbow-mode
                                      re-builder
                                      recentf
                                      regex-tool
                                      rspec-mode
                                      scratch
                                      scratch-message
                                      stripe-buffer
                                      sudo-edit
                                      tabbar
                                      tiny
                                      titlecase
                                      ;; tldr
                                      ;; transcribe
                                      unfill
                                      visible-mark
                                      wc-mode
                                      web-mode
                                      wrap-region
                                      ;; writeroom-mode
                                      ;; wttrin
                                      xah-replace-pairs
                                      ;; yahoo-weather
                                      ;; zone
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    adaptive-wrap
                                    auto-encryption-mode
                                    clean-aiindent-mode
                                    company
                                    counsel
                                    diff-auto-refine-mode
                                    diff-hl
                                    eldoc
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
                                    helm
                                    highlight-indentation
                                    hl-line
                                    hs-mode
                                    ivy
                                    notmuch
				                            org-download
                                    org-projectile
                                    org-superstar
                                    popwin
                                    spaceline
                                    swiper
                                    treemacs-icons-dired
                                    vterm
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
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;; ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

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
   ;; This has no effect in terminal or if "all-the-icons" package or the font
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
   ;; dotspacemacs-initial-scratch-message nilq

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         whiteboard
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
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
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
   dotspacemacs-activate-smartparens-mode t

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
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

(load "/Users/jay/emacs/emacs-settings/spacemacs-new-config.el")
  )


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
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-disabled-checkers '(proselint))
 '(org-agenda-files
   '("/Users/jay/Dropbox/writing/notationaldata/accountability.org"))
 '(org-export-preserve-breaks nil)
 '(org-noter-auto-save-last-location t)
 '(org-noter-insert-note-no-questions t)
 '(org-sidebar-jump-indirect t)
 '(org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source)
 '(org-tempo-keywords-alist
   '(("l " . "src emacs-lisp ")
     ("H" . "html")
     ("A" . "ascii")
     ("i" . "index")))
 '(package-selected-packages
   '(org-noter benchmark-init magit-section emacsql-sqlite s mw-thesaurus ox-epub org-brain packed json-snatcher org-drill persist org-noter-pdftools org-pdftools pdf-tools tablist auto-complete emacsql-sqlite3 emacsql goto-chg popup log4e gcmh map transient skewer-mode focus org-super-agenda org-mind-map pos-tip ov flx bind-key f htmlize plain-org-wiki ts hyperbole simple-httpd org-sidebar org-ql dash-functional evil sudo-edit smartparens async graphviz-dot-mode iedit yasnippet ht lv visual-fill-column avy hydra helm-core company projectile dash xterm-color swiper ivy flycheck js2-mode expand-region anzu helm alert paradox w3m multiple-cursors org-plus-contrib zenburn-theme yahoo-weather xah-replace-pairs wttrin ws-butler writeroom-mode wrap-region winum web-mode web-beautify wc-mode visible-mark uuidgen use-package unfill transcribe toc-org tldr tiny tabbar stripe-buffer spotify spinner solarized-theme smex shell-pop scratch-message scratch rspec-mode restart-emacs request regex-tool rainbow-mode rainbow-delimiters project-explorer polymode point-stack pcre2el paredit palimpsest ox-twbs ox-tufte ox-gfm ox-clip orgalist org-sticky-header org-projectile org-present org-pomodoro org-mime org-fstree org-bookmark-heading openwith open-junk-file olivetti nm neotree multicolumn multi-term mu4e-maildirs-extension mu4e-alert mpv move-text monokai-theme maxframe macrostep lorem-ipsum livid-mode linum-relative link-hint key-seq json-mode js2-refactor js-doc indent-guide imenu-list ido-hacks hungry-delete highlight-parentheses highlight-numbers helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-company helm-cmd-t helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gist fuzzy frame-cmds fountain-mode flyspell-lazy flycheck-pos-tip flx-ido fill-column-indicator fastdef fancy-narrow fancy-battery evil-visualstar evil-visual-mark-mode evil-tutor evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emms elisp-slime-nav dumb-jump discover-my-major dired-sort-menu dired-quick-sort dired-details+ dired+ diminish define-word cyberpunk-theme crux counsel company-statistics command-log-mode column-enforce-mode coffee-mode clean-aindent-mode cheatsheet change-inner caps-lock buffer-stack bongo blimp beacon auto-yasnippet auto-highlight-symbol auto-compile auto-capitalize aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))
 '(plain-org-wiki-directory "~/book/sending-the-proposal/")
 '(safe-local-variable-values
   '((eval triplicate-code)
     (eval define-key key-minor-mode-map
           (kbd "SPC")
           'insert-space)
     (eval define-key org-mode-map
           (kbd ":")
           'insert-colon)
     (org-html-head-include-scripts)
     (org-export-allow-bind-keywords . t)
     (eval define-key key-minor-mode-map
           (kbd "SPC")
           'insert-space)))
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
 '(font-lock-comment-face ((t (:foreground "red" :weight bold))))
 '(italic ((t (:foreground "DarkViolet" :slant italic :height 1.4 :family "Garamond"))))
 '(org-code ((t (:inherit shadow :underline nil))))
 '(org-drill-visible-cloze-face ((t (:background "#FFFFD7" :foreground "black"))))
 '(org-ellipsis ((t (:foreground "DarkGoldenrod" :underline nil))))
 '(org-link ((t (:inherit link :underline nil))))
 '(org-list-dt ((t (:box nil :weight bold :height 1.4))))
 '(org-quote ((t (:inherit default :background "#FFFFE0" :foreground "dim sgray" :slant normal))))
 '(tab-bar ((t (:inherit (default variable-pitch) :background "black" :foreground "black" :height 2.0))))
 '(tab-bar-tab ((t (:inherit tab-bar :box 1))))
 '(zz-fringe-for-narrowing ((t (:background "#darkgrey")))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-disabled-checkers '(proselint))
 '(org-agenda-files '("~/Dropbox/writing/notationaldata/accountability.org"))
 '(org-export-preserve-breaks nil)
 '(org-noter-auto-save-last-location t)
 '(org-noter-insert-note-no-questions t)
 '(org-sidebar-jump-indirect t)
 '(org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source)
 '(package-selected-packages
   '(markdown-mode magit-section emacsql-sqlite s mw-thesaurus ox-epub org-brain packed org-roam                             org-bookmark-heading
				                              ;; org-download
                                      ;; org-drill
                                      org-fstree
                                      org-mac-link
                                      ;; org-mime
                                      org-pomodoro
                                      org-roam
                                      org-roam-ui
                                      org-sticky-header
                                      ox-clip
                                      ;; ox-epub
                                      ox-gfm
                                      org-modern
                                      ;; org-noter
                                      ;; org-noter-pdftools
                                      ;; org-pdftools
                                      ;; org-sidebar
                                      ;; ox-twbs
                                      ox-tufte
                                      palimpsest
                                      paredit
                                      peg
                                      persp-mode
                                      plain-org-wiki
                                      point-stack
                                      polymode
                                      popup
                                      project-explorer
				                              quelpa-use-package
                                      rainbow-mode
                                      re-builder
                                      recentf
                                      regex-tool
                                      rspec-mode
                                      scratch
                                      scratch-message
                                      stripe-buffer
                                      sudo-edit
                                      tabbar
                                      tiny
                                      titlecase
                                      ;; tldr
                                      ;; transcribe
                                      unfill
                                      visible-mark
                                      wc-mode
                                      web-mode
                                      wrap-region
                                      ;; writeroom-mode
                                      ;; wttrin
                                      xah-replace-pairs
                                      ;; yahoo-weather
                                      ;; zone
                                      )
))
