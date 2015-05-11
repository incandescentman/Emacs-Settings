;; load org-mode
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org" load-path)
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org/lisp" load-path)
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org/contrib" load-path)
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org/contrib/lisp" load-path)

;; export formats I'm not currently using 
;; (require 'ox-md)
;; (require 'ox-ascii)
;; (require 'ox-html)
;; (require 'ox-publish)
;; (require 'ox-s5)
;; (require 'ox-slidy)
;; (load "~/Dropbox/elisp/org/contrib/lisp/ox-s5.el")


;; handle emacs utf-8 input
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")


;;;; STICKY WINDOWS
(global-set-key [(control x) (?0)] 'delete-other-windows)
(global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)
(global-set-key  (kbd "s-0") 'delete-window)
(global-set-key  (kbd "s-1") 'delete-other-windows)
(global-set-key  (kbd "s-9") 'sticky-window-keep-window-visible)
(global-set-key  (kbd "s-2") 'split-window-vertically)
(global-set-key  (kbd "s-3") 'split-window-horizontally)

;;;; ABBREVIATIONS (autocorrect)
;; ===== Automatically load abbreviations table =====
(setq-default abbrev-mode t)
(read-abbrev-file "~/Dropbox/elisp/.abbrev_defs")
(read-abbrev-file "~/Dropbox/elisp/own-abbrevs.abbrev_defs")
(setq save-abbrevs t)


;;;; UI/APPEARANCE
;; clean UI
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)

;; don't highlight lines
(add-hook 'org-mode-hook
          (lambda()
            (hl-line-mode -1)
            (global-hl-line-mode -1))
          't
          )

;; (set-face-attribute 'default nil :family "Inconsolata" :weight 'normal)
(setq prelude-whitespace nil)

;; wrap text
(global-visual-line-mode)





;; add stuff for eshell
;; http://eschulte.github.io/emacs24-starter-kit/starter-kit-eshell.html
;; gmail http://eschulte.github.io/emacs24-starter-kit/starter-kit-gnus.html
;; google docs http://eschulte.github.io/emacs24-starter-kit/starter-kit-g-client.html
;; javascript http://eschulte.github.io/emacs24-starter-kit/starter-kit-js.html
;; elisp http://eschulte.github.io/emacs24-starter-kit/starter-kit-lisp.html



;; load org-babel
(org-babel-lob-ingest
 (expand-file-name
  "library-of-babel.org"
  (expand-file-name
   "babel"
   (expand-file-name
    "contrib"
    (expand-file-name
     "org")))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (js . t)))

;; (setq org-confirm-babel-evaluate nil)


;;;; create custom keybinding prefix 
;; Source: http://stackoverflow.com/questions/5682631/what-are-good-custom-keybindings-in-emacs/5682737#5682737
;; "I have an unconventional approach to this that I recommend highly. I have redefined the C-l ('ell') key to be a prefix key, and I use that to prefix my favorite commands. This key is very easy to type and it is bound to a function ('recenter) that isn't used that much. Well, I don't use 'recenter much, but even if you did, it can be assigned to C-l C-l which is almost as easy to type, and a small price to pay for the possibilities opened up by the Ctrl-L-map. (Actually I prefer 'redraw-display to 'recenter, so I gave that the place of honor.)"

;; use ⌘-m as prefix for my own custom keybindings
(global-unset-key (kbd "s-m"))
(defvar s-m-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by (Command-M)")
(define-key global-map (kbd "s-m") 's-m-prefix)
(fset 's-m-prefix s-m-map)




;;;; fullscreen settings
;; http://amitp.blogspot.ca/2008/05/emacs-full-screen-on-mac-os-x.html
(defvar maxframe-maximized-p nil "maxframe is in fullscreen mode")
(defun toggle-maxframe ()
  "Toggle maximized frame"
  (interactive)
  (setq maxframe-maximized-p (not maxframe-maximized-p))
  (cond (maxframe-maximized-p (maximize-frame))
        (t (restore-frame))))
(define-key global-map [(s-return)] 'toggle-maxframe)

;; make it easy to go fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; and the keybinding 
(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'toggle-fullscreen))
(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f13>") 'toggle-fullscreen))

  (global-set-key (kbd "<f13>") 'toggle-fullscreen)



;;;; directory settings
;; -- DIRECTORY SETTINGS --
;; there is probably a lot of redundancy here, I don't understand this stuff too well
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort)
  (let ((dired-details-internal-overlay-list  ())) (dired-details-hide)))

(add-hook 'dired-load-hook
	  (lambda ()
(require 'dired-sort-menu)))


(defcustom dired-details-hidden-string ""
  "*This string will be shown in place of file details and symbolic links."
  :group 'dired-details
  :type 'string)

(defcustom dired-details-initially-hide t
  "*Hide dired details on entry to dired buffers."
  :group 'dired-details
  :type 'boolean)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable

;; omit the following uninteresting file types from dired displays 
(setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$\\|\\.DS_Store\\|\\.doc$\\|\\.docx$\\|\\.xlsx$\\|\\.ini$\\|\\.fsLockFile$\\|Icon")

;; Enable toggling of uninteresting files.
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t) ; this is buffer-local variable

(defun enable-dired-omit-mode () (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)


;; Load Dired X when Dired is loaded.
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

;; so that I can hide details on dired
(require 'dired-details+)



;;;; other functions

;; function manually remove the scrollbar if default setting fails when creating a new frame
(defun scrollbar-init ()
  (interactive)
  (scroll-bar-mode -1)
  )


;; forgot what this does, I think it's in case Emacs crashes maybe to recover autosaves?
(defadvice recover-session (around disable-dired-omit-for-recover activate)
  (let ((dired-mode-hook dired-mode-hook))
    (remove-hook 'dired-mode-hook 'enable-dired-omit-mode)
    ad-do-it))

;; make kill-sentence work in a more intuitive way
(defun kill-sentence-to-period ()
"Leave the fucking period in there mofo."
(interactive)
(kill-sentence)
(push-mark)
 (insert ".")
 (backward-char))

;; and the keybinding
(global-set-key (kbd "M-k") 'kill-sentence-to-period)



;;;; Isolate Emacs kill ring from the OSX system pasteboard (clipboard).
;; these are custom functions to separate the OSX clipboard from Emacs' kill ring, effectively giving me two separate clipboards to work from. The below are the traditional OSX keybindings for cut/copy/paste, and they will now work with the OSX clipboard. The yank and pop functions still work, and use the Emacs kill ring instead.

(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)

(defun pasteboard-copy()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "pbcopy"))

(defun pasteboard-paste()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (shell-command-on-region
   (point) (if mark-active (mark) (point)) "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'" nil t))

(defun pasteboard-cut()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end)))



;;;; KEYBINDINGS
;; create a custom minor mode to override other keybindings and use mine instead
(defvar key-minor-mode-map (make-keymap) "key-minor-mode keymap.")
(define-minor-mode key-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " key" 'key-minor-mode-map)
(key-minor-mode 1)
(defun my-minibuffer-setup-hook ()
  (key-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; unbind some existing keybindings 
(define-key undo-tree-map (kbd "C-x r") nil)

;; and the keybindings 
;; mk - mykeybindings

(define-key key-minor-mode-map (kbd "s-v") 'pasteboard-paste)
(define-key key-minor-mode-map (kbd "s-x") 'pasteboard-cut)
(define-key key-minor-mode-map (kbd "s-c") 'pasteboard-copy)

;; and make it work in the minibuffer too
(define-key minibuffer-local-map (kbd "s-v") 'pasteboard-paste)
(define-key minibuffer-local-map (kbd "s-x") 'pasteboard-cut)
(define-key minibuffer-local-map (kbd "s-c") 'pasteboard-copy)

(define-key key-minor-mode-map (kbd "C-c C-v") 'org-refile-region)
(define-key key-minor-mode-map (kbd "H-w") 'widen)
(define-key key-minor-mode-map (kbd "C-c e") 'eval-buffer)
(define-key key-minor-mode-map (kbd "C-c r") 'eval-region)
(define-key key-minor-mode-map (kbd "C--") 'goto-last-change) ; super useful when editing
(define-key key-minor-mode-map (kbd "C-d") 'kill-word)
(define-key key-minor-mode-map (kbd "C-j") 'prelude-top-join-line)
(define-key key-minor-mode-map (kbd "=") 'smex) ; call any function with easiest keystroke possible
(define-key key-minor-mode-map (kbd "M-x") 'helm-M-x) ; call helm-M-x instead of regular M-x
(define-key key-minor-mode-map (kbd "\|") 'deft)

(define-key key-minor-mode-map (kbd "C-c j") 'helm-org-headlines) ; also bound to keychord jj
(define-key key-minor-mode-map (kbd "C-x b") 'helm-mini) ; shows recent files; also bound to ⌘-r 
(define-key key-minor-mode-map (kbd "M-b d") 'book-dired) ; show directory of my book folder
(define-key key-minor-mode-map (kbd "M-b r") 'read-a-book) ; show directory of my PDF books 
(define-key key-minor-mode-map (kbd "M-b j") 'read-jd) ; show PDF books I have annotated
(define-key key-minor-mode-map (kbd "M-b M-b") 'book-helm-strict) ; this is a smart function, show recent files in my book folder


;; can't get this to work. for some reason GNU Emacs interprets ⌘-shift-d as s-c
(define-key key-minor-mode-map (kbd "s-D") 'diredp-dired-recent-dirs) 

(define-key key-minor-mode-map (kbd "C-c C-`") 'move-region-to-other-window) ; very useful when working with a split frame

;; for extracting content from my browser
 (define-key key-minor-mode-map (kbd "s-W") 'web-research)
 (define-key key-minor-mode-map (kbd "s-I") 'web-research-quotes)
(define-key key-minor-mode-map (kbd "s-V") 'kdm/html2org-clipboard) ; paste HTML content that I've copied from the web, automatically converting to proper org-mode syntax

;; use OSX standard keybindings ⌘-up and ⌘-down to go to top or bottom of buffer
(define-key key-minor-mode-map [s-up] 'beginning-of-buffer)
(define-key key-minor-mode-map [s-down] 'end-of-buffer)

;; use OSX standard keybinding for "Redo"
(define-key key-minor-mode-map (kbd "s-y") 'undo-tree-redo)

;; use OSX standard keybinding to increase or decrease font size 
(define-key key-minor-mode-map (kbd "s-=") 'text-scale-increase)
(define-key key-minor-mode-map (kbd "s--") 'text-scale-decrease)

;; rebind global help command so that I can use C-h for backspace
(define-key key-minor-mode-map (kbd "M-h") 'help-command)

;; very useful when encountering names and other unfamiliar words
(define-key key-minor-mode-map (kbd "M-+") 'add-word-to-personal-dictionary)

;; navigate between buffers, including uninteresting ones that are hidden by default
(define-key key-minor-mode-map (kbd "M-s-<right>") 'switch-to-next-buffer)
(define-key key-minor-mode-map (kbd "M-s-<left>") 'previous-buffer)

;; a keybinding for "delete" in addition to "backspace"
(define-key key-minor-mode-map (kbd "C-<backspace>") 'delete-char)

;; pomodoro
(define-key key-minor-mode-map (kbd "C-c C-x pi") 'pomodoro-start)
(define-key key-minor-mode-map (kbd "C-c C-x po") 'pomodoro-stop)

;; find files using helm
(define-key key-minor-mode-map (kbd "C-x C-f") 'helm-find-files) 

;; search using helm-swoop
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "M-i") 'helm-multi-swoop-all)

;; edit Emacs preferences using standard OSX keybinding for preferences
(define-key key-minor-mode-map (kbd "s-,") 'customize-group)

;; grep, using current folder as default
(define-key key-minor-mode-map (kbd "s-G") 'helm-do-grep)


;;;; keybindings not currently in use 
;; (define-key key-minor-mode-map (kbd "DEL")  'new-org-delete-backward-char)
;; (global-set-key (kbd "C-c s") 'org-copy-subtree)
;; (define-key key-minor-mode-map (kbd "C-v s") 'org-paste-subtree)
;; (define-key key-minor-mode-map (kbd "s-l") 'org-insert-link)
;; (define-key key-minor-mode-map (kbd "s-i") 'markdown-insert-image)
;; (define-key key-minor-mode-map (kbd "s-\\") 'org-ctrl-c-ctrl-c)
;; (define-key key-minor-mode-map (kbd "s-u") 'ido-dired)
;; (define-key key-minor-mode-map (kbd "H-n") 'org-narrow-to-subtree)
;; (define-key key-minor-mode-map (kbd "H-w") 'widen)
;; (define-key key-minor-mode-map (kbd "H-G") 'prelude-google)
;; (define-key key-minor-mode-map (kbd "s-G") 'osx-browse-guess)
;; (define-key key-minor-mode-map (kbd "s-L") 'org-mac-chrome-insert-frontmost-url)
;; (define-key key-minor-mode-map (kbd "s-;") 'google-define-word-or-phrase)
;; (define-key key-minor-mode-map (kbd "C-c C-x C-o") 'org-pomodoro)
;; (define-key key-minor-mode-map (kbd "s-R") 'web-research)
;; (define-key key-minor-mode-map (kbd "s-v") 'clipboard-yank) 




;;;; startup
(toggle-maxframe)
