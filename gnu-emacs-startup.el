
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")

(global-set-key [(control x) (?0)] 'delete-other-windows)
(global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)
(global-set-key  (kbd "s-0") 'delete-window)
(global-set-key  (kbd "s-1") 'delete-other-windows)
(global-set-key  (kbd "s-9") 'sticky-window-keep-window-visible)
(global-set-key  (kbd "s-2") 'split-window-vertically)
(global-set-key  (kbd "s-3") 'split-window-horizontally)

(setq-default abbrev-mode t)
(read-abbrev-file "~/Dropbox/elisp/.abbrev_defs")
(read-abbrev-file "~/Dropbox/elisp/own-abbrevs.abbrev_defs")
(setq save-abbrevs t)

(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)

(add-hook 'org-mode-hook
          (lambda()
            (hl-line-mode -1)
            (global-hl-line-mode -1))
          't
          )

(setq prelude-whitespace nil)

(global-visual-line-mode)

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


;; (setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$\\|\\.DS_Store\\|\\.doc$\\|\\.docx$\\|\\.xlsx$\\|\\.ini$\\|\\.fsLockFile$\\|Icon")

(setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$\\|\\.DS_Store$\\|\\.doc$\\|\\.docx$\\|\\.ini$\\|\\.rtf$\\|\\Icon$")

;; Enable toggling of uninteresting files.
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t) ; this is buffer-local variable

(defun enable-dired-omit-mode () (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)


;; Load Dired X when Dired is loaded.
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

;; so that I can hide details on dired
(require 'dired-details+)

(defun scrollbar-init ()
  (interactive)
  (scroll-bar-mode -1)
  )

(defadvice recover-session (around disable-dired-omit-for-recover activate)
  (let ((dired-mode-hook dired-mode-hook))
    (remove-hook 'dired-mode-hook 'enable-dired-omit-mode)
    ad-do-it))

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
   (point) (if mark-active (mark) (point)) "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'" nil t)
(my/fix-space)
)

(defun pasteboard-cut()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end))
(my/fix-space)
)

(global-unset-key (kbd "s-m"))
(defvar s-m-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by (Command-M)")
(define-key global-map (kbd "s-m") 's-m-prefix)
(fset 's-m-prefix s-m-map)

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

;; projectile
(define-key key-minor-mode-map (kbd "s-P") 'projectile-commander)

;; and make it work in the minibuffer too
(define-key minibuffer-local-map (kbd "s-v") 'pasteboard-paste)
(define-key minibuffer-local-map (kbd "s-x") 'pasteboard-cut)
(define-key minibuffer-local-map (kbd "s-c") 'pasteboard-copy)

(define-key key-minor-mode-map (kbd "C-c C-v") 'org-refile-region)
(define-key key-minor-mode-map (kbd "H-w") 'widen)
(define-key key-minor-mode-map (kbd "C-c e") 'eval-buffer)
(define-key key-minor-mode-map (kbd "C-c r") 'eval-region)
(define-key key-minor-mode-map (kbd "C--") 'goto-last-change) ; super useful when editing
(define-key key-minor-mode-map (kbd "C-d") 'kill-word-correctly)
(define-key key-minor-mode-map (kbd "C-j") 'prelude-top-join-line)
(define-key key-minor-mode-map (kbd "=") 'smex) ; call any function with easiest keystroke possible
(define-key key-minor-mode-map (kbd "M-x") 'helm-M-x) ; call helm-M-x instead of regular M-x
(define-key key-minor-mode-map (kbd "\|") 'deft)

(define-key key-minor-mode-map (kbd "M-K") 'kill-clause)


(define-key key-minor-mode-map (kbd "C-c j") 'helm-org-headlines) ; also bound to keychord jj
(define-key key-minor-mode-map (kbd "C-x b") 'helm-mini) ; shows recent files; also bound to ⌘-r 
(define-key key-minor-mode-map (kbd "M-b M-d") 'book-dired) ; show directory of my book folder
(define-key key-minor-mode-map (kbd "M-b r") 'read-a-book) ; show directory of my PDF books 
(define-key key-minor-mode-map (kbd "M-b j") 'read-jd) ; show PDF books I have annotated
(define-key key-minor-mode-map (kbd "M-b M-b") 'work-on-book) ; 

(define-key key-minor-mode-map (kbd "M-b M-w") 'work-on-book) ; 

;; book bindings
(define-key key-minor-mode-map (kbd "M-b M-p") 'book-proposal-directory) ; go to my book folder
(define-key key-minor-mode-map (kbd "M-b M-r") 'book-helm-strict) ; this is a smart function, show recent files in my book folder

;; can't get this to work. for some reason GNU Emacs interprets ⌘-shift-d as s-c
(define-key key-minor-mode-map (kbd "s-D") 'diredp-dired-recent-dirs) 

;; recent directories... but how to populate it?
(define-key key-minor-mode-map (kbd "C-S-d") 'diredp-dired-recent-dirs) 

;; own structure editing
(define-key key-minor-mode-map (kbd "C-c C-`") 'move-region-to-other-window) ; very useful when working with a split frame

;; (define-key key-minor-mode-map (kbd "C-c C-w") 'org-refile) ; very useful when working with a split frame

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

;; deleting things
(define-key key-minor-mode-map (kbd "<backspace>") 'my/delete-backward)

;; a keybinding for "delete" in addition to "backspace"
(define-key key-minor-mode-map (kbd "C-<backspace>") 'delete-char)
(define-key key-minor-mode-map (kbd "M-<backspace>") 'backward-kill-word-correctly)
 
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

;; some custom functions
(define-key key-minor-mode-map (kbd "C-c C-m") 'move-region-to-other-window)

(defun kill-sentence-to-period ()
  "Leave the period in there."
  (interactive)
  (kill-sentence)
  (push-mark)
  (insert ".")
  (backward-char))

(defun my/forward-to-sentence-end ()
  "Move point to just before the end of the current sentence."
  (forward-sentence)
  (backward-char)
  (unless (looking-back "[[:alnum:]]")
    (backward-char)))

(defun my/beginning-of-sentence-p ()
  "Return  t if point is at the beginning of a sentence."
  (let ((start (point))
        (beg (save-excursion (forward-sentence) (forward-sentence -1))))
    (eq start beg)))

(defun my/kill-sentence-dwim ()
  "Kill the current sentence up to and possibly including the punctuation.
When point is at the beginning of a sentence, kill the entire
sentence. Otherwise kill forward but preserve any punctuation at the sentence end."
  (interactive)
  (if (my/beginning-of-sentence-p)
      (progn
        (kill-sentence)
        (just-one-space)
        (when (looking-back "^[[:space:]]+") (delete-horizontal-space)))
      (kill-region (point) (progn (my/forward-to-sentence-end) (point)))
      (just-one-space 0)))

;; and the keybinding
(global-set-key (kbd "M-k") 'my/kill-sentence-dwim)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(add-to-list 'load-path "~/gnulisp/emacs-pastebin-master/")
(require 'neopastebin)
(pastebin-create-login :dev-key "e5ccb53890f16065d90ebd6064a381d0"
                       :username "petersalazar")

(defun my/fix-space ()
  "Delete all spaces and tabs around point, leaving one space except at the beginning of a line."
  (interactive)
  (just-one-space)
  (when (looking-back "^[[:space:]]+") (delete-horizontal-space)))

(defun kill-word-correctly ()
  "Kill word."
  (interactive)
  (if (not(looking-at "[[:punct:]]")) ; if character at point is NOT a punctuation mark
    (progn                            ; THEN
  (kill-word 1) ; kill word
  (my/fix-space)) ; and fix space
(progn ; else 
(delete-forward-char 1) ; just delete the punctuation mark
(my/fix-space) ; and delete the space as well
)
))

(defun backward-kill-word-correctly ()
  "Kill word."
  (interactive)
  (backward-kill-word 1)
  (my/fix-space))

;; delete backward one char unless the region is active: 
(defun my/delete-backward ()
"When there is an active region, delete it and then fix up the whitespace"
  (interactive)
  (if (use-region-p)                  ; IF
    (progn                            ; THEN
      (delete-region (region-beginning) (region-end))
      (my/fix-space)) 
(progn ; ELSE 
    (delete-backward-char 1)
(just-one-space) 
  (when (looking-back "^[[:space:]]+")
(progn 
(delete-horizontal-space)
(right-char)
)
)
 
(left-char)
)
)
) 


(defun timesvr ()
  "Task request to my virtual assistant."
  (interactive)
  (message-mail)
  (message-goto-subject) (insert "task request: " (format-time-string "%F %l:%M%P"))
  (message-goto-body) (insert "\n")
  )
(global-set-key (kbd "C-c t") 'timesvr)
(global-set-key (kbd "C-c m") 'compose-mail)

;; add stuff for eshell
;; http://eschulte.github.io/emacs24-starter-kit/starter-kit-eshell.html
;; gmail http://eschulte.github.io/emacs24-starter-kit/starter-kit-gnus.html
;; google docs http://eschulte.github.io/emacs24-starter-kit/starter-kit-g-client.html
;; javascript http://eschulte.github.io/emacs24-starter-kit/starter-kit-js.html
;; elisp http://eschulte.github.io/emacs24-starter-kit/starter-kit-lisp.html

;; (setq org-confirm-babel-evaluate nil)

(toggle-maxframe)
(monaco-font)

(defun jay/insert-space ()
  "Insert space and then clean up whitespace."
  (interactive)
(expand-abbrev)
  (insert "\ ")
  (just-one-space)
)
 
(define-key key-minor-mode-map (kbd "<SPC>") 'jay/insert-space)

(defun jay/save-some-buffers ()
(interactive)
  (save-some-buffers 'no-confirm (lambda ()
    (cond
      ((and buffer-file-name (equal buffer-file-name abbrev-file-name)))
      ((and buffer-file-name (eq major-mode 'latex-mode)))
      ((and buffer-file-name (eq major-mode 'emacs-lisp-mode)))
      ((and buffer-file-name (derived-mode-p 'org-mode)))))))

(defgroup helm-org-wiki nil
  "Simple jump-to-org-file package."
  :group 'org
  :prefix "helm-org-wiki-")
(defcustom helm-org-wiki-directory "~/nd/"
  "Directory where files for `helm-org-wiki' are stored."
  :group 'helm-org-wiki
  :type 'directory)
(defun helm-org-wiki-files ()
  "Return .org files in `helm-org-wiki-directory'."
  (let ((default-directory helm-org-wiki-directory))
    (mapcar #'file-name-sans-extension
            (file-expand-wildcards "*.txt"))))
(defvar helm-source-org-wiki
  `((name . "Projects")
    (candidates . helm-org-wiki-files)
    (action . ,(lambda (x)
                  (find-file (expand-file-name
                              (format "%s.txt" x)
                              helm-org-wiki-directory))))))
(defvar helm-source-org-wiki-not-found
  `((name . "Create org-wiki")
    (dummy)
    (action . (lambda (x)
                (helm-switch-to-buffer
                 (find-file
                  (format "%s/%s.org"
                          helm-org-wiki-directory x)))))))
;;;###autoload
(defun helm-org-wiki ()
  "Select an org-file to jump to."
  (interactive)
  (helm :sources
        '(helm-source-org-wiki
          helm-source-org-wiki-not-found)))
(provide 'helm-org-wiki)
