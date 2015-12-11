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
(read-abbrev-file "~/elisp/.abbrev_defs")
(setq abbrev-file-name "~/elisp/.abbrev_defs")

(set (make-local-variable 'abbrev-file-name) (expand-file-name "~/Dropbox/elisp/own-abbrevs.abbrev_defs"))
(read-abbrev-file "~/elisp/own-abbrevs.abbrev_defs")
(setq save-abbrevs t)
(setq only-global-abbrevs t)

(defun reflash-indentation ()
"One sentence summary of what this command do."
  (interactive)
  (org-indent-mode 1)
(recenter-top-bottom)
  )

(defun org-checkbox-p ()
"Predicate: Checks whether the current line org-checkbox"
  (and
    (eq 'org-mode major-mode)
    (string-match "^\s*\\([-+*]\\|[0-9]+[.\\)]\\)\s\\[.?\\]\s" (or (thing-at-point 'line) ""))))

(defun org-plain-text-list-p ()
"Predicate: Checks whether the current line org-plain-text-list"
  (and
    (eq 'org-mode major-mode)
    (string-match "^\s*\\([-+]\\|\s[*]\\|[0-9]+[.\\)]\\)\s" (or (thing-at-point 'line) ""))))

(add-hook 'org-mode-hook 'turn-on-olivetti-mode)
(add-hook 'org-mode-hook (smartparens-mode 1))
(add-hook 'org-mode-hook (auto-revert-mode 1))
(setq org-hierarchical-todo-statistics nil)

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

(require 'dired-details+)

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

(defcustom dired-details-hidden-string ""
  "*This string will be shown in place of file details and symbolic links."
  :group 'dired-details
  :type 'string)

(defcustom dired-details-initially-hide t
  "*Hide dired details on entry to dired buffers."
  :group 'dired-details
  :type 'boolean)

;; (require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable


;; (setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$\\|\\.DS_Store\\|\\.doc$\\|\\.docx$\\|\\.xlsx$\\|\\.ini$\\|\\.fsLockFile$\\|Icon*")

(setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$\\|\\.DS_Store$\\|\\.doc$\\|\\.docx$\\|\\.ini$\\|\\.rtf$\\|\\Icon*\\|\\*html")

;; Enable toggling of uninteresting files.
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t) ; this is buffer-local variable

(defun enable-dired-omit-mode () (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)


;; Load Dired X when Dired is loaded.
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

;; so that I can hide details on dired
;;(require 'dired-details+)

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

(defun pasteboard-paste ()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (let ((start (point))
        (end (if mark-active
                 (mark)
               (point))))
    (shell-command-on-region start end "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'" nil t)
    (my/fix-space)
    (save-excursion
      (goto-char start)
      (my/fix-space))))

(defun pasteboard-paste-without-smart-quotes ()
  (interactive)
  (let ((beg (point)))
    (pasteboard-paste)
    (replace-smart-quotes beg (point))))

(defun pasteboard-paste-spaces-maybe ()
(interactive) 
;; begin if 
(if 
(or 
(looking-back "'")
(looking-back "(")
(looking-back "\\[")
(looking-back "\"")
)
;; end if 

    (pasteboard-paste-no-spaces) ; then
  (pasteboard-paste-without-smart-quotes))   ; else
  )

(defun pasteboard-paste-no-spaces ()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (let ((start (point))
  (end (if mark-active
     (mark)
         (point))))
    (shell-command-on-region start end
           "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'"
           nil t)
    (save-excursion

      )))

(defun pasteboard-cut ()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end))
  (my/fix-space)
  ) 

(defun pasteboard-cut-and-capitalize ()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end))
  (my/fix-space)
  (save-excursion
    (when (my/beginning-of-sentence-p)
      (capitalize-unless-org-heading))))

(defun pasteboard-search-for-clipboard-contents ()
  (interactive)
  (let ((search-term
         (with-temp-buffer
           (pasteboard-paste-no-spaces)
           (buffer-string))))
    (search-forward search-term)))

(setq x-select-enable-clipboard t) 
(defun push-kill-ring-to-pasteboard ()
  (interactive)
  (x-select-text (current-kill 0)))

(defun gist-buffer-to-pasteboard ()
  (interactive)
  (gist-buffer)
  (push-kill-ring-to-pasteboard)
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



;; (define-key key-minor-mode-map (kbd "s-p") 'refile-region)

(define-key key-minor-mode-map (kbd "M-0") 'move-region-to-other-window)

(define-key key-minor-mode-map (kbd "s-b") 'org-narrow-to-subtree)
; narrow-or-widen-dwim 

(define-key key-minor-mode-map (kbd "C-x <return> RET") 'mc/mark-all-dwim)

(define-key key-minor-mode-map (kbd "s-H") 'replace-inner)

(define-key key-minor-mode-map (kbd "M-e") 'smart-forward-sentence)

(define-key key-minor-mode-map (kbd "M-q") 'org-refile)

(define-key key-minor-mode-map (kbd "s-F") 'pasteboard-search-for-clipboard-contents)

;; (define-key key-minor-mode-map (kbd "M-\"") 'edit-abbrevs)

(define-key key-minor-mode-map (kbd "M-\"") 'open-abbrevs)

(define-key key-minor-mode-map (kbd "s-\"") 'path-copy-full-path-to-clipboard)

(define-key key-minor-mode-map (kbd "<s-return>") 'toggle-fullscreen)

;; (define-key key-minor-mode-map (kbd "s-v") 'pasteboard-paste-without-smart-quotes)
;; (define-key orgstruct-mode-map (kbd "s-v") 'pasteboard-paste-without-smart-quotes)
(global-set-key (kbd "s-v") 'pasteboard-paste-without-smart-quotes)
(define-key org-mode-map (kbd "s-v") 'pasteboard-paste-spaces-maybe)
;; (define-key fundamental-mode-map (kbd "s-v") 'pasteboard-paste-without-smart-quotes)
(define-key text-mode-map (kbd "s-v") 'pasteboard-paste-without-smart-quotes)
;; (define-key markdown-mode-map (kbd "s-v") 'pasteboard-paste-without-smart-quotes)

;; (define-key sh-mode-map (kbd "s-v") 'pasteboard-paste-no-spaces)
(define-key emacs-lisp-mode-map (kbd "s-v") 'pasteboard-paste-no-spaces)

(define-key key-minor-mode-map (kbd "M-v") 'kdm/html2org-clipboard)

(define-key key-minor-mode-map (kbd "s-x") 'pasteboard-cut-and-capitalize)
(define-key key-minor-mode-map (kbd "s-c") 'pasteboard-copy)
(define-key key-minor-mode-map (kbd "s-V") 'pasteboard-paste-no-spaces)

(define-key key-minor-mode-map (kbd "s-F") 'pasteboard-search-for-clipboard-contents)

(define-key emacs-lisp-mode-map (kbd "s-v") 'pasteboard-paste-no-spaces)
;; (define-key rebuilder-mode-map (kbd "s-v") 'pasteboard-paste-no-spaces)


(define-key key-minor-mode-map (kbd "s-Z") 'unexpand-abbrev)

(define-key key-minor-mode-map (kbd "s-h") 'replace-string)

(global-unset-key (kbd "C-S-r"))
(define-key key-minor-mode-map (kbd "C-S-r") nil)
(define-key org-mode-map (kbd "C-S-r") nil)

(define-key key-minor-mode-map (kbd "M-s-g") 'gnugol-word-at-point)
(define-key key-minor-mode-map (kbd "M-s-d") 'define-word-at-point)



(define-key dired-mode-map (kbd "s-O") 'reveal-in-finder)
(define-key key-minor-mode-map (kbd "s-O") 'reveal-in-finder)

(define-key dired-mode-map (kbd "s-o") 'projectile-find-file)
(define-key key-minor-mode-map (kbd "s-o") 'projectile-find-file)


;; pop mark
(define-key key-minor-mode-map (kbd "C-x p")'pop-to-mark-command)

;; projectile
(define-key key-minor-mode-map (kbd "s-P") 'projectile-commander)

;; and make it work in the minibuffer too
(define-key minibuffer-local-map (kbd "s-v") 'pasteboard-paste-no-spaces)
(define-key minibuffer-local-map (kbd "s-x") 'pasteboard-cut)
(define-key minibuffer-local-map (kbd "s-c") 'pasteboard-copy)

(define-key key-minor-mode-map (kbd "C-c C-v") 'refile-region)

(define-key key-minor-mode-map (kbd "s-0") 'widen)
(define-key key-minor-mode-map (kbd "s-W") 'widen)
(define-key key-minor-mode-map (kbd "C-c e") 'eval-buffer)
(define-key key-minor-mode-map (kbd "C-c r") 'eval-region)

(define-key key-minor-mode-map (kbd "C-9") 'goto-last-change-reverse) ; super useful when editing
(define-key key-minor-mode-map (kbd "C--") 'goto-last-change) ; super useful when editing


(define-key key-minor-mode-map (kbd "M-=") 'er/expand-region)
(define-key key-minor-mode-map (kbd "C-=") 'er/expand-region)
; (define-key key-minor-mode-map (kbd "C-8") 'embolden-or-bold)
; replaced it with multiple-cursors-hydra/body for now

;; (define-key key-minor-mode-map (kbd "C-8") '(lambda (arg) (interactive "p") (wrap-region-trigger arg "*"))) ; wow this was a stroke of genius



(define-key key-minor-mode-map (kbd "C-d") 'kill-word-correctly-and-capitalize)
;; (define-key key-minor-mode-map (kbd "m-d") 'kill-word-correctly-and-capitalize)

;; (define-key key-minor-mode-map (kbd "m-D") 'org-shiftleft)

(define-key key-minor-mode-map (kbd "C-j") 'prelude-top-join-line)


(define-key key-minor-mode-map (kbd "C-l") 'reflash-indentation)
;; (define-key org-mode-map (kbd "C-l") 'reflash-indentation)


(define-key key-minor-mode-map (kbd "=") 'smex) ; call any function with easiest keystroke possible
;; (define-key key-minor-mode-map (kbd "=") 'counsel-M-x) ; call any function with easiest keystroke possible
(define-key key-minor-mode-map (kbd "M-x") 'helm-M-x) ; call helm-M-x instead of regular M-as
;; (define-key key-minor-mode-map (kbd "\|") 'deft)

(define-key org-mode-map (kbd "M-K") 'kill-clause)
(define-key emacs-lisp-mode-map (kbd "M-K") 'kill-sexp)

(define-key key-minor-mode-map (kbd "C-M-8") 'org-toggle-heading) ; i.e. subheading


(define-key key-minor-mode-map (kbd "M-8") 'org-toggle-heading-same-level)
(define-key key-minor-mode-map (kbd "M-*") 'org-toggle-todo-heading)
;; (define-key key-minor-mode-map (kbd "C-M-*") 'org-toggle-todo-subheading)


(define-key key-minor-mode-map (kbd "C-t") 'transpose-words)

(define-key key-minor-mode-map (kbd "M--") 'cycle-hyphenation-or-toggle-item)

(define-key key-minor-mode-map (kbd "s-'") 'refile-region-or-subtree)

(define-key key-minor-mode-map (kbd "C-c j") 'helm-org-headlines) ; also bound to keychord jj
(define-key key-minor-mode-map (kbd "C-x b") 'helm-mini) ; shows recent files; also bound to ⌘-r
(define-key key-minor-mode-map (kbd "M-b M-d") 'book-dired) ; show directory of my book folder
(define-key key-minor-mode-map (kbd "M-b r") 'read-a-book) ; show directory of my PDF books
(define-key key-minor-mode-map (kbd "M-b j") 'read-jd) ; show PDF books I have annotated
(define-key key-minor-mode-map (kbd "M-b M-b") 'work-on-book) ;

(define-key key-minor-mode-map (kbd "M-b M-w") 'work-on-book) ;

(define-key key-minor-mode-map (kbd "M-b lc") 'book-load-current) ;

(define-key key-minor-mode-map (kbd "M-b ho") 'spacemacs/toggle-highlight-current-line-globally)


;; book bindings
(define-key key-minor-mode-map (kbd "M-b M-p") 'book-proposal-directory)
(define-key key-minor-mode-map (kbd "M-b M-m") 'book-mistakes-directory)

(define-key key-minor-mode-map (kbd "M-b M-r") 'book-helm-strict) ; this is a smart function, show recent files in my book folder

;; can't get this to work. for some reason GNU Emacs interprets ⌘-shift-d as s-c
(define-key key-minor-mode-map (kbd "s-D") 'diredp-dired-recent-dirs)

;; recent directories... but how to populate it?
(define-key key-minor-mode-map (kbd "C-S-d") 'diredp-dired-recent-dirs)

;; own structure editing
(define-key key-minor-mode-map (kbd "s-o") 'move-region-to-other-window) ; very useful when working with a split frame
(define-key org-mode-map (kbd "s-o") 'move-region-to-other-window)



;; For extracting content from my browser

(define-key key-minor-mode-map (kbd "s-I") 'web-research-quotes)
;; (define-key key-minor-mode-map (kbd "s-V") 'kdm/html2org-clipboard) ; paste HTML content that I've copied from the web, automatically converting to proper org-mode syntax


;; indirect buffer
(define-key key-minor-mode-map (kbd "s-i") 'clone-indirect-buffer-other-window)



;; and the keybinding
(define-key org-mode-map (kbd "C-k") 'my/kill-line-dwim)
(define-key key-minor-mode-map (kbd "C-k") 'my/kill-line-dwim)

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
;; (define-key key-minor-mode-map (kbd "<backspace>") 'my/delete-backward)
(define-key key-minor-mode-map (kbd "<backspace>") 'my/delete-backward-and-capitalize)

;; a keybinding for "delete" in addition to "backspace"
(define-key key-minor-mode-map (kbd "C-<backspace>") 'delete-char)
(define-key key-minor-mode-map (kbd "M-<backspace>") 'backward-kill-word-correctly-and-capitalize)

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
(define-key key-minor-mode-map (kbd "s-G") 'helm-ag)

;; some custom functions

(define-key key-minor-mode-map (kbd "C-c v i") 'org-insert-src-block)

;; org-mime
;; (define-key org-mode-map (kbd "M-n") 'new-email-from-subtree-no-signature)
;; (define-key key-minor-mode-map (kbd "M-N") 'new-email-from-subtree)

(define-key key-minor-mode-map (kbd "}rf") 'prelude-rename-file-and-buffer)
(define-key key-minor-mode-map (kbd "}vi") 'org-insert-src-block)
(define-key key-minor-mode-map (kbd "}nl") 'new-lisp-buffer)
(define-key key-minor-mode-map (kbd "]d") 'wn-org)

(defun kill-sentence-to-period ()
  "Leave the period in there."
  (interactive)
  (kill-sentence)
  (push-mark)
  (insert ".")
  (backward-char)
)

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
(smart-expand) 
  (if (my/beginning-of-sentence-p)
      (progn
        (kill-sentence)
        (just-one-space)
        (when (looking-back "^[[:space:]]+") (delete-horizontal-space)))
      (kill-region (point) (progn (my/forward-to-sentence-end) (point)))
      (just-one-space 0))
(when (looking-at ".. ")
(delete-forward-char 1))
)

(defun my/kill-line-dwim ()
  "Kill the current line."
  (interactive)
;; don't leave stray stars behind when killing a line
(when 
(or
(looking-back "\\[") 
(looking-back "\* ")
(looking-back "\* TODO ")
(looking-back "^\*+")
(looking-back "- ")
(looking-back "# ")
)
(beginning-of-line)
) 
;;  (expand-abbrev)
  (org-kill-line)
;;  (save-excursion
;;    (when (my/beginning-of-sentence-on)
;;      (capitalize-unless-org-heading)))
)

(defun kill-sentence-maybe-else-kill-line ()
  (interactive)
(when
    (not (looking-at "$"))
  (my/kill-sentence-dwim))
  (when
      (looking-at "$")
    (my/kill-line-dwim))
)
;; and the keybinding
(global-set-key (kbd "M-k") 'kill-sentence-maybe-else-kill-line)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(add-to-list 'load-path "~/gnulisp/emacs-pastebin-master/")
(require 'neopastebin)
(pastebin-create-login :dev-key "e5ccb53890f16065d90ebd6064a381d0"
                       :username "petersalazar")

(defun jay/insert-space ()
  "Insert space and then clean up whitespace."
  (interactive)
(smart-expand)
(insert "\ ")
  (just-one-space)
)

(define-key org-mode-map (kbd "<SPC>") 'jay/insert-space)
(define-key orgstruct-mode-map (kbd "<SPC>") 'jay/insert-space)

(defun jay/insert-paren-single ()
  "Insert paren without expanding abbrev."
  (interactive)
(smart-expand)
;; (insert-parentheses 1) 
(insert "\)")
)

;; (define-key org-mode-map (kbd ")") 'jay/insert-paren-single)
;; (define-key key-minor-mode-map (kbd ")") 'jay/insert-paren-single)
;; (define-key key-minor-mode-map (kbd "/") 'jay/insert-slash)

;;; I changed this a)) bunch, not sure if it still works correctly. 
;; (defun my/fix-space ()
;;   "Delete all spaces and tabs around point, leaving one space except at the beginning of a line and before a punctuation mark."
;;   (interactive)
;;   (just-one-space)
;; 
;;     (when (or
;;            (looking-back "^[[:space:]]+")
;;            (looking-back "-[[:space:]]+")
;;            (looking-at "[.,:;!?»)-]")
;;            (looking-back"( ")
;;            (looking-at " )")
;;            ))
;;       (unless
;;       (looking-back "^-[[:space:]]+")
;;   (delete-horizontal-space))
;; 
;; (unless 
;;  (looking-back "^") 
;; (just-one-space)
;; )
;; 
;; )

(defun my/fix-space ()
  "Delete all spaces and tabs around point, leaving one space except at the beginning of a line and before a punctuation mark."
  (interactive)
  (just-one-space)
  (when (and (or
              (looking-back "^[[:space:]]+")
              (looking-back "-[[:space:]]+")
              (looking-at "[.,:;!?»)-]")
              (looking-back"( ")
              (looking-at " )")
              )
             (not (looking-back "^-[[:space:]]+"))
             (not (looking-back " - "))

)
    (delete-horizontal-space)))

(defun insert-space ()
  (interactive)
  (let ((last-command-event ? ))
    (call-interactively 'self-insert-command))
;; (unexpand-abbrev)
)

(global-set-key (kbd "M-SPC") 'insert-space)

(setq org-blank-before-new-entry
      '((heading . always)
       (plain-list-item . nil)))

(defun call-rebinding-org-blank-behaviour (fn)
  (let ((org-blank-before-new-entry
         (copy-tree org-blank-before-new-entry)))
    (when (org-at-heading-p)
      (rplacd (assoc 'heading org-blank-before-new-entry) nil))
    (call-interactively fn)))

(defun smart-org-meta-return-dwim ()
  (interactive)
(if

    (and
     (looking-back "^")
     (looking-at ".+")
     )                               ; if
    (org-toggle-heading-same-level) ; then
 (call-rebinding-org-blank-behaviour 'org-meta-return)) ; else 

) 



(defun smart-org-insert-heading-respect-content-dwim ()
(interactive) 
  (call-rebinding-org-blank-behaviour 'org-insert-heading-respect-content)
)

(defun smart-org-insert-todo-heading-dwim ()
  (interactive)
  (let ((listitem-or-checkbox (org-plain-text-list-p)))
    (call-rebinding-org-blank-behaviour 'org-insert-heading-respect-content)
    (if listitem-or-checkbox
        (insert "[ ] ")
        (insert "TODO ")))
)

(defun smart-org-insert-todo-heading-respect-content-dwim ()
  (interactive)
  (call-rebinding-org-blank-behaviour 'org-insert-todo-heading-respect-content)
)

(defun smart-org-insert-subheading ()
  (interactive)

(call-rebinding-org-blank-behaviour 'org-meta-return) 
(org-demote-subtree)
  )


(define-key org-mode-map (kbd "M-<return>") 'smart-org-meta-return-dwim) 
(define-key org-mode-map (kbd "M-S-<return>") 'smart-org-insert-todo-heading-dwim) 
(define-key org-mode-map (kbd "C-<return>") 'return-insert-blank-line-before)
(define-key org-mode-map (kbd "C-S-<return>") 'smart-org-insert-todo-heading-respect-content-dwim) 
(define-key org-mode-map (kbd "C-M-<return>") 'smart-org-insert-subheading)

(defun smart-return ()
  (interactive)

  ;; don't leave stray stars or links
  (when 
      (or
       (looking-back "\\[") 
       ;; (looking-back "\* ")
       (looking-back "^\*+[ ]*") ; hopefully this means: at the beginning of the line, 1 or more asterisks followed by zero or more spaces
       (looking-back "^# ")
       ;; (looking-back "* TODO ") ; actually I don't think I want this 
       ;; (looking-back "^*+")
       ;; (looking-back "- ") 

       )
    (beginning-of-line)
    ) 
  ;;
  (cond (mark-active
         (progn (delete-region (mark) (point))
                (newline))) 
        ;; Shamefully lifted from `org-return'. Why isn't there an
        ;; `org-at-link-p' function?!
        ((and (eq major-mode 'org-mode)
              org-return-follows-link
              (org-in-regexp org-any-link-re))
         (cond
          ((or
;; (looking-at "\\[\\[.*")
               (looking-back "\\]\\]")
               (and (thing-at-point 'url)
                    (let ((bnds (bounds-of-thing-at-point 'url)))
                      (or (>= (car bnds) (point))
                          (<= (cdr bnds) (point))))))
           (newline))
          ((char-equal (string-to-char "]") (following-char))
           (progn (forward-char 2)
                  (newline)))
          (t (call-interactively 'org-open-at-point))))
        ((and (eq major-mode 'org-mode)
              (let ((el (org-element-at-point)))
                (and el
                     ;; point is at an item
                     (eq (first el) 'item)
                     ;; item is empty
                     (eql (getf (second el) :contents-begin)
                          (getf (second el) :contents-end)))))
         (beginning-of-line)
         (let ((kill-whole-line nil))
           (kill-line))
         (newline))
        ((and (eq major-mode 'org-mode)
              (let ((el (org-element-at-point)))
                (and el
                     (or (member (first el) '(item plain-list))
                         (let ((parent (getf (second el) :parent)))
                           (and parent
                                (member (first parent) '(item plain-list))))))))
         (let ((is-org-chbs (org-checkbox-p)))
           (org-meta-return)
           (when is-org-chbs
             (insert "[ ] "))))
        (t (org-return))))

(define-key org-mode-map (kbd "<return>") 'smart-return)

(defun kill-word-correctly ()
  "Kill word."
  (interactive)
  (smart-expand)
  (if (or (re-search-forward "\\=[  ]*\n" nil t)
          (re-search-forward "\\=\\W*?[[:punct:]]+" nil t)) ; IF there's a sequence of punctuation marks at point
      (kill-region (match-beginning 0) (match-end 0)) ; THEN just kill the punctuation marks
    (kill-word 1))                                    ; ELSE kill word
  (my/fix-space))

(defun kill-word-correctly-and-capitalize ()
  "Check to see if the point is at the beginning of the sentence. If yes, then kill-word-correctly and endless/capitalize to capitalize the first letter of the word that becomes the first word in the sentence. Otherwise simply kill-word-correctly."
  (interactive)
  (let ((fix-capitalization (my/beginning-of-sentence-p)))
    (call-interactively 'kill-word-correctly)
    (when fix-capitalization
      (save-excursion (capitalize-unless-org-heading)))))

(defun timesvr ()
  "Task request to my virtual assistant."
  (interactive)
  (message-mail)
  (message-goto-subject) (insert "task request: " (format-time-string "%F %l:%M%P"))
  (message-goto-body) (insert "\n")
  )
(global-set-key (kbd "C-c t") 'timesvr)
(global-set-key (kbd "C-c m") 'compose-mail)

(defun jay/left-char ()
  "Move point to the left or the beginning of the region.
 Like `backward-char', but moves point to the beginning of the region
provided the (transient) mark is active."
  (interactive)
  (let ((this-command 'left-char)) ;; maintain compatibility
    (let ((left (min (point)
                     ;; `mark' returning nil is ok; we'll only use this
                     ;; if `mark-active'
                     (or (mark t) 0))))
      (if (and transient-mark-mode mark-active)
          (progn
            (goto-char left)
            (setq deactivate-mark t))
        (call-interactively 'left-char)))))


(defun jay/right-char ()
  "Move point to the right or the end of the region.
 Like `right-char', but moves point to the end of the region
provided the (transient) mark is active."
  (interactive)
  (let ((this-command 'right-char)) ;; maintain compatibility
    (let ((right (max (point)
                      ;; `mark' returning nil is ok; we'll only use this
                      ;; if `mark-active'
                      (or (mark t) 0))))
      (if (and transient-mark-mode mark-active)
          (progn (goto-char right)
     (setq deactivate-mark t))
  (call-interactively 'right-char)))))

(define-key org-mode-map (kbd "<left>") 'jay/left-char)
(define-key org-mode-map (kbd "<right>") 'jay/right-char)

(defun words-dictionary ()
  (interactive)
  (browse-url
   (format
    "http://dictionary.reference.com/browse/%s?s=t"
    (thing-at-point 'word))))

(defun words-thesaurus ()
  (interactive)
  (browse-url
   (format
    "http://www.thesaurus.com/browse/%s"
    (thing-at-point 'word))))

(defun words-google ()
  (interactive)
  (browse-url
   (format
    "http://www.google.com/search?q=%s"
    (if (region-active-p)
  (url-hexify-string (buffer-substring (region-beginning)
               (region-end)))
      (thing-at-point 'word)))))


(defvar words-funcs '()
 "functions to run in `words'. Each entry is a list of (key menu-name function).")

(setq words-funcs
  '(("d" "ictionary" words-dictionary)
    ("t" "hesaurus" words-thesaurus)
    ("g" "oogle" words-google)))


(defun words ()
  (interactive)
   (message
   (concat
    (mapconcat
     (lambda (tup)
       (concat "[" (elt tup 0) "]"
         (elt tup 1) " "))
     words-funcs "") ": "))
   (let ((input (read-char-exclusive)))
     (funcall
      (elt
       (assoc
  (char-to-string input) words-funcs)
       2))))

(defun words-twitter ()
  (interactive)
  (browse-url
   (format
    "https://twitter.com/search?q=%s"
    (if (region-active-p)
  (url-hexify-string (buffer-substring (region-beginning)
               (region-end)))
      (thing-at-point 'word)))))

(add-to-list 'words-funcs
  '("w" "twitter" words-twitter)
  t) ; append

(defun words-atd ()
  "Send paragraph at point to After the deadline for spell and grammar checking."
  (interactive)

  (let* ((url-request-method "POST")
   (url-request-data (format
          "key=some-random-text-&data=%s"
          (url-hexify-string
           (thing-at-point 'paragraph))))
   (xml  (with-current-buffer
       (url-retrieve-synchronously
        "http://service.afterthedeadline.com/checkDocument")
     (xml-parse-region url-http-end-of-headers (point-max))))
   (results (car xml))
   (errors (xml-get-children results 'error)))

    (switch-to-buffer-other-frame "*ATD*")
    (erase-buffer)
    (dolist (err errors)
      (let* ((children (xml-node-children err))
       ;; for some reason I could not get the string out, and had to do this.
       (s (car (last (nth 1 children))))
       ;; the last/car stuff doesn't seem right. there is probably
       ;; a more idiomatic way to get this
       (desc (last (car (xml-get-children children 'description))))
       (type (last (car (xml-get-children children 'type))))
       (suggestions (xml-get-children children 'suggestions))
       (options (xml-get-children (xml-node-name suggestions) 'option))
       (opt-string  (mapconcat
         (lambda (el)
           (when (listp el)
             (car (last el))))
         options
         " ")))

  (insert (format "** %s ** %s
Description: %s
Suggestions: %s

" s type desc opt-string))))))

(add-to-list 'words-funcs
  '("s" "spell/grammar" words-atd)
  t) ; append

(defun send-mail (userid password)
  "send email to sunjaydixit@gmail.com containing their password"
  (interactive)
  (mail)
  (mail-to)
  (insert (format "%s@jaydixit.com" userid))
  (mail-subject)
  (insert "[06-640] account information")
  (mail-text)
  (insert (format "
An account has been created on jaydixit.com
userid: %s
password: %s" userid password))
  (mail-send-and-exit))

;; (send-mail "jkitchin" "trustme99")

(let ((data (quote (("user1" "trustme99") ("user2" "foolme99") ("user3" "blameme99")))))
;; (defun fun (a b) (princ (format "user: %s\npassword: %s\n" a but)))

;; (mapcar (lambda (x) (fun (car x) (cadr x))) data)
)

(let ((data (quote (("user1" "trustme99") ("user2" "foolme99") ("user3" "blameme99")))))
;; (defun fun (a b) (princ (format "user: %s\npassword: %s\n" a but)))

;; (mapcar (lambda (x) (fun (nth 0 x) (nth 1 x))) data)
)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)

(define-minor-mode embolden-next-word
    "Make the next word you type bold."
  nil
  :lighter " EMBOLDEN"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") (lambda ()
                      (interactive)
                      (save-excursion
                        (goto-char (get-register 'p))
                        (insert "*"))
                      (insert "* ")
                      (embolden-next-word -1)))
        (define-key map (kbd ".") (lambda ()
                    (interactive)
                    (save-excursion
                      (goto-char (get-register 'p))
                      (insert "*"))
                    (insert "*. ")
                    (embolden-next-word -1)))
            map)
  (if embolden-next-word
      (set-register 'p (point))
    (set-register 'p nil)))

(global-set-key "\C-o" 'embolden-or-bold)
(define-key key-minor-mode-map (kbd "C-o") 'embolden-or-bold)

(define-minor-mode insert-slash-no-abbrev
    "Make the next word you type bold."
  nil
  :lighter " don't abbreviate"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") (lambda ()
                      (interactive)
(insert "/ ")
))
            map)
  (if insert-slash-no-abbrev
      (set-register 'p (point))
    (set-register 'p nil)))
;; (global-set-key "/" 'insert-slash-no-abbrev)

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "sh" "css" "dot" "latex")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(local-set-key (kbd "C-c v e")
               'org-edit-src-code)
;; keybinding for inserting code blocks

(defun play-mp3 ()
  (interactive)
  (let ((file (buffer-file-name)))
    (kill-buffer (current-buffer))
    (ora-dired-start-process (format "rhythmbox \"%s\"" file))))
(add-to-list 'auto-mode-alist '("\\.mp3\\'" . ora-mp3))

(defun hello ()
      "Hello World and you can call it via M-x hello."
      (interactive)
      (message "Hello World!"))

(defun hello (someone)
      "Say hello to SOMEONE via M-x hello."
      (interactive "sWho do you want to say hello to? ")
      (message "Hello %s!" someone))

(defun multiple-hello (someone num)
      "Say hello to SOMEONE via M-x hello, for NUM times."
      (interactive "sWho do you want to say hello to? \nnHow many times? ")
      (dotimes (i num)
        (insert (format "Hello %s!\n" someone))))

(defun dwiw-auto-capitalize ()
  (if (org-in-block-p '("src"))
      (when auto-capitalize
  (auto-capitalize-mode -1))
    (unless auto-capitalize
      (auto-capitalize-mode 1))))

;; (add-hook 'post-command-hook dwiw-auto-capitalize)

(defun email-region (start end)
  "Send region as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (compose-mail)
    (message-goto-body)
    (insert content)
    (message-goto-to)))

(defvar *email-heading-point* nil
  "global variable to store point in for returning")

(defvar *email-to-addresses* nil
  "global variable to store to address in email")

(defun email-heading-return ()
  "after returning from compose do this"
  (switch-to-buffer (marker-buffer  *email-heading-point*))
  (goto-char (marker-position  *email-heading-point*))
  (setq *email-heading-point* nil)
  (org-set-property "SENT-ON" (current-time-string))
  ;; reset this incase you added new ones
  (org-set-property "TO" *email-to-addresses*)
  )

(defun email-send-action ()
  "send action for compose-mail"
  (setq *email-to-addresses* (mail-fetch-field "To")))

(defun email-heading ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

save when it was sent as s SENT property. this is overwritten on
subsequent sends. could save them all in a logbook?
"
  (interactive)
  ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (org-mark-subtree)
  (let ((content (buffer-substring (point) (mark)))
  (TO (org-entry-get (point) "TO" t))
  (CC (org-entry-get (point) "CC" t))
  (BCC (org-entry-get (point) "BCC" t))
  (SUBJECT (nth 4 (org-heading-components)))
  (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
  (continue nil)
  (switch-function nil)
  (yank-action nil)
  (send-actions '((email-send-action . nil)))
  (return-action '(email-heading-return)))

    (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
    (message-goto-body)
    (insert content)
    (when CC
      (message-goto-cc)
      (insert CC))
    (when BCC
      (message-goto-bcc)
      (insert BCC))
    (if TO
  (message-goto-body)
      (message-goto-to))
    ))

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil) 


;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

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

(defun turn-on-autocomplete-mode ()
   (auto-complete-mode 1))
(add-hook 'emacs-lisp-mode-hook 'turn-on-autocomplete-mode )

(defun cycle-hyphenation ()
  (interactive)
  (cond ((re-search-forward "\\=\\w*\\(-\\)\\w+" nil t)
         (save-excursion (replace-match " " t t nil 1)))
        ((re-search-forward "\\=\\w*\\( +\\)\\w+" nil t)
         (save-excursion (replace-match "-" t t nil 1)))))

(defvar *punctuation-markers-to-cycle-between*  ".?!")

(defun cycle-punctuation ()
  (interactive)
  (save-excursion
    (forward-sentence)
    (when (re-search-backward (format "\\>\\([%s]\\)[[:space:]]*\\="
                                      *punctuation-markers-to-cycle-between*)
                              nil t)
      (let ((next (elt *punctuation-markers-to-cycle-between*
                       ;; circular string; should be abstracted
                       (mod (1+ (position (elt (match-string 1) 0)
                                          *punctuation-markers-to-cycle-between*))
                            (length *punctuation-markers-to-cycle-between*)))))
        (replace-match (format "%c" next) t t nil 1)))))

(define-key key-minor-mode-map (kbd "M-.") 'cycle-punctuation)

(defun org-clone-subtree ()
  (interactive)
  (org-clone-subtree-with-time-shift 1)
  (save-excursion
    (org-goto-sibling)
    ;; This part was lifted partly and adapted from
    ;; http://orgmode.org/worg/org-hacks.html#orgheadline10.
    ;; There should be a better way to change the contents of an org heading
    ;; though...
    (when (org-at-heading-p)
      (let ((hl-text (nth 4 (org-heading-components)))
            (buffer-undo-list))
        (when hl-text
          (beginning-of-line)
          (search-forward hl-text (point-at-eol))
          (replace-match (format "%s - original" hl-text) nil t)
          (org-align-tags-here org-tags-column))))))

;; Identify the end of sentences globally.
(setq sentence-end-base "[][.?!…}]+[\"”]?")
(defun kill-clause ()
  (interactive)
  (smart-expand)

(if
(let ((sm (string-match "*+\s" (thing-at-point 'line)))) (and sm (= sm 0)))
(kill-line)


  (let ((old-point (point))
        (kill-punct (my/beginning-of-sentence-p)))
    (when (re-search-forward "--\\|[][,;:?!…\"”()}]+\\|\\.+ " nil t)
      (kill-region old-point
                   (if kill-punct
                       (match-end 0)
                     (match-beginning 0)))))
  (my/fix-space)
  (save-excursion
    (when (my/beginning-of-sentence-p)
      (capitalize-unless-org-heading)))))

(defvar *smart-punctuation-marks*
  ".,;:!?-")

(setq *smart-punctuation-exceptions*
  (list "?!" ".." "..." "............................................." "---" ";;" "!!" "!!!" "??" "???" "! :" ". :" ") ; "))

;; How do I add an exception for ") ; "? 
;; e.g. if I want to add a comment after a line of lisp?

(defun smart-punctuation (new-punct &optional not-so-smart)
  (smart-expand)
  (save-restriction
    (when (and (eql major-mode 'org-mode)
               (org-at-heading-p))
      (save-excursion
        (org-beginning-of-line)
        (let ((heading-text (fifth (org-heading-components))))
          (when heading-text
            (search-forward heading-text)
            (narrow-to-region (match-beginning 0) (match-end 0))))))
    (cl-flet ((go-back (regexp)
                (re-search-backward regexp nil t)
                (ignore-errors      ; might signal `end-of-buffer'
                  (forward-char (length (match-string 0))))))
      (if not-so-smart
          (let ((old-point (point)))
            (go-back "[^ \t]")
            (insert new-punct)
            (goto-char old-point)
            (forward-char (length new-punct)))
        (let ((old-point (point)))
          (go-back (format "[^ \t%s]\\|\\`" *smart-punctuation-marks*))
          (let ((was-after-space (and (< (point) old-point)
                                      (find ?  (buffer-substring (point) old-point)))))
            (re-search-forward (format "\\([ \t]*\\)\\([%s]*\\)"
                                       *smart-punctuation-marks*)
                               nil t)
            (let* ((old-punct (match-string 2))
                   (was-after-punct (>= old-point (point))))
              (replace-match "" nil t nil 1)
              (replace-match (or (when (and was-after-punct
                                            (not (string= old-punct "")))
                                   (let ((potential-new-punct (concat old-punct new-punct)))
                                     (find-if (lambda (exception)
                                                (search potential-new-punct exception))
                                              *smart-punctuation-exceptions*)))
                                 new-punct)
                             nil t nil 2)
              (if was-after-space
                  (my/fix-space)
                (when (looking-at "[ \t]*\\<")
                  (save-excursion (my/fix-space))))))))))
  (when (and (eql major-mode 'org-mode)
             (org-at-heading-p))
    (org-align-tags-here org-tags-column)))

(defun smart-period ()
  (interactive)
(smart-punctuation ".")
(save-excursion
(unless 
(or 
(looking-at "[ ]*$")
(looking-at "\"[ ]*$") 
(looking-at "\)[ ]*$") 
)
(capitalize-unless-org-heading))
))

(define-key org-mode-map (kbd ".") 'smart-period)
(define-key orgstruct-mode-map (kbd ".") 'smart-period)

(defun smart-comma ()
  (interactive)
  (smart-punctuation ",")
(unless 
(or
(looking-at "\\W*$") 
(looking-at "\\W*I\\b")          ; never downcase the word "I"
(looking-at "[ ]*I\'")          ; never downcase the word "I'
(looking-at "[ ]*\"")          ; beginning of a quote
)

(save-excursion (downcase-word 1)))
)


(define-key org-mode-map (kbd ",") 'smart-comma)
(define-key orgstruct-mode-map (kbd ",") 'smart-comma)

(defun smart-question-mark ()
  (interactive)
  (smart-punctuation "?")
(save-excursion
(unless (looking-at "[ ]*$")
(capitalize-unless-org-heading))
))

(define-key org-mode-map (kbd "?") 'smart-question-mark)
(define-key orgstruct-mode-map (kbd "?") 'smart-question-mark)

(defun smart-exclamation-point ()
  (interactive)
  (smart-punctuation "!")
(save-excursion
(unless (looking-at "[ ]*$")
(capitalize-unless-org-heading))
))

(define-key org-mode-map (kbd "!") 'smart-exclamation-point)
(define-key orgstruct-mode-map (kbd "!") 'smart-exclamation-point)

(defun smart-semicolon ()
  (interactive)
  (smart-punctuation ";")
(unless
(or
(looking-at "\\W*$")
(looking-at "\\W*I\\b")          ; never downcase the word "I"
)

(save-excursion (downcase-word 1))))

(define-key org-mode-map (kbd ";") 'smart-semicolon)
(define-key orgstruct-mode-map (kbd ";") 'smart-semicolon)

(defun smart-colon ()
  (interactive)
  (smart-punctuation ":" t)
(unless
(or
(looking-at "\\W*$")
(looking-at "\\W*I\\b")          ; never downcase the word "I"
)

(save-excursion (downcase-word 1))))



(define-key org-mode-map (kbd ":") 'smart-colon)
(define-key orgstruct-mode-map (kbd ":") 'smart-colon)

(defun backward-kill-word-correctly ()
  "Kill word."
  (interactive)
  (if (re-search-backward "\\>\\W*[[:punct:]]+\\W*\\=" nil t)
      (kill-region (match-end 0) (match-beginning 0))
    (backward-kill-word 1))
  (my/fix-space)

;; I added this ↓↓↓ #######################
(when (and
(not (looking-back "---")) ; I added this
(not (looking-back "^"))) ; I added this
;; I added this ↑↑↑ #######################

(jay/insert-space)
)
)

(defun my/delete-backward ()
  "When there is an active region, delete it and then fix up the whitespace"
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-backward-char 1))
  (save-excursion
    (when (or (looking-at "[[:space:]]")
              (looking-back "[[:space:]]"))
(unless (looking-back "\\w ")
      (my/fix-space)))))

(defcustom capitalize-after-deleting-single-char nil
  "Determines whether capitalization should occur after deleting a single character.")

(defun my/delete-backward-and-capitalize ()
  "When there is an active region, delete it and then fix up the whitespace"
  (interactive)
(when (looking-back "^[*]+ ")
(kill-line 0)
(insert " ") ; this line is super hacky I put it here because when I tried to use "unless", the rest of the function, and then this at the end, it didn't work; however, this does produce the behavior I desire 
)

  (let ((capitalize capitalize-after-deleting-single-char))
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (setf capitalize t))
      (new-org-delete-backward-char 1))
    (save-excursion
      (when (or (looking-at "[[:space:]]")
    (looking-back "[[:space:]]"))
;; unless there's already exactly one space between words, since I need to be able to delete backward past spaces
(unless (and
(looking-back "\\w ")
(looking-at "\\w") 
) 
  (my/fix-space))))
    (when (and capitalize (my/beginning-of-sentence-p))
      (save-excursion
        (capitalize-unless-org-heading)))))

(defun backward-kill-word-correctly-and-capitalize ()
  "Backward kill word correctly. Then check to see if the point is at the beginning of the sentence. If yes, then kill-word-correctly and endless/capitalize to capitalize the first letter of the word that becomes the first word in the sentence. Otherwise simply kill-word-correctly."
  (interactive)
(call-interactively 'backward-kill-word-correctly) 
  (let ((fix-capitalization (my/beginning-of-sentence-p))) 
    (when fix-capitalization
      (save-excursion (capitalize-unless-org-heading)))))

(defadvice capitalize-word (after capitalize-word-advice activate)
  "After capitalizing the new first word in a sentence, downcase the next word which is no longer starting the sentence."

  (unless

      (or
       (looking-at "\\W*I\\b")          ; never downcase the word "I"
       (looking-at "[ ]*I\'")          ; never downcase the word "I'
       ;; (looking-at "\\") ; how do you search for a literal backslash?
       (looking-at (sentence-end))
       (looking-at "\\W*$") ; hopefully this means "zero or more whitespace then end of line"
(looking-at "\"[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"
(looking-at "\)[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"
       (looking-at (user-full-name))

       )

    (save-excursion
      (downcase-word 1))))

(defun capitalize-unless-org-heading ()
  (interactive)
(unless 
(or
(looking-at "[\n\t ]*\\*")
;; (looking-at "\\* TODO"); redundant
(let ((case-fold-search nil))
  (looking-at "[\n\t ]*[A-Z]")) 
(looking-at "[\n\t ]*#\\+")
) 
(capitalize-word 1))
)

(defun downcase-save-excursion ()
  (interactive)
(unless
(or

(looking-at "[ ]*I\\b") ; never downcase the word "I"
;; (looking-at "[ ]*I\'") ; never downcase the word "I'"
(looking-at "[ ]*I'")  ; never downcase I'm I've etc.
(looking-at "[[:punct:]]*[ ]*$") ; zero or more whitespaces followed by zero or more punctuation followed by zero or more whitespaces followed by a line break
(looking-at "\"[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"
(looking-at "\)[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"
(looking-at (sentence-end)) ; quotation mark followed by "zero or more whitespace then end of line?"
       (looking-at (user-full-name))


)
  (save-excursion
      (downcase-word 1))
  ))

(defun smart-expand ()
  (interactive) 

  (unless
  
    (or
       (looking-back "\)\n*")
(looking-back "\)[ ]*")
(looking-back ":t[ ]*") 

;; (looking-back "\\\w") ; for some reason this matches all words, not just ones that start with a backslash
)
    (expand-abbrev)
)
)

(add-hook 'fountain-mode-hook 'turn-on-olivetti-mode)

(setq frame-title-format (concat "Hey bro, just FYI, this file is called %b or something like that."))

(define-key key-minor-mode-map (kbd "M-(") 'backward-word)
(define-key key-minor-mode-map (kbd "M-)") 'forward-word)

(defun capitalize-sentence ()
  (interactive)
(unless (my/beginning-of-sentence-p)
(org-backward-sentence))
  (endless/capitalize)
(org-forward-sentence 1)
(jay/right-char) 
)
(define-key key-minor-mode-map (kbd "M-C") 'capitalize-sentence)

(defun downcase-sentence ()
  (interactive)
(unless (my/beginning-of-sentence-p)
(org-backward-sentence))
  (downcase-word 1)
(org-forward-sentence 1)
(jay/right-char)
)

(define-key key-minor-mode-map (kbd "M-L") 'downcase-sentence)

(defun return-insert-blank-line-before ()
  (interactive)
  (beginning-of-line)
(newline)
  )

(defadvice load-theme (before theme-dont-propagate activate)
 (mapcar #'disable-theme custom-enabled-themes))

(defun toggle-item-or-hyphenation ()
(interactive "P")
(if

    (region-active-p)                               ; if
    (org-toggle-item) ; then
    (cycle-hyphenation); else
)
)

(defun smart-forward-sentence ()
  (interactive)
  (org-forward-sentence)
  (my/fix-space)
  )

(defun replace-inner ()
  (interactive)
(change-inner)
  (pasteboard-paste-no-spaces)
  )

(require 'smex)
(setq smex-completion-method 'ivy)
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

(defcustom ivy-height 30
  "Number of lines for the minibuffer window."
  :type 'integer)

;;advise swiper to recenter on exit
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter)
  )
(advice-add 'swiper :after #'bjm-swiper-recenter)

(defun embolden-or-bold (arg)
  (interactive "p")
  (if (region-active-p)
    (wrap-region-trigger arg "*")
    (embolden-next-word)))

(defun send-message-without-bullets ()
  (interactive)
  (remove-hook 'org-mode-hook 'org-bullets-mode)
  (message-send)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'send-message-without-bullets)))

(defadvice load-theme (after load-theme-advice activate)
(custom-set-faces
'(bold ((t (:inherit font-lock-warning-face :weight bold))))
'(org-link ((t (:underline nil))))) 
(org-mode) 
  )
