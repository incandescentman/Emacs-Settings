;; (setq inhibit-startup-message t)
; (add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/")


(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org" load-path)
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org/lisp" load-path)
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org/contrib" load-path)
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/org/contrib/lisp" load-path)

(load "~/Dropbox/elisp/eshell-autojump.el")
(load "~/Dropbox/elisp/play-sound.el")

(require 'point-stack)



(require 'org)
(require 'ox-latex)
					; (require 'ox-md)
					; (require 'ox-ascii)
                                        ;(require 'ox-html)
                                        ;(require 'ox-publish)
					; (require 'ox-s5)
					; (require 'ox-slidy)

					; (load "~/Dropbox/elisp/org/contrib/lisp/ox-s5.el")

(global-visual-line-mode)

(setq auto-mode-alist (cons '("\\.txt" . org-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.msg" . message-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . org-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.abbrev_defs\\'" . org-mode))
(setq auto-mode-alist (cons '("\\.abbrev_defs" . emacs-lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html" . web-mode) auto-mode-alist))


;; STICKY WINDOWS

(global-set-key [(control x) (?0)] 'delete-other-windows)

(global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)
(global-set-key  (kbd "s-0") 'delete-window)
(global-set-key  (kbd "s-1") 'delete-other-windows)
(global-set-key  (kbd "s-9") 'sticky-window-keep-window-visible)
(global-set-key  (kbd "s-2") 'split-window-vertically)
(global-set-key  (kbd "s-3") 'split-window-horizontally)


;; ===== Automatically load abbreviations table =====
;; Note that emacs chooses, by default, the filename
;; "~/.abbrev_defs", so don't try to be too clever
;; by changing its name
(setq-default abbrev-mode t)
(read-abbrev-file "~/Dropbox/elisp/.abbrev_defs")
(read-abbrev-file "~/Dropbox/elisp/own-abbrevs.abbrev_defs")
(setq save-abbrevs t)





(add-hook 'minibuffer-setup-hook (lambda ()
                                   (abbrev-mode -1)))




'(org-support-shift-select (quote always))
'(cua-enable-cua-keys (quote shift))
'(cua-highlight-region-shift-only t)
'(cua-mode nil nil (cua-base))
'(shift-select-mode nil)


(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'mail-mode-hook 'turn-on-visual-line-mode)
(add-hook 'message-mode-hook 'turn-on-visual-line-mode)
(add-hook 'message-mode-hook 'turn-on-auto-capitalize-mode)
                                        ; (add-hook 'message-mode-hook 'turn-on-orgstruct)


(add-hook 'org-mode-hook 'turn-on-flyspell)



;; ---------- MESSAGE MODE ---------------------------------------
;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")



;; Here's a wrapper for message-mail that prompts you for the 'to' and 'subject' lines:
(defun mail-region (b e to subject)
  "Send the current region in an email"
  (interactive "r\nsRecipient: \nsSubject: ")
  (let ((orig-buffer (current-buffer)))
    (message-mail to subject)
    (message-goto-body)
    (insert (save-excursion (set-buffer orig-buffer)
			    (buffer-substring-no-properties b e)))
    (message-send-and-exit)))

;; --------------------------------------------------------------------


;; (set-face-attribute 'default nil :family "Inconsolata" :weight 'normal)


(setq prelude-whitespace nil)



(global-set-key (kbd "`") 'flyspell-auto-correct-word)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key "\C-ce" 'eval-buffer)
(global-set-key "\C-cr" 'eval-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-y") 'redo)


;; mk - mykeybindings
(defvar key-minor-mode-map (make-keymap) "key-minor-mode keymap.")
(define-key key-minor-mode-map (kbd "s-i") 'org-mac-chrome-insert-frontmost-url)
(define-key key-minor-mode-map (kbd "s-\\") 'visit-most-recent-file)
(define-key key-minor-mode-map (kbd "s-f") 'isearch-forward)
;; (define-key key-minor-mode-map (kbd "s-r") 'xsteve-ido-choose-from-recentf)

(define-key key-minor-mode-map (kbd "s-r") 'helm-mini)



;; (define-key key-minor-mode-map (kbd "DEL")  'new-org-delete-backward-char)


(define-key key-minor-mode-map (kbd "s-t") 'new-buffer)
(define-key key-minor-mode-map (kbd "s-g") 'isearch-repeat-forward)
(define-key key-minor-mode-map (kbd "s-h") 'replace-string)
(define-key key-minor-mode-map (kbd "s-k") 'ido-kill-buffer)
(define-key key-minor-mode-map (kbd "s-K") 'zin/org-checkbox-next)

(define-key key-minor-mode-map (kbd "H-w") 'widen)
(define-key key-minor-mode-map (kbd "C-c e") 'eval-buffer)
(define-key key-minor-mode-map (kbd "C-c r") 'eval-region)
(define-key key-minor-mode-map (kbd "s-d") 'org-todo)
(define-key key-minor-mode-map (kbd "C--") 'goto-last-change)
(define-key key-minor-mode-map (kbd "s-4") 'clone-indirect-buffer-other-window)
(define-key key-minor-mode-map (kbd "s-5") 'point-stack-push)
(define-key key-minor-mode-map (kbd "s-6") 'point-stack-pop)
(define-key key-minor-mode-map (kbd "s-7") 'point-stack-forward-stack-pop)
(define-key key-minor-mode-map (kbd "s-8") 'search-open-buffers)
(define-key key-minor-mode-map (kbd "s-L") 'org-mac-chrome-insert-frontmost-url)
(define-key key-minor-mode-map (kbd "s-S") 'org-mac-skim-insert-page)
(define-key key-minor-mode-map (kbd "C-d") 'kill-word)
; (define-key key-minor-mode-map (kbd "=") 'smex)
(define-key key-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key key-minor-mode-map (kbd "=") 'smex)

(define-key key-minor-mode-map (kbd "C-c j") 'helm-org-headlines)
(define-key key-minor-mode-map (kbd "C-x b") 'helm-mini) ; this looks cool
(define-key key-minor-mode-map (kbd "M-b d") 'book-dired)
(define-key key-minor-mode-map (kbd "M-b r") 'read-a-book)
(define-key key-minor-mode-map (kbd "M-b j") 'read-jd) 
(define-key key-minor-mode-map (kbd "M-b M-b") 'book-helm-strict)



 (define-key key-minor-mode-map (kbd "s-v") 'pasteboard-paste)
 (define-key key-minor-mode-map (kbd "s-x") 'pasteboard-cut)
 (define-key key-minor-mode-map (kbd "s-c") 'pasteboard-copy)

 (define-key minibuffer-local-map (kbd "s-v") 'pasteboard-paste)
 (define-key key-minor-mode-map (kbd "s-x") 'pasteboard-cut)
 (define-key key-minor-mode-map (kbd "s-c") 'pasteboard-copy)



(define-key key-minor-mode-map [s-down] 'end-of-buffer)
(define-key key-minor-mode-map [s-up] 'beginning-of-buffer)



(define-key key-minor-mode-map (kbd "s-b") 'org-narrow-to-subtree)
(define-key key-minor-mode-map (kbd "s-B") 'clone-indirect-buffer-other-window)

(define-key key-minor-mode-map (kbd "s-a") 'mark-whole-buffer)
(define-key key-minor-mode-map (kbd "s-o") 'eval-buffer)
(define-key key-minor-mode-map (kbd "s-y") 'undo-tree-redo)
(define-key key-minor-mode-map (kbd "s-y") 'undo-tree-redo)
(define-key key-minor-mode-map (kbd "s-=") 'text-scale-increase)
(define-key key-minor-mode-map (kbd "s--") 'text-scale-decrease)
(define-key key-minor-mode-map (kbd "s-w") 'delete-window)
(define-key key-minor-mode-map (kbd "M-h") 'help-command)
(define-key key-minor-mode-map (kbd "M-+") 'add-word-to-personal-dictionary)
(define-key key-minor-mode-map (kbd "M-s-<right>") 'switch-to-next-buffer)
(define-key key-minor-mode-map (kbd "M-s-<left>") 'previous-buffer)

(define-key key-minor-mode-map (kbd "C-<backspace>") 'delete-char)

;; (define-key key-minor-mode-map (kbd "s-<backspace>") 'delete-char)


(define-key key-minor-mode-map (kbd "s-F") 'locate)
(define-key key-minor-mode-map (kbd "s-(") 'org-velocity)
(define-key key-minor-mode-map (kbd "s-[") 'org-backward-heading-same-level)
(define-key key-minor-mode-map (kbd "s-]") 'org-forward-heading-same-level)
;; (global-set-key (kbd "C-c s") 'org-copy-subtree)
;; (define-key (kbd "C-v s") 'org-paste-subtree)
;; (define-key key-minor-mode-map (kbd "C-v s") 'org-paste-subtree)
;; (define-key key-minor-mode-map (kbd "s-l") 'org-insert-link)
;; (define-key key-minor-mode-map (kbd "s-y") 'redo)
;; (define-key key-minor-mode-map (kbd "s-i") 'markdown-insert-image)
(define-key key-minor-mode-map (kbd "s-`") 'other-window)
;; (define-key key-minor-mode-map (kbd "s-/") 'visit-most-recent-file)
;; available key mappings
;; ; (define-key key-minor-mode-map (kbd "s-\\") 'org-ctrl-c-ctrl-c)
;; (define-key key-minor-mode-map (kbd "s-d") 'org-todo)
;; (define-key key-minor-mode-map (kbd "s-u") 'ido-dired)
(define-key key-minor-mode-map (kbd "s-s") 'save-some-buffers)
;; ; (define-key key-minor-mode-map (kbd "s-b") 'org-narrow-to-subtree)
;; (define-key key-minor-mode-map (kbd "s-b") 'org-tree-to-indirect-buffer)
;; (define-key key-minor-mode-map (kbd "H-n") 'org-narrow-to-subtree)
;; (define-key key-minor-mode-map (kbd "H-w") 'widen)
;; (define-key key-minor-mode-map (kbd "H-g") 'prelude-google)
;; (define-key key-minor-mode-map (kbd "s-j") 'org2blog/wp-post-subtree)
;; (define-key key-minor-mode-map (kbd "s-G") 'osx-browse-guess)
;; (define-key key-minor-mode-map (kbd "s-L") 'org-mac-chrome-insert-frontmost-url)
;; (define-key key-minor-mode-map (kbd "s-;") 'google-define-word-or-phrase)
(define-key key-minor-mode-map (kbd "C-c C-x pi") 'pomodoro-start)
(define-key key-minor-mode-map (kbd "C-c C-x po") 'pomodoro-stop)
;; (define-key key-minor-mode-map (kbd "C-c C-x C-o") 'org-pomodoro)
;; (define-key key-minor-mode-map (kbd "s-R") 'web-research)
;; (define-key key-minor-mode-map (kbd "s-v") 'clipboard-yank) 


;; shortcuts for my own functions
(define-key key-minor-mode-map (kbd "s-m cy") 'cyberpunk-jay) 
(define-key key-minor-mode-map (kbd "s-m cl") 'cyberpunk-large) 
(define-key key-minor-mode-map (kbd "s-m sl") 'solarized-light)
(define-key key-minor-mode-map (kbd "s-m sd") 'solarized-dark) 
(define-key key-minor-mode-map (kbd "s-m ri") 'ritchie) 
(define-key key-minor-mode-map (kbd "s-m wr") 'writeroom-mode) 
(define-key key-minor-mode-map (kbd "s-m wf") 'workflowy-mode) 





(define-key key-minor-mode-map (kbd "C-c C-o") 'helm-org-headlines) ; learn this!
(define-key key-minor-mode-map (kbd "s-m lt") 'large-type) 
(define-key key-minor-mode-map (kbd "s-m mt") 'medium-type) 
(define-key key-minor-mode-map (kbd "s-m df") 'delete-file-and-buffer) 
(define-key key-minor-mode-map (kbd "s-m rf") 'rename-file-and-buffer) 
(define-key key-minor-mode-map (kbd "s-m a") 'org-agenda) 
(define-key key-minor-mode-map (kbd "s-m j") 'helm-imenu-anywhere) 


(define-key key-minor-mode-map (kbd "C-x C-f") 'helm-find-files) 

(define-key key-minor-mode-map (kbd "s-f") 'helm-swoop)

;; (global-set-key "\C-s" 'delete-char)


(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "M-i") 'helm-multi-swoop-all)




(define-key key-minor-mode-map (kbd "s-;") 'ido-goto-symbol)




(define-minor-mode key-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " key" 'key-minor-mode-map)
(key-minor-mode 1)
(defun my-minibuffer-setup-hook ()
  (key-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)


(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(require 'buffer-stack)
(global-set-key [(s-right)] 'buffer-stack-down)
(global-set-key [(s-left)] 'buffer-stack-up)


;; reduce the number of system alarms
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))


;; here's stuff from emacs starter kit
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)

(setq browse-url-browser-function 'browse-default-macosx-browser)




;; add stuff for eshell
;; http://eschulte.github.io/emacs24-starter-kit/starter-kit-eshell.html
;; gmail http://eschulte.github.io/emacs24-starter-kit/starter-kit-gnus.html
;; google docs http://eschulte.github.io/emacs24-starter-kit/starter-kit-g-client.html
;; javascript http://eschulte.github.io/emacs24-starter-kit/starter-kit-js.html
;; elisp http://eschulte.github.io/emacs24-starter-kit/starter-kit-lisp.html


(add-hook 'org-mode-hook
          (lambda()
            (hl-line-mode -1)
            (global-hl-line-mode -1))
          't
          )

;; latex
(if (eq window-system 'mac)
    (add-to-list 'exec-path "/usr/local/texlive/2013/bin/universal-darwin")
  )


;; XeLaTeX customisations
;; org to latex customisations, -shell-escape needed for minted
(setq  ; org-export-dispatch-use-expert-ui t non-intrusive export dispatch
 org-latex-pdf-process               ; for regular export

 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; load my Latex files ====================
;; (load "~/Dropbox/elisp/latex.el")
(load "~/Dropbox/elisp/signal-flare.el")
(load "~/Dropbox/elisp/signal-flare-wide.el")
(load "~/Dropbox/elisp/signal-flare-wide-different-image.el")
(load "~/Dropbox/elisp/jay-dixit-latex.el")


;; (setq aquamacs-scratch-file "~/Dropbox/writing/notationaldata/playful.org")


;; backups ====================================================
(setq
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


					;; '(org-modules (quote (org-bbdb org-bibtex org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-choose org-collector org-invoice)))
'(org-modules (quote (org-info org-jsinfo org-pomodoro org-mac-link org-mime )))


(defadvice org-archive-subtree (around my-org-archive-subtree activate)
  (let ((org-archive-location
	 (if (save-excursion (org-back-to-heading)
			     (> (org-outline-level) 1))
	     (concat (car (split-string org-archive-location "::"))
		     "::* "
		     (car (org-get-outline-path)))
	   org-archive-location)))
    ad-do-it))


'(initial-major-mode (quote org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)

(delete-selection-mode 1) ; make typing override text selection

'(cua-enable-cua-keys (quote shift))
'(cua-highlight-region-shift-only t)
'(cua-mode nil nil (cua-base))
'(cursor-type (quote box))

'(org-replace-disputed-keys t)
'(org-use-extra-keys nil)

'(send-mail-function (quote sendmail-send-it))
'(shift-select-mode nil)
'(transient-mark-mode t)
'(user-mail-address "dixit@aya.yale.edu")
'(global-flyspell-mode t)
'(initial-major-mode (quote org-mode))
'(message-send-mail-function (quote message-send-mail-with-sendmail))
'(mail-send-mail-function (quote message-send-mail-with-sendmail))
'(org-adapt-indentation nil)
'(org-edit-src-content-indentation 4)
'(org-ellipsis (quote org-warning))
'(org-enforce-todo-checkbox-dependencies t)
'(org-enforce-todo-dependencies t)
'(org-html-postamble nil)
'(org-fontify-emphasized-text t)
(setq mail-user-agent 'message-user-agent)
(global-set-key [(A-W)]  'buffer-stack-bury-and-kill)




;; C-c C-x b → open this in a full window
(setq org-indirect-buffer-display 'current-window)

(setq undo-limit 100000)

(setq org-fontify-quote-and-verse-blocks t)


;; automatically open files in their correct modes ===============================================
(setq auto-mode-alist (cons '("\\.txt" . org-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.msg" . message-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.abbrev_defs\\'" . org-mode))
(add-to-list 'auto-mode-alist '("README$" . org-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'css-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'html-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'html-helper-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'shell-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'shell-script-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'term-mode-hook (lambda () (abbrev-mode -1)))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))


;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))



(transient-mark-mode t)

(setq org-directory "~/Dropbox/writing/notationaldata/")
(setq org-default-notes-file (concat org-directory "notes.txt"))
(setq org-ctrl-k-protect-subtree t)

(setq org-use-property-inheritance t)

;; org-mode key bindings =============================================================================
(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cc" 'org-capture)
(setq org-ctrl-k-protect-subtree t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)



(defun visit-most-recent-file ()
  "Visits the most recently open file in `recentf-list' that is not already being visited."
  (interactive)
  (let ((buffer-file-name-list (mapcar 'buffer-file-name (buffer-list)))
	most-recent-filename)
    (dolist (filename recentf-list)
      (unless (memq filename buffer-file-name-list)
	(setq most-recent-filename filename)
	(return)))
    (find-file most-recent-filename)))

(add-to-list 'recentf-exclude "\\ido.last\\'")
(add-to-list 'recentf-exclude "\\recent-addresses\\'")
(add-to-list 'recentf-exclude "org-clock-save.el")
(add-to-list 'recentf-exclude "*message*")
(add-to-list 'recentf-exclude ".tex")
(add-to-list 'recentf-exclude "helm")
(add-to-list 'recentf-exclude "\\ido*")



(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'mail-mode-hook 'turn-on-visual-line-mode)
(add-hook 'message-mode-hook 'turn-on-visual-line-mode)

(visual-line-mode t)
(global-visual-line-mode t)


;; ---------- MESSAGE MODE ---------------------------------------
;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq mail-user-agent 'message-user-agent)



(add-to-list 'completion-styles 'initials t)


(setq browse-url-browser-function 'browse-url-default-macosx-browser)




;; I know that string is in my Emacs somewhere!
(require 'cl)
(defcustom search-open-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS" "Preferences" "Backtrace" "Messages" "Custom" "scratch") eos)))
  "Files to ignore when searching buffers via \\[search-open-buffers]."
  :type 'editable-list)

(require 'grep)
(defun search-open-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-open-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-open-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))


;; search in Spotlight
(setq locate-command "mdfind")


(define-key org-mode-map
  (kbd "RET")
  (lambda()
    (interactive)
    (if (region-active-p)
        (delete-region (region-beginning)
                       (region-end))
      (call-interactively 'org-return))))


(require 'org-pomodoro)



(defun pomodoro-start ()
  (interactive)
  (play-sound-file "~/sounds/mgm-lion-roar.mp3")
  (org-pomodoro)
  )


(require 'reveal-in-finder)
(require 'org-fstree)
(setenv "PATH" (shell-command-to-string "source ~/.profile; echo -n $PATH"))
(require 'eshell-autojump)


(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(require 'buffer-stack)
(global-set-key [(A-right)] 'buffer-stack-down)
(global-set-key [(A-left)] 'buffer-stack-up)

(defun conditionally-disable-abbrev ()
  ""
  (if (string-match "smex-" (format "%s" this-command))
      (abbrev-mode -1)))

;; to enable Edit with Emacs in Chrome
(require 'edit-server)
(edit-server-start)
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

(when (and (require 'edit-server nil t) (daemonp))
  (edit-server-start))

(add-hook 'edit-server-start-hook
	  (lambda ()
	    (when (string-match "github.com" (buffer-name))
	      (org-mode))))


;; ========================================= for "edit with emacs"
(when (and (daemonp) (locate-library "edit-server"))
  (require 'edit-server)
  (edit-server-start))

(when (locate-library "edit-server")
  (require 'edit-server)
  (setq edit-server-new-frame nil)
  (edit-server-start))

(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))


(require 'xml-rpc)
(setq org2blog/wp-blog-alist
      '(
        ("prolific"
         :url "http://prolific.dixit.ca/xmlrpc.php"
         :username "jay"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("random")
         :tags-as-categories t)



        ("gf"
         :url "http://greenfield.dixit.ca/xmlrpc.php"
         :username "jay"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("Found")
         :tags-as-categories t)

        ("jd"
         :url "http://jaydixit.com/wordpress/xmlrpc.php"
         :username "admin"
	 :password "ca9e011jd"
         :default-title "Hello World"
         :default-categories ("Found")
         :tags-as-categories t)

	("newyorkwritersintensive"
         :url "http://www.newyorkwritersintensive.com/xmlrpc.php"
         :username "admin"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("Readings")
         :tags-as-categories t)


        ("prolific"
         :url "http://prolific.dixit.ca/xmlrpc.php"
         :username "jay"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("Found")
         :tags-as-categories t)
	))




;; Save all
(add-hook 'org-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'markdown-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'text-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'css-mode-hook (lambda () (setq buffer-save-without-query t)))






'(cua-enable-cua-keys (quote shift))
'(cua-highlight-region-shift-only t)
'(cua-mode nil nil (cua-base))
'(cursor-type (quote box))
'(ns-right-command-modifier (quote meta))
'(ns-tool-bar-display-mode (quote both) t)
'(ns-tool-bar-size-mode nil t)
'(org-replace-disputed-keys t)
'(org-src-preserve-indentation t)
'(org-startup-align-all-tables t)
'(org-startup-folded showeverything)
'(org-startup-indented nil)
'(org-use-extra-keys nil)
'(send-mail-function (quote sendmail-send-it))
'(shift-select-mode nil)
'(standard-indent 3)
'(transient-mark-mode t)
'(user-mail-address "dixit@aya.yale.edu")
'(global-flyspell-mode t)
'(initial-major-mode (quote org-mode))
'(message-send-mail-function (quote message-send-mail-with-sendmail))
'(mail-send-mail-function (quote message-send-mail-with-sendmail))
'(ns-function-modifier (quote meta))
'(org-adapt-indentation nil)
'(org-edit-src-content-indentation 4)
'(org-ellipsis (quote org-warning))
'(org-enforce-todo-checkbox-dependencies t)
'(org-enforce-todo-dependencies t)
'(org-html-postamble nil)

'(org-fontify-emphasized-text t)
'(org-hide-leading-stars t)
'(org-indent-mode-turns-off-org-adapt-indentation nil)
'(org-indent-mode-turns-on-hiding-stars nil)
'(org-insert-mode-line-in-empty-file t)
'(org-list-indent-offset 3)
'(org-log-done (quote time))
'(org-log-refile (quote time))

'(org-n-level-faces 9)
'(org-odd-levels-only nil)
'(org-priority-faces nil)
'(org-provide-checkbox-statistics t)



;; ---------- MESSAGE MODE ---------------------------------------
;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")

(setq mail-user-agent 'message-user-agent)

(setq auto-mode-alist (cons '("\\.email" . message-mode) auto-mode-alist))
                                        ;



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-stack-show-position nil)
 '(buffer-stack-untracked
   (quote
    ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Agenda*")))
 '(case-fold-search t)
 '(openwith-associations (quote (("\\.pdf\\'" "open" (file)) ("\\.mp3\\'" "xmms" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))

 '(ccm-recenter-at-end-of-file t)
 '(clean-buffer-list-delay-general 1)
 '(column-number-mode nil)
 '(mml-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(message-kill-buffer-on-exit t)
                       '(abbrev-all-caps nil)
                       '(abbrev-file-name "~/Dropbox/elisp/.abbrev_defs")
                       '(flyspell-abbrev-p t)
                       '(flyspell-use-global-abbrev-table-p t)
                       '(global-flyspell-mode t)
                       '(ac-auto-show-menu 2.0)
                       '(ac-auto-start 4)
                       '(ac-candidate-menu-min 3)
                       '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*")))
                       '(only-global-abbrevs t)
                       '(mail-kill-buffer-on-exit t)

'(message-kill-buffer-on-exit t)
 '(mail-kill-buffer-on-exit t)


  '(smex-prompt-string "I love you.  ")
  '(undo-limit 800000)
  '(user-full-name "Jay Dixit")
  '(user-mail-address "dixit@aya.yale.edu")



 '(compose-mail-user-agent-warnings nil)
 '(cua-highlight-region-shift-only t)
 '(cua-mode nil nil (cua-base))
 '(cursor-type (quote box))
 '(debug-on-error t)
 '(delete-window-preserve-buffer
   (quote
    ("*scratch*" "current-book-research.txt" "accountability.txt")))
 '(dired-details-hidden-string "")
 '(display-time-mode t)
 '(edit-server-default-major-mode (quote org-mode))
 '(edit-server-new-frame t)
 '(eshell-load-hook (quote ((lambda nil (abbrev-mode -1)))))
 '(flyspell-abbrev-p t)
 '(flyspell-use-global-abbrev-table-p t)
 '(global-flyspell-mode t)
 '(gmm/auto-mode-list
   (quote
    ("[\\\\/]mail-google-com.*\\.\\(ckr\\|gmm\\|html?\\|txt\\)\\'" "[\\\\/]itsalltext[\\\\/]mail\\.google\\..*\\.txt\\'")))
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js" "*.doc" "*.pdf" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "Icon*")))
 '(grep-highlight-matches (quote always))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "pdf" "tex" "html" ".mm" "Icon*")))
 '(initial-major-mode (quote org-mode))
 '(mail-default-directory
   "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(mail-kill-buffer-on-exit t)
 '(make-backup-files t)
 '(message-draft-headers (quote (From References Date)))
 '(message-kill-buffer-on-exit t)
 '(message-required-headers (quote (From (optional . References))))
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(mml-default-directory
   "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(only-global-abbrevs t)
 '(org-M-RET-may-split-line (quote ((item . t))))
 '(org-activate-links (quote (bracket plain radio tag date footnote)))
 '(org-archive-location "archive/%s_archive::")
 '(org-ascii-headline-spacing (quote (1 . 1)))
 '(org-ascii-table-use-ascii-art t)
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
 '(org-bullets-face-name (quote \"Lucida\ Sans\ Typeriter\"))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-auto-clock-resolution t)
 '(org-clock-idle-time 5)
 '(org-clock-in-resume t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-clocktable-defaults
   (quote
    (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))
 '(org-closed-string "COMPLETED:")
 '(org-ctrl-k-protect-subtree t)
 '(org-custom-properties (quote (">")))
 '(org-default-notes-file "~/Dropbox/writing/notationaldata/notes.txt")
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "SOURCE")))
 '(org-edit-src-content-indentation 4)
 '(org-ellipsis (quote org-warning))
 '(org-enable-fixed-width-editor nil)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-allow-bind-keywords t)
 '(org-export-blocks-witheld (quote (hidden)))
 '(org-export-html-inline-image-extensions (quote ("png" "jpeg" "jpg" "gif" "svg" "tif" "gif")))
 '(org-export-html-style
   "<link rel='stylesheet' type='text/css' href='~/Dropbox/web-design/custom-css/gmail.css' /> <link rel='stylesheet' type='text/css' href='http://jaydixit.github.io/custom-css/gmail.css' />")
 '(org-export-html-style-extra
   "<link rel='stylesheet' type='text/css' href='~/Dropbox/web-design/custom-css/gmail.css' /> <link rel='stylesheet' type='text/css' href='http://jaydixit.github.io/custom-css/gmail.css' />")
 '(org-export-html-style-include-default t)
 '(org-export-latex-date-format "%d %B %Y.")
 '(org-export-latex-emphasis-alist
   (quote
    (("*" "\\emph{%s}" nil)
     ("/" "\\textit{%s}" nil)
     ("_" "\\underline{%s}" nil)
     ("+" "\\st{%s}" nil)
     ("=" "\\verb" t)
     ("~" "\\verb" t))))
 '(org-export-latex-image-default-option "width=20.5cm")
 '(org-export-latex-verbatim-wrap (quote ("\\begin{quote}" . "\\end{quote}")))
 '(org-export-preserve-breaks t)
 '(org-export-time-stamp-file nil)
 '(org-export-with-clocks t)
 '(org-export-with-drawers t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)
 '(org-extend-today-until 8)
 '(org-fontify-done-headline t)
 '(org-fontify-emphasized-text t)
 '(org-footnote-define-inline t)
 '(org-footnote-section "Footnotes")
 '(org-footnote-tag-for-non-org-mode-files "Footnotes:")
 '(org-hidden-keywords (quote (author title)) nil nil "#+BEGIN_QUOTE")
 '(org-hide-block-startup nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-html-container-element "div")
 '(org-html-footnotes-section
   "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s </h2>
<div id=\"footnote\">
%s
</div>
</div>")
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(org-html-html5-fancy t)
 '(org-html-postamble nil)
 '(org-html-text-markup-alist
   (quote
    ((bold . "<strong>%s</strong>")
     (code . "<blockquote>%s</blockquote>")
     (italic . "<em>%s</em>")
     (strike-through . "<del>%s</del>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>"))))
 '(org-html-toplevel-hlevel 2)
 '(org-indent-indentation-per-level 2)
 '(org-indent-mode-turns-off-org-adapt-indentation nil)
 '(org-indent-mode-turns-on-hiding-stars nil)
 ; '(org-indirect-buffer-display (quote other-window))
 '(org-insert-mode-line-in-empty-file t)
 '(org-latex-text-markup-alist
   (quote
    ((bold . "\\textbf{%s}")
     (code . verb)
     (italic . "\\textit{%s}")
     (strike-through . "\\sout{%s}")
     (underline . "\\uline{%s}")
     (verbatim . protectedtext))))
 '(org-latex-toc-command "\\tableofcontents
\\newpage
")
 '(org-list-allow-alphabetical t)
 '(org-list-indent-offset 3)
 '(org-log-done nil)
 '(org-log-note-clock-out nil)
 '(org-log-refile nil)
 '(org-mac-Skim-highlight-selection-p t)
 '(org-mac-grab-Firefox+Vimperator-p nil)
 '(org-mac-grab-Firefox-app-p nil)
 '(org-mac-grab-Mail-app-p nil)
 '(org-mac-grab-Safari-app-p nil)
 '(org-mac-grab-Together-app-p nil)
 '(org-n-level-faces 9)
 '(org-odd-levels-only nil)
 '(org-pomodoro-format "Pomodoro: %s")
 '(org-pomodoro-killed-sound "~/sounds/autodestructsequencearmed_ep.mp3")
 '(org-pomodoro-length 50)
 '(org-pomodoro-long-break-format "Long Break: %s")
 '(org-pomodoro-long-break-sound "~/sounds/tng-computer-programcomplete.mp3")
 '(org-pomodoro-play-ticking-sounds nil)
 '(org-pomodoro-short-break-format "Short Break: %s")
 '(org-pomodoro-short-break-sound "~/sounds/tng-picard-engage.mp3")
 '(org-pomodoro-sound "~/sounds/large-applause.mp3")
 '(org-pomodoro-ticking-sound
   "~/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/com.taptanium.thunderstorm.DreamQuest_preview.m4a")
 '(org-priority-faces nil)
 '(org-provide-checkbox-statistics t)
 '(org-replace-disputed-keys nil)
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-src-preserve-indentation t)
 '(org-startup-align-all-tables t)
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-support-shift-select (quote always))
 '(org-time-clocksum-use-effort-durations t)
  '(org-export-date-timestamp-format "%Y%m%d %I:%M%p")
 ;; '(org-html-metadata-timestamp-format "%Y%m%d %a %l:%M%p")
                                        ; '(org-time-stamp-custom-formats (quote ("<%m/%d/%Y %a>" . "<%m/%d/%Y %a %l:%M%p>"))) ; full dates
 ;; '(org-time-stamp-custom-formats (quote ("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %I:%M%p>"))) ; precise dates
 ;; '(org-time-stamp-custom-formats (quote ("<%m/%d %a>" . "<%m/%d %a %I:%M%p>"))) ; shorthand numeric dates
                                        ;'(org-time-stamp-custom-formats (quote ("<%b %e %a>" . "<%m/%d %a %I:%M%p>"))) ; shorthand word dates with day of the week
 ;; '(org-time-stamp-custom-formats (quote ("<%b %e>" . "<%m/%d %a %I:%M%p>"))) ; shorthand word dates no day of the week, abbreviated month
 ;; '(org-time-stamp-custom-formats (quote ("<%a %B %e>" . "<%m/%d %a %I:%M%p>"))) ; shorthand word dates full month


 '(org-use-speed-commands t)
 '(org-yank-adjusted-subtrees t)
 '(org2blog/wp-confirm-post nil)
 '(org2blog/wp-default-categories (quote ("inspiration" "personal growth" "miscellany")))
 '(org2blog/wp-keep-new-lines t)
 '(org2blog/wp-show-post-in-browser t)
 '(org2blog/wp-use-tags-as-categories t)
 '(osx-browse-prefer-background nil)
 '(osx-browse-prefer-browser "com.google.Chrome")
 '(osx-browse-prefer-new-window t)
 '(pomodoro-break-time 10)
 '(pomodoro-work-time 50)
 '(reb-re-syntax (quote string))
 '(recentf-exclude
   (quote
    ( ".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "elpa" ".bmk" ".jabber" "helm")))
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 999)
 '(safe-local-variable-values
   (quote
    ((eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1)))))
 '(send-mail-function (quote sendmail-send-it))
 '(standard-indent 3)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(undo-limit 800000)
 '(user-full-name "Jay Dixit")
 '(user-mail-address "dixit@aya.yale.edu")
 '(visual-line-mode nil t)
 '(cua-mode nil)
 )



(require 'auto-capitalize)


;; byte compile config file if changed
(add-hook 'after-save-hook
          '(lambda ()
             (when (string-match
                    (concat (expand-file-name "~/.elisp/cfg/") ".*\.el$")
                    buffer-file-name)
               (byte-compile-file buffer-file-name))))

(setq transient-mark-mode t) ; visually show region

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)





;; make org-capture open in full window! :-)
(add-hook 'org-capture-mode-hook 'turn-on-auto-capitalize-mode)
(add-hook 'org-capture-mode-hook 'delete-other-windows)




(global-set-key (kbd "M-]") 'outline-next-visible-heading)
(global-set-key (kbd "M-[") 'outline-previous-visible-heading)
(global-set-key (kbd "M-1") 'auto-capitalize-mode)
(tooltip-mode -1)





(add-to-list 'completion-styles 'initials t)

;; (set-face-attribute 'default nil :font "Lucida Sans Typewriter" :height 180)
; (set-face-attribute 'default nil :font "Courier"  :height 200)




;; turn on flyspell-mode for org-mode




;; turn off fill mode, which adds random line breaks in my text files
(auto-fill-mode -1)
(add-hook 'text-mode-hook  '(lambda () (auto-fill-mode -1)))
(add-hook 'org-mode-hook  '(lambda () (auto-fill-mode -1)))
(add-hook 'org-mode-hook  '(lambda () (writegood-mode 1)))
(add-hook 'markdown-mode-hook  '(lambda () (auto-fill-mode -1)))
(add-hook 'message-mode-hook  '(lambda () (auto-fill-mode -1)))


(setq ns-function-modifier 'hyper)


(delete-selection-mode 1) ; make typing override text selection



;; Global counter to ensure every new buffer will be unique
(defvar new-buffer-count 0)
(defun new-buffer ()
  (interactive)
  (setq new-buffer-count (+ new-buffer-count 1))
  (switch-to-buffer (concat "buffer" (int-to-string new-buffer-count)))
(org-mode)
  )
(global-set-key (kbd "s-T") 'new-buffer)

                                        ;(define-key key-minor-mode-map "\s-\S-T" 'new-buffer)


(setq org-indirect-buffer-display 'current-window)
(defun org-new-scratch-buffer ()
  (interactive)
  (insert "* scratch " (format-time-string "%F %l:%M%P\n\n"))
  (org-tree-to-indirect-buffer 'current-window)
  )




;; (global-set-key (kbd "s-u") 'dired-single)


(define-key global-map (kbd "<C-wheel-up>") (lambda ()
                                              (interactive)
                                              (scroll-up-command)))
(define-key global-map (kbd "<C-wheel-down>") (lambda ()
                                               (interactive)                                                (scroll-down-command)))



;; (add-to-list 'load-path "~/Dropbox/elisp/bbdb/lisp")
;; (require 'bbdb) ;; (3)
;; (bbdb-initialize 'gnus 'message)   ;; (4)
;; (setq bbdb-north-american-phone-numbers-p nil)   ;; (5)

;; (setq vc-handled-backends ())


;; use key chords invoke commands
(require 'key-chord)
(key-chord-mode 1)




;; (load "~/Dropbox/emacs/prelude/personal/tabula-rasa-mode.el")


;; open files in an existing frame instead of a new frame
(setq ns-pop-up-frames nil)


(setq org-src-fontify-natively t)

;;
;; Org Mode
;;
;; (add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\|txt_archive\\)$" . org-mode))

(require 'auto-complete)
(org-mode)


; (require 'icicles)
; (icy-mode 1)



;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))




;; -- DIRECTORY SETTINGS --
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
(setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$\\|\\.DS_Store\\|\\.doc$\\|\\.docx$\\|\\.xlsx$\\|\\.ini$\\|\\.fsLockFile$\\|Icon")

;; Load Dired X when Dired is loaded.
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

;; Enable toggling of uninteresting files.
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t) ; this is buffer-local variable

(defun enable-dired-omit-mode () (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)

(defadvice recover-session (around disable-dired-omit-for-recover activate)
  (let ((dired-mode-hook dired-mode-hook))
    (remove-hook 'dired-mode-hook 'enable-dired-omit-mode)
    ad-do-it))

(require 'dired-details+)






;;

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
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (haskell . t)
   (clojure . t)
   (ditaa . t)))

                                        ;; (setq org-confirm-babel-evaluate nil)



;; http://stackoverflow.com/questions/5682631/what-are-good-custom-keybindings-in-emacs/5682737#5682737
;; 
;; I have an unconventional approach to this that I recommend highly. I have redefined the C-l ('ell') key to be a prefix key, and I use that to prefix my favorite commands. This key is very easy to type and it is bound to a function ('recenter) that isn't used that much. Well, I don't use 'recenter much, but even if you did, it can be assigned to C-l C-l which is almost as easy to type, and a small price to pay for the possibilities opened up by the Ctrl-L-map . (Actually I prefer 'redraw-display to 'recenter, so I gave that the place of honor.)
;; 
;; 
(global-unset-key (kbd "s-m"))
(defvar s-m-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by (Command-M)")
(define-key global-map (kbd "s-m") 's-m-prefix)
(fset 's-m-prefix s-m-map)




 (defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))
                                        ; or any key you see fit





;; http://amitp.blogspot.ca/2008/05/emacs-full-screen-on-mac-os-x.html
(defvar maxframe-maximized-p nil "maxframe is in fullscreen mode")
(defun toggle-maxframe ()
  "Toggle maximized frame"
  (interactive)
  (setq maxframe-maximized-p (not maxframe-maximized-p))
  (cond (maxframe-maximized-p (maximize-frame))
        (t (restore-frame))))
(define-key global-map [(s-return)] 'toggle-maxframe)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))



(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'toggle-fullscreen))

;; make block quotes appear nicely in buffer http://thread.gmane.org/gmane.emacs.orgmode/64980/focus=65987
  (font-lock-add-keywords
       'org-mode '(("^\\(:+\\) " 1 (compose-region (match-beginning 1) (match-end 1) ?> ) nil)))


(setq org-fontify-quote-and-verse-blocks t)

;; Get visual indication of an exception
;; During a normal editing session Emacs gives a warning signal quite often. Every time you type Ctrl-G, to stop searching, or stop what you were typing and do something else, and so on  you will get the bell. Some people find all the beeping annoying. To get a visual signal instead, put the following in your .emacs:

;; (setq visible-bell 1)


;; Reduce the number of warnings
;; In some cases, you d like to reduce the number of warnings or eliminate warnings in certain conditions. The following turns off the alarm bell when you hit  C-g  in the minibuffer or during an  isearch .

(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))


;; Play a different sound
;; Instead of beeping or flashing, Emacs could play a cool sound file, whenever an error occurs (this handles .au or .wav files):
;;
(setq ring-bell-function (lambda () (play-sound-file "~/sounds/InkSoundStroke3.mp3")))


;; Turn off alarms completely
;; Now some people find the flashing annoying. To turn the alarm totally off, you can use this:
;; (setq ring-bell-function 'ignore)


(define-key key-minor-mode-map (kbd "s-,") 'customize-group)
(define-key key-minor-mode-map (kbd "s-G") 'helm-do-grep)


;; (run-with-idle-timer 300 t 'jump-to-org-agenda)

(defun kiwon/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
(run-at-time nil 300 'kiwon/org-agenda-redo-in-other-window)

(defun medium-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 260)
  (set-frame-width (selected-frame) 89)
  )


 (medium-type)
;; (transparent-serenity)
(toggle-maxframe)
