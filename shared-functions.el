;; define packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/" ))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;; UI


(require 'auto-complete)
(require 'org-bullets)

;; ;; This customization with the big blocky boxy red cursor makes me so happy.
(set-cursor-color "red")
(setq default-frame-alist
      '((cursor-color . "red")))
(add-to-list 'default-frame-alist '(cursor-color . "red"))

;; and a function to restore it if a color-theme overwrites it:
(defun  incarnadine-cursor ()
  (interactive)
  (set-cursor-color "red")
  (setq default-frame-alist
	'((cursor-color . "red")))
  (add-to-list 'default-frame-alist '(cursor-color . "red")))


;; take notes from the web and attribute it properly
(defun web-research ()
  (interactive)
  (insert "#+BEGIN_QUOTE\n")
  (let ((p (point)))
    (insert "\n\n#+END_QUOTE\n")
    (org-mac-chrome-insert-frontmost-url)
    (goto-char p))
  )


(defun zenburn ()
  "Open my favorite file:  ~.emacs.
The function is poorly named, didn't really want to 'load' it, just open it."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/zenburn-jay.el")
  (org-mode)
  (set-cursor-color "violet")
  )

(defun ritchie ()
  (interactive)
  (load-theme 'ritchie)
  (incarnadine-cursor)
  (org-mode)
  )



(defun courier-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Courier" :height 215)
    (set-frame-width (selected-frame) 97)
)

(defun medium-type ()
  (interactive)
  (set-face-attribute 'default nil :font "Courier" :height 260)
  (set-frame-width (selected-frame) 89)
  )


(defun large-type ()
  (interactive)
  (set-face-attribute 'default nil :font "Courier" :height 350)
  (set-frame-width (selected-frame) 68)
  )


(defun lucida-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Ludida Sans Typewriter" :height 200))

(defun monaco-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Monaco" :height 160))

(defun inconsolata-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Inconsolata" :height 160))

(defun last-words ()
  (interactive)
  (set-face-attribute 'default nil :font "Last Words" :height 200))



(defun solarized-dark ()
  "Open my own customized version of the Solarized color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/solarized-jay.el")
  (load-theme 'solarized-dark)
  (incarnadine-cursor)
  )

(defun solarized-light ()
  "Open my own customized version of the Solarized color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/solarized-jay.el")
  (load-theme 'solarized-light)
  (incarnadine-cursor)
  )



(defun big-cyberpunk ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-big-jay.el")
  (org-mode)
  (org-bullets-mode 1)
  (incarnadine-cursor)
  (turn-on-auto-capitalize-mode)
  )

(defun cyberpunk-jay ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-jay.el")
  (org-mode)
  (org-bullets-mode 1)
  (incarnadine-cursor)
;;  (hl-line-mode 1)
  (turn-on-auto-capitalize-mode)
  (toggle-maxframe)
(play-sound-file "/Users/jay/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/Space Age Motor Cycle 02.wav"))

(defun cyberpunk-large ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-jay.el")
  (org-mode)
  (org-bullets-mode 1)
  (incarnadine-cursor)
  (turn-on-auto-capitalize-mode)
(large-type)
  (toggle-maxframe)
(play-sound-file "/Users/jay/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/Space Age Motor Cycle 02.wav"))


(require 'color-theme)


(defun lion ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
(play-sound-file "/Users/jay/Music/iTunes/iTunes Media/Music/MGM/Unknown Album/MGM Logo 3 Roar 2008 Restoration.mp3"))


(add-hook 'org-after-todo-state-change-hook
	  'lion)



(defun matrix ()
  (interactive)
  (set-face-attribute 'default nil :background "black"
                      :foreground "lime" :font "Courier" :height 180))






(defun timesvr ()
"Task request to my virtual assistant."
(interactive)
  (message-mail)
  (message-goto-subject) (insert "task request: " (format-time-string "%F %l:%M%P"))
(message-goto-body) (insert "\n")
  )
(global-set-key (kbd "C-c t") 'timesvr)
(global-set-key (kbd "C-c m") 'compose-mail)


(defun gf ()
"log in to my Wordpress blog"
  "turn on org2blog/wp-mode and log in"
  (interactive)
  (org2blog/wp-mode)
  (org2blog/wp-login)
  )

(defun path-copy-full-path-to-clipboard ()
  "Copy the full current filename and path to the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


(delete-selection-mode 1) ; make typing override text selection



;; some favorite org settings
(setq org-indent-mode t)
(setq org-indent-indentation-per-level 2)
(setq org-use-property-inheritance t)
(setq org-ctrl-k-protect-subtree t)



;; org-capture Setup ==================================================================================
(setq org-capture-templates
      (quote
       (
        ("m" "Mail" entry (file+olp org-default-notes-file "Emails") "** Email %T
From: Jay Dixit <dixit@aya.yale.edu>
To: %^{Send mail to}
Subject: %^{Subject}
--text follows this line--
%?")

	("g" "gratitude" entry (file "gratitude.txt")
	 "\n\n\n\n* %U\n\n1. %?\n\n" :prepend t :kill-buffer t)


	("l" "Later" checkitem (file+headline "playful.org" "later") "\n\n [ ] %?\n\n" :prepend t :kill-buffer t)


	("n" "note" entry (file org-default-notes-file)
	 "* %? :NOTE:\n%U\n%a\n  %i" :prepend t :kill-buffer t :clock-in t :clock-resume t)

	("b" "book" entry (file "../book/book-capture.txt" :prepend t :kill-buffer t)
	 "\n\n* %i%?\n\nEntered on %U %i\n\n" :prepend t :kill-buffer t)

	("s" "Signal flare" entry (file "../signal-flare-assets/signal-flare-capture.txt")
	 "\n\n\n\n*  %? %i\n \n" :prepend t :kill-buffer t)

	("e" "expression" entry (file "expression.txt")
	 "\n\n* %U\n  %i\n %?\nEntered on %U  %i\n" :prepend t :kill-buffer t)

	("h" "historical interest" entry (file "historical-lifestream.txt")
	 "\n\n* %U\n  %i\n %?\nEntered on %U  %i\n" :prepend t :kill-buffer t)

	("p" "pages" entry (file "~/Dropbox/writing/notationaldata/pages.txt")
	 "\n\n\n\n* %U\n\n%?\n\nEntered on %U  %i\n\n" :prepend t :kill-buffer t)


	("w" "writing" entry (file "~/Dropbox/writing/notationaldata/writing-teacher/teaching-writing-and-storytelling.txt")
	 "\n\n\n\n* %U\n\n%?\n\nEntered on %U  %i\n\n" :prepend t :kill-buffer t)


	("v" "Vegas journal" entry (file "vegas-journal-capture.txt")
	 "\n\n\n\n* %U\n\n%?\n\nEntered on %U  %i\n\n" :prepend t :kill-buffer t)

	("f" "flowy" entry (file "flowy.org")
	 "\n\n*  %i\n %?\n" :prepend t :kill-buffer t))))


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))




;; enable recent files not using Ido mode C-x C-r =============================================
(recentf-mode 1) ; recentf
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
	 (tocpl (mapcar (function
			 (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
	 (prompt (append '("File name: ") tocpl))
	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-string fname tocpl)))))
(global-set-key [(control x)(control r)] 'recentf-open-files-compl)



;; enable recent files using Ido mode COMMAND-R (then RIGHT-ARROW to browse) =============================================
;; xsteve-ido-choose-from-recentf allows me to switch to any recently opened file.
;; The nice thing, using that function is, that it does not matter, if I have the buffer already opened, or if the file must be opened now. With that function I have a persistent buffer list available.
(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the 'recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read ""
			  (mapcar (lambda (path)
				    (replace-regexp-in-string home "~" path))
				  recentf-list)
			  nil t))))




(global-set-key "\C-ce" 'eval-buffer)
(global-set-key "\C-cr" 'eval-region)



(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)




(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
			  (buffer-substring-no-properties
			   (progn (skip-syntax-backward "w_") (point))
			   (progn (skip-syntax-forward "w_") (point)))
			  "\\>")))
      (if (and isearch-case-fold-search
	       (eq 'not-yanks search-upper-case))
	  (setq string (downcase string)))
      (setq isearch-string string
	    isearch-message
	    (concat isearch-message
		    (mapconcat 'isearch-text-char-description
			       string ""))
	    isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

					; (global-set-key "\C-cw" 'my-isearch-word-at-point)



;; To preserve (somewhat) the integrity of your archive structure while archiving lower level items to a file, you can use the following defadvice:
(defadvice org-archive-subtree (around my-org-archive-subtree activate)
  (let ((org-archive-location
	 (if (save-excursion (org-back-to-heading)
			     (> (org-outline-level) 1))
	     (concat (car (split-string org-archive-location "::"))
		     "::* "
		     (car (org-get-outline-path)))
	   org-archive-location)))
    ad-do-it))




(add-hook 'ido-setup-hook
	  (lambda ()
	    ;; Go straight home
	    (define-key ido-file-completion-map
	      (kbd "~")
	      (lambda ()
		(interactive)
		(if (looking-back "/")
		    (insert "~/")
		  (call-interactively 'self-insert-command))))))



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


;; Save all
(add-hook 'org-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'org-mode-hook (lambda () (setq palimpsest-mode t)))
(add-hook 'markdown-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'text-mode-hook (lambda () (setq buffer-save-without-query t)))
(add-hook 'css-mode-hook (lambda () (setq buffer-save-without-query t)))


(add-to-list 'auto-mode-alist '("README$" . org-mode))



(define-skeleton my-orgfootnote "Docstring." nil
  "[fn:: " _ "] ")


(define-skeleton fws "Docstring." nil
  "# ###################################################################################\n#+HTML: [full_width_section bg_pos='Left Top' parallax_bg='true' bg_repeat='No-Repeat' text_color='Light' top_padding='' bottom_padding='' background_color='#000' image_url='" _ "']\n\n#+HTML: <H1></H1>\n\n#+HTML: [/full_width_section]\n# ####################################################################################\n\n")

(define-skeleton fwh "Docstring." nil
  "# ###################################################################################\n#+HTML: [full_width_section bg_pos='Left Top' parallax_bg='true' bg_repeat='No-Repeat' text_color='Light' top_padding='' bottom_padding='200' background_color='#000' image_url='" _ "']\n\n#+HTML: <H1 class='fwh'></H1>\n\n#+HTML: [/full_width_section]\n# ####################################################################################\n\n")




(define-skeleton my-org-slide "Docstring." nil
  "* " _ " :slide:")


(define-skeleton slidy-image "Docstring." nil
  "<figure >
<img src='"_"'>
<figcaption></figcaption>
</figure>")


(define-skeleton shaded "Docstring." nil
  "<DIV CLASS='shaded'>
"_"
</DIV>")


(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

(defun paste-and-replace-quotes ()
  "Yank (paste) and replace smart quotes from the source with ascii quotes."
  (interactive)
  (clipboard-yank)
  (replace-smart-quotes (mark) (point)))





(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
	(org-back-to-heading)
	(org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))



;; need to add "cleanup buffer safe" here from whattheemacsd.com
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)



(setq org-export-with-smart-quotes t)


(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))



;; make org-mode temporarily emulate traditional outlining keybindings?
(define-minor-mode zin/org-outline-mode
  "" nil
  :lighter " OOut"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<return>") 'org-meta-return)
            (define-key map (kbd "<tab>") 'org-metaright)
            (define-key map (kbd "S-<tab>") 'org-metaleft)
            (define-key map (kbd "<M-return>") 'org-return)
            map))
(global-set-key "\C-co" 'zin/org-outline-mode)



;; org-mode function to check checkbox and move to next in list?
                                        ; http://superuser.com/questions/568482/org-mode-function-to-check-checkbox-and-move-to-next-in-list#
(defun zin/org-checkbox-next ()
  (interactive)
  (when (org-at-item-checkbox-p)
    (org-toggle-checkbox))
  (org-next-item))


(defun add-word-to-personal-dictionary ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))


;; search in Spotlight
(setq locate-command "mdfind")


;; Set transparency of emacs
(defun transparency-set-value (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (incarnadine-cursor)
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))



;; search in Spotlight
(setq locate-command "mdfind")


(defun org-day ()
  "foo"
  (interactive)
  (insert (format-time-string "[%H:%M]"))
  )


(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


					;; (global-hl-line-mode t) ; turn it on for all modes by default
;; (global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'message-mode-hook (lambda () (setq global-hl-line-mode nil)))

(add-hook 'org-mode-hook
          (lambda()
            (hl-line-mode 1)
            (global-hl-line-mode t))
          't)




;;; Nice size for the default window
(defun get-default-height ()
  (/ (- (display-pixel-height) 40)
     (frame-char-height)))

; (add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))




(setq sentence-end-double-space nil)
(defun my-next-sentence ()
  "Move point forward to the next sentence.
Start by moving to the next period, question mark or exclamation.
If this punctuation is followed by one or more whitespace
characters followed by a capital letter, or a '\', stop there. If
not, assume we're at an abbreviation of some sort and move to the
next potential sentence end"
  (interactive)
  (re-search-forward "[.?!]")
  (if (looking-at "[    \n]+[A-Z]\\|\\\\")
      nil
    (my-next-sentence)))

(defun my-last-sentence ()
  (interactive)
  (re-search-backward "[.?!][   \n]+[A-Z]\\|\\.\\\\" nil t)
  (forward-char))




(defun transparent-serenity ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(80 80))
  (add-to-list 'default-frame-alist '(alpha 80 80))
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-serenity.el")
  )

(defun opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100))
  )




(require 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)


(add-to-list 'custom-theme-load-path "~/Dropbox/emacs/prelude/personal/sublime-themes-jay/")

(require 'auto-complete)
(defun ac-ispell-get-word ()
  (format "\\(%s\\)" (car (ispell-get-word nil "\\*"))))

(defun ac-ispell-get-candidates (prefix)
  (let ((word prefix)
        (interior-frag nil))
    (lookup-words (concat (and interior-frag "*") word
                          (if (or interior-frag (null ispell-look-p))
                              "*"))
                  ispell-complete-word-dict)))

(ac-define-source ispell
  '((prefix . ac-prefix)
    (candidates . ac-ispell-get-candidates)))

;; it's not a good idea to add `ac-source-ispell' to `ac-sources',
;; I'll recommend manual invoking.

;; newer version of `ac-define-source' would provide us an `ac-complete-ispell-word'.
;; in case it didn't:
(defun ac-expand-ispell-word ()
  (interactive)
  (let ((ac-sources '(ac-source-ispell)))
    (call-interactively 'ac-start)))

(define-key global-map (kbd "s-/ s") 'ac-expand-ispell-word)

(ac-flyspell-workaround)


(load-file "~/Library/Preferences/Aquamacs Emacs/ac-ispell.el")
;;    ;; Completion words longer than 4 characters

;;
(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))


(require 'help-fns+)








(add-to-list 'load-path "~/Dropbox/emacs/prelude/personal/")
(require 'org-serenity-mode)



(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-decorations (quote ("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not so readable bro]" " [Too big yo]" " [Make it so.]"))
      ido-enable-last-directory-history t
      ido-enter-matching-directory t
      ido-use-faces t
      ido-use-url-at-point t
      ido-max-prospects 10)
(setq ido-everywhere t)


;; Using ido-mode for org-refile (and archiving via refile)
(setq org-complegtion-use-ido t)
(setq confirm-nonexistent-file-or-buffer nil)
(ido-everywhere 1)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
(setq ido-use-filename-at-point t) ;; prefer file names near point
(setq ido-use-filename-at-point 'guess)

(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".org" ".txt" ".md"  ".emacs" ".el"))





(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(ido-first-match ((t (:foreground "red"))))
 '(message-header-cc ((t (:foreground "CornflowerBlue"))))
 '(message-header-name ((t (:foreground "green2"))))
; '(message-header-other ((t (:foreground "VioletRed1"))))
 '(message-header-subject ((t (:foreground "pink" :weight bold))))
 '(message-header-to ((t (:foreground "LightGoldenrod1" :weight bold))))
 '(message-separator ((t (:foreground "LightSkyBlue1"))))
 '(hl-line ((t (:inherit highlight))))
 '(org-headline-done ((t (:strike-through t))))
 '(writegood-weasels-face ((t (:underline (:color "orange" :style wave)))))
 '(tabula-rasa-cursor ((t (:inherit nil :foreground "red" :inverse-video t)))
                      '(ido-first-match ((t (:inherit error :weight normal))))
 t))



                                        ; Make yasnippet work properly with org-mode.



;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
 (setq org-refile-targets (quote ((nil :maxlevel . 2)                               (org-agenda-files :maxlevel . 2))))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

(setq org-goto-interface 'outline-path-completion
     org-goto-max-level 2)


;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)


;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)


(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (
                                      ("k" . org-kill-note-or-show-branches)
                                      ("q" . bh/show-org-agenda)
                                      ("h" . org-agenda-schedule)
                                      ("d" . org-deadline)
                                      ("w" . org-refile)
                                      ("z" . org-add-note)
                                      ("A" . org-archive-subtree-default-with-confirmation)
                                      ("J" . org-clock-goto)
                                      ("Z" . ignore))))


(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (org-agenda-list)
    (org-agenda-list))
  (delete-other-windows))




;; Use System Settings For File-Application Selection
;; To get consistent applications for opening tasks I set the org-file-apps variable as follows:
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)

                            ("\\.pdf\\'" . open)
)))

;; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

;; Delete IDs When Cloning
(setq org-clone-delete-id t)

(setq org-startup-folded t)

;; allow alphabetical list entries, i.e. "a. this b. that c. another"
(setq org-alphabetical-lists t)

;; orgstruct++-mode is enabled in Gnus message buffers to aid in creating structured email messages.
(add-hook 'message-mode-hook 'orgstruct-mode 'append)
; (add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)


(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)


(custom-set-variables
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"  ".tex" ".mm" "Icon" ".html" ".zip")))

 '(org-modules
   (quote
    (org-bbdb org-bibtex org-gnus org-info org-annotate-file org-bullets org-invoice org-mac-iCal org-mac-link  org-panel org-secretary org-velocity org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-choose org-collector org-invoice)))
 '(ido-use-faces t)

 '(display-time-mode t)
  '(abbrev-all-caps nil)
 '(abbrev-file-name "~/Dropbox/elisp/.abbrev_defs")
 '(blink-cursor-mode nil)
 '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Archive*" "*Agenda*" "*fontification*"  "*Warnings*" "*prolific*")))
 '(calendar-latitude 40.7)
 '(case-fold-search t)
 '(cua-highlight-region-shift-only t)
 '(cua-mode nil nil (cua-base))
 '(cursor-type (quote box) t)
 '(debug-on-error t)
 '(deft-directory "~/Dropbox/writing/notationaldata/")
 '(dired-clean-up-buffers-too nil)
 '(dired-details-hidden-string "")
 '(dired-kept-versions 8)
 '(display-time-mode t)
 '(edit-server-default-major-mode (quote org-mode))
 '(edit-server-new-frame t)
 '(eshell-load-hook (quote ((lambda nil (abbrev-mode -1)))))
 '(flyspell-abbrev-p t)
 '(flyspell-use-global-abbrev-table-p t)
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink")))
 '(grep-find-ignored-files (quote (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js" "*.doc" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "Icon*" "*fontification*" "*helm*")))
 '(grep-highlight-matches (quote always))
 '(ido-save-directory-list-file "~/Dropbox/emacs/prelude/personal/savefile/ido.hist")
 '(ido-use-faces t)
 '(ido-use-url-at-point t)
 '(initial-buffer-choice "~/Dropbox/writing/notationaldata/playful.org")
 '(initial-major-mode (quote org-mode))
 '(mail-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(mail-kill-buffer-on-exit t)
 '(make-backup-files t)
 '(message-draft-headers (quote (From References Date)))
 '(message-kill-buffer-on-exit t)
 '(message-required-headers (quote (From (optional . References))))
 '(message-send-hook (quote (recent-addresses-add-headers)))
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(mml-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(only-global-abbrevs t)
 '(org-M-RET-may-split-line (quote ((item . t))))
 '(org-activate-links (quote (bracket plain radio tag date footnote)))
 '(org-agenda-jump-prefer-future t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-timegrid-use-ampm t)
 '(org-archive-location "archive/%s_archive::")
 '(org-ascii-headline-spacing (quote (1 . 1)))
 '(org-ascii-table-use-ascii-art t)
 '(org-bullets-face-name (quote \"Courier\"))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-auto-clock-resolution t)
 '(org-clock-idle-time 5)
 '(org-clock-in-resume t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-clocktable-defaults (quote (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))
 '(org-closed-string "COMPLETED:")
 '(org-ctrl-k-protect-subtree t)
 '(org-custom-properties (quote (">")))
 '(org-default-notes-file "~/Dropbox/writing/notationaldata/notes.txt")
 '(org-display-custom-times nil)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "SOURCE")))
 '(org-edit-src-content-indentation 4)
 '(org-ellipsis (quote org-warning))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-allow-bind-keywords t)

 '(recent-addresses-file "~/Dropbox/emacs/prelude/recent-addresses")
 '(recentf-exclude (quote (".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "elpa" ".bmk" ".jabber" "helm")))
 '(org-export-blocks-witheld (quote (hidden)))
 '(org-export-html-inline-image-extensions (quote ("png" "jpeg" "jpg" "gif" "svg" "tif" "gif")))
 '(org-export-html-style "<link rel='stylesheet' type='text/css' href='~/Dropbox/web-design/custom-css/gmail.css' /> <link rel='stylesheet' type='text/css' href='http://jaydixit.github.io/custom-css/gmail.css' />")
 '(org-export-html-style-extra "<link rel='stylesheet' type='text/css' href='~/Dropbox/web-design/custom-css/gmail.css' /> <link rel='stylesheet' type='text/css' href='http://jaydixit.github.io/custom-css/gmail.css' />")
 '(org-export-html-style-include-default t)
 '(org-export-latex-date-format "%d %B %Y.")
 '(org-export-latex-emphasis-alist (quote (("*" "\\emph{%s}" nil) ("/" "\\textit{%s}" nil) ("_" "\\underline{%s}" nil) ("+" "\\st{%s}" nil) ("=" "\\verb" t) ("~" "\\verb" t))))
 '(org-export-latex-verbatim-wrap (quote ("\\begin{quote}" . "\\end{quote}")))
 '(org-export-preserve-breaks t)
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
 '(org-headline-done ((t (:strike-through t))))
 '(org-hidden-keywords (quote (author title)) nil nil "#+BEGIN_QUOTE")
 '(org-hide-block-startup nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-html-container-element "div")
 '(org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s </h2>
<div id=\"footnote\">
%s
</div>
</div>")
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(org-html-html5-fancy t)
 '(org-html-postamble nil)
 '(org-html-text-markup-alist (quote ((bold . "<strong>%s</strong>") (code . "<blockquote>%s</blockquote>") (italic . "<em>%s</em>") (strike-through . "<del>%s</del>") (underline . "<span class=\"underline\">%s</span>") (verbatim . "<code>%s</code>"))))

 '(recentf-save-file "~/Dropbox/emacs/savefile/recentf")
 '(org-time-stamp-custom-formats (quote ("<%a %b %d>" . "<%m/%d %a %I:%M%p>"))) ; like this: "Apr 18 Fri"
 '(smex-prompt-string "I love you. "))



;; remember recent email addresses
(add-to-list 'load-path "~/Dropbox/elisp/recent-addresses-0.1")
(require 'recent-addresses)
(recent-addresses-mode 1)
(add-hook 'message-setup-hook 'recent-addresses-add-first-to)


(setq org-todo-keywords
      '(
        (sequence "TODO" "STARTED" "|" "DONE!")
        (sequence "DELEGATE" "WAITING" "|" "DONE!")
        (sequence "QUESTION" "|" "ANSWERED")
        (sequence "SOMEDAY/MAYBE" "|" "DONE")
        (sequence "STRATEGY" "|")
        (sequence "IF" "THEN" "|")
        (sequence "GOAL" "PLAN" "|" "DONE!")
        ))

(add-hook 'after-init-hook 'org-agenda-list)


(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )


;; Mark heading done when all checkboxes are checked.
;; see http://thread.gmane.org/gmane.emacs.orgmode/42715

(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
          (if (match-end 1)
              (if (equal (match-string 1) "100%")
                  ;; all done - do the state change
                  (org-todo 'done)
                (org-todo 'todo))
            (if (and (> (match-end 2) (match-beginning 2))
                     (equal (match-string 2) (match-string 3)))
                (org-todo 'done)
              (org-todo 'todo)))))))

(require 'org-mime)
(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "p" "font-family: Georgia; color:#333;")))

(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))))


(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             ".DONE"
             "color:#859900;")))

(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))


(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))


(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-subtree)))






;; You can use org-clock-in-prepare-hook to add an effort estimate. This way you can easily have a "tea-timer" for your tasks when they don't already have an effort estimate.
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

                                        ; Or you can use a default effort for such a timer:

(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-add-default-effort)

(defvar org-clock-default-effort "1:00")

(defun my-org-mode-add-default-effort ()
  "Add a default effort estimation."
  (unless (org-entry-get (point) "Effort")
    (org-set-property "Effort" org-clock-default-effort)))


;; Reschedule agenda items to today with a single command
(defun org-agenda-reschedule-to-today ()
  (interactive)
  (cl-flet ((org-read-date (&rest rest) (current-time)))
	   (call-interactively 'org-agenda-schedule)))


(defun buffer-background-black ()
  (interactive)
  (setq buffer-face-mode-face `(:background "black" :foreground "LightSkyBlue"))
  (buffer-face-mode 1))

;;
(defun my/enable-ac-ispell ()
  (add-to-list 'ac-sources 'ac-source-ispell))
(add-hook 'org-mode-hook 'my/enable-ac-ispell)
(add-hook 'message-mode-hook 'my/enable-ac-ispell)
(add-hook 'message-mode-hook 'buffer-background-black)

(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)



(add-to-list 'recentf-exclude "\\ido.last\\'")
(add-to-list 'recentf-exclude "\\ido")
(add-to-list 'recentf-exclude "\\recent-addresses\\'")
(add-to-list 'recentf-exclude "org-clock-save.el")
(add-to-list 'recentf-exclude "*message*")
(add-to-list 'recentf-exclude ".tex\\")
(add-to-list 'recentf-exclude "html")
(add-to-list 'recentf-exclude "gz")
(add-to-list 'recentf-exclude "System")
(add-to-list 'recentf-exclude "usr")
(add-to-list 'recentf-exclude "Applications")
(add-to-list 'recentf-exclude "bookmark")



;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; define my default directory
(setq default-directory "~/Dropbox/writing/" )


(require 'reveal-in-finder)
(require 'gnugol)


(require 'point-stack)

(global-set-key '[(f5)] 'point-stack-push)
(global-set-key '[(f6)] 'point-stack-pop)
(global-set-key '[(f7)] 'point-stack-forward-stack-pop)
(global-set-key '[(f8)] 'search-open-buffers)






(defun org-ido-completing-read (&rest args)
  "Completing-read using `ido-mode' speedups if available"
  (if (and ido-mode (listp (second args)))
      (apply 'ido-completing-read args)
    (apply 'completing-read args)))




;; Use full outline paths for refile targets - we file directly with IDO
 (setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))



;; maybe this? http://stackoverflow.com/questions/2715616/can-the-point-have-a-longer-history

(setq set-mark-command-repeat-pop t)


;; http://trey-jackson.blogspot.ca/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))


;; best guide to keybindings
;; http://www.nongnu.org/emacs-tiny-tools/keybindings/

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)




;; Don't start the server unless we can verify that it isn't running.
(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p)))
  (server-start))




;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)


;; Show calendar after start
(add-hook 'desktop-after-read-hook 'calendar)

(require 'ido-hacks)

;; sort ido filelist by mtime instead of alphabetically
;; Why would anyone want an alphabetically sorted list? You can save keystrokes if the most recently modified files are at the front:
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))





; (require 'org-devonthink)

(setq bookmark-default-file  (concat user-emacs-directory "bookmarks"))

;; Defining stuck projects as todos that do not contain "scheduled" or "deadline"
;; see also http://www.gnu.org/software/emacs/manual/html_node/org/Stuck-projects.html
(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

(require 'auto-compile)



(require 'osx-browse)
(add-hook 'minibuffer-setup-hook 'conditionally-disable-abbrev)
(add-hook 'minibuffer-exit-hook (lambda () (abbrev-mode 1)))


; (add-to-list 'load-path "~/.emacs-live/packs/stable/colour-pack/lib/color-theme/")



                                        ;(set-cursor-color "yellow")

                                        ; (color-theme-gandalf)
                                        ; (set-cursor-color "red")

(setq split-width-threshold 75)





;;make ^h delete rather than help
(global-set-key (kbd "C-h") 'delete-backward-char)
;;redefine help shortcut
(global-set-key (kbd "M-h") 'help-command)



;; org archive done
;; http://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
(defun my-org-archive-done-tasks ()
(interactive)
(org-map-entries 'org-archive-subtree "/DONE" 'file))




;; and a function to restore it if a color-theme overwrites it:
(defun  incarnadine-cursor ()
  (interactive)
  (set-cursor-color "red")
  (setq default-frame-alist
	'((cursor-color . "red")))
  (add-to-list 'default-frame-alist '(cursor-color . "red")))



(defun  boss-mode ()
  (interactive)
(global-hl-line-mode -1)
(hl-line-mode -1)
)


(unless (and (fboundp 'play-sound-internal)
	     (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))


(add-hook 'org-after-todo-state-change-hook 'my-org-after-todo)
(defun my-org-after-todo ()
  (play-sound-file "~/sounds/InkSoundStroke3.mp3"))





;; (define-key key-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
;; (define-key key-minor-mode-map (kbd "C-x C-f") 'ido-find-file-in-dir)




(global-set-key (kbd "C-c h") 'helm-mini)
;; helm keybindings
;;  [C-n] Move to next helm suggestion.
;; [C-p] Move to previous helm suggestion.


;; add superior flex matching to ido-mode
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)


(require 'palimpsest)
(palimpsest-mode t)
;; C-C C-R to move text to the bottom of the buffer

;; (require 'org-drill)
;; (org-drill)


;; (require 'org-writers-room)





;; defines ibuffer-do-replace-string
(define-ibuffer-op replace-string (from-str to-str)
  "Perform a `replace-string' in marked buffers."
  (:interactive
   (let* ((from-str (read-from-minibuffer "Replace string: "))
          (to-str (read-from-minibuffer (concat "Replace " from-str
                                                " with: "))))
     (list from-str to-str))
   :opstring "replaced in"
   :complex t
   :modifier-p :maybe)
  (save-window-excursion
    (switch-to-buffer buf)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search ibuffer-case-fold-search))
        (while (search-forward from-str nil t)
          (replace-match to-str nil t))))
    t))






;; http://metasandwich.com/2013/01/19/emacs-config-youre-doing-it-wrong/

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
;; helm-imenu




(defun ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))


(require 'helm-config)



;; smex-describe-function
;; smex-find-function
;; smex-where-is










(require 'org-inlinetask)



(require 'openwith)
'(openwith-associations (quote (("\\.skim\\'" "open" (file)) ("\\.pdf\\'" "open" (file)))))
(openwith-mode t)




(require 'org-gcal)

(require 'org-mac-iCal)

;; (require 'org-import-icalendar)



;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)
;; (add-hook 'my-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook 'imenu-add-menubar-index)

(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "I love you.") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)




(make-face 'hard-to-read-font)
(set-face-attribute 'hard-to-read-font nil :background "darkgrey" :foreground "grey")

(define-minor-mode hard-to-read-mode
  "This mode might be useful when you don't like certain text to be seen over your shoulders."
  :init-value nil :lighter " hard-to-read" :keymap nil
  (if hard-to-read-mode
      (progn
        (font-lock-mode nil)
        (buffer-face-mode t)
        (buffer-face-set 'hard-to-read-font))
    (progn
      (font-lock-mode t)
      (buffer-face-mode nil))))





;; #############
;; Position of the Cursor after Searching
;; Note that when you type ‘C-r’, isearch goes backwards. When you search backwards, find something, and then end the search, for example by hitting ‘RET’, point is at the beginning of the string you searched. When you search forwards, point is not necessarily at the end of the string you were searching for, but only after the last character that you typed – bad usability! That means that you cannot predict point based on the string you are searching for (and thinking about). You must predict point based on the numbers of characters you typed. Here is how to change that in your InitFile, courtesy of AdrianKubala:


(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
    (defun my-goto-match-beginning ()
      (when (and isearch-forward isearch-other-end)
        (goto-char isearch-other-end)))
    (defadvice isearch-exit (after my-goto-match-beginning activate)
      "Go to beginning of match."
      (when (and isearch-forward isearch-other-end)
        (goto-char isearch-other-end)))

   (defun my-goto-match-beginning ()
      (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
        (goto-char isearch-other-end)))




(defun isearch-from-buffer-start ()
  (interactive)
(push-mark)
  (goto-char (point-min))
  (isearch-forward))


(add-hook 'dired-mode-hook 'hl-line-mode)



;; Source: http://blog.danieljanus.pl/blog/2008/12/18/fighting-procrastination
;;; Written by Daniel Janus, 2008/12/18.
;;; This snippet is placed into the public domain.  Feel free
;;; to use it in any way you wish.  I am not responsible for
;;; any damage resulting from its usage.

(defvar store-last-modification-time t)
(defvar last-modification-time nil)
(defun mark-last-modification-time (beg end len)
  (let ((b1 (substring (buffer-name (current-buffer)) 0 1)))
    (when (and store-last-modification-time
               (not (string= b1 " "))
               (not (string= b1 "*")))
      (setq last-modification-time (current-time)))))
(add-hook 'after-change-functions 'mark-last-modification-time)
(defun write-lmt ()
  (setq store-last-modification-time nil)
  (when last-modification-time
    (with-temp-file "/tmp/emacs-lmt"
      (multiple-value-bind (a b c) last-modification-time
        (princ a (current-buffer))
        (terpri (current-buffer))
        (princ b (current-buffer)))))
  (setq store-last-modification-time t))
(run-at-time nil 1 'write-lmt)



(defun org-check-misformatted-subtree ()
  "If you think you might have subtrees with misplaced SCHEDULED and DEADLINE cookies, this command lets you check the current buffer: for misformatted entries."
  (interactive)
  (show-all)
  (org-map-entries
   (lambda ()
     (when (and (move-beginning-of-line 2)
                (not (looking-at org-heading-regexp)))
       (if (or (and (org-get-scheduled-time (point))
                    (not (looking-at (concat "^.*" org-scheduled-regexp))))
               (and (org-get-deadline-time (point))
                    (not (looking-at (concat "^.*" org-deadline-regexp)))))
           (when (y-or-n-p "Fix this subtree? ")
             (message "Call the function again when you're done fixing this subtree.")
             (recursive-edit))
         (message "All subtrees checked."))))))


(defun my-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))




(defun my-org-extract-link ()
  "Extract the link location at point and put it on the killring."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (kill-new (org-link-unescape (org-match-string-no-properties 1)))))



;;; Insert link with HTML title as default description
;; When using `org-insert-link' (`C-c C-l') it might be useful to extract contents from HTML <title> tag and use it as a default link description. Here is a way to accomplish this:
(require 'mm-url) ; to include mm-url-decode-entities-string

(defun my-org-insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))



;; It is often helpful to limit yourself to a subset of your agenda files. For instance, at work, you might want to see only files related to work (e.g., bugs, clientA, projectxyz, etc.). The FAQ has helpful information on filtering tasks using filetags and custom agenda commands. These solutions, however, require reapplying a filter each time you call the agenda or writing several new custom agenda commands for each context. Another solution is to use directories for different types of tasks and to change your agenda files with a function that sets org-agenda-files to the appropriate directory. But this relies on hard and static boundaries between files.
;;
;; The following functions allow for a more dynamic approach to selecting a subset of files based on filetags:
(defun my-org-agenda-restrict-files-by-filetag (&optional tag)
  "Restrict org agenda files only to those containing filetag."
  (interactive)
  (let* ((tagslist (my-org-get-all-filetags))
         (ftag (or tag
                   (completing-read "Tag: "
                                    (mapcar 'car tagslist)))))
    (org-agenda-remove-restriction-lock 'noupdate)
    (put 'org-agenda-files 'org-restrict (cdr (assoc ftag tagslist)))
    (setq org-agenda-overriding-restriction 'files)))

(defun my-org-get-all-filetags ()
  "Get list of filetags from all default org-files."
  (let ((files org-agenda-files)
        tagslist x)
    (save-window-excursion
      (while (setq x (pop files))
        (set-buffer (find-file-noselect x))
        (mapc
         (lambda (y)
           (let ((tagfiles (assoc y tagslist)))
             (if tagfiles
                 (setcdr tagfiles (cons x (cdr tagfiles)))
               (add-to-list 'tagslist (list y x)))))
         (my-org-get-filetags)))
      tagslist)))

(defun my-org-get-filetags ()
  "Get list of filetags for current buffer"
  (let ((ftags org-file-tags)
        x)
    (mapcar
     (lambda (x)
       (org-substring-no-properties x))
     ftags)))
;;
;; Calling my-org-agenda-restrict-files-by-filetag results in a prompt with all filetags in your "normal" agenda files. When you select a tag, org-agenda-files will be restricted to only those files containing the filetag. To release the restriction, type C-c C-x > (org-agenda-remove-restriction-lock).




;; Add an effort estimate on the fly when clocking in
;;
;; You can use org-clock-in-prepare-hook to add an effort estimate. This way you can easily have a "tea-timer" for your tasks when they don't already have an effort estimate.
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))



(defun kiwon/org-agenda-redo-in-other-window ()
  "Refresh the agenda view regularly."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
(run-at-time nil 300 'kiwon/org-agenda-redo-in-other-window)




;;; Mark heading done when all checkboxes are checked.

;; An item consists of a list with checkboxes. When all of the checkboxes are checked, the item should be considered complete and its TODO state should be automatically changed to DONE. The code below does that. This version is slightly enhanced over the one in the mailing list (see http://thread.gmane.org/gmane.emacs.orgmode/42715/focus=42721) to reset the state back to TODO if a checkbox is unchecked.
;; Note that the code requires that a checkbox statistics cookie (the [/] or [%] thingie in the headline - see the Checkboxes section in the manual) be present in order for it to work. Note also that it is too dumb to figure out whether the item has a TODO state in the first place: if there is a statistics cookie, a TODO/DONE state will be added willy-nilly any time that the statistics cookie is changed.
;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))




 
(setq org-use-fast-todo-selection t)

(require 'org-drill)

(defun my-org-insert-sub-task ()
  (interactive)
  (let ((parent-deadline (org-get-deadline-time nil)))
    (org-goto-sibling)
    (org-insert-todo-subheading t)
    (when parent-deadline
      (org-deadline nil parent-deadline))))


;; How can I stop the mouse cursor from highlighting lines in the agenda?
(add-hook 'org-finalize-agenda-hook
          (lambda () (remove-text-properties
                      (point-min) (point-max) '(mouse-face t))))



;;; Ignore / Exclude Uninteresting Things
;; 
;; Make Buffer-stack ignore uninteresting buffers
(defun buffer-stack-filter-regexp (buffer)
  "Non-nil if buffer is in buffer-stack-tracked."
  (not (or (string-match "Help\\|minibuf\\|org2blog\\|echo\\|conversion\\|server\\|Messages\\|tex\\|Output\\|temp\\|autoload\\|Customize\\|address\\|clock\\|Backtrace\\|Completions\\|grep\\|Calendar\\|Work\\|Compile\\|tramp\\|accountability\\|helm\\|Alerts\\|Minibuf\\|Agenda\\|Echo\\|gnugol\\|RNC\\|fontification\\|Helm\\|daycolate\\|*Warnings*\\|*scratch*" (buffer-name buffer))
	   (member buffer buffer-stack-untracked))))
(setq buffer-stack-filter 'buffer-stack-filter-regexp)

;; ido-ignore
(setq
 ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "html" "*.mm" "Icon*" "*gz" "*ido.hist")))





(require 'helm-swoop)
;; Change the keybinds to whatever you like :)
; (global-set-key (kbd "M-i") (lambda() (interactive) (helm-swoop :$query nil)))

(setq helm-swoop-pre-input-function
      (lambda () nil))

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)







(defun replace-garbage-chars ()
"Replace goofy MS and other garbage characters with latin1 equivalents."
(interactive)
(save-excursion				;save the current point
  (replace-string "΄" "\"" nil (point-min) (point-max))
  (replace-string "“" "\"" nil (point-min) (point-max))
  (replace-string "’" "'" nil (point-min) (point-max))
  (replace-string "“" "\"" nil (point-min) (point-max))
  (replace-string "—" "--" nil (point-min) (point-max)) ; multi-byte
  (replace-string "" "'" nil (point-min) (point-max))
  (replace-string "" "'" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "" "\"" nil (point-min) (point-max))
  (replace-string "‘" "\"" nil (point-min) (point-max))
  (replace-string "’" "'" nil (point-min) (point-max))
  (replace-string "¡\"" "\"" nil (point-min) (point-max))
  (replace-string "¡­" "..." nil (point-min) (point-max))
  (replace-string "" "..." nil (point-min) (point-max))
  (replace-string "" " " nil (point-min) (point-max)) ; M-SPC
  (replace-string "" "`" nil (point-min) (point-max))  ; \221
  (replace-string "" "'" nil (point-min) (point-max))  ; \222
  (replace-string "" "``" nil (point-min) (point-max))
  (replace-string "" "''" nil (point-min) (point-max))
  (replace-string "" "*" nil (point-min) (point-max))
  (replace-string "" "--" nil (point-min) (point-max))
  (replace-string "" "--" nil (point-min) (point-max))
  (replace-string " " " " nil (point-min) (point-max)) ; M-SPC
  (replace-string "¡" "\"" nil (point-min) (point-max))
  (replace-string "´" "\"" nil (point-min) (point-max))
  (replace-string "»" "<<" nil (point-min) (point-max))
  (replace-string "Ç" "'" nil (point-min) (point-max))
  (replace-string "È" "\"" nil (point-min) (point-max))
  (replace-string "é" "e" nil (point-min) (point-max)) ;; &eacute;
  (replace-string "ó" "-" nil (point-min) (point-max))

 (replace-string "Õ" "'" nil (point-min) (point-max))
 (replace-string "Õ" "'" nil (point-min) (point-max))
 (replace-string "Ñ" "---" nil (point-min) (point-max))
))



(cl-dolist (map '(message-mode-map orgstruct-mode-map))
  (cl-dolist (key '("<M-S-left>" "<M-S-right>" "<M-S-up>" "<M-S-down>"
                    "<M-left>" "<M-right>" "<M-up>" "<M-down>"))
    (define-key (eval map) (kbd key) nil)))


(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
 
  
(helm-mode t)

(lucida-font)


;; MARKED!!!
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
	   (shell-quote-argument (buffer-file-name)))))

(setq wc-modeline-format "[Words: %tw, Chars: %tc]")

(require 'wc-mode)






(defun workflowy-mode ()
  "workflowy"
  (interactive)
  (setq org-bullets-bullet-list (quote ("• ")))
  (zin/org-outline-mode)  
(org-bullets-mode)
(org-bullets-mode)
(boss-mode)
   (define-key org-mode-map (kbd "DEL") 
     'new-org-delete-backward-char)
(define-key key-minor-mode-map (kbd "DEL")  'new-org-delete-backward-char)
 (insert "\n* "))

(require 'org-serenity-mode)
(defun serenity-mode ()
  "serenity"
  (interactive)
  (setq org-bullets-bullet-list (quote ("  ")))
  (org-serenity-mode)  
(org-bullets-mode)
(org-bullets-mode)
)





;;; Isolate Emacs kill ring from OS X system pasteboard.
(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)

;; handle emacs utf-8 input
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")

(defun pasteboard-copy()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "pbcopy"))

(defun pasteboard-paste()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (shell-command-on-region
   (point) (if mark-active (mark) (point)) "pbpaste" nil t))

(defun pasteboard-cut()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end)))


(require 'ls-lisp)
(setq ls-lisp-ignore-case 't)



(defun new-org-delete-backward-char (N)
  (interactive "p")
  (cond ((region-active-p)
         (delete-region
          (region-beginning)
          (region-end)))
        ((looking-back "[*]+ ")
         (previous-line)
         (end-of-line))
        (t
         (org-delete-backward-char N))))

(add-hook 
 'org-mode-hook
 (lambda ()
   (define-key org-mode-map (kbd "DEL") 
     'new-org-delete-backward-char)))


