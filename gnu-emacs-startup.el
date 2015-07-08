
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
(setq abbrev-file-name "~/Dropbox/elisp/.abbrev_defs")

(set (make-local-variable 'abbrev-file-name) (expand-file-name "~/Dropbox/elisp/own-abbrevs.abbrev_defs"))
(read-abbrev-file "~/Dropbox/elisp/own-abbrevs.abbrev_defs")
(setq save-abbrevs t)
(setq only-global-abbrevs t)

(defun reflash-indentation ()
"One sentence summary of what this command do."
  (interactive)
  (org-indent-mode 1)
(recenter-top-bottom)
  )

(add-hook 'org-mode-hook 'turn-on-olivetti-mode) 

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
    (shell-command-on-region start end
                             "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'"
                             nil t)
    (my/fix-space)
    (save-excursion
      (goto-char start)
      (my/fix-space))))

(defun pasteboard-paste-without-smart-quotes ()
  (interactive)
  (let ((beg (point)))
    (pasteboard-paste)
    (replace-smart-quotes beg (point))))

(defun minibuffer-pasteboard-paste ()
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

(defun pasteboard-cut()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end))
(my/fix-space)
)

(defun pasteboard-search-in-current-buffer ()
  (interactive)
  (let ((search-term
         (with-temp-buffer
           (pasteboard-paste)
           (buffer-string))))
    (search-forward search-term)))

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


(define-key key-minor-mode-map (kbd "<s-return>") 'toggle-fullscreen)

(define-key key-minor-mode-map (kbd "s-v") 'pasteboard-paste-without-smart-quotes)
(define-key key-minor-mode-map (kbd "s-x") 'pasteboard-cut)
(define-key key-minor-mode-map (kbd "s-c") 'pasteboard-copy)

(define-key key-minor-mode-map (kbd "s-F") 'pasteboard-search-in-current-buffer)


(define-key key-minor-mode-map (kbd "s-Z") 'unexpand-abbrev)

(global-unset-key (kbd "C-S-r"))
(define-key key-minor-mode-map (kbd "C-S-r") nil)
(define-key org-mode-map (kbd "C-S-r") nil)


(define-key key-minor-mode-map (kbd "s-N") 'ni-narrow-to-region-indirect-other-window)



;; pop mark
(define-key key-minor-mode-map (kbd "C-x p")'pop-to-mark-command)

;; projectile
(define-key key-minor-mode-map (kbd "s-P") 'projectile-commander)

;; and make it work in the minibuffer too
(define-key minibuffer-local-map (kbd "s-v") 'minibuffer-pasteboard-paste)
(define-key minibuffer-local-map (kbd "s-x") 'pasteboard-cut)
(define-key minibuffer-local-map (kbd "s-c") 'pasteboard-copy)

(define-key key-minor-mode-map (kbd "C-c C-v") 'refile-region)
(define-key key-minor-mode-map (kbd "H-w") 'widen)
(define-key key-minor-mode-map (kbd "C-c e") 'eval-buffer)
(define-key key-minor-mode-map (kbd "C-c r") 'eval-region)
(define-key key-minor-mode-map (kbd "C--") 'goto-last-change) ; super useful when editing
(define-key key-minor-mode-map (kbd "C-d") 'kill-word-correctly-and-capitalize)
(define-key key-minor-mode-map (kbd "C-j") 'prelude-top-join-line)


(define-key key-minor-mode-map (kbd "C-l") 'reflash-indentation)
;; (define-key org-mode-map (kbd "C-l") 'reflash-indentation)


(define-key key-minor-mode-map (kbd "=") 'smex) ; call any function with easiest keystroke possible
(define-key key-minor-mode-map (kbd "M-x") 'helm-M-x) ; call helm-M-x instead of regular M-as
;; (define-key key-minor-mode-map (kbd "\|") 'deft)

(define-key key-minor-mode-map (kbd "M-K") 'kill-clause)

(define-key key-minor-mode-map (kbd "M-8") 'org-toggle-heading)

(define-key key-minor-mode-map (kbd "C-t") 'transpose-words)

(define-key key-minor-mode-map (kbd "M--") 'cycle-hyphenation)

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


;; and the keybinding
(define-key org-mode-map (kbd "C-k") 'my/kill-line-dwim)


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
(define-key key-minor-mode-map (kbd "s-G") 'helm-do-grep)

;; some custom functions
(define-key key-minor-mode-map (kbd "C-c C-m") 'move-region-to-other-window)

(define-key key-minor-mode-map (kbd "C-c v i") 'org-insert-src-block)

;; org-mime
(define-key org-mode-map (kbd "M-n") 'new-email-from-subtree)

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
(expand-abbrev)
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

;; and the keybinding
(global-set-key (kbd "M-k") 'my/kill-sentence-dwim)

(defun my/kill-line-dwim ()
  "Kill the current line."
  (interactive)
(expand-abbrev)
(org-kill-line)
(my/fix-space))

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(add-to-list 'load-path "~/gnulisp/emacs-pastebin-master/")
(require 'neopastebin)
(pastebin-create-login :dev-key "e5ccb53890f16065d90ebd6064a381d0"
                       :username "petersalazar")

(defun jay/insert-space ()
  "Insert space and then clean up whitespace."
  (interactive)
(expand-abbrev)
(insert "\ ")
  (just-one-space)
)

(define-key org-mode-map (kbd "<SPC>") 'jay/insert-space)

;;; new version
(defun my/fix-space ()
  "Delete all spaces and tabs around point, leaving one space except at the beginning of a line and before a punctuation mark."
  (interactive)
  (just-one-space)
  (when (or (looking-back "^[[:space:]]+")
            (looking-back "-[[:space:]]+")
            (looking-at "[.,:;!?»)-]")
            (looking-back"( ")
            (looking-at " )")
            )
    (delete-horizontal-space)))

(defun insert-space ()
  (interactive)
  (let ((last-command-event ? ))
    (call-interactively 'self-insert-command)))

(global-set-key (kbd "M-SPC") 'insert-space)

(setq org-blank-before-new-entry
      '((heading . always)
       (plain-list-item . nil)))
(setq org-return-follows-link t)

(defun call-rebinding-org-blank-behaviour (fn)
  (let ((org-blank-before-new-entry
         (copy-tree org-blank-before-new-entry)))
    (when (org-at-heading-p)
      (rplacd (assoc 'heading org-blank-before-new-entry) nil))
    (call-interactively fn)))

(defun smart-org-meta-return-dwim ()
  (interactive)
  (call-rebinding-org-blank-behaviour 'org-meta-return))

(defun smart-org-insert-todo-heading-dwim ()
  (interactive)
  (call-rebinding-org-blank-behaviour 'org-insert-todo-heading))

(define-key org-mode-map (kbd "M-<return>") 'smart-org-meta-return-dwim) 
(define-key org-mode-map (kbd "M-S-<return>") 'smart-org-insert-todo-heading-dwim) 

(defun smart-return ()
  (interactive)
  (cond (mark-active
         (progn (delete-region (mark) (point))
                (newline)))
        ;; Shamefully lifted from `org-return'. Why isn't there an
        ;; `org-at-link-p' function?!
        ((and org-return-follows-link
              (let ((tprop (get-text-property (point) 'face)))
                (or (eq tprop 'org-link)
                    (and (listp tprop) (memq 'org-link tprop)))))
         (call-interactively 'org-open-at-point))
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
         (org-meta-return))
        (t (org-return))))

(define-key org-mode-map (kbd "<return>") 'smart-return) 

(defun kill-word-correctly ()
  "Kill word."
  (interactive)
  (expand-abbrev)
  (if (or (re-search-forward "\\=[ 	]*\n" nil t)
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
      (save-excursion (capitalize-word 1)))))

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

(let ((data (quote (("userid" "password") hline ("user1" "trustme99") ("user2" "foolme99") ("user3" "blameme99")))))
;; (defun fun (a b) (princ (format "user: %s\npassword: %s\n" a but)))

;; (mapcar (lambda (x) (fun (car x) (cadr x))) data)
)

(let ((data (quote (("userid" "password") hline ("user1" "trustme99") ("user2" "foolme99") ("user3" "blameme99")))))
;; (defun fun (a b) (princ (format "user: %s\npassword: %s\n" a but)))

;; (mapcar (lambda (x) (fun (nth 0 x) (nth 1 x))) data)
)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

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

(global-set-key "\C-o" 'embolden-next-word)

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "sh" "css"
"dot"

"latex")))
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
  (expand-abbrev)
  (let ((old-point (point))
        (kill-punct (my/beginning-of-sentence-p)))
    (when (re-search-forward "--\\|[][,;:?!…\"”()}]+\\|\\.+ " nil t)
      (kill-region old-point
                   (if kill-punct
                       (match-end 0)
                       (match-beginning 0)))))
  (my/fix-space))

(defvar *smart-punctuation-marks*
  ".,;:!?-")

(setq *smart-punctuation-exceptions*
  (list "?!" ".." "..." "............................................." "---" "!!!" "!:"))

(defun smart-punctuation (new-punct &optional not-so-smart)
  (expand-abbrev)
  (let ((old-point (point)))
    ;; 0. forward smart punctuation marks right after point
    (unless not-so-smart
      (re-search-forward (format "\\=[%s]*" *smart-punctuation-marks*) nil t))
    ;; 1. go back until there are no more spaces/tabs
    (when (re-search-backward "[^ 	][ 	]+\\="
                              nil t)
      (forward-char 1))
    (flet ((replace (text)
             (let ((nr-new-chars (- (length text) (length (match-string 1)))))
               (replace-match text t t nil 1)
               (goto-char old-point)
               (forward-char nr-new-chars))))
      (let (exception)
        (cond ((or not-so-smart
                   (not (re-search-backward (format "[^%s]\\([%s]+\\)\\="
                                                    *smart-punctuation-marks*
                                                    *smart-punctuation-marks*)
                                            nil t)))
               ;; 2.1. if there's not a series of punctuation marks, or if we
               ;; don't want to replace (`not-so-smart'), just insert the
               ;; `new-punct', and move to the `old-point' + the length of
               ;; `new-punct'.
               (insert new-punct)
               (goto-char old-point)
               (forward-char (length new-punct)))
              ((setf exception
                     (let ((potential-new-punct
                            (concat (match-string 1) new-punct)))
                       (find-if (lambda (exception)
                                  (search potential-new-punct exception))
                                *smart-punctuation-exceptions*)))
               ;; 2.2. if the series of punctuation marks concatenated with
               ;; `new-punct' form (even if partially) an exception, then replace
               ;; it with that exception and fix the spaces.
               (replace exception))
              (t
               ;; 2.3. if there is a series of punctuation marks and there is no
               ;; matching exception, replace by the `new-punct' and fix the
               ;; spaces.
               (replace new-punct)))))))

(defun smart-period ()
  (interactive)
  (smart-punctuation "."))

(define-key org-mode-map (kbd ".") 'smart-period)

(defun smart-comma ()
  (interactive)
  (smart-punctuation ","))

(define-key org-mode-map (kbd ",") 'smart-comma)

(defun smart-question-mark ()
  (interactive)
  (smart-punctuation "?"))

(define-key org-mode-map (kbd "?") 'smart-question-mark)

(defun smart-exclamation-point ()
  (interactive)
  (smart-punctuation "!"))

(define-key org-mode-map (kbd "!") 'smart-exclamation-point)

(defun smart-semicolon ()
  (interactive)
  (smart-punctuation ";" t))

(define-key org-mode-map (kbd ";") 'smart-semicolon)

(defun smart-colon ()
  (interactive)
  (smart-punctuation ":" to))

;; (define-key org-mode-map (kbd ":") 'smart-colon)

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
      (my/fix-space))))

(defun my/delete-backward-and-capitalize ()
  "When there is an active region, delete it and then fix up the whitespace"
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-backward-char 1))
  (save-excursion
    (when (or (looking-at "[[:space:]]")
              (looking-back "[[:space:]]"))
      (my/fix-space)))
  (save-excursion
    (when (my/beginning-of-sentence-p)
      (capitalize-word 1))))

(defun backward-kill-word-correctly-and-capitalize ()
  "Backward kill word correctly. Then check to see if the point is at the beginning of the sentence. If yes, then kill-word-correctly and endless/capitalize to capitalize the first letter of the word that becomes the first word in the sentence. Otherwise simply kill-word-correctly."
  (interactive)
(call-interactively 'backward-kill-word-correctly) 
  (let ((fix-capitalization (my/beginning-of-sentence-p))) 
    (when fix-capitalization
      (save-excursion (capitalize-word 1)))))
