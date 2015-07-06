
(require 'package)

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/" ))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; (package-initialize)

(setq package-enable-at-startup nil)
  (package-initialize 'noactivate)

(add-to-list 'load-path "~/Dropbox/emacs/prelude/personal/")

(require 'auto-complete) ;; but only for elisp mode
(require 'org)

(require 'org-bullets)
(require 'ox-latex)
; (require 'org-fstree)

(defun add-word-to-personal-dictionary ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(add-hook 'org-mode-hook 'turn-on-flyspell)

'(mouse-highlight nil)

(setq confirm-kill-emacs 'yes-or-no-p) 

(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))

(setq ring-bell-function (lambda () (play-sound-file "~/sounds/InkSoundStroke3.mp3")))

(defun  incarnadine-cursor ()
  (interactive)
  (set-cursor-color "red")
  (setq default-frame-alist
	'((cursor-color . "red")))
  (add-to-list 'default-frame-alist '(cursor-color . "red")))

(setq org-indirect-buffer-display 'current-window)
(setq undo-limit 100000)
(setq split-width-threshold 75)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'mail-mode-hook 'turn-on-visual-line-mode)
(add-hook 'message-mode-hook 'turn-on-visual-line-mode)
(visual-line-mode t)
(global-visual-line-mode t)

;; (global-hl-line-mode t) ; turn it on for all modes by default
;; (global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'message-mode-hook (lambda () (setq global-hl-line-mode nil)))

(auto-fill-mode -1)
(add-hook 'text-mode-hook  '(lambda () (auto-fill-mode -1)))
(add-hook 'org-mode-hook  '(lambda () (auto-fill-mode -1)))
;; (add-hook 'org-mode-hook  '(lambda () (writegood-mode 1)))
(add-hook 'markdown-mode-hook  '(lambda () (auto-fill-mode -1)))
(add-hook 'message-mode-hook  '(lambda () (auto-fill-mode -1)))

(unless (and (fboundp 'play-sound-internal)
	     (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))


(add-hook 'org-after-todo-state-change-hook 'my-org-after-todo)
(defun my-org-after-todo ()
  (play-sound-file "~/sounds/InkSoundStroke3.mp3"))

(setq sentence-end-double-space nil)

(global-auto-revert-mode 1)

(delete-selection-mode 1)

(autopair-mode 1)
(setq buffer-save-without-query nil)

(setq locate-command "mdfind")

(setq auto-mode-alist (cons '("\\.txt" . org-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.msg" . message-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.abbrev_defs\\'" . org-mode))
(add-to-list 'auto-mode-alist '("README$" . org-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'css-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'html-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'html-helper-mode-hook (lambda () (abbrev-mode -1)))
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

(setq user-mail-address "dixit@aya.yale.edu")
(setq user-full-name "Jay Dixit")

(setq org-use-property-inheritance t)
(setq org-ctrl-k-protect-subtree t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-export-with-smart-quotes t)
(setq org-fontify-quote-and-verse-blocks t)
;; blank lines before new headings
(setq org-blank-before-new-entry
      '((heading . always)
       (plain-list-item . nil))) 
(setq org-return-follows-link t) 

'(org-modules (quote (org-info org-jsinfo org-pomodoro org-mac-link org-mime )))

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

(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(add-hook 'org-finalize-agenda-hook
          (lambda () (remove-text-properties
                      (point-min) (point-max) '(mouse-face t))))

(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

(add-hook 'after-init-hook 'org-agenda-list)
(require 'org-inlinetask)
;; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

;; Delete IDs When Cloning
(setq org-clone-delete-id t)

;; start org in folded mode
(setq org-startup-folded t)

;; allow alphabetical list entries, i.e. "a. this b. that c. another"
(setq org-alphabetical-lists t)

;; fast TODO selection
(setq org-use-fast-todo-selection t)

;; more org settings
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-src-fontify-natively t)

;; (add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\|txt_archive\\)$" . org-mode))

(setq org-todo-keywords
      '(
        (sequence "TODO" "|" "DONE! :-)")
        (sequence "DELEGATE" "DELEGATED" "|" "DONE! :-)")
        (sequence "QUESTION" "|" "ANSWERED")
        (sequence "QUESTIONS" "|" "ANSWERS")
        (sequence "SOMEDAY/MAYBE" "|" "DONE! :-)")
        (sequence "MAYBE" "|" "MAYBE NOT" "DONE! :-)")
        (sequence "NEXT" "|" "DONE! :-)")
        (sequence "DID NOT DO :-/" "STARTED" "|" "DONE! :-)") 
        (sequence "STRATEGY" "|")
(sequence "DONE" "|") 
        (sequence "IF" "THEN" "|")
        (sequence "GOAL" "PLAN" "|" "DONE! :-)")
        ))

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

(defun my-org-export-change-options (plist backend)
  (cond
   ((equal backend 'html)
    (plist-put plist :with-toc nil)
    (plist-put plist :section-numbers nil))
   ((equal backend 'latex)
    (plist-put plist :with-toc t)
    (plist-put plist :section-numbers t)))
  plist)
(add-to-list 'org-export-filter-options-functions 'my-org-export-change-options)

(setq org-export-with-drawers t)
(defun jbd-org-export-format-drawer (name content)
  "Export drawers to drawer HTML class."
  (setq content (org-remove-indentation content))
  (format "@<div class=\"drawer\">%s@</div>\n" content))
(setq org-export-format-drawer-function 'jbd-org-export-format-drawer)
(setq org-icalendar-include-todo t)

'(initial-major-mode (quote org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
'(org-replace-disputed-keys t)
'(org-use-extra-keys nil)
'(org-adapt-indentation nil)
'(org-edit-src-content-indentation 4)
'(org-ellipsis (quote org-warning))
'(org-enforce-todo-checkbox-dependencies t)
'(org-enforce-todo-dependencies t)
'(org-html-postamble nil)
'(org-fontify-emphasized-text t)
'(org-src-preserve-indentation t)
'(org-startup-align-all-tables t)
'(org-startup-folded showeverything)
'(org-startup-indented nil)
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
'(org-provide-checkbox-statistics to)
;; '(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-directory "~/Dropbox/writing/notationaldata/")
(setq org-default-notes-file (concat org-directory "notes.txt"))

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "^"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

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

	("L" "Later" checkitem (file+headline "playful.org" "Later") "\n\n [ ] %?\n\n" :prepend t :kill-buffer t)

	("l" "learnings" entry (file "learnings.org" :prepend t :kill-buffer t)
	 "\n\n* %i%?\n\nEntered on %U %i\n\n" :prepend t :kill-buffer t)

	("n" "note" entry (file org-default-notes-file)
	 "* %? :NOTE:\n%U\n%a\n  %i" :prepend t :kill-buffer t :clock-in t :clock-resume t)

	("b" "book" entry (file "../book/book-capture.txt" :prepend t :kill-buffer t)
	 "\n\n* %i%?\n\n" :prepend t :kill-buffer t)

	("v" "visualness and visual actions" entry (file "visual-actions.txt")
	 "\n\n\n\n*  %? %i\n \n" :prepend t :kill-buffer t)

	("e" "expression" entry (file "expression.txt")
	 "\n\n* %U\n  %i\n %?\nEntered on %U  %i\n" :prepend t :kill-buffer t)

	("h" "historical interest" entry (file "historical-lifestream.txt")
	 "\n\n* %U\n  %i\n %?\nEntered on %U  %i\n" :prepend t :kill-buffer t)

	("p" "pages" entry (file "~/Dropbox/writing/notationaldata/pages.txt")
	 "\n\n\n\n* %U\n\n%?\n\nEntered on %U  %i\n\n" :prepend t :kill-buffer t)

	("s" "storytelling and writing" entry (file "~/Dropbox/writing/notationaldata/writing-teacher/teaching-writing-and-storytelling.txt")
	 "\n\n\n\n* %U\n\n%?\n\nEntered on %U  %i\n\n" :prepend t :kill-buffer t)

	("F" "Funny" entry (file "~/Dropbox/writing/notationaldata/funny.txt")
	 "\n\n\n\n* %U\n\n%?\n" :prepend t :kill-buffer t)

	("V" "Vegas journal" entry (file "vegas-journal-capture.txt")
	 "\n\n\n\n* %U\n\n%?\n\nEntered on %U  %i\n\n" :prepend t :kill-buffer t)

("M" "Memorize" entry
               (file+headline (concat org-directory "org-drill-jays-decks.org")
                              "Vocabulary")
               "* Word :drill:\n%^ \n** Answer \n%^")

;; source: http://stackoverflow.com/questions/14666625/combine-org-mode-capture-and-drill-modules-to-learn-vocabulary
;; http://lists.gnu.org/archive/html/emacs-orgmode/2010-09/msg00924.html

	("f" "flowy" entry (file "flowy.org")
	 "\n\n*  %i\n %?\n" :prepend t :kill-buffer t))))

(defun org-ido-completing-read (&rest args)
  "Completing-read using `ido-mode' speedups if available"
  (if (and ido-mode (listp (second args)))
      (apply 'ido-completing-read args)
    (apply 'completing-read args)))

(setq org-mobile-directory "/Users/jay/Dropbox/Apps/mobileorg/")

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

(defun workflowy-mode ()
  "workflowy"
  (interactive)
  (setq org-bullets-bullet-list (quote ("• ")))
  (zin/org-outline-mode)  
  (org-bullets-mode)
  (org-bullets-mode)
  (boss-mode)
  (incarnadine-cursor)
  (define-key org-mode-map (kbd "DEL") 
    'new-org-delete-backward-char)
  (define-key key-minor-mode-map (kbd "DEL")  'new-org-delete-backward-char)
  (insert "\n* "))

(defun org-checkbox-next ()
  (interactive)
  (when (org-at-item-checkbox-p)
    (org-toggle-checkbox))
  (org-next-item))

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

(defun my-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(defun my-org-extract-link ()
  "Extract the link location at point and put it on the killring."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (kill-new (org-link-unescape (org-match-string-no-properties 1)))))

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

(defun my-org-insert-sub-task ()
  (interactive)
  (let ((parent-deadline (org-get-deadline-time nil)))
    (org-goto-sibling)
    (org-insert-todo-subheading t)
    (when parent-deadline
      (org-deadline nil parent-deadline))))

(defun org-agenda-reschedule-to-today ()
  (interactive)
  (cl-flet ((org-read-date (&rest rest) (current-time)))
	   (call-interactively 'org-agenda-schedule)))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(add-hook 'org-capture-mode-hook 'turn-on-auto-capitalize-mode)
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(add-hook 'org-capture-mode-hook 'writeroom-mode)

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))
