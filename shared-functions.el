
(require 'package)

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/" ))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(add-to-list 'load-path "~/Dropbox/emacs/prelude/personal/")
(load "~/Dropbox/elisp/eshell-autojump.el")
(load "~/Dropbox/elisp/play-sound.el")
(load-file "~/gnulisp/appearance-jay-custom-functions.el")

(require 'auto-complete) ;; but only for elisp mode
(require 'org)
(require 'auto-complete)

(require 'org-bullets)
(require 'ox-latex)
(require 'org-fstree)

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_US")))
 )

(defun add-word-to-personal-dictionary ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

'(mouse-highlight nil)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
          (ding))))

(setq ring-bell-function (lambda () (play-sound-file "~/sounds/InkSoundStroke3.mp3")))

(set-cursor-color "red")
(setq default-frame-alist
      '((cursor-color . "red")))
(add-to-list 'default-frame-alist '(cursor-color . "red"))

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

;; (set-face-attribute 'default nil :font "Lucida Sans Typewriter" :height 180)
;; (set-face-attribute 'default nil :font "Courier"  :height 200)
;; (set-face-attribute 'default nil :font "Monaco" :height 190)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(ido-first-match ((t (:foreground "red"))))
'(bold ((t (:bold t :foreground "red"))))

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
(delete-selection-mode 1) ; make typing override text selection

(electric-pair-mode 1)
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

(setq org-indent-mode t)
(setq org-indent-indentation-per-level 2)
(setq org-use-property-inheritance t)
(setq org-ctrl-k-protect-subtree t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-export-with-smart-quotes t)
(setq org-fontify-quote-and-verse-blocks t)

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

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))

(add-hook 'after-init-hook 'org-agenda-list)

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
        (sequence "IF" "THEN" "|")
        (sequence "GOAL" "PLAN" "|" "DONE! :-)")
        ))

(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

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

(setq org-mobile-directory "/Users/jay/Dropbox/Apps/mobileorg/")

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

(add-hook 'org-finalize-agenda-hook
          (lambda () (remove-text-properties
                      (point-min) (point-max) '(mouse-face t))))

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
'(org-provide-checkbox-statistics t)
'(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-directory "~/Dropbox/writing/notationaldata/")
(setq org-default-notes-file (concat org-directory "notes.txt"))

(add-hook 'org-capture-mode-hook 'turn-on-auto-capitalize-mode)
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(add-hook 'org-capture-mode-hook 'writeroom-mode)

(require 'buffer-stack)

(global-set-key [(s-right)] 'buffer-stack-down)
(global-set-key [(s-left)] 'buffer-stack-up)

(global-set-key [(A-right)] 'buffer-stack-down)
(global-set-key [(A-left)] 'buffer-stack-up)

(defvar new-buffer-count 0)
(defun new-buffer ()
  (interactive)
  (setq new-buffer-count (+ new-buffer-count 1))
  (switch-to-buffer (concat "buffer" (int-to-string new-buffer-count)))
  (org-mode))
(global-set-key (kbd "s-T") 'new-buffer)
;; (define-key key-minor-mode-map "\s-\S-T" 'new-buffer)

(defun org-new-scratch-buffer ()
  (interactive)
  (insert "* oh hi there! " (format-time-string "%F %l:%M%P\n\n"))
  (org-tree-to-indirect-buffer 'current-window)
  )

(add-hook 'minibuffer-setup-hook 'conditionally-disable-abbrev)
(add-hook 'minibuffer-exit-hook (lambda () (abbrev-mode 1)))
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (abbrev-mode -1)))

(setq org-src-fontify-natively t)

;; (add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\|txt_archive\\)$" . org-mode))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Make URLs in comments/strings clickable:
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; spellcheck
(add-hook 'org-mode-hook 'turn-on-flyspell)

'(cua-enable-cua-keys (quote shift))
'(cua-highlight-region-shift-only t)
'(cua-mode nil nil (cua-base))
'(cursor-type (quote box))
'(send-mail-function (quote sendmail-send-it))
'(shift-select-mode nil)
'(transient-mark-mode t)
'(user-mail-address "dixit@aya.yale.edu")
'(global-flyspell-mode t)
'(message-send-mail-function (quote message-send-mail-with-sendmail))
'(mail-send-mail-function (quote message-send-mail-with-sendmail))
'(setq mail-user-agent 'message-user-agent)
'(global-set-key [(A-W)]  'buffer-stack-bury-and-kill)
'(ns-right-command-modifier (quote meta))
'(ns-tool-bar-display-mode (quote both) t)
'(ns-tool-bar-size-mode nil t)
'(standard-indent 3)
'(ns-function-modifier (quote meta))
(transient-mark-mode t)
(tooltip-mode -1)
(setq ns-function-modifier 'hyper)
;; open files in an existing frame instead of a new frame
(setq ns-pop-up-frames nil)

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

(defun org-ido-completing-read (&rest args)
  "Completing-read using `ido-mode' speedups if available"
  (if (and ido-mode (listp (second args)))
      (apply 'ido-completing-read args)
    (apply 'completing-read args)))

(setq auto-mode-alist (cons '("\\.md" . org-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.abbrev_defs" . emacs-lisp-mode) auto-mode-alist))
;; is this the best mode for editing HTML? 
(setq auto-mode-alist (cons '("\\.html" . web-mode) auto-mode-alist))

'(org-support-shift-select (quote always))

(require 'auto-capitalize)
(add-hook 'message-mode-hook 'turn-on-auto-capitalize-mode)
;; (add-hook message-mode-hook turn-on-orgstruct)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq default-directory "~/Dropbox/writing/" )

(if (eq window-system 'mac)
    (add-to-list 'exec-path "/usr/local/texlive/2013/bin/universal-darwin")
  )

;; (load "~/Dropbox/elisp/latex.el")
(load "~/Dropbox/elisp/signal-flare.el")
(load "~/Dropbox/elisp/signal-flare-wide.el")
;; (load "~/Dropbox/elisp/signal-flare-wide-different-image.el")
(load "~/Dropbox/elisp/jay-dixit-latex.el")
(load "~/Dropbox/elisp/signal-flare-smaller-fonts.el")

(setq  ; org-export-dispatch-use-expert-ui t non-intrusive export dispatch
 org-latex-pdf-process               ; for regular export

 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq backup-directory-alist `(("." . "~/.saves")))
(setq
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq vc-make-backup-files t)

(setq smtpmail-debug-info t)

(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq mail-user-agent 'message-user-agent)
(setq auto-mode-alist (cons '("\\.email" . message-mode) auto-mode-alist))

(defun mail-region (b e to subject)
  "Send the current region in an email"
  (interactive "r\nsRecipient: \nsSubject: ")
  (let ((orig-buffer (current-buffer)))
    (message-mail to subject)
    (message-goto-body)
    (insert (save-excursion (set-buffer orig-buffer)
                            (buffer-substring-no-properties b e)))
    (message-send-and-exit)))

(add-to-list 'completion-styles 'initials t)

;; orgstruct++-mode is enabled in Gnus message buffers to aid in creating structured email messages.
;; (add-hook 'message-mode-hook 'orgstruct-mode 'append)
; (add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)

(add-to-list 'load-path "~/Dropbox/elisp/recent-addresses-0.1")
(require 'recent-addresses)
(recent-addresses-mode 1)
(add-hook 'message-setup-hook 'recent-addresses-add-first-to)

(require 'org-pomodoro)

(defun pomodoro-start ()
  (interactive)
  (play-sound-file "~/sounds/mgm-lion-roar-short.mp3")
  (org-pomodoro)
  )

(require 'reveal-in-finder)

(setenv "PATH" (shell-command-to-string "source ~/.profile; echo -n $PATH"))
(require 'eshell-autojump)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

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
 ; (org-indirect-buffer-display (quote other-window))
 '(org-insert-mode-line-in-empty-file t)
 '(org-latex-text-markup-alist
   (quote
    ((bold . "\\textbf{%s}")
     (code . verb)
     (italic . "\\textit{%s}")
     (strike-through . "\\sout{%s}")
     (underline . "\\uline{%s}")
     ;; (verbatim . protectedtext)
     )))

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
    ( ".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "elpa" ".bmk" ".jabber" "helm" "Calendar")))
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
'(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"  ".tex" ".mm" "Icon" ".html" ".zip")))

 '(org-modules
   (quote
    (org-bbdb org-bibtex org-gnus org-info org-annotate-file org-bullets org-invoice org-mac-iCal org-mac-link  org-panel org-secretary org-velocity org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-choose org-collector org-invoice)))
 '(ido-use-faces t)

 '(display-time-mode t)
  '(abbrev-all-caps nil)
 '(abbrev-file-name "~/Dropbox/elisp/.abbrev_defs")
 '(blink-cursor-mode nil)
 '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Archive*" "*Agenda*" "*fontification*"  "*Warnings*" "*prolific*" "*750words*" "Calendar")))
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
 '(web-mode-load-hook (quote ((lambda nil (abbrev-mode -1)))))

 '(flyspell-abbrev-p t)
 '(flyspell-use-global-abbrev-table-p t)
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink")))
 '(grep-find-ignored-files (quote (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js" "*.doc" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "Icon*" "*fontification*" "*helm*" "*750words*")))
 '(grep-highlight-matches (quote always))
 '(ido-save-directory-list-file "~/Dropbox/emacs/prelude/personal/.savefile/ido.hist")
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
 '(recentf-exclude (quote (".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "\\ido.hist\\'" "elpa" ".bmk" ".jabber" "helm")))
 '(org-export-blocks-witheld (quote (hidden)))
 '(org-export-html-inline-image-extensions (quote ("png" "jpeg" "jpg" "gif" "svg" "tif" "gif")))
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

 '(recentf-save-file "~/Dropbox/emacs/.savefile/recentf")
 '(org-time-stamp-custom-formats (quote ("<%a %b %d>" . "<%m/%d %a %I:%M%p>"))) ; like this: "Apr 18 Fri"
 '(smex-prompt-string "I love you. "))


;; There are two `custom-set-variables' calls, with repeated vars; the following
;; is the result of (programmatically) merging the two assoc lists, *assuming
;; there is no repetition* (in which case, the second wins, so as to try to
;; preserve the semantics). I am leaving this commented because the potential
;; for fsck-up is considerable. In any case, there is also a
;; `custom-set-variables' call in custom.el, which will probably override most
;; of the stuff here, since this is loaded before...
;; -- Rúdi Araújo @ 2015/05/16
;; (custom-set-variables
;;  '(org-time-stamp-custom-formats
;;    '("<%a %b %d>" . "<%m/%d %a %I:%M%p>"))
;;  '(recentf-save-file "~/Dropbox/emacs/.savefile/recentf")
;;  '(org-headline-done
;;    ((t
;;      (:strike-through t))))
;;  '(recent-addresses-file "~/Dropbox/emacs/prelude/recent-addresses")
;;  '(org-display-custom-times nil)
;;  '(org-agenda-timegrid-use-ampm t)
;;  '(org-agenda-skip-scheduled-if-done t)
;;  '(org-agenda-jump-prefer-future t)
;;  '(message-send-hook
;;    '(recent-addresses-add-headers))
;;  '(initial-buffer-choice "~/Dropbox/writing/notationaldata/playful.org")
;;  '(ido-use-url-at-point t)
;;  '(ido-save-directory-list-file "~/Dropbox/emacs/prelude/personal/.savefile/ido.hist")
;;  '(web-mode-load-hook
;;    '((lambda nil
;;        (abbrev-mode -1))))
;;  '(dired-kept-versions 8)
;;  '(dired-clean-up-buffers-too nil)
;;  '(deft-directory "~/Dropbox/writing/notationaldata/")
;;  '(calendar-latitude 40.7)
;;  '(blink-cursor-mode nil)
;;  '(ido-use-faces t)
;;  '(org-modules
;;    '(org-bbdb org-bibtex org-gnus org-info org-annotate-file org-bullets org-invoice org-mac-iCal org-mac-link org-panel org-secretary org-velocity org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-choose org-collector org-invoice))
;;  '(completion-ignored-extensions
;;    '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".tex" ".mm" "Icon" ".html" ".zip"))
;;  '(buffer-stack-show-position nil)
;;  '(buffer-stack-untracked
;;    '("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Agenda*"))
;;  '(case-fold-search t)
;;  '(openwith-associations
;;    '(("\\.pdf\\'" "open"
;;       (file))
;;      ("\\.mp3\\'" "xmms"
;;       (file))
;;      ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
;;       ("-idx" file))
;;      ("\\.\\(?:jp?g\\|png\\)\\'" "display"
;;       (file))))
;;  '(ccm-recenter-at-end-of-file t)
;;  '(clean-buffer-list-delay-general 1)
;;  '(column-number-mode nil)
;;  '(mml-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
;;  '(message-kill-buffer-on-exit t)
;;  '(abbrev-all-caps nil)
;;  '(abbrev-file-name "~/Dropbox/elisp/.abbrev_defs")
;;  '(flyspell-abbrev-p t)
;;  '(flyspell-use-global-abbrev-table-p t)
;;  '(global-flyspell-mode t)
;;  '(ac-auto-show-menu 2.0)
;;  '(ac-auto-start 4)
;;  '(ac-candidate-menu-min 3)
;;  '(buffer-stack-untracked
;;    '("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*"))
;;  '(only-global-abbrevs t)
;;  '(mail-kill-buffer-on-exit t)
;;  '(message-kill-buffer-on-exit t)
;;  '(mail-kill-buffer-on-exit t)
;;  '(smex-prompt-string "I love you.  ")
;;  '(undo-limit 800000)
;;  '(user-full-name "Jay Dixit")
;;  '(user-mail-address "dixit@aya.yale.edu")
;;  '(compose-mail-user-agent-warnings nil)
;;  '(cua-highlight-region-shift-only t)
;;  '(cua-mode nil nil
;;             (cua-base))
;;  '(cursor-type 'box)
;;  '(debug-on-error t)
;;  '(delete-window-preserve-buffer
;;    '("*scratch*" "current-book-research.txt" "accountability.txt"))
;;  '(dired-details-hidden-string "")
;;  '(display-time-mode t)
;;  '(edit-server-default-major-mode 'org-mode)
;;  '(edit-server-new-frame t)
;;  '(eshell-load-hook
;;    '((lambda nil
;;        (abbrev-mode -1))))
;;  '(flyspell-abbrev-p t)
;;  '(flyspell-use-global-abbrev-table-p t)
;;  '(global-flyspell-mode t)
;;  '(gmm/auto-mode-list
;;    '("[\\\\/]mail-google-com.*\\.\\(ckr\\|gmm\\|html?\\|txt\\)\\'" "[\\\\/]itsalltext[\\\\/]mail\\.google\\..*\\.txt\\'"))
;;  '(grep-find-ignored-directories
;;    '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink"))
;;  '(grep-find-ignored-files
;;    '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js" "*.doc" "*.pdf" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "Icon*"))
;;  '(grep-highlight-matches 'always)
;;  '(ido-ignore-files
;;    '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "pdf" "tex" "html" ".mm" "Icon*"))
;;  '(initial-major-mode 'org-mode)
;;  '(mail-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
;;  '(mail-kill-buffer-on-exit t)
;;  '(make-backup-files t)
;;  '(message-draft-headers
;;    '(From References Date))
;;  '(message-kill-buffer-on-exit t)
;;  '(message-required-headers
;;    '(From
;;      (optional . References)))
;;  '(message-send-mail-function 'message-send-mail-with-sendmail)
;;  '(mml-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
;;  '(only-global-abbrevs t)
;;  '(org-M-RET-may-split-line
;;    '((item . t)))
;;  '(org-activate-links
;;    '(bracket plain radio tag date footnote))
;;  '(org-archive-location "archive/%s_archive::")
;;  '(org-ascii-headline-spacing
;;    '(1 . 1))
;;  '(org-ascii-table-use-ascii-art t)
;;  '(org-blank-before-new-entry
;;    '((heading)
;;      (plain-list-item . auto)))
;;  '(org-bullets-face-name '\"Lucida\ Sans\ Typeriter\")
;;  '(org-catch-invisible-edits 'error)
;;  '(org-clock-auto-clock-resolution t)
;;  '(org-clock-idle-time 5)
;;  '(org-clock-in-resume t)
;;  '(org-clock-persist-query-resume nil)
;;  '(org-clock-report-include-clocking-task t)
;;  '(org-clocktable-defaults
;;    '(:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil))
;;  '(org-closed-string "COMPLETED:")
;;  '(org-ctrl-k-protect-subtree t)
;;  '(org-custom-properties
;;    '(">"))
;;  '(org-default-notes-file "~/Dropbox/writing/notationaldata/notes.txt")
;;  '(org-drawers
;;    '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "SOURCE"))
;;  '(org-edit-src-content-indentation 4)
;;  '(org-ellipsis 'org-warning)
;;  '(org-enable-fixed-width-editor nil)
;;  '(org-enforce-todo-checkbox-dependencies t)
;;  '(org-enforce-todo-dependencies t)
;;  '(org-export-allow-bind-keywords t)
;;  '(org-export-blocks-witheld
;;    '(hidden))
;;  '(org-export-html-inline-image-extensions
;;    '("png" "jpeg" "jpg" "gif" "svg" "tif" "gif"))
;;  '(org-export-html-style-include-default t)
;;  '(org-export-latex-date-format "%d %B %Y.")
;;  '(org-export-latex-emphasis-alist
;;    '(("*" "\\emph{%s}" nil)
;;      ("/" "\\textit{%s}" nil)
;;      ("_" "\\underline{%s}" nil)
;;      ("+" "\\st{%s}" nil)
;;      ("=" "\\verb" t)
;;      ("~" "\\verb" t)))
;;  '(org-export-latex-image-default-option "width=20.5cm")
;;  '(org-export-latex-verbatim-wrap
;;    '("\\begin{quote}" . "\\end{quote}"))
;;  '(org-export-preserve-breaks t)
;;  '(org-export-time-stamp-file nil)
;;  '(org-export-with-clocks t)
;;  '(org-export-with-drawers t)
;;  '(org-export-with-section-numbers nil)
;;  '(org-export-with-toc nil)
;;  '(org-extend-today-until 8)
;;  '(org-fontify-done-headline t)
;;  '(org-fontify-emphasized-text t)
;;  '(org-footnote-define-inline t)
;;  '(org-footnote-section "Footnotes")
;;  '(org-footnote-tag-for-non-org-mode-files "Footnotes:")
;;  '(org-hidden-keywords
;;    '(author title)
;;    nil nil "#+BEGIN_QUOTE")
;;  '(org-hide-block-startup nil)
;;  '(org-hide-emphasis-markers t)
;;  '(org-hide-leading-stars t)
;;  '(org-html-container-element "div")
;;  '(org-html-footnotes-section "<div id=\"footnotes\">\n<h2 class=\"footnotes\">%s </h2>\n<div id=\"footnote\">\n%s\n</div>\n</div>")
;;  '(org-html-head-include-default-style nil)
;;  '(org-html-head-include-scripts nil)
;;  '(org-html-html5-fancy t)
;;  '(org-html-postamble nil)
;;  '(org-html-text-markup-alist
;;    '((bold . "<strong>%s</strong>")
;;      (code . "<blockquote>%s</blockquote>")
;;      (italic . "<em>%s</em>")
;;      (strike-through . "<del>%s</del>")
;;      (underline . "<span class=\"underline\">%s</span>")
;;      (verbatim . "<code>%s</code>")))
;;  '(org-html-toplevel-hlevel 2)
;;  '(org-indent-indentation-per-level 2)
;;  '(org-indent-mode-turns-off-org-adapt-indentation nil)
;;  '(org-indent-mode-turns-on-hiding-stars nil)
;;  '(org-insert-mode-line-in-empty-file t)
;;  '(org-latex-text-markup-alist
;;    '((bold . "\\textbf{%s}")
;;      (code . verb)
;;      (italic . "\\textit{%s}")
;;      (strike-through . "\\sout{%s}")
;;      (underline . "\\uline{%s}")))
;;  '(org-latex-toc-command "\\tableofcontents\n\\newpage\n")
;;  '(org-list-allow-alphabetical t)
;;  '(org-list-indent-offset 3)
;;  '(org-log-done nil)
;;  '(org-log-note-clock-out nil)
;;  '(org-log-refile nil)
;;  '(org-mac-Skim-highlight-selection-p t)
;;  '(org-mac-grab-Firefox+Vimperator-p nil)
;;  '(org-mac-grab-Firefox-app-p nil)
;;  '(org-mac-grab-Mail-app-p nil)
;;  '(org-mac-grab-Safari-app-p nil)
;;  '(org-mac-grab-Together-app-p nil)
;;  '(org-n-level-faces 9)
;;  '(org-odd-levels-only nil)
;;  '(org-pomodoro-format "Pomodoro: %s")
;;  '(org-pomodoro-killed-sound "~/sounds/autodestructsequencearmed_ep.mp3")
;;  '(org-pomodoro-length 50)
;;  '(org-pomodoro-long-break-format "Long Break: %s")
;;  '(org-pomodoro-long-break-sound "~/sounds/tng-computer-programcomplete.mp3")
;;  '(org-pomodoro-play-ticking-sounds nil)
;;  '(org-pomodoro-short-break-format "Short Break: %s")
;;  '(org-pomodoro-short-break-sound "~/sounds/tng-picard-engage.mp3")
;;  '(org-pomodoro-sound "~/sounds/large-applause.mp3")
;;  '(org-pomodoro-ticking-sound "~/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/com.taptanium.thunderstorm.DreamQuest_preview.m4a")
;;  '(org-priority-faces nil)
;;  '(org-provide-checkbox-statistics t)
;;  '(org-replace-disputed-keys nil)
;;  '(org-return-follows-link t)
;;  '(org-special-ctrl-a/e t)
;;  '(org-src-preserve-indentation t)
;;  '(org-startup-align-all-tables t)
;;  '(org-startup-folded nil)
;;  '(org-startup-indented t)
;;  '(org-support-shift-select 'always)
;;  '(org-time-clocksum-use-effort-durations t)
;;  '(org-export-date-timestamp-format "%Y%m%d %I:%M%p")
;;  '(org-use-speed-commands t)
;;  '(org-yank-adjusted-subtrees t)
;;  '(org2blog/wp-confirm-post nil)
;;  '(org2blog/wp-default-categories
;;    '("inspiration" "personal growth" "miscellany"))
;;  '(org2blog/wp-keep-new-lines t)
;;  '(org2blog/wp-show-post-in-browser t)
;;  '(org2blog/wp-use-tags-as-categories t)
;;  '(osx-browse-prefer-background nil)
;;  '(osx-browse-prefer-browser "com.google.Chrome")
;;  '(osx-browse-prefer-new-window t)
;;  '(pomodoro-break-time 10)
;;  '(pomodoro-work-time 50)
;;  '(reb-re-syntax 'string)
;;  '(recentf-exclude
;;    '(".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "elpa" ".bmk" ".jabber" "helm" "Calendar"))
;;  '(recentf-max-menu-items 100)
;;  '(recentf-max-saved-items 999)
;;  '(safe-local-variable-values
;;    '((eval when
;;            (fboundp 'rainbow-mode)
;;            (rainbow-mode 1))))
;;  '(send-mail-function 'sendmail-send-it)
;;  '(standard-indent 3)
;;  '(tooltip-mode nil)
;;  '(tramp-default-method "ssh")
;;  '(undo-limit 800000)
;;  '(user-full-name "Jay Dixit")
;;  '(user-mail-address "dixit@aya.yale.edu")
;;  '(visual-line-mode nil t)
;;  '(cua-mode nil))

(require 'key-chord)
(key-chord-mode 1)

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

(add-to-list 'custom-theme-load-path "~/Dropbox/emacs/prelude/personal/sublime-themes-jay/")

; (add-to-list 'load-path "~/.emacs-live/packs/stable/colour-pack/lib/color-theme/")


;; (set-cursor-color "yellow")

;; (set-cursor-color "red")

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

(defun ac-expand-ispell-word ()
  (interactive)
  (let ((ac-sources '(ac-source-ispell)))
    (call-interactively 'ac-start)))

(define-key global-map (kbd "s-/ s") 'ac-expand-ispell-word)

(ac-flyspell-workaround)

(load-file "~/Library/Preferences/Aquamacs Emacs/ac-ispell.el")
;; Completion words longer than 4 characters

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

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

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

(setq org-complegtion-use-ido t)
(setq confirm-nonexistent-file-or-buffer nil)
(ido-everywhere 1)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
(setq ido-use-filename-at-point t) ;; prefer file names near point
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".org" ".txt" ".md"  ".emacs" ".el"))

(setq org-refile-use-outline-path t)

(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 2)

(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-completion-use-ido t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

(require 'ido-hacks)

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

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)

(defun ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))

(setq
 ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "html" "*.mm" "Icon*" "*gz" "*ido.hist" "*archive*" "ics")))

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

(setq set-mark-command-repeat-pop t)

(define-key org-mode-map
  (kbd "RET")
  (lambda()
    (interactive)
    (if (region-active-p)
        (delete-region (region-beginning)
                       (region-end))
      (call-interactively 'org-return))))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

(add-hook 'desktop-after-read-hook 'calendar)

(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p)))
  (server-start))

(require 'openwith)
'(openwith-associations (quote (("\\.skim\\'" "open" (file)) ("\\.pdf\\'" "open" (file)))))
(openwith-mode t)

(setq bookmark-default-file  (concat user-emacs-directory "bookmarks"))

(defun  boss-mode ()
  (interactive)
  (global-hl-line-mode -1)
  (hl-line-mode -1))

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
;; helm-imenu

(global-set-key (kbd "C-x C-i") 'ido-imenu)
;; (add-hook 'my-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook 'imenu-add-menubar-index)

(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "I love you.") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

(eval-after-load 'helm-grep
  '(setq helm-grep-default-command helm-grep-default-recurse-command))

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

(add-hook 'dired-mode-hook 'hl-line-mode)

(require 'dired-x)

(setq-default dired-omit-files-p t) ; Buffer-local variable

(setq-default dired-omit-mode t)

(define-key dired-mode-map (kbd "C-o") 'dired-omit-mode)

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Note: If you are using Dired Omit Mode with dired+, remember to put the config of Dired Omit Mode before loading (require) dired+ since some feature of dired+ use the config from Dired Omit Mode (for example for displaying the file names).

(defun tmtxt/dired-do-shell-mac-open ()
  (interactive)
  (save-window-excursion
    (let ((files (dired-get-marked-files nil current-prefix-arg))
          command)
      ;; the open command
      (setq command "open ")
      (dolist (file files)
        (setq command (concat command (shell-quote-argument file) " ")))
      (message command)
      ;; execute the command
      (async-shell-command command))))
(define-key dired-mode-map (kbd "s-o") 'tmtxt/dired-do-shell-mac-open)

(defun dired-open-current-directory-in-finder ()
  "Open the current directory in Finder"
  (interactive)
  (save-window-excursion
    (dired-do-async-shell-command
     "open .")))

(define-key dired-mode-map (kbd "s-O") 'dired-open-current-directory-in-finder)

;; https://truongtx.me/2013/04/25/dired-as-default-file-manager-5-customize-ls-command/

;; look at this: https://truongtx.me/2013/12/22/emacs-search-for-text-occurences-with-grep/

(defun buffer-stack-filter-regexp (buffer)
  "Non-nil if buffer is in buffer-stack-tracked."
  (not (or (string-match "Help\\|minibuf\\|org2blog\\|echo\\|conversion\\|server\\|Messages\\|tex\\|Output\\|temp\\|autoload\\|Customize\\|address\\|clock\\|Backtrace\\|Completions\\|grep\\|Calendar\\|archive\\||Compile\\|tramp\\|accountability\\|helm\\|Alerts\\|Minibuf\\|Agenda\\|Echo\\|gnugol\\|RNC\\|widget\\|acct\\|melpa\\|fontification\\|Helm\\|daycolate\\|*Warnings*\\|*tags*\\|*gnugol*\\|*guide-key*" (buffer-name buffer))
           (member buffer buffer-stack-untracked))))
(setq buffer-stack-filter 'buffer-stack-filter-regexp)

(add-to-list 'recentf-exclude "\\ido.last\\'")
(add-to-list 'recentf-exclude "\\ido")
(add-to-list 'recentf-exclude "\\recent-addresses\\'")
(add-to-list 'recentf-exclude "org-clock-save.el")
(add-to-list 'recentf-exclude "*message*")
(add-to-list 'recentf-exclude ".tex\\")
(add-to-list 'recentf-exclude "html")
(add-to-list 'recentf-exclude "gz")
(add-to-list 'recentf-exclude "System")
(add-to-list 'recentf-exclude "Applications")
(add-to-list 'recentf-exclude "bookmark")
(add-to-list 'recentf-exclude "750words")
(add-to-list 'recentf-exclude "Calendar")

(add-to-list 'recentf-exclude ".tex")
(add-to-list 'recentf-exclude "helm")
(add-to-list 'recentf-exclude "\\ido*")
(add-to-list 'recentf-exclude "archive")
(add-to-list 'recentf-exclude "ics")
(add-to-list 'recentf-exclude "agenda")
(add-to-list 'recentf-exclude "gnugol")
(add-to-list 'recentf-exclude "PDF")
(add-to-list 'recentf-exclude "koma")
(add-to-list 'recentf-exclude "LaTeX")

(defun replace-garbage-chars ()
  "Replace goofy MS and other garbage characters with latin1 equivalents."
  (interactive)
  (save-excursion                       ;save the current point
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
    (replace-string "" "`" nil (point-min) (point-max)) ; \221
    (replace-string "" "'" nil (point-min) (point-max)) ; \222
    (replace-string "" "``" nil (point-min) (point-max))
    (replace-string "" "'" nil (point-min) (point-max))
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

(setq wc-modeline-format "[Words: %tw, Chars: %tc]")
(require 'wc-mode)

(require 'org-serenity-mode)
(defun serenity-mode ()
  "serenity"
  (interactive)
  (setq org-bullets-bullet-list (quote ("  ")))
  (org-serenity-mode)  
  (org-bullets-mode)
  (org-bullets-mode)
  )

(require 'ls-lisp)
(setq ls-lisp-ignore-case 't)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(defun org-mac-chrome-insert-frontmost-url-with-quotes ()
  "with quotes"
  (interactive)
  (insert "\"")
  (org-mac-chrome-insert-frontmost-url)
  (insert ",\"")
  )

(defun web-research ()
  (interactive)
  (insert "#+BEGIN_QUOTE\n")
  (let ((p (point)))
    (insert "\n#+END_QUOTE\nSource: ")
    (org-mac-chrome-insert-frontmost-url)
    (goto-char p))
  (pasteboard-paste)
  (next-line)
  (next-line)
  (next-line)
    (insert "\n"))

(defun web-research-quotes ()
  (interactive)
  (insert "\"")
    (org-mac-chrome-insert-frontmost-url)
       (insert "\,\" "))

(defun kdm/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (setq cmd "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org")
  (kill-new (shell-command-to-string cmd))
  (yank))

(defun conditionally-disable-abbrev ()
  ""
  (if (string-match "smex-" (format "%s" this-command))
      (abbrev-mode -1)))

(define-skeleton my-orgfootnote "Docstring." nil
  "[fn:: " _ "] ")


(define-skeleton fws "Docstring." nil
  "# ###################################################################################\n#+HTML: [full_width_section bg_pos='Left Top' parallax_bg='true' bg_repeat='No-Repeat' text_color='Light' top_padding=' bottom_padding=' background_color='#000' image_url='" _ "']\n\n#+HTML: <H1></H1>\n\n#+HTML: [/full_width_section]\n# ####################################################################################\n\n<BR>\n")

(define-skeleton fwh "Docstring." nil
  "# ###################################################################################\n#+HTML: [full_width_section bg_pos='Left Top' parallax_bg='true' bg_repeat='No-Repeat' text_color='Light' top_padding=' bottom_padding='200' background_color='#000' image_url='" _ "']\n\n#+HTML: <H1 class='fwh'></H1>\n\n#+HTML: [/full_width_section]\n# ####################################################################################\n\n")


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

(defun org-day ()
  "foo"
  (interactive)
  (insert (format-time-string "[%H:%M]"))
  )

(defun jd-org-today ()
  "insert a new heading with today's date"
  (interactive)
  (org-insert-subheading ())
  (insert "accountability ")
  (org-insert-time-stamp (current-time))
  (insert " [0%]\n")
  )

(defun jd-clock-in ()
  "insert a new heading with today's date, and then clock in"
  (interactive)
  (org-insert-subheading ())
  (org-insert-time-stamp (current-time))
  (org-clock-in)
  (next-line)
  (next-line)
  )

(require 'discover)

(discover-add-context-menu
 :context-menu (assq 'isearch discover-context-menus)
 :mode nil
 :mode-hook nil
 :bind "C-c s")

(global-discover-mode 1)

(load "makey") 

(discover-add-context-menu
 :context-menu '(isearch
              (description "Isearch, occur and highlighting")
              (lisp-switches
               ("-cf" "Case should fold search" case-fold-search t nil))
              (lisp-arguments
               ("=l" "context lines to show (occur)"
                "list-matching-lines-default-context-lines"
                (lambda (dummy) (interactive) (read-number "Number of context lines to show: "))))
              (actions
               ("Isearch"
                ("_" "isearch forward symbol" isearch-forward-symbol)
                ("w" "isearch forward word" isearch-forward-word))
               ("Occur"
                ("o" "occur" occur))
               ("More"
                ("h" "highlighters ..." makey-key-mode-popup-isearch-highlight))))
 :bind "M-s")

(setq ac-auto-start 3)
(setq company-minimum-prefix-length 3)

(defun endless/config-prose-completion ()
  "Make auto-complete less agressive in this buffer."
  (setq-local company-minimum-prefix-length 6)
  (setq-local ac-auto-start 6))

(add-hook 'org-mode-hook
  #'endless/config-prose-completion)

(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to text-mode."
  (let ((f "\\(%s\\)\\(%s\\)")
        (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space))))
        (replace-match rp nil nil nil 1))))

(defun endless/capitalize ()
  "Capitalize region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'capitalize-word)))

(defun endless/downcase ()
  "Downcase region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'downcase-word)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'upcase-word)))

(global-set-key "\M-c" 'endless/capitalize)
(global-set-key "\M-l" 'endless/downcase)
(global-set-key "\M-u" 'endless/upcase)

(defun endless/upgrade ()
  "Upgrade all packages, no questions asked."
  (interactive)
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)))

(defvar lawlist-movement-syntax-table
  (let ((st (make-syntax-table)))
    ;; ` default = punctuation
    ;;  default = punctuation
    ;; , default = punctuation
    ;; ; default = punctuation
    (modify-syntax-entry ?{ "." st)  ;; { = punctuation
    (modify-syntax-entry ?} "." st)  ;; } = punctuation
    (modify-syntax-entry ?\" "." st) ;; " = punctuation
    (modify-syntax-entry ?\\ "_" st) ;; \ = symbol
    (modify-syntax-entry ?\$ "_" st) ;; $ = symbol
    (modify-syntax-entry ?\% "_" st) ;; % = symbol
    st)
  "Syntax table used while executing custom movement functions.")

(defun lawlist-forward-entity ()
"http://stackoverflow.com/q/18675201/2112489"
(interactive "^")
  (with-syntax-table lawlist-movement-syntax-table
    (cond
      ((eolp)
        (forward-char))
      ((and
          (save-excursion (< 0 (skip-chars-forward " \t")))
          (not (region-active-p)))
        (skip-chars-forward " \t"))
      ((and
          (save-excursion (< 0 (skip-chars-forward " \t")))
          (region-active-p))
        (skip-chars-forward " \t")
        (cond
          ((save-excursion (< 0 (skip-syntax-forward "w")))
            (skip-syntax-forward "w"))
          ((save-excursion (< 0 (skip-syntax-forward ".")))
            (skip-syntax-forward "."))
          ((save-excursion (< 0 (skip-syntax-forward "_()")))
            (skip-syntax-forward "_()"))))
      ((save-excursion (< 0 (skip-syntax-forward "w")))
        (skip-syntax-forward "w")
        (if (and
              (not (region-active-p))
              (save-excursion (< 0 (skip-chars-forward " \t"))))
          (skip-chars-forward " \t")))
      ((save-excursion (< 0 (skip-syntax-forward ".")))
        (skip-syntax-forward ".")
        (if (and
              (not (region-active-p))
              (save-excursion (< 0 (skip-chars-forward " \t"))))
          (skip-chars-forward " \t")))
      ((save-excursion (< 0 (skip-syntax-forward "_()")))
        (skip-syntax-forward "_()")
        (if (and
              (not (region-active-p))
              (save-excursion (< 0 (skip-chars-forward " \t"))))
          (skip-chars-forward " \t"))))))

(defun lawlist-backward-entity ()
"http://stackoverflow.com/q/18675201/2112489"
(interactive "^")
  (with-syntax-table lawlist-movement-syntax-table
    (cond
      ((bolp)
        (backward-char))
      ((save-excursion (> 0 (skip-chars-backward " \t")) (bolp))
        (skip-chars-backward " \t"))
      ((save-excursion (> 0 (skip-chars-backward " \t")) (> 0 (skip-syntax-backward "w")))
        (skip-chars-backward " \t")
        (skip-syntax-backward "w"))
      ((save-excursion (> 0 (skip-syntax-backward "w")))
        (skip-syntax-backward "w"))
      ((save-excursion (> 0 (skip-syntax-backward ".")))
        (skip-syntax-backward "."))
      ((save-excursion (> 0 (skip-chars-backward " \t")) (> 0 (skip-syntax-backward ".")))
        (skip-chars-backward " \t")
        (skip-syntax-backward "."))
      ((save-excursion (> 0 (skip-syntax-backward "_()")))
        (skip-syntax-backward "_()"))
      ((save-excursion (> 0 (skip-chars-backward " \t")) (> 0 (skip-syntax-backward "_()")))
        (skip-chars-backward " \t")
        (skip-syntax-backward "_()")))))

(define-key global-map [M-s-right] 'lawlist-forward-entity)
(define-key global-map [M-s-left] 'lawlist-backward-entity)

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

(defun move-region-to-other-window (start end)
  "Move selected text to other window"
  (interactive "r")
  (if (use-region-p) 
      (let ((count (count-words-region start end)))
        (save-excursion
          (kill-region start end)
          (other-window 1)   
          (yank)
          (newline))
        (other-window -1)     
        (message "Moved %s words" count))
    (message "No region selected")))

(require 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)

(defun remove-link ()
    "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
        (description (if (match-end 3) 
                 (org-match-string-no-properties 3)
                 (org-match-string-no-properties 1))))
    (apply 'delete-region remove)
    (insert description))))

(defvar org-refile-region-format "\n%s\n")

(defvar org-refile-region-position 'top
  "Where to refile a region. Use 'bottom to refile at the
end of the subtree. ")

(defun org-refile-region (beg end copy)
  "Refile the active region.
If no region is active, refile the current paragraph.
With prefix arg C-u, copy region instad of killing it."
  (interactive "r\nP")
  ;; mark paragraph if no region is set
  (unless (use-region-p)
    (setq beg (save-excursion
                (backward-paragraph)
                (skip-chars-forward "\n\t ")
                (point))
          end (save-excursion
                (forward-paragraph)
                (skip-chars-backward "\n\t ")
                (point))))
  (let* ((target (save-excursion (org-refile-get-location)))
         (file (nth 1 target))
         (pos (nth 3 target))
         (text (buffer-substring-no-properties beg end)))
    (unless copy (kill-region beg end))
    (deactivate-mark)
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char pos)
        (if (eql org-refile-region-position 'bottom)
            (org-end-of-subtree)
          (org-end-of-meta-data-and-drawers))
        (insert (format org-refile-region-format text))))))


(defun my-org-files-list ()
  (mapcar (lambda (buffer)
            (buffer-file-name buffer))
          (org-buffer-list 'files t)))


(setq org-refile-targets '((my-org-files-list :maxlevel . 4)))

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

(set-face-attribute 'default nil :font "Inconsolata" :height 180)
(medium-type)

(require 'engine-mode)
(engine-mode t)

;; (defengine google  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"  "g")

;; (require 'gnugol)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(require 'point-stack)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("s-m" "C-x 4"))
(guide-key-mode 1)  ; Enable guide-key-mode
(setq guide-key/guide-key-sequence '("C-x"))
(setq guide-key/recursive-key-sequence-flag t)

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

;; (add-to-list 'load-path "~/Dropbox/elisp/bbdb/lisp")
;; (require 'bbdb) ;; (3)
;; (bbdb-initialize 'gnus 'message)   ;; (4)
;; (setq bbdb-north-american-phone-numbers-p nil)   ;; (5)

(global-set-key (kbd "M-]") 'outline-next-visible-heading)
(global-set-key (kbd "M-[") 'outline-previous-visible-heading)
(global-set-key (kbd "M-1") 'auto-capitalize-mode)
;; (global-set-key (kbd "s-u") 'dired-single)


(define-key global-map (kbd "<C-wheel-up>") (lambda ()
                                              (interactive)
                                              (scroll-up-command)))
(define-key global-map (kbd "<C-wheel-down>") (lambda ()
                                               (interactive)
                                               (scroll-down-command)))

(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)

(global-set-key '[(f5)] 'point-stack-push)
(global-set-key '[(f6)] 'point-stack-pop)
(global-set-key '[(f7)] 'point-stack-forward-stack-pop)
(global-set-key '[(f8)] 'search-open-buffers)

(global-set-key (kbd "C-h") 'delete-backward-char)

(global-set-key (kbd "M-h") 'help-command)

;; (define-key key-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
;; (define-key key-minor-mode-map (kbd "C-x C-f") 'ido-find-file-in-dir)

(global-set-key (kbd "C-c h") 'helm-mini)

(cl-dolist (map '(message-mode-map orgstruct-mode-map))
  (cl-dolist (key '("<M-S-left>" "<M-S-right>" "<M-S-up>" "<M-S-down>" "<M-left>" "<M-right>" "<M-up>" "<M-down>"))
    (define-key (eval map) (kbd key) nil)))

(global-set-key "\C-ce" 'eval-buffer)
(global-set-key "\C-cr" 'eval-region)
(global-set-key (kbd "`") 'flyspell-auto-correct-word)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-y") 'redo)

(defvar gnuemacs-flag (string-match "GNU" (emacs-version)))
(defvar aquamacs-flag (string-match "Aquamacs" (emacs-version)))

(defun define-hyper-key (key fun)
  (cond
   (aquamacs-flag
    (define-key osx-key-mode-map (kbd (concat "A-" key)) fun))
   (gnuemacs-flag
    (define-key key-minor-mode-map (kbd (concat "s-" key)) fun))))

(define-hyper-key "h" 'replace-string)
(define-hyper-key "i" 'org-mac-chrome-insert-frontmost-url)
(define-hyper-key "\\" 'visit-most-recent-file)
(define-hyper-key "f" 'isearch-forward)
;; (define-hyper-key "r" 'xsteve-ido-choose-from-recentf)
(define-hyper-key "R" 'helm-projectile-recentf)
(define-hyper-key "r" 'helm-mini)
(define-hyper-key "t" 'new-buffer)
(define-hyper-key "T" 'org-new-scratch-buffer)
(define-hyper-key "g" 'isearch-repeat-forward)
(define-hyper-key "k" 'ido-kill-buffer)
(define-hyper-key "K" 'org-mac-chrome-insert-frontmost-url-with-quotes)
(define-hyper-key "d" 'org-todo)
(define-hyper-key "L" 'org-mac-chrome-insert-frontmost-url)
(define-hyper-key "S" 'org-mac-skim-insert-page)
(define-hyper-key "b" 'org-narrow-to-subtree)
(define-hyper-key "a" 'mark-whole-buffer) ; select all
(define-hyper-key "w" 'delete-window) ; close
(define-hyper-key "`" 'other-window)
(define-hyper-key "s" 'jay/save-some-buffers ) ; save all

(define-hyper-key "4" 'clone-indirect-buffer-other-window)
(define-hyper-key "5" 'point-stack-push)
(define-hyper-key "6" 'point-stack-pop)
(define-hyper-key "7" 'point-stack-forward-stack-pop)
(define-hyper-key "8" 'search-open-buffers)
(define-hyper-key "B" 'clone-indirect-buffer-other-window)
(define-hyper-key "o" 'eval-buffer)
(define-hyper-key "F" 'locate)
(define-hyper-key "(" 'org-velocity)
(define-hyper-key "[" 'org-backward-heading-same-level)
(define-hyper-key "]" 'org-forward-heading-same-level)

(define-hyper-key "m a" 'org-agenda) 
(define-hyper-key "m j" 'helm-imenu-anywhere) 
(define-hyper-key ";" 'ido-goto-symbol)
(define-hyper-key "D" 'diredp-dired-recent-dirs)

(define-hyper-key "m cy" 'cyberpunk-jay) 
(define-hyper-key "m cl" 'cyberpunk-large) 
(define-hyper-key "m cw" 'cyberpunk-writeroom) 
(define-hyper-key "m wb" 'whiteboard) 
(define-hyper-key "m sl" 'solarized-light)
(define-hyper-key "m sd" 'solarized-dark) 
(define-hyper-key "m ri" 'ritchie) 
(define-hyper-key "m sp" 'spolsky) 
(define-hyper-key "m wr" 'writeroom-mode) 
(define-hyper-key "m wf" 'workflowy-mode) 
(define-hyper-key "m st" 'small-type) 
(define-hyper-key "m mp" 'morning-pages) 
(define-hyper-key "m rf" 'prelude-rename-file-and-buffer) 
(define-hyper-key "m lt" 'large-type) 
(define-hyper-key "m mt" 'medium-type) 
(define-hyper-key "m df" 'delete-file-and-buffer) 
(define-hyper-key "m rf" 'rename-file-and-buffer)

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

;; (global-set-key "\C-cw" 'my-isearch-word-at-point)

(require 'cl)


(defcustom search-open-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS" "Preferences" "Backtrace" "Messages" "Custom" "scratch") eos)))
  "Files to ignore when searching buffers via \\[search-open-buffers]."
  :type 'editable-list)

(require 'grep)

(defun search-open-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP. With
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

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defun isearch-from-buffer-start ()
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (isearch-forward))

(require 'helm-config)
(helm-mode t)
(helm-adaptative-mode t)

(require 'helm-swoop)
; (global-set-key (kbd "M-i") (lambda() (interactive) (helm-swoop :$query nil)))

(setq helm-swoop-pre-input-function
      (lambda () nil))

(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

(setq helm-multi-swoop-edit-save t)

(setq helm-swoop-split-with-multiple-windows nil)

(setq helm-swoop-split-direction 'split-window-vertically)

(setq helm-swoop-speed-or-color nil)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "M-s /")   #'helm-multi-swoop)

(setq helm-ff-transformer-show-only-basename nil
      helm-adaptive-history-file             "~/.emacs.d/data/helm-history"
      helm-yank-symbol-first                 t
      helm-move-to-line-cycle-in-source      t
      helm-buffers-fuzzy-matching            t
      helm-ff-auto-update-initial-value      t)

(autoload 'helm-descbinds      "helm-descbinds" t)
(autoload 'helm-eshell-history "helm-eshell"    t)
(autoload 'helm-esh-pcomplete  "helm-eshell"    t)

(global-set-key (kbd "M-h a")    #'helm-apropos)
(global-set-key (kbd "M-h i")    #'helm-info-emacs)
(global-set-key (kbd "M-h b")    #'helm-descbinds)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "TAB")     #'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))


(global-set-key (kbd "C-x c!")   #'helm-calcul-expression)
(global-set-key (kbd "C-x c:")   #'helm-eval-expression-with-eldoc)
(define-key helm-map (kbd "M-o") #'helm-previous-source)

(global-set-key (kbd "M-s s")   #'helm-ag)

(require 'helm-projectile)
(setq helm-projectile-sources-list (cons 'helm-source-projectile-files-list
                                         (remove 'helm-source-projectile-files-list 
                                              helm-projectile-sources-list)))
(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c p /")
  #'(lambda ()
      (interactive)
      (helm-ag (projectile-project-root))))

(require 'palimpsest)
(palimpsest-mode 1)

(font-lock-add-keywords
 'org-mode '(("^\\(:+\\) " 1 (compose-region (match-beginning 1) (match-end 1) ?> ) nil)))

(defun replace-word (tosearch toreplace)
  (interactive "sSearch for word: \nsReplace with: ")
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (re-search-forward (concat "\\b" tosearch "\\b") nil t)
        (setq count (1+ count))
        (replace-match toreplace 'fixedcase 'literal))
      (message "Replaced %s match(es)" count))))

(setq auto-capitalize-predicate
      (lambda () (not (looking-back "\\([Ee]\\.g\\|[Uu]\\.S\\|[Ii]\\.e\\|\\.\\.\\)\\.[^.]*" (- (point) 20)))))
