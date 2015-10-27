(add-to-list 'load-path "plus-/Users/jay/Dropbox/emacs/prelude/elpa/org-contrib-20150810/")
(require 'org)
(require 'org-mime)

(require 'package)

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/" ))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(add-to-list 'load-path "~/Dropbox/emacs/prelude/personal/")

(require 'org)

;; (require 'org-bullets)
(require 'ox-latex)
(require 'ox-org)
  (require 'ox-html)

; (require 'org-fstree)

;; (require 'use-package)

(defun add-word-to-personal-dictionary ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(add-hook 'org-mode-hook 'turn-on-flyspell)

(setq mouse-highlight nil)
(setq-local cursor-in-non-selected-windows nil)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq ring-bell-function
      (lambda ()
  (unless (memq this-command
          '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
    (ding))))

;; (setq ring-bell-function (lambda () (play-sound-file "~/sounds/InkSoundStroke3.mp3")))

;; turn off alarms completely
(setq ring-bell-function 'ignore)

(defun  incarnadine-cursor ()
  (interactive)
  (set-cursor-color "red")
  (setq default-frame-alist
  '((cursor-color . "red"))) 
  )

(defun  magenta-cursor ()
  (interactive)
  (set-cursor-color "#DC8CC3")
  (setq default-frame-alist
  '((cursor-color . "#DC8CC3"))) 
  )

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


;; (add-hook 'org-after-todo-state-change-hook 'my-org-after-todo)
;; (defun my-org-after-todo () (play-sound-file "~/sounds/InkSoundStroke3.mp3"))

(setq sentence-end-double-space nil)

(global-auto-revert-mode 1)

(delete-selection-mode 1)

(autopair-mode -1)
(setq buffer-save-without-query nil)

(setq locate-command "mdfind")

(setq auto-mode-alist (cons '("\\.txt" . org-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tmode" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.msg" . message-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.abbrev_defs\\'" . org-mode))
(add-to-list 'auto-mode-alist '("README$" . org-mode))
(add-to-list 'auto-mode-alist '("shared-functions$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("gnu-emacs-startup$" . emacs-lisp-mode))
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
(add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))

; (setq org-use-property-inheritance t)
(setq org-ctrl-k-protect-subtree t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-fontify-quote-and-verse-blocks t)
;; blank lines before new headings
(setq org-blank-before-new-entry
      '((heading . always)
       (plain-list-item . nil)))
(setq org-return-follows-link t)

;; leave an empty line between folded subtrees
(setq org-cycle-separator-lines 1)

'(org-modules (quote (org-info org-jsinfo org-pomodoro org-mac-link org-mime )))

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (
                                      ("k" . org-kill-note-or-show-branches)
                                      ("q" . bh/show-org-agenda)
                                      ("h" . org-agenda-schedule)
                                      ("d" . org-deadline)
                                      ("w" . org-refile)
                                      ("z" . org-add-note)
                                      ("m" . (lambda nil (interactive) (org-todo "MISSED")))

                                      ("A" . org-archive-subtree-default-with-confirmation)
                                      ("J" . org-clock-goto)
                                      ("Z" . ignore))))

(setq org-export-with-smart-quotes t) 

(setq org-html-head "<link rel='stylesheet' type='text/css' href='http://dixit.ca/css/email.css'>")
(setq org-export-time-stamp-file nil)
(setq org-export-with-clocks t)
(setq org-export-with-drawers t)
(setq org-export-with-section-numbers nil)
(setq org-export-with-timestamps (quote active))
(setq org-export-with-toc nil)

 (setq org-export-date-timestamp-format "%Y%m%d %I:%M%p")
 (setq org-export-html-inline-image-extensions (quote ("png" "jpeg" "jpg" "gif" "svg" "tif" "gif")))
;; (setq org-export-html-style-include-default t)
 (setq org-export-latex-date-format "%d %B %Y.")
 (setq org-export-latex-emphasis-alist (quote (("*" "\\emph{%s}" nil) ("/" "\\textit{%s}" nil) ("_" "\\underline{%s}" nil) ("+" "\\st{%s}" nil) ("=" "\\verb" t) ("~" "\\verb" t))))
 (setq org-export-latex-verbatim-wrap (quote ("\\begin{quote}" . "\\end{quote}")))
 (setq org-export-with-clocks t)
 (setq org-export-with-drawers t)
 (setq org-export-with-section-numbers nil) 
(setq org-export-with-planning nil)
(setq org-export-allow-bind-keywords t)
;; (setq org-export-blocks-witheld (quote (hidden)) t) 
(setq org-export-date-timestamp-format "%Y%m%d %I:%M%p")
(setq org-export-latex-emphasis-alist (quote    (("*" "\\emph{%s}" nil)
     ("/" "\\textit{%s}" nil)
     ("_" "\\underline{%s}" nil)
     ("+" "\\st{%s}" nil)
     ("=" "\\verb" t)
     ("~" "\\verb" t))))

(setq org-html-footnotes-section
   "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s </h2>
<div id=\"footnote\">
%s
</div>
</div>")

(setq org-html-text-markup-alist (quote    ((bold . "<strong>%s</strong>")
     (code . "<blockquote>%s</blockquote>")
     (italic . "<em>%s</em>")
     (strike-through . "<del>%s</del>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>"))))

(setq org-latex-text-markup-alist (quote    ((bold . "\\textbf{%s}")
     (code . verb)
     (italic . "\\textit{%s}")
     (strike-through . "\\sout{%s}")
     (underline . "\\uline{%s}")
     ;; (verbatim . protectedtext)
     )))

(setq org-latex-toc-command "\\tableofcontents
\\newpage
")

(setq safe-local-variable-values (quote    ((eval when
     (fboundp
      (quote rainbow-mode))
     (rainbow-mode 1)))))

(setq org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s </h2>
<div id=\"footnote\">
%s
</div>
</div>") 

(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-html-html5-fancy t)
(setq org-html-metadata-timestamp-format "%m-%d %a %H:%M")
(setq org-html-postamble nil)
(setq org-html-text-markup-alist
   (quote
    ((bold . "<strong>%s</strong>")
     (code . "<blockquote>%s</blockquote>")
     (italic . "<em>%s</em>")
     (strike-through . "<del>%s</del>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>"))))
(setq org-html-toplevel-hlevel 2)

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

(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-prefix-format
   (quote
    ((agenda . " %?-12t% s")
     (timeline . "  % s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c"))))

;; (setq org-agenda-prefix-format "%t %s")

(add-hook 'org-finalize-agenda-hook
(lambda () (remove-text-properties
(point-min) (point-max) '(mouse-face t))))

;; (setq org-stuck-projects      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

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
(sequence "TODO" "STARTED" "|" "DONE")
        (sequence "MISSED" "|" "DONE")
        (sequence "COMMITTED" "RESULTS" "|")
(sequence "WAITING" "DAILIES" "WEEKLIES" "MONTHLIES" "QUARTERLIES" "YEARLIES" "GOALS" "SOMEDAY" "|") 
        (sequence "QUESTION" "|" "ANSWERED")
        (sequence "QUESTIONS" "|" "ANSWERS")
        (sequence "STRATEGY" "|")
        (sequence "IF" "THEN" "|")
        (sequence "GOAL" "PLAN" "|" "DONE")
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
'(org-n-level-faces 9)
'(org-odd-levels-only nil)
'(org-priority-faces nil)
'(org-provide-checkbox-statistics t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-directory "~/Dropbox/writing/notationaldata/")
(setq org-default-notes-file (concat org-directory "notes.txt"))

(setq org-capture-templates
      (quote
       (

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

("W" "Wise Mind" entry (file "wisemind.txt")
   "\n\n* wm%?\n" :prepend t :kill-buffer t)

  ("e" "expression" entry (file "expression.txt")
   "\n\n* %U\n  %i\n %?\nEntered on %U  %i\n" :prepend t :kill-buffer t)

("k" "nika" entry (file "nika-capture.txt")
   "\n\n* %U\n %i\n %?\nEntered on %U  %i\n" :prepend t :kill-buffer t) 

  ("h" "historical interest" entry (file "historical-lifestream.txt")
   "\n\n* %U\n  %i\n %?\nEntered on %U  %i\n" :prepend t :kill-buffer t)

  ("p" "pages" entry (file "~/Dropbox/writing/notationaldata/pages.txt")
   "\n\n\n\n* %U\n\n%?\n\nEntered on %U  %i\n\n" :prepend t :kill-buffer t)

  ("s" "storytelling and writing" entry (file "/Users/jay/Dropbox/writing/writing-teacher/writing-teacher-stuff/teaching-writing-and-storytelling.txt")
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
  (boss-mode)
  (incarnadine-cursor)
  (define-key org-mode-map (kbd "DEL")
    'new-org-delete-backward-char)
  (define-key key-minor-mode-map (kbd "DEL")  'new-org-delete-backward-char)
  (insert "\n* "))

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

(add-hook 'org-capture-mode-hook 'turn-on-auto-capitalize-mode 'append)
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(add-hook 'org-capture-mode-hook 'writeroom-mode)
;; (add-hook 'org-capture-mode-hook '(setq olivetti-body-width 80)); doesn't work

(defun org-show-level-1 () 
 (interactive) 
(org-content 1)) 

(defun org-show-level-2 () 
 (interactive) 
(org-content 2)) 

(defun org-show-level-3 () 
 (interactive) 
(org-content 3)) 

(defun org-show-level-4 () 
 (interactive) 
(org-content 4)) 

(defun org-show-level-5 () 
 (interactive) 
(org-content 5)) 

(defun org-show-level-6 () 
 (interactive) 
(org-content 6)) 

(defun org-show-level-7 () 
 (interactive) 
(org-content 7)) 

(defun org-show-level-8 () 
 (interactive) 
(org-content 8)) 

(define-key key-minor-mode-map (kbd "C-s-1") 'org-show-level-1) 

(define-key key-minor-mode-map (kbd "C-s-2") 'org-show-level-2) 

(define-key key-minor-mode-map (kbd "C-s-3") 'org-show-level-3) 

(define-key key-minor-mode-map (kbd "C-s-4") 'org-show-level-4) 

(define-key key-minor-mode-map (kbd "C-s-5") 'org-show-level-5) 

(define-key key-minor-mode-map (kbd "C-s-6") 'org-show-level-6) 

(define-key key-minor-mode-map (kbd "C-s-7") 'org-show-level-7) 

(define-key key-minor-mode-map (kbd "C-s-8") 'org-show-level-8)

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

;; (require 'buffer-stack)

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

(defun new-lisp-buffer ()
  (interactive)
  (setq new-buffer-count (+ new-buffer-count 1))
  (switch-to-buffer (concat "buffer" (int-to-string new-buffer-count)))
  (emacs-lisp-mode))

(defun org-new-scratch-buffer ()
  (interactive)
  (insert "* oh hi there! " (format-time-string "%F %l:%M%P\n\n"))
  (org-tree-to-indirect-buffer 'current-window)
  )

(add-hook 'minibuffer-setup-hook 'conditionally-disable-abbrev)
(add-hook 'minibuffer-exit-hook (lambda () (abbrev-mode 1)))
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (abbrev-mode -1)))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(add-hook 'find-file-hooks 'goto-address-prog-mode)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

'(cua-enable-cua-keys (quote shift))
'(cua-highlight-region-shift-only t)
'(cua-mode nil nil (cua-base))
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

(setq auto-mode-alist (cons '("\\.md" . org-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.abbrev_defs" . emacs-lisp-mode) auto-mode-alist))
;; is this the best mode for editing HTML?
(setq auto-mode-alist (cons '("\\.html" . web-mode) auto-mode-alist))

'(org-support-shift-select (quote always))

(require 'auto-capitalize)
(add-hook 'message-mode-hook 'turn-on-auto-capitalize-mode)
(add-hook 'org-mode-hook 'turn-on-auto-capitalize-mode)
;; (add-hook 'message-mode-hook '(orgstruct-mode 1))

(setq default-directory "~/Dropbox/writing/" )

(if (eq window-system 'mac)
    (add-to-list 'exec-path "/usr/local/texlive/2015/bin/universal-darwin")
  )

(setq  ; org-export-dispatch-use-expert-ui t non-intrusive export dispatch
 org-latex-pdf-process               ; for regular export

 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; don't add extra lines to numbered lists and bulleted lists (set to nil)
(setq org-export-preserve-breaks nil) 

;; add padding to numbered lists and bulleted lists (set to t)
;; (setq org-export-preserve-breaks t)

(load "/Users/jay/Dropbox/emacs/prelude/personal/new-latex-templates/blue-ruin.el") 
(load "/Users/jay/Dropbox/emacs/prelude/personal/new-latex-templates/blue-ruin_no_cover.el") 
(load "/Users/jay/Dropbox/emacs/prelude/personal/new-latex-templates/jay-latex-yosemite-setup.el") 
(require 'blue-ruin) 
(require 'blue-ruin-no-cover)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq vc-make-backup-files t)

(setq smtpmail-debug-info t)

;; (setq message-default-mail-headers "Cc: \nBcc: \n")
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
(add-hook 'message-mode-hook 'orgstruct-mode 'append)
; (add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)

(setq recent-addresses-file "~/Dropbox/emacs/prelude/recent-addresses")
(add-to-list 'load-path "~/gnulisp/recent-addresses-0.1/")
(require 'recent-addresses)
(recent-addresses-mode 1)
;; (add-hook 'message-setup-hook 'recent-addresses-add-first-to)

(setq mail-default-directory
   "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
(setq mail-kill-buffer-on-exit t)
(setq make-backup-files t)
(setq message-draft-headers (quote (From References Date)))
(setq message-kill-buffer-on-exit t)
(setq message-required-headers (quote (From (optional . References))))
;; (setq message-send-hook (quote (recent-addresses-add-headers)))
(setq message-send-hook (quote (org-mime-htmlize))) ; broke my other functions

(setq message-citation-line-format "On %e %B %Y at %R %Z, %f wrote:\not")
;; (setq message-citation-line-function 'message-insert-formatted-citation-line)

;; (require 'org-pomodoro)

(defun pomodoro-start ()
  (interactive)
  (play-sound-file "~/sounds/mgm-lion-roar-short.mp3")
  (org-pomodoro)
  )

;; (require 'reveal-in-finder)

(setenv "PATH" (shell-command-to-string "source ~/.profile; echo -n $PATH"))
;; (require 'eshell-autojump)

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

;; (require 'edit-server)
(edit-server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

'(abbrev-all-caps nil)
                       '(ac-auto-show-menu 2.0)
                       '(ac-auto-start 4)
                       '(ac-candidate-menu-min 3)
                       '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*")))
                       '(flyspell-use-global-abbrev-table-p t)
                       '(global-flyspell-mode t)
                       '(mail-kill-buffer-on-exit t)
  '(abbrev-all-caps nil)
'(undo-limit 800000)
  '(user-full-name "Jay Dixit")
  '(user-mail-address "dixit@aya.yale.edu")
 '(blink-cursor-mode nil)
 '(buffer-stack-show-position nil)
 '(buffer-stack-untracked (quote    ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Agenda*")))
 '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Archive*" "*Agenda*" "*fontification*"  "*Warnings*" "*prolific*" "*750words*" "Calendar")))
 '(calendar-latitude 40.7)
 '(case-fold-search t)
 '(ccm-recenter-at-end-of-file t)
 '(clean-buffer-list-delay-general 1)
 '(column-number-mode nil)
 '(compose-mail-user-agent-warnings nil)
 '(cua-highlight-region-shift-only t)
 '(cua-mode nil nil (cua-base))
 '(cua-mode nil)
 '(debug-on-error t)
 '(deft-directory "~/Dropbox/writing/notationaldata/")
 '(delete-window-preserve-buffer (quote ("*scratch*" "current-book-research.txt" "accountability.txt")))
 '(dired-clean-up-buffers-too nil)
 '(dired-details-hidden-string "")
 '(dired-kept-versions 8)
 '(display-time-mode t)
 '(edit-server-default-major-mode (quote org-mode))
 '(edit-server-new-frame t)
 '(eshell-load-hook (quote ((lambda nil (abbrev-mode -1)))))
 '(flyspell-abbrev-p t)
 '(flyspell-use-global-abbrev-table-p t)
 '(global-flyspell-mode t)
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink")))
 '(grep-find-ignored-files (quote (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js" "*.doc" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "Icon*" "*fontification*" "*helm*" "*750words*")))
 '(grep-find-ignored-files (quote (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js" "*.doc" "*.pdf" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "Icon*")))
 '(grep-highlight-matches (quote always))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "pdf" "tex" "html" ".mm" "Icon*")))
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
 '(mml-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(org-M-RET-may-split-line (quote ((item . t))))
 '(org-activate-links (quote (bracket plain radio tag date footnote)))
; '(org-agenda-jump-prefer-future t)
; '(org-agenda-skip-scheduled-if-done t)
; '(org-agenda-timegrid-use-ampm t)
 '(org-archive-location "archive/%s_archive::")
 '(org-ascii-headline-spacing (quote (1 . 1)))
 '(org-ascii-table-use-ascii-art t)
 '(org-ascii-table-use-ascii-art t)
 '(org-bullets-face-name (quote \"Courier\"))
 '(org-catch-invisible-edits (quote error))
 '(org-catch-invisible-edits (quote smart))
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
 '(org-enable-fixed-width-editor nil)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
'(org-extend-today-until 8)
 '(org-fontify-done-headline t)
 '(org-fontify-emphasized-text t)
 '(org-footnote-define-inline t)
 '(org-footnote-section "Footnotes")
 '(org-footnote-tag-for-non-org-mode-files "Footnotes:")
 '(org-headline-done ((t (:strike-through t))))
 '(org-hide-block-startup nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-html-container-element "div")
 '(org-html-head-include-scripts nil)
 '(org-html-html5-fancy t)
 '(org-html-postamble nil)
 '(org-html-text-markup-alist (quote ((bold . "<strong>%s</strong>") (code . "<blockquote>%s</blockquote>") (italic . "<em>%s</em>") (strike-through . "<del>%s</del>") (underline . "<span class=\"underline\">%s</span>") (verbatim . "<code>%s</code>"))))
 '(org-indent-mode-turns-off-org-adapt-indentation nil)
 '(org-indent-mode-turns-on-hiding-stars nil)
 '(org-insert-mode-line-in-empty-file t)
 '(org-list-indent-offset 3)
 '(org-log-done nil)
 '(org-log-note-clock-out nil)
'(org-mac-Skim-highlight-selection-p t)
 '(org-mac-grab-Firefox+Vimperator-p nil)
 '(org-mac-grab-Firefox-app-p nil)
 '(org-mac-grab-Mail-app-p nil)
 '(org-mac-grab-Safari-app-p nil)
 '(org-mac-grab-Together-app-p nil)
 '(org-modules (quote    (org-bbdb org-bibtex org-gnus org-info org-annotate-file org-bullets org-invoice org-mac-iCal org-mac-link  org-panel org-secretary org-velocity org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-choose org-collector org-invoice)))
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
'(org-pomodoro-ticking-sound "~/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/com.taptanium.thunderstorm.DreamQuest_preview.m4a") 
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
 '(org-time-stamp-custom-formats (quote ("<%a %b %d>" . "<%m/%d %a %I:%M%p>"))) ; like this: "Apr 18 Fri"
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
 '(recentf-exclude (quote    ( ".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "elpa" ".bmk" ".jabber" "helm" "Calendar")))
'(recentf-exclude (quote (".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "\\ido.hist\\'" "elpa" ".bmk" ".jabber" "helm"))) 
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 999)
 '(recentf-save-file "~/Dropbox/emacs/.savefile/recentf")
 '(smex-prompt-string "I love you. ")
 '(standard-indent 3)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(undo-limit 800000)
 '(user-full-name "Jay Dixit")
 '(user-mail-address "dixit@aya.yale.edu")
 '(visual-line-mode nil t)
 '(web-mode-load-hook (quote ((lambda nil (abbrev-mode -1)))))
 ; (org-indirect-buffer-display (quote other-window))
'(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"  ".tex" ".mm" "Icon" ".html" ".zip")))
'(flyspell-abbrev-p t)
'(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink")))
'(message-kill-buffer-on-exit t)
'(message-send-mail-function (quote message-send-mail-with-sendmail))
'(mml-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
'(openwith-associations (quote (("\\.pdf\\'" "open" (file)) ("\\.mp3\\'" "xmms" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))
'(org-bullets-face-name (quote \"Lucida\ Sans\ Typeriter\"))
'(org-export-latex-image-default-option "width=20.5cm")
'(org-export-time-stamp-file nil)
'(org-export-with-clocks t)
'(org-hide-block-startup nil)
'(org-html-head-include-default-style nil)
'(org-html-toplevel-hlevel 2)
'(org-indent-indentation-per-level 2)
'(org-list-allow-alphabetical t)
'(org-priority-faces nil)
'(safe-local-variable-values (quote ((org-export-allow-bind-keywords . t))))
'(send-mail-function (quote sendmail-send-it))


;; end
)

;;(require 'key-chord)
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
ido-enter-matching-directory nil
      ido-use-faces t
      ido-use-url-at-point t
      ido-max-prospects 10)
(setq ido-everywhere t)

(setq confirm-nonexistent-file-or-buffer nil)
(ido-everywhere 1)
(setq ido-enable-last-directory-history t)
(setq ido-confirm-unique-completion t) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired nil) ;; put . as the first item
(setq ido-use-filename-at-point t) ;; prefer file names near point
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".org" ".txt" ".md"  ".emacs" ".el"))

(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido nil) 
;; (setq org-refile-use-outline-path t) 



(setq org-goto-interface 'outline-path-completion org-goto-max-level 3) 
(setq org-refile-targets '((my-org-files-list :maxlevel . 3)))

;; (setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq ido-max-directory-size 100000)
(ido-mode (quote both))

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; (require 'ido-hacks)

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

;; (require 'flx-ido)
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

(setq org-mime-default-header "")

;; put a div tag around the whole message to put it in Georgia font.
(add-hook 'org-mime-html-hook
          (lambda ()
            (goto-char (point-min))
            (insert "<div style=\"font-family:Georgia,serif\">")
            (goto-char (point-max))
            (insert "</div>")))

;; (add-hook 'org-mime-html-hook
;;          (lambda ()
;;            (org-mime-change-element-style
;;             "p" "font-family: Georgia,serif; color:#000;")))

(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))))


(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "blockquote" "
    color: #777;
    quotes: none;
    border-radius: 15px;
    font-weight: 400;
    color: #87ceeb;
    line-height: 1.3em;
    background: none repeat scroll 0% 0% rgb(61, 61, 61);
    padding: 20px;
quotes: '«' '»';
font-family: Courier, 'Courier New', monospace;
    font-weight: 400 !important;")))


(add-hook 'message-mode-hook
          (lambda ()
;;;            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
(local-set-key "\M-p" 'org-mime-htmlize)))

(defun mime-send-mail ()
      "org-mime-subtree and HTMLize"
      (interactive)
(org-narrow-to-subtree)
(end-of-buffer)
(insert "\n\n---\nJay Dixit
[[http://jaydixit.com/][jaydixit.com]]
(646) 355-8001\n")
(widen)
(org-mime-subtree)
(org-mime-htmlize)
)

(define-key org-mode-map
  (kbd "RET")
  (lambda()
    (interactive)
    (if (region-active-p)
        (delete-region (region-beginning)
                       (region-end))
      (call-interactively 'org-return))))

(add-hook 'desktop-after-read-hook 'calendar)

(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p)))
  (server-start))

;; (require 'openwith)
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

(setq helm-M-x-always-save-history t)
(eval-after-load 'helm-grep
  '(setq helm-grep-default-command helm-grep-default-recurse-command))

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
  (define-key dired-mode-map (kbd "s-O") 'reveal-in-finder)

  ;; https://truongtx.me/2013/04/25/dired-as-default-file-manager-5-customize-ls-command/

  ;; look at this: https://truongtx.me/2013/12/22/emacs-search-for-text-occurences-with-grep/


(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable 
(setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$")

(defun buffer-stack-filter-regexp (buffer)
  "Non-nil if buffer is in buffer-stack-tracked."
  (not (or (string-match "Help\\|minibuf\\|org2blog\\|echo\\|conversion\\|converting\\|agenda\\|server\\|Messages\\|tex\\|Output\\|temp\\|autoload\\|Customize\\|address\\|clock\\|Backtrace\\|Completions\\|grep\\|Calendar\\|archive\\||*Compile-Log*\\|tramp\\|helm\\|Alerts\\|Minibuf\\|Agenda\\|Echo\\|gnugol\\|RNC\\|ediff\\|widget\\|melpa\\|git\\|hydra\\|which\\|fontification\\|Helm\\|popwin\\|Custom\\|*Warnings*\\|*tags*\\|*emacs*\\|*gnugol*\\|*guide-key*\\|*scratch*\\|vc\\|booktime\\|Compile\\|*mm*\\|nntpd\\|Gnus agent\\|dribble\\|gnus work\\|Original Article\\|Prefetch\\|Backlog\\|article copy\\|Gnorb\\|wordnik\\|log\\|accountability\\|debug\\|Re-Builder\\|spacemacs\\|Ilist\\|later.txt\\|book-capture.txt" (buffer-name buffer))
     (member buffer buffer-stack-untracked))))
(setq buffer-stack-filter 'buffer-stack-filter-regexp)
(setq buffer-stack-filter 'buffer-stack-filter-regexp)
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
(add-to-list 'recentf-exclude "*archive")
(add-to-list 'recentf-exclude "ics")
(add-to-list 'recentf-exclude "agenda")
(add-to-list 'recentf-exclude "gnugol")
(add-to-list 'recentf-exclude "PDF")
(add-to-list 'recentf-exclude "koma")
(add-to-list 'recentf-exclude "LaTeX")
(add-to-list 'recentf-exclude "recentf")

(add-to-list 'recentf-exclude '("doc" " docx" "xls" "xlsx" "ppt" "odt" "ods" "odg" "odp"))

(add-to-list 'recentf-exclude '(".mp4" ".mpg" ".mpeg"
".avi" ".wmv" ".wav" ".mov" ".flv" ".ogm" ".ogg" ".mkv"
".png" ".gif" ".bmp" ".tif" ".jpeg" "png" ".jpg" ".doc" ".docx" ".xls" ".xlsx" ".ppt" ".odt" ".ods" ".odg" ".odp"))

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

(require 'wc-mode)
(setq wc-modeline-format "[Words: %tw, Chars: %tc]")

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

(defun org-day ()
  "foo"
  (interactive)
  (insert (format-time-string "[%H:%M]"))
  )

(defun jd-org-today ()
  "insert a new heading with today's date"
  (interactive)
(insert "\n** committed actions: ")
  (org-insert-time-stamp (current-time))
  (insert " [0%]\n")

(insert "*** TODO wake up by 8:30am\n") 
(insert "*** TODO blue light therapy\n") 
(insert "*** TODO meditate\n") 
(insert "*** TODO morning pages\n")
(insert "*** TODO work on book\n") 
(insert "*** TODO \n") 
(left-char)
  )

(defun jd-clock-in ()
  "insert a new heading with today's date, and then clock in"
  (interactive)
  (org-insert-heading ())
  (org-insert-time-stamp (current-time))
  (org-clock-in)
  (next-line)
  (next-line)
  )

;; (require 'discover)

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
  "Update all packages, no questions asked."
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
        (if (eql refile-region-position 'bottom)
            (org-end-of-subtree)
          (org-end-of-meta-data-and-drawers))
        (insert (format refile-region-format text))))))


(defun my-org-files-list ()
  (mapcar (lambda (buffer)
            (buffer-file-name buffer))
          (org-buffer-list 'files t)))

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

(defun prelude-rename-file-and-buffer (new-name)
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

;; (require 'engine-mode)
;; (engine-mode t)

;; (defengine google  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"  "g")

;; (require 'gnugol)

(defun gnugol-word-at-point ()
  (interactive)
(gnugol-search-google (thing-at-point 'word))
)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; (require 'point-stack)

;; (add-to-list 'load-path "~/elisp/bbdb/lisp")
;; (require 'bbdb) ;; (3)
;; (bbdb-initialize 'gnus 'message)   ;; (4)
;; (setq bbdb-north-american-phone-numbers-p nil)   ;; (5)

(global-set-key (kbd "M-n") 'outline-next-visible-heading)
(global-set-key (kbd "M-p") 'outline-previous-visible-heading)
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
(define-hyper-key "O" 'reveal-in-finder)
(define-hyper-key "o" 'projectile-find-file)

(define-hyper-key "i" 'org-mac-chrome-insert-frontmost-url)
(define-hyper-key "\\" 'visit-most-recent-file)
(define-hyper-key "]" 'visit-most-recent-file)
;; (define-hyper-key "f" 'isearch-forward)
(define-hyper-key "F" 'pasteboard-search-for-clipboard-contents) 
;; (define-hyper-key "R" 'xsteve-ido-choose-from-recentf)
;; (define-hyper-key "R" 'helm-projectile-recentf)
;; (define-hyper-key "r" 'helm-mini)
(define-hyper-key "r" 'recentf-open-files-compl)
(define-hyper-key "R" 'projectile-find-file)
(define-hyper-key "t" 'new-buffer)
(define-hyper-key "T" 'org-new-scratch-buffer)
(define-hyper-key "g" 'isearch-repeat-forward)
(define-hyper-key "k" 'ido-kill-buffer)
(define-hyper-key "K" 'widen)
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
(define-hyper-key "F" 'pasteboard-search-in-current-buffer)
(define-hyper-key "(" 'org-velocity)
(define-hyper-key "[" 'org-backward-heading-same-level)
(define-hyper-key "]" 'org-forward-heading-same-level)
(define-hyper-key "{" 'org-previous-visible-heading)
(define-hyper-key "}" 'org-next-visible-heading)
;; why not use N and P here? TODO



;; (define-hyper-key "m a" 'org-agenda)
(define-hyper-key "m j" 'helm-imenu-anywhere)
(define-hyper-key ";" 'ido-goto-symbol)
(define-hyper-key "D" 'diredp-dired-recent-dirs)
(define-hyper-key "F" 'pasteboard-search-in-current-buffer)

(define-hyper-key "m cy" 'cyberpunk-jay)
(define-hyper-key "m cl" 'cyberpunk-large)
(define-hyper-key "m zb" 'zenburn)
(define-hyper-key "m le" 'leuven)
(define-hyper-key "m ts" 'transparent-serenity)
(define-hyper-key "m tg" 'top-gun-mode)
(define-hyper-key "m tn" 'tomorrow-night)

(define-hyper-key "m ma" 'inverse-add-global-abbrev) 

(define-hyper-key "m rr" 'replace-regexp) 


(define-hyper-key "m cf" 'customize-face) 

(define-hyper-key "m h" 'org-export-dispatch)




(define-hyper-key "m nm" 'notmuch-hello) 
(define-hyper-key "m mu" 'mu4e)

;; (define-hyper-key "m cl" 'cyberpunk-large)

(define-hyper-key "m dd" 'delete-duplicate-lines-keep-blanks) 

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
(define-hyper-key "m df" 'prelude-delete-file-and-buffer)

(define-hyper-key "m bl" 'blue-light)

(define-hyper-key "m eu" 'endless/upgrade)


;; accountability
(define-hyper-key "m td" 'jd-org-today)
(define-hyper-key "m ek" 'erika-send-email-styled)

(defun keybinding-read-and-insert (key)
  (interactive "kKey: ")
(insert "(define-key key-minor-mode-map ")
        (insert (format "(kbd \"%s\")" (key-description key)))
        (insert " '")
(save-excursion (insert ")")
                ))

(define-hyper-key "m kb" 'keybinding-read-and-insert)
(define-hyper-key "m mk" 'keybinding-read-and-insert)

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
    (goto-char isearch-other-end))
(recenter-top-bottom)
)

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
      helm-adaptive-history-file             "~/Dropbox/emacs/.savefile/helm-history"
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

(global-set-key (kbd "M-s s")   #'helm-again)

(setq projectile-completion-system (quote helm))
(setq projectile-enable-caching nil)
(setq projectile-globally-ignored-buffers (quote ("docx ")))
(setq projectile-globally-ignored-file-suffixes (quote ("docx ")))
(setq projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "archived-work" "images" "excel-mindnode-omni")))

(require 'helm-projectile)
(setq helm-projectile-sources-list (cons 'helm-source-projectile-files-list
(remove 'helm-source-projectile-files-list helm-projectile-sources-list)))
(helm-projectile-on)

(define-key projectile-mode-map (kbd "C-c p /")
  #'(lambda ()
      (interactive)
      (helm-ag (projectile-project-root))))

;; (require 'palimpsest)
(palimpsest-mode 1)

(font-lock-add-keywords
 'org-mode '(("^\\(:+\\) " 1 (compose-region (match-beginning 1) (match-end 1) ?❱) nil)))

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
      (lambda () 
        (save-match-data
          (not (looking-back "\\([Ee]\\.g\\|[Uu]\\.S\\|Mr\\|Mrs\\|Ms\\|cf\\|[N]\\.B\\|[U]\\.N\\|[E]\\.R\\|[M]\\.C\\|[Vv]S\\|[Ii]\\.e\\|\\.\\.\\)\\.[^.\n]*" (- (point) 20))))))

(setq magit-last-seen-setup-instructions "1.4.0")

;; (load-theme 'leuven)
;; (incarnadine-cursor)
;; (monaco-font)

;;; Tell ispell.el that ’ can be part of a word.
(setq ispell-local-dictionary-alist
      `((nil "[[:alpha:]]" "[^[:alpha:]]"
             "['\x2019]" nil ("-B") nil utf-8)))

;;; Don't send ’ to the subprocess.
(defun endless/replace-apostrophe (args)
  (cons (replace-regexp-in-string
         "’" "'" (car args))
        (cdr args)))
(advice-add #'ispell-send-string :filter-args
            #'endless/replace-apostrophe)

;;; Convert ' back to ’ from the subprocess.
(defun endless/replace-quote (args)
  (if (not (derived-mode-p 'org-mode))
      args
    (cons (replace-regexp-in-string
           "'" "’" (car args))
          (cdr args))))
(advice-add #'ispell-parse-output :filter-args
            #'endless/replace-quote)

(run-with-idle-timer 60 t 'recentf-save-list)

(advice-add #'org-remove-angle-brackets :before-until
            (lambda (s) (if (string-prefix-p "mailto:" s) s)))

(defun jay/save-some-buffers ()
(interactive)
  (save-some-buffers 'no-confirm (lambda ()
    (cond
      ((and buffer-file-name (equal buffer-file-name abbrev-file-name)))
      ((and buffer-file-name (eq major-mode 'latex-mode)))
((and buffer-file-name (eq major-mode 'emacs-lisp-mode)))
((and buffer-file-name (eq major-mode 'fundamental-mode)))
((and buffer-file-name (eq major-mode 'markdown-mode)))
((and buffer-file-name (eq major-mode 'graphviz-dot-mode)))
((and buffer-file-name (eq major-mode 'python-mode)))
((and buffer-file-name (eq major-mode 'text-mode)))
((and buffer-file-name (eq major-mode 'snippet-mode))) 
((and buffer-file-name (eq major-mode 'css-mode))) 
((and buffer-file-name (eq major-mode 'xml-mode))) 
((and buffer-file-name (eq major-mode 'nmxml-mode)))
((and buffer-file-name (eq major-mode 'conf-mode)))
((and buffer-file-name (eq major-mode 'gitconfig-mode)))
((and buffer-file-name (eq major-mode 'gitignore-mode)))
      ((and buffer-file-name (eq major-mode 'sh-mode)))
      ((and buffer-file-name (derived-mode-p 'org-mode)))))))

(add-hook 'find-file-hook (lambda () (palimpsest-mode 1)))

(setq set-mark-command-repeat-pop t)

(setq custom-safe-themes t)

(setq user-mail-address "dixit@aya.yale.edu")
(setq user-full-name "Jay Dixit")
(setq gnus-always-read-dribble-file t)
(setq gnus-select-method '(nnml ""))
(setq gnus-select-method '(nnimap "gmail"
(nnimap-address "imap.gmail.com")
(nnimap-server-port 993)
(nnimap-stream ssl)))
(setq gnus-use-cache t) 




(setq gnus-select-method
      '(nnimap "gmail"
         (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
         (nnimap-server-port "imaps")
         (nnimap-stream ssl)))




;; store email in ~/gmail directory
(setq nnml-directory "~/gmail")
(setq message-directory "~/gmail") 

;; define gnus directories
(setq message-directory "~/Dropbox/emacs/gnus/mail/")
(setq gnus-directory "~/Dropbox/emacs/gnus/news/")
(setq nnfolder-directory "~/Dropbox/emacs/gnus/mail/archive") 

;; How to read HTML mail
(setq mm-text-html-renderer 'w3m)
(setq gnus-summary-line-format "%-6,6B%-15,15f |%* %-40,40s |  %&user-date; | %U\n")

;; sort by most recent date
(setq gnus-article-sort-functions (quote ((not gnus-article-sort-by-date))))
(setq gnus-thread-sort-functions (quote ((not gnus-thread-sort-by-date))))


;; More attractive Summary View
;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

(defun org-mime-htmlize (&optional arg)
"Export a portion of an email body composed using `mml-mode' to
html using `org-mode'.  If called with an active region only
export that region, otherwise export the entire body."
  (interactive "P")
  (require 'ox-org)
  (require 'ox-html)
  (let* ((region-p (org-region-active-p))
         (html-start (or (and region-p (region-beginning))
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward mail-header-separator)
                           (+ (point) 1))))
         (html-end (or (and region-p (region-end))
                       ;; TODO: should catch signature...
                       (point-max)))
         (raw-body (concat org-mime-default-header
         (buffer-substring html-start html-end)))
         (tmp-file (make-temp-name (expand-file-name
            "mail" temporary-file-directory)))
         (body (org-export-string-as raw-body 'org t))
         ;; because we probably don't want to export a huge style file
         (org-export-htmlize-output-type 'inline-css)
         ;; makes the replies with ">"s look nicer
         (org-export-preserve-breaks org-mime-preserve-breaks)
   ;; dvipng for inline latex because MathJax doesn't work in mail
   (org-html-with-latex 'dvipng)
         ;; to hold attachments for inline html images
         (html-and-images
          (org-mime-replace-images
     (org-export-string-as raw-body 'html t) tmp-file))
         (html-images (unless arg (cdr html-and-images)))
         (html (org-mime-apply-html-hook
                (if arg
                    (format org-mime-fixedwith-wrap body)
                  (car html-and-images)))))
    (delete-region html-start html-end)
    (save-excursion
      (goto-char html-start)
      (insert (org-mime-multipart
         body html (mapconcat 'identity html-images "\n"))))))

(defun new-email-from-subtree-with-signature ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive)
  ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
                     (unless (org-on-heading-p) (outline-previous-heading))
                     (let ((headline (org-element-at-point)))
                       (buffer-substring
                        (org-element-property :contents-begin headline)
                        (org-element-property :contents-end headline)))))
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
(end-of-buffer)
(insert "\nWarm regards,\nJay Dixit\n\n---\nJay Dixit
(646) 355-8001
[[http://jaydixit.com/][jaydixit.com]]
\n")
(message-goto-to))
))


(defun new-email-from-subtree-no-signature ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive)
  ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
                     (unless (org-on-heading-p) (outline-previous-heading))
                     (let ((headline (org-element-at-point)))
                       (buffer-substring
                        (org-element-property :contents-begin headline)
                        (org-element-property :contents-end headline)))))
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
;; (end-of-buffer)
)
))

(defun erika-send-email ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive)
; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
                     (unless (org-on-heading-p) (outline-previous-heading))
                     (let ((headline (org-element-at-point)))
                       (buffer-substring
                        (org-element-property :contents-begin headline)
                        (org-element-property :contents-end headline)))))
          (TO "\"Erika Casriel\" <erika.casriel@comcast.net>")
          (CC (org-entry-get (point) "CC" t))
(BCC "Luke Haseloff <luke.haseloff@gmail.com>") 
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
) 
))

(defun erika-send-email-styled ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive)
                                        ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
                     (unless (org-on-heading-p) (outline-previous-heading))
                     (let ((headline (org-element-at-point)))
                       (buffer-substring
                        (org-element-property :contents-begin headline)
                        (org-element-property :contents-end headline)))))
          (TO "Erika Casriel <erika.casriel@comcast.net>")
          (CC (org-entry-get (point) "CC" t))
(BCC "Luke Haseloff <luke.haseloff@gmail.com>")
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
      )
    (let ((org-mime-html-hook
           (list* (lambda ()
                    (goto-char (point-min))
                    (while (re-search-forward "</?\\(h2\\)" nil t)
                      (replace-match "li" nil t nil 1)))
                  (lambda ()
                    (goto-char (point-min))
                    (org-mime-change-class-style "todo TODO" "color:red;font-weight:bold")
                    (goto-char (point-min))
                    (org-mime-change-class-style "todo MISSED" "color:red;font-weight:bold")
                    (goto-char (point-min))

(org-mime-change-class-style "off" "list-style-type: none;")
                    (goto-char (point-min)) 

                    (org-mime-change-class-style "done DONE" "color:green;font-weight:bold")
                    (goto-char (point-min))
                    (org-mime-change-class-style "done DONE" "color:green;font-weight:bold"))
                  org-mime-html-hook)))
(message-send-and-exit)
))
)

(defun erika-send-email-test ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive)
                                        ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
                     (unless (org-on-heading-p) (outline-previous-heading))
                     (let ((headline (org-element-at-point)))
                       (buffer-substring
                        (org-element-property :contents-begin headline)
                        (org-element-property :contents-end headline)))))
          (TO "Erika Casriel <sunjaydixit@gmail.com>")
          (CC (org-entry-get (point) "CC" t))
(BCC "Luke Haseloff <sunjaydixit@gmail.com>")
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
      )
    (let ((org-mime-html-hook
           (list* (lambda ()
                    (goto-char (point-min))
                    (while (re-search-forward "</?\\(h2\\)" nil t)
                      (replace-match "li" nil t nil 1)))
                  (lambda ()
                    (goto-char (point-min))
                    (org-mime-change-class-style "todo TODO" "color:red;font-weight:bold")
                    (goto-char (point-min))
                    (org-mime-change-class-style "todo MISSED" "color:red;font-weight:bold")
                    (goto-char (point-min))

(org-mime-change-class-style "off" "list-style-type: none;")
                    (goto-char (point-min)) 

                    (org-mime-change-class-style "done DONE" "color:green;font-weight:bold")
                    (goto-char (point-min))
                    (org-mime-change-class-style "done DONE" "color:green;font-weight:bold"))
                  org-mime-html-hook)))
(message-send-and-exit)
))
)

(defun email-heading-to-me ()
  "Send the current org-mode heading as the body of an email, with headline
as the subject."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (let ((content (buffer-substring (point) (mark)))
          (SUBJECT (nth 4 (org-heading-components))))

      (compose-mail "your@email.here" SUBJECT)
      (message-goto-body)
      (insert content)
      (message-send)
      (message-kill-buffer))))

(require 'key-seq)
(key-seq-define-global "qd" 'dired)
(key-seq-define text-mode-map "qf" 'flyspell-buffer)

(key-seq-define-global "nm" 'new-email-from-subtree-no-signature)
(key-seq-define-global "mn" 'new-email-from-subtree-with-signature)

(defun org-toggle-heading-same-level ()
  "Toggles the current line between a non-heading and TODO heading."
  (interactive)
  (let ((is-heading))
    (save-excursion
      (forward-line 0)
      (when (looking-at "^\\*")
        (setq is-heading t)))
    (if is-heading
        (progn
          (org-todo 'none) ; remove TODO
          (org-toggle-heading)) ; remove heading
      (progn
        (org-toggle-heading) ; convert to heading
(org-do-promote)
;        (org-todo 'nextset)
)))) ; add TODO  #+END_SRC

(defun org-toggle-todo-heading ()
  "Toggles the current line between a non-heading and TODO heading."
  (interactive)
  (let ((is-heading))
    (save-excursion
      (forward-line 0)
      (when (looking-at "^\\*")
        (setq is-heading t)))
    (if is-heading
        (progn
          (org-todo 'none) ; remove TODO
          (org-toggle-heading)) ; remove heading
      (progn
        (org-toggle-heading) ; convert to heading
(org-do-promote)
        (org-todo 'nextset))))) ; add TODO  #+END_SRC

(defun fix-recent-addresses-file ()
"One sentence summary of what this command do."
  (interactive)
  ;; do something …
  (shell-command "sed -i '' 's/\\\.\\\.\\\.//g' /Users/jay/.emacs.d/recent-addresses")
  (load "/Users/jay/Dropbox/emacs/prelude/recent-addresses"))

(defun oleh-ido-setup-hook ()
  (define-key ido-file-dir-completion-map "~"
    (lambda ()
      (interactive)
      (ido-set-current-directory "~/")
      (setq ido-exit 'refresh)
      (exit-minibuffer))))

(add-hook 'ido-setup-hook 'oleh-ido-setup-hook)

(defun ido-find-file-jump (dir)
  "Return a command that sends DIR to `ido-find-file'."
  `(lambda ()
     (interactive)
     (ido-set-current-directory ,dir)
     (setq ido-exit 'refresh)
     (exit-minibuffer)))



(defvar oleh-ido-shortcuts
  '(("~/" "~")
    ("~/Dropbox/source/site-lisp/" "!")
    ("~/git/lispy/" "@")))

(mapc (lambda (x)
        (setcar x (ido-find-file-jump (car x))))
      oleh-ido-shortcuts)

(defun oleh-ido-setup-hook ()
  (mapc
   (lambda (x)
     (define-key ido-file-dir-completion-map (cadr x) (car x)))
   oleh-ido-shortcuts))

(add-hook 'ido-setup-hook 'oleh-ido-setup-hook)

(defun delete-extra-whitespace-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+" nil t)
        (replace-match "")))))

(defun double-line-breaks-in-region (begin end)
  (interactive "r")
  (xah-replace-pairs-region begin end
 '(
 ["\r" "\n\n"]
["\n" "\n\n"]
)))

(defun fixup-css-region (begin end)
(interactive "r")
  (xah-replace-pairs-region begin end
                          '(["h2" "li"]
["<span class=\"todo DONE\">" "<span class=\"todo DONE\" style=\"color:green;font-weight:bold\">"]

["<span class=\"todo MISSED\">" "<span class=\"todo MISSED\" style=\"color:red;font-weight:bold\">"]

["<span class=\"todo TODO\">" "<span class=\"todo TODO\" style=\"color:red;font-weight:bold\">"]

))
)

(require 'xah-replace-pairs)
(defun replace-html-chars-region (begin end)
  (interactive "r")
  (xah-replace-pairs-region begin end
 '(
 ["&" "&amp;"]
 ["<" "&lt;"]
 [">" "&gt;"]
 )))

(when (require 'openwith nil 'noerror)
      (setq openwith-associations
            (list
             (list (openwith-make-extension-regexp
                    '())
                   "open"
                   '(file))

(list (openwith-make-extension-regexp
                    '("mp3"))
                   "open"
                   '(file))

(list (openwith-make-extension-regexp
                    '("mp4" "mpg" "mpeg"
                      "avi" "wmv" "wav" "mov" "flv"
                      "ogm" "ogg" "mkv" "webm"))
                   "open -a vlc"
                   '(file))

             (list (openwith-make-extension-regexp
                    '("xbm" "pbm" "pgm" "ppm" "pnm"
                      "png" "gif" "bmp" "tif" "jpeg" "jpg"))
                   "open"
                   '(file))

             (list (openwith-make-extension-regexp
                    '("doc" "docx" "xls" "xlsx" "ppt" "odt" "ods" "odg" "odp"))
                   "open"
                   '(file))
             '("\\.lyx" "lyx" (file))
             '("\\.chm" "kchmviewer" (file))
             (list (openwith-make-extension-regexp
                    '("pdf"))
                   "open"
                   '(file))
             ))
      (openwith-mode 1))

(setq org-startup-with-inline-images nil)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'rainbow-delimiters)
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error)

(require 'org-download)
(setq-default org-download-image-dir "/Users/jay/Downloads")
(setq org-download-method (quote directory))

(defun delete-duplicate-lines-keep-blanks ()
  (interactive)
  (delete-duplicate-lines (region-beginning) (region-end) nil nil t))

(defun helm-do-grep-current-directory-tree ()
  "Recursively search current directory.
If a parent directory has a `dir-locals-file', use that as the
root instead."
  (interactive)
  (let ((variables-file (dir-locals-find-file
                         (or (buffer-file-name) default-directory))))
    (helm-do-grep-1
     (list
      (cond
       ((stringp variables-file)
        (file-name-directory variables-file))
       ((consp variables-file)
        (nth 0 variables-file))
       (t default-directory)))
     t nil '("*"))))

(defun contract-contractions (begin end)
(interactive "r")
  (xah-replace-pairs-region begin end
 '(
 ["I have" "I've"]
["I am" "I'm"] 
)))

(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(add-hook 'text-mode-hook #'dubcaps-mode)
(add-hook 'org-mode-hook #'dubcaps-mode)

(defun prelude-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(require 'yasnippet) 
(yas-global-mode 1) 

;; load yasnippet directories
;; TODO needs to be set before loading snippets somehow
(setq yas-snippet-dirs '("~/emacs/interesting-snippets" "~/Dropbox/emacs/snippets"))

;; don't insert random spaces in my prose
(setq yas-indent-line (quote none)) 

;; take input word including hyphen.
(setq yas/key-syntaxes '("w_" "w_." "^ ")) ; default is '("w" "w_" "w_." "^ ")

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>") 


   (add-hook 'org-mode-hook
                    (lambda ()
                      (org-set-local 'yas-trigger-key [tab])
                      (define-key yas-keymap [tab] 'yas-next-field-or-maybe-expand))) 

    (defun yas-org-very-safe-expand ()
            (let ((yas-fallback-behavior 'return-nil)) (yas-expand))) 

    (add-hook 'org-mode-hook
                    (lambda ()
                      (make-variable-buffer-local 'yas-trigger-key)
                      (setq yas-trigger-key [tab])
                      (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
                      (define-key yas-keymap [tab] 'yas-next-field)))

;; NO spell check for embedded snippets
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let ((rlt ad-return-value)
        (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
        (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
        old-flag
        b e)
    (when ad-return-value
      (save-excursion
        (setq old-flag case-fold-search)
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))

(require 'tiny)
(tiny-setup-default) 

(defun new-week ()
  (interactive)
(tiny-expand "m0\n7|*** committed actions:  <%(date "mon" x)>\n**** TODO \n") 
  )

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp "^;;; ")
            (make-local-variable 'outline-heading-end-regexp)
            (setq outline-heading-end-regexp ":\n")
            (outline-minor-mode 1)
))

;; (require 'which-key)
;; (setq which-key-popup-type 'side-window) 
;; (setq which-key-side-window-location 'bottom) 
;; (which-key-mode)

(defun accountability-open ()
  (interactive)
  (find-file "/Users/jay/Dropbox/writing/notationaldata/accountability.org")
  )

(define-key key-minor-mode-map (kbd "<s-S-right>") 'accountability-open)

(defun playful-open ()
  (interactive)
  (find-file "/Users/jay/Dropbox/writing/notationaldata/playful.org")
  )
(define-key key-minor-mode-map (kbd "<s-S-left>") 'playful-open)

;; (add-to-list 'load-path "/Users/jay/Downloads/dictionary-el-master")

(require 're-builder)
(setq reb-re-syntax 'string)

;;(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

(global-fasd-mode 1)
(setq fasd-enable-initial-prompt nil)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path)) 
(setq mu4e-mu-binary "/usr/local/bin/mu") 
(require 'mu4e)
(setq mu4e-maildir "/Users/jay/Dropbox/mail/gmail") 
(setq mu4e-sent-folder   "/sent")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-trash-folder  "/trash") 

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap")
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-change-filenames-when-moving t)

(setq mu4e-attachment-dir "~/Downloads") 

;; shortcuts
(setq mu4e-maildir-shortcuts
    '( ("/starred"               . ?i)
       ("/sent"   . ?s)))

;; something about ourselves
(setq mu4e-compose-signature
    (concat
      "Best,\n"
      "Jay\n"))

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; fetch mail every 10 mins
(setq mu4e-update-interval 600)


;; Use fancy chars
(setq mu4e-use-fancy-chars t) 


(setq mu4e-confirm-quit nil
      mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format
      mu4e-html2text-command "html2text -utf8 -width 72"
      ) 

;; maildirs
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension) 
(setq mu4e-maildirs-extension-title "Folders")

;; (define-key mu4e-mode-map "r" 'mu4e-compose-reply)

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(define-key gnus-summary-mode-map "c"
  'compose-mail)
 
(define-key gnus-summary-mode-map "a"
  'gnus-summary-wide-reply)

(add-to-list 'load-path "/Users/jay/Dropbox/emacs/prelude/personal/notmuch/")
(require 'notmuch)
(setq notmuch-search-line-faces (quote (("unread" :weight bold) ("flagged"))))
(setq notmuch-tag-formats

   (quote
    (("unread"
      (propertize tag
                  (quote face)
                  (quote
                   (:foreground "red"))))
     ("flagged"
      (propertize tag
                  (quote face)
                  (quote
                   (:foreground "pink")))))))

(custom-set-faces

'(notmuch-search-date ((t (:inherit default :height 1.2))))
 '(notmuch-search-matching-authors ((t (:inherit default :height 1.2))))
 '(notmuch-search-subject ((t (:inherit default :height 1.2))))
 '(notmuch-search-count ((t (:inherit default :height 0.1))))
 '(notmuch-tag-face ((t (:foreground "OliveDrab1" :height 1)))) )


(setq notmuch-search-oldest-first nil)
(defadvice notmuch-mua-reply (around notmuch-fix-sender)
     (let ((sender "Jay Dixit <dixit@aya.yale.edu>"))
       ad-do-it))
   (ad-activate 'notmuch-mua-reply) 

;; Initially the cursor is positioned at the beginning of buffer. 
;; Some users liked the "ancient" version where cursor was moved to the first Saved searches button. 
;; Add the following code to your notmuch emacs configuration file in case you want this behaviour:

    (add-hook 'notmuch-hello-refresh-hook
              (lambda ()
                (if (and (eq (point) (point-min))
                         (search-forward "Saved searches:" nil t))
                    (progn
                      (forward-line)
                      (widget-forward 1))
                  (if (eq (widget-type (widget-at)) 'editable-field)
                      (beginning-of-line))))) 

;; modify the keybindings 
(define-key notmuch-show-mode-map "y"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-show-tag (list "-flagged" "-inbox"))
(notmuch-refresh-this-buffer))) 

(define-key notmuch-show-mode-map "u"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-bury-or-kill-this-buffer)
(notmuch-refresh-this-buffer) 
)) 


(define-key notmuch-search-mode-map "d"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-search-tag (list "-flagged" "-inbox"))
(notmuch-search-next-thread))) 

(define-key notmuch-search-mode-map "u"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-search-tag (list "+flagged" "+inbox"))
(notmuch-search-next-thread))) 


;; show mode
(define-key notmuch-show-mode-map "d"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-show-tag (list "-flagged" "-inbox"))
(notmuch-bury-or-kill-this-buffer)
(notmuch-refresh-this-buffer) 
)) 

(define-key notmuch-show-mode-map "u"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-show-tag (list "+flagged" "+inbox"))
(notmuch-bury-or-kill-this-buffer)
(notmuch-refresh-this-buffer) 

)) 




(define-key notmuch-search-mode-map "y"
      (lambda ()
        "toggle flagged tag for message"
        (interactive)
        (if (member "flagged" (notmuch-search-get-tags))
            (notmuch-search-tag (list "-flagged" "-inbox"))
          (notmuch-search-tag (list "+flagged" "+inbox")))))
(define-key notmuch-tree-mode-map "y"
      (lambda ()
        "toggle flagged tag for message"
        (interactive)
        (if (member "flagged" (notmuch-tree-get-tags))
            (notmuch-tree-tag (list "-flagged" "-inbox"))
          (notmuch-tree-tag (list "+flagged" "+inbox"))))) 


(define-key notmuch-search-mode-map "S"
      (lambda ()
        "toggle flagged tag for message"
        (interactive)
        (if (member "flagged" (notmuch-search-get-tags))
            (notmuch-search-tag (list "-flagged"))
          (notmuch-search-tag (list "+flagged")))))
(define-key notmuch-tree-mode-map "S"
      (lambda ()
        "toggle flagged tag for message"
        (interactive)
        (if (member "flagged" (notmuch-tree-get-tags))
            (notmuch-tree-tag (list "-flagged"))
          (notmuch-tree-tag (list "+flagged")))))


(define-key notmuch-show-mode-map "g"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-refresh-this-buffer))) 
(define-key notmuch-search-mode-map "g"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-refresh-this-buffer))) 
(define-key notmuch-tree-mode-map "g"
      (lambda ()
        "archive"
(notmuch-refresh-this-buffer))) 



(define-key notmuch-show-mode-map "x"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-refresh-this-buffer))) 
(define-key notmuch-search-mode-map "x"
      (lambda ()
        "archive"
        (interactive)
        (notmuch-refresh-this-buffer))) 
(define-key notmuch-tree-mode-map "x"
      (lambda ()
        "archive"
(notmuch-refresh-this-buffer))) 


(define-key notmuch-hello-mode-map "g" (notmuch-refresh-this-buffer)) 
;; doesn't work

;; modify the documentation about the keybindings
(defun notmuch-hello-insert-footer ()
  "Insert the notmuch-hello footer."
  (let ((start (point)))
    (widget-insert "Type a search query and hit RET to view matching threads.\n")
    (when notmuch-search-history
      (widget-insert "Hit RET to re-submit a previous search. Edit it first if you like.\n")
      (widget-insert "Save recent searches with the `save' button.\n"))
    (when notmuch-saved-searches
      (widget-insert "Edit saved searches with the `edit' button.\n"))
    (widget-insert "Hit RET or click on a saved search or tag name to view matching threads.\n")
(widget-insert "`G' to refresh this screen. `s' to search messages. `q' to quit.\n")
    (widget-insert "`S' to star or unstar messages.\n")
    (widget-insert "`y' to unstar and archive a message.\n")
    (widget-create 'link
       :notify (lambda (&rest ignore)
           (customize-variable 'notmuch-hello-sections))
       :button-prefix "" :button-suffix ""
       "Customize")
    (widget-insert " this page.")
    (let ((fill-column (- (window-width) notmuch-hello-indent)))
      (center-region start (point))))) 
;; doesn't work 


(require 'org-notmuch) 

;; display in the middle of large displays
;; (add-hook 'notmuch-show-hook 'turn-on-olivetti-mode 'append)
;; (add-hook 'notmuch-hello-mode-hook 'turn-on-olivetti-mode 'append)
;; (add-hook 'notmuch-search-hook 'turn-on-olivetti-mode 'append)

(add-hook 'message-mode-hook 'turn-on-olivetti-mode 'append) 
(add-hook 'message-mode-hook 'turn-on-olivetti-mode 'append)

(add-hook 'nm-mode-hook 'turn-on-olivetti-mode 'append) 
(setq nm-results-window-size 25)

(require 'org-contacts) 
(require 'org-vcard)
(setq org-contacts-files (quote ("/Users/jay/nd/contacts-org-jay.txt")))

(defun kill-to-buffer-end ()
  (interactive) 
  (end-of-buffer)
(delete-region (mark) (point))
(recenter-top-bottom)
)
(define-key key-minor-mode-map (kbd "C-w") 'kill-to-buffer-end)

(add-to-list 'load-path "/Users/jay/Dropbox/emacs/prelude/personal/zone-matrix/")  

;; (setq zone-programs [zone-pgm-drip]) 
;; (setq zone-programs [zone-pgm-five-oclock-swan-dive]) 
(setq zone-programs [zone-pgm-putz-with-case])

(defun unbind-orgstruct-keys ()
  (interactive)
       (cl-dolist (map '(message-mode-map orgstruct-mode-map))
  (cl-dolist (key '("S-<right>" "S-<left>" "S-<down>" "S-<up>" "<M-S-left>" "<M-S-right>" "<M-S-up>" "<M-S-down>"
                    "<M-left>" "<M-right>" "<M-up>" "<M-down>"))
    (define-key (eval map) (kbd key) nil)))
  )
(add-hook 'message-mode-hook 'unbind-orgstruct-keys)

(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-c n") 'my-dired-create-file)
     (defun my-dired-create-file (file)
       "Create a file called FILE.
If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))

(setq olivetti-body-width 120)

;; (run-at-time nil 30 'jay/save-some-buffers)

(defun load-shared-functions ()
  (interactive)
(find-file "/Users/jay/Dropbox/emacs/prelude/personal/shared-functions.org"))

(defun load-gnu-startup ()
  (interactive)
(find-file "/Users/jay/Dropbox/emacs/prelude/personal/gnu-emacs-startup.org")) 

(define-key key-minor-mode-map (kbd "M-[") 'load-shared-functions)
(define-key key-minor-mode-map (kbd "M-]") 'load-gnu-startup)

(setq org-ellipsis " ◦◦◦ ") 
; (set-face-attribute org-ellipsis '(((:foreground "violet" :underline t))))

;; Set to the location of your Org files on your local system
(setq org-directory "/Users/jay/Dropbox/writing/notationaldata")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "/Users/jay/Dropbox/writing/notationaldata/accountability.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(defun bold-region-or-point ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "*")
        (goto-char (region-beginning))
        (insert "*"))
    (insert "**")
    (backward-char)))

(define-key key-minor-mode-map (kbd "M-s-b") 'bold-region-or-point)

;; add org-opml directory to load-path
(add-to-list 'load-path "/Users/jay/Dropbox/emacs/prelude/personal/org-opml/")

;; load org-opml
(load-library "org-opml")

(require 'org-element-debug)

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
;;  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
;; (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
;  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
 )

(defun bb/next-heading (&rest args) 
(when 

(or
(org-entry-is-done-p) 
(string= (org-get-todo-state) "MISSED") 
)
(outline-next-visible-heading 1))) 



(advice-add 'org-todo :after 'bb/next-heading)

(defmacro my/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun my/org-checkbox-toggle-advice (orig-fn &rest args)
  "Advice to move to next list item on checkbox toggle."
  (my/with-advice 
      ((#'org-update-checkbox-count-maybe
        :after (lambda ()
                 (ignore-errors (org-next-item)))))
    (apply orig-fn args)))

(advice-add #'org-ctrl-c-ctrl-c   :around #'my/org-checkbox-toggle-advice)
(advice-add #'org-toggle-checkbox :around #'my/org-checkbox-toggle-advice)

(define-key key-minor-mode-map (kbd "M-s-k") 'org-cut-subtree)
(define-key key-minor-mode-map (kbd "C-s-k") 'org-cut-subtree)

(require 'multiple-cursors) 
(require 'multiple-cursors-core)
;; This is globally useful, so it goes under `C-x', and `m'
;; for "multiple-cursors" is easy to remember.
(define-key ctl-x-map "\C-m" #'mc/mark-all-dwim)
;; Usually, both `C-x C-m' and `C-x RET' invoke the
;; `mule-keymap', but that's a waste of keys. Here we put it
;; _just_ under `C-x RET'.
(define-key ctl-x-map (kbd "<return>") mule-keymap)

;; Remember `er/expand-region' is bound to M-2!
;; (define-key key-minor-mode-map (kbd "M-#") 'mc/mark-all-dwim)
(global-set-key (kbd "M-2") #'mc/mark-all-dwim)
;; (global-set-key (kbd "M-3") #'mc/mark-next-like-this)
;; (global-set-key (kbd "M-4") #'mc/mark-previous-like-this)
(define-key key-minor-mode-map (kbd "M-3") 'mc/mark-next-like-this)
(define-key key-minor-mode-map (kbd "M-4") 'mc/mark-previous-like-this)


;; These vary between keyboards. They're supposed to be
;; Shifted versions of the two above.
(global-set-key (kbd "M-£") #'mc/unmark-next-like-this)
(global-set-key (kbd "M-$") #'mc/unmark-previous-like-this)

(define-prefix-command 'endless/mc-map)
;; C-x m is usually `compose-mail'. Bind it to something
;; else if you use this command.
(define-key ctl-x-map "m" 'endless/mc-map)

;;; Really really nice!
(define-key endless/mc-map "i" #'mc/insert-numbers)
(define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
(define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
(define-key endless/mc-map "d"
  #'mc/mark-all-symbols-like-this-in-defun)
(define-key endless/mc-map "r" #'mc/reverse-regions)
(define-key endless/mc-map "s" #'mc/sort-regions)
(define-key endless/mc-map "l" #'mc/edit-lines)
(define-key endless/mc-map "\C-a"
  #'mc/edit-beginnings-of-lines)
(define-key endless/mc-map "\C-e"
  #'mc/edit-ends-of-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swiper                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom ivy-height 50
  "Number of lines for the minibuffer window."
  :type 'integer) 
(global-set-key (kbd "C-s") 'swiper)
(setq ivy-display-style 'fancy)
(define-key key-minor-mode-map (kbd "C-7") 'swiper-mc)

;; (require 'wrap-region)
;; (wrap-region-add-wrapper "*" "*" "*")  
;; (wrap-region-add-wrapper "\/" "\/" "\/")  
;; (add-hook 'org-mode-hook 'wrap-region-mode) 

;; wrap-region
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-add-wrappers
   '(
;;     ("*" "*" nil org-mode)
;;     ("/" "/" nil org-mode)
     ("~" "~" nil org-mode)
     ("_" "_" nil org-mode)
     ("*" "*" nil (org-mode message-mode))
     ("/" "/" nil (org-mode message-mode))
      ("$" "$" nil (org-mode latex-mode))
      ))
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (add-hook 'latex-mode-hook 'wrap-region-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:inherit font-lock-warning-face :weight bold))))
)

(require 'god-mode) 
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)


             (require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(define-key god-local-mode-map (kbd ".") 'repeat)

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-c\C-c" #'org-edit-src-exit))

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
 (define-key ctl-x-map "n" #'narrow-or-widen-dwim)

;; Add <p for python expansion

(add-to-list 'org-structure-template-alist
'("p" "#+BEGIN_SRC python\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>") 
'("a" "#+BEGIN_LaTeX\n?\n#+END_LaTeX"  "<literal style=\"latex\">\n?\n</literal>") 
             ) 
(add-to-list 'org-structure-template-alist 
'("l" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n?\n</src>") 
)

(add-hook 'org-mode-hook '(lambda () '(element-debug-mode 1)))

(setq org-cycle-emulate-tab)

;; This setup is tested on Emacs 24.3 & Emacs 24.4 on Linux/OSX
;; org v7 bundled with Emacs 24.3
(setq org-export-odt-preferred-output-format "doc")
;; org v8 bundled with Emacs 24.4
(setq org-odt-preferred-output-format "doc")
;; BTW, you can assign "pdf" in above variables if you prefer PDF format

;; Only OSX need below setup
(defun my-setup-odt-org-convert-process ()
  (interactive)
  (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
    (when (and (eq system-type 'darwin) (file-exists-p cmd))
      ;; org v7
      (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
      ;; org v8
      (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))
    ))
(my-setup-odt-org-convert-process)

(defun insert-file-link-from-clipboard ()
  "Make sure the full path of file exist in clipboard. This command will convert
The full path into relative path and insert it as a local file link in org-mode"
  (interactive)
  (let (str)
    (with-temp-buffer
      (shell-command
       (cond
        ((eq system-type 'cygwin) "getclip")
        ((eq system-type 'darwin) "pbpaste")
        (t "xsel -ob"))
       1)
      (setq str (buffer-string)))

    ;; convert to relative path (relative to current buffer) if possible
    (let ((m (string-match (file-name-directory (buffer-file-name)) str) ))
      (when m
        (if (= 0 m )
            (setq str (substring str (length (file-name-directory (buffer-file-name)))))
          ))
        (insert (format "[[file:%s]]" str)))
    ))
