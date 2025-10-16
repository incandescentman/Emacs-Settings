;; -*- lexical-binding: t -*-

;; General Settings
(setq user-full-name "Jay Dixit"
      abbrev-all-caps nil
      blink-cursor-mode -1
      calendar-latitude 40.7
      case-fold-search t
      ccm-recenter-at-end-of-file t
      clean-buffer-list-delay-general 1
      column-number-mode nil
            display-time-mode t
      grep-highlight-matches 'always
      initial-major-mode 'org-mode
      make-backup-files t
      reb-re-syntax 'string
      standard-indent 3
      tooltip-mode nil
      completion-ignored-extensions '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln"
                                      ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx"
                                      ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/"
                                      "_darcs/" "_MTN/" ".fmt" ".tfm" ".class"
                                      ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl"
                                      ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl"
                                      ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo"
                                      ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp"
                                      ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps"
                                      ".vrs" ".pyc" ".pyo" ".tex" ".mm" "Icon"
                                      ".html" ".zip"))

;; CUA Mode Settings
(setq cua-highlight-region-shift-only t
      cua-mode nil)

;; Deft Settings
(setq deft-directory "~/Dropbox/writing/notationaldata/")

;; Edit Server Settings

(setq edit-server-default-major-mode 'org-mode
      edit-server-new-frame t)

;; Eshell Settings
(setq eshell-load-hook '((lambda () (abbrev-mode -1))))

;; Ido Settings
(setq ido-use-faces t
      ido-use-url-at-point t)

;; Org-Mode Settings
(setq org-M-RET-may-split-line '((item . t))
      org-activate-links '(bracket plain radio tag date footnote)
      org-archive-location "archive/%s_archive::"
      org-ascii-headline-spacing '(1 . 1)
      org-ascii-table-use-ascii-art t
      org-catch-invisible-edits 'smart
      org-ctrl-k-protect-subtree t
      org-custom-properties '(">")
      org-default-notes-file "~/Dropbox/writing/notationaldata/notes.txt"
      org-display-custom-times nil
      org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "SOURCE")
      org-edit-src-content-indentation 4
      org-ellipsis 'org-warning
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies t
      org-extend-today-until 8
      org-fontify-done-headline t
      org-fontify-emphasized-text t
      org-footnote-define-inline t
      org-footnote-section "Footnotes"
      org-footnote-tag-for-non-org-mode-files "Footnotes:"
      org-hide-block-startup nil
      org-hide-emphasis-markers t
      org-html-container-element "div"
      org-html-head-include-scripts nil
      org-html-html5-fancy t
      org-html-postamble nil
      org-html-text-markup-alist '((bold . "<strong>%s</strong>")
                                   (code . "<code>%s</code>")
                                   (italic . "<em>%s</em>")
                                   (strike-through . "<del>%s</del>")
                                   (underline . "<span class=\"underline\">%s</span>")
                                   (verbatim . "<code>%s</code>"))
      org-indent-mode-turns-off-org-adapt-indentation nil
      org-indent-mode-turns-on-hiding-stars nil
      org-insert-mode-line-in-empty-file t
      org-log-done nil
      org-mac-link-skim-highlight-selection-p t
      org-n-level-faces 9
      org-odd-levels-only nil
      org-provide-checkbox-statistics t
      org-replace-disputed-keys nil
      org-special-ctrl-a/e t
      org-src-preserve-indentation t
      org-startup-align-all-tables t
      org-startup-indented t
      org-use-speed-commands t
      org-yank-adjusted-subtrees t
      org2blog/wp-confirm-post nil
      org2blog/wp-default-categories '("inspiration" "personal growth" "miscellany")
      org2blog/wp-keep-new-lines t
      org2blog/wp-show-post-in-browser t
      org2blog/wp-use-tags-as-categories t)

;; OSX Browse Settings
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq osx-browse-prefer-background nil
      osx-browse-prefer-browser "com.google.Chrome"
      osx-browse-prefer-new-window t)

;; Smex Settings
(setq smex-prompt-string "I love you. ")

;; Org Export Settings
(setq org-export-latex-image-default-option "width=20.5cm"
      org-export-time-stamp-file nil
      org-export-with-clocks t
      org-html-head-include-default-style nil
      org-html-toplevel-hlevel 2
      org-indent-indentation-per-level 2
      org-list-allow-alphabetical t
      org-priority-faces nil)

;; Uncomment if needed
;; (setq org-agenda-jump-prefer-future t)
;; (setq org-agenda-skip-scheduled-if-done t)
;; (setq org-agenda-timegrid-use-ampm t)
;; (setq tramp-default-method "ssh")
;; (setq visual-line-mode nil t)
;; (setq org-list-indent-offset 3)

;; A. keep the new, cached version -- recommended
(defun jay/adjust-font-size (&optional frame)
  "Resize default font once per frame size."
  (let* ((frame (or frame (selected-frame)))
         (w     (frame-pixel-width frame)))
    (unless (equal w (frame-parameter frame 'sed--last-width))
      (set-frame-parameter frame 'sed--last-width w)
      (set-face-attribute 'default frame
                          :font "Monaco"
                          :height (if (> w 2540) 230 200)))))

(add-hook 'after-make-frame-functions #'jay/adjust-font-size)
(add-hook 'window-size-change-functions #'jay/adjust-font-size)

;; Highlight completed checklist items in org-mode
(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
 'append)

;; Set package archives
(setq package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("tromey" . "https://tromey.com/elpa/")))

(use-package use-package
  :custom

;;(package-native-compile t)
  (warning-minimum-level :emergency))

(setq use-package-verbose nil) ;; Set to t for debugging

(use-package ox-LaTeX
:defer t
  )
(use-package ox-html
  :defer t)

(defun my/org-remove-links-and-source-lines (backend)
  "Remove lines starting with 'Links ::' or 'Source ::' before export."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*\\(- Links ::\\|- Source ::\\).*$" nil t)
      (replace-match ""))))

(add-hook 'org-export-before-processing-hook #'my/org-remove-links-and-source-lines)

; (setq org-use-property-inheritance t)
  (setq org-fontify-quote-and-verse-blocks t)
;; blank lines before new headings
(setq org-blank-before-new-entry
      '((heading . always)
       (plain-list-item . nil)))
(setq org-return-follows-link t)

;; leave an empty line between folded subtrees
(setq org-cycle-separator-lines 1)

(setq org-support-shift-select (quote always))

(setq org-export-with-smart-quotes t)
(setq org-export-exclude-tags (quote ("noexport" "extra")))

;; (setq org-html-head "<link rel='stylesheet' type='text/css' href='/Users/jay/Dropbox/github/incandescentman.github.io/css/neo.css'>")
(setq org-export-time-stamp-file nil)
(setq org-export-with-clocks t)
(setq org-export-with-drawers t)
(setq org-export-with-section-numbers nil)
(setq org-export-with-timestamps (quote active))
(setq org-export-with-toc nil)

 (setq org-export-date-timestamp-format "%Y-%m-%d %I:%M%p")
 (setq org-export-date-timestamp-format "%B %d, %Y")

 (setq org-export-html-inline-image-extensions (quote ("png" "jpeg" "jpg" "gif" "svg" "tif" "gif")))

;; (setq org-latex-inline-image-rules (quote (("file" . "\\.\\(pdf\\|jpeg\\|gif\\|jpg\\|png\\|ps\\|eps\\|tikz\\|pgf\\|svg\\)\\'"))))

(setq org-latex-default-class "elegant-garamond")

(setq org-latex-inline-image-rules '(("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\|tikz\\|pgf\\|svg\\|gif\\)\\'")))

;; (setq org-export-html-style-include-default t)
 (setq org-export-latex-date-format "%d %B %Y.")
 (setq org-export-latex-emphasis-alist (quote (("*" "\\textit{%s}" nil) ("/" "\\textit{%s}" nil) ("_" "\\underline{%s}" nil) ("+" "\\st{%s}" nil) ("=" "\\verb" t) ("~" "\\verb" t))))
;; (setq org-export-latex-emphasis-alist (quote (("*" "\\emph{%s}" nil) ("/" "\\textit{%s}" nil) ("_" "\\underline{%s}" nil) ("+" "\\st{%s}" nil) ("=" "\\verb" t) ("~" "\\verb" t))))
;; (setq org-export-latex-verbatim-wrap (quote ("\\begin{quote}" . "\\end{quote}")))
 (setq org-export-with-clocks t)
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
(setq org-html-doctype "html5")
(setq org-html-metadata-timestamp-format "%m-%d %a %H:%M")
(setq org-html-postamble nil)
(setq org-html-text-markup-alist
   (quote
    ((bold . "<strong>%s</strong>")
     (code . "<code>%s</code>")
     (italic . "<em>%s</em>")
     (strike-through . "<del>%s</del>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>"))))
(setq org-html-toplevel-hlevel 2)

(defun org-html-export-to-html-and-open
   (&optional async subtreep visible-only body-only ext-plist)
   (interactive)
(let* ((outfile (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'html outfile async subtreep visible-only body-only ext-plist #'find-file)))

(defun my-org-export-change-options (plist backend)
 (cond
  ((equal backend 'html)
  (plist-put plist :with-toc nil)
  (plist-put plist :section-numbers nil))
  ((equal backend 'latex)
  (plist-put plist :with-toc nil)
  (plist-put plist :section-numbers t)))
 plist)

;; (add-to-list 'org-export-filter-options-functions 'my-org-export-change-options)

(defun jbd-org-export-format-drawer (name content)
  "Export drawers to drawer HTML class."
  (setq content (org-remove-indentation content))
  (format "@<div class=\"drawer\">%s@</div>\n" content))
(setq org-export-format-drawer-function 'jbd-org-export-format-drawer)

(with-eval-after-load 'ox
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(setq lexical-binding t)

(add-to-list 'load-path "~/emacs/emacs-settings/")
(add-to-list 'load-path "/Users/jay/emacs/emacs-settings/timeline/")

(use-package flyspell
  :ensure nil                    ; built-in
  :hook ((text-mode . flyspell-mode)
         (org-mode  . flyspell-mode))
  :bind (:map flyspell-mode-map
              ("C-." . nil))     ; unbind default
  :init
  ;; --- Hunspell setup ---
;; Add to your :init block *instead* of the previous setq for this var
(add-to-list
 'ispell-dictionary-alist
 '("en_US-large" "[[:alpha:]]" "[^[:alpha:]]" "[']"
   nil ("-d" "en_US-large") nil utf-8))
  (setq ispell-program-name "hunspell"
        ispell-dictionary   "en_US-large"           ; default dict
        ispell-personal-dictionary "~/emacs/Spelling/personal.dic"
        ispell-hunspell-dictionary-paths-alist
        '(("en_US"       "~/emacs/Spelling")
          ("en_US-large" "~/emacs/Spelling"))
        ;; teach Flyspell about contractions etc.
        flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-mark-duplications-exceptions
        '((nil "that" "had" "really" "very" "much")
          ("\\`francais\\>" "nous" "vous")))
  :config
  ;; Skip #+begin_src ... #+end_src blocks in Org
  (defun my/org-setup-flyspell ()
    (when (derived-mode-p 'org-mode)
      (make-local-variable 'ispell-skip-region-alist)
      (add-to-list 'ispell-skip-region-alist
                   '("^#\\+begin_src" . "^#\\+end_src"))))
  (add-hook 'org-mode-hook #'my/org-setup-flyspell))

(defun add-word-to-personal-dictionary ()
  "Add the word at point to the personal dictionary."
  (interactive)
  (require 'ispell)
  (let ((word (or (thing-at-point 'word t)
                  (user-error "No word at point"))))
    ;; Tell the ispell process to add WORD
    (ispell-send-string (format "*%s\n" word))
    ;; Tell ispell to save the personal dictionary now
    (ispell-send-string "#\n")
    (ispell-pdict-save t)
    (message "Added \"%s\" to personal dictionary" word)))

;; Disable mouse highlights globally
(setq mouse-highlight nil)

;; Avoid cursor in non-selected windows
(setq-default cursor-in-non-selected-windows nil)

(setq org-cycle-emulate-tab nil)

;; Always split windows vertically (side-by-side)
(setq split-width-threshold 0)
(setq split-height-threshold nil)

;; Minimum size for split windows
(setq window-min-width 20)
(setq window-min-height 5)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))

;; (setq ring-bell-function (lambda () (play-sound-file "~/sounds/InkSoundStroke3.mp3")))

;; turn off alarms completely
(setq ring-bell-function 'ignore)

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook  #'visual-line-mode)

(auto-fill-mode -1) ; turn off fill mode, which adds random line breaks in my text files:
(add-hook 'text-mode-hook  #'(lambda () (auto-fill-mode -1)))
(add-hook 'markdown-mode-hook  #'(lambda () (auto-fill-mode -1)))
(add-hook 'message-mode-hook  #'(lambda () (auto-fill-mode -1)))

(setq-default sentence-end-double-space nil)

(delete-selection-mode 1)

(setq buffer-save-without-query nil)

(setq locate-command "mdfind")

(add-hook 'emacs-lisp-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'css-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'html-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'html-helper-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'shell-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'shell-script-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'term-mode-hook (lambda () (abbrev-mode -1)))

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

;;(use-package buffer-stack
;; defer)

(global-set-key [(s-right)] 'buffer-stack-down)
(global-set-key [(s-left)] 'buffer-stack-up)

(global-set-key [(A-right)] 'buffer-stack-down)
(global-set-key [(A-left)] 'buffer-stack-up)

;; meaningful names for buffers with the same name
;; from prelude
;; http://bit.ly/1Woabxz
(use-package uniquify
:ensure nil
:defer
:init
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

:config

; Uniqify new buffers
(defvar new-buffer-count 0)

;; open new buffers without prompting me for a filename global counter to ensure every new buffer will be unique:

(defun new-buffer ()
 (interactive)
 (setq new-buffer-count (+ new-buffer-count 1))
 (switch-to-buffer (concat "buffer" (int-to-string new-buffer-count)))
 (org-mode))

(defun new-lisp-buffer ()
 (interactive)
 (setq new-buffer-count (+ new-buffer-count 1))
 (switch-to-buffer (concat "buffer" (int-to-string new-buffer-count)))
 (emacs-lisp-mode))

)

(defun org-new-scratch-buffer ()
  (interactive)
  (insert "* oh hi there! " (format-time-string "%F %l:%M%P\n\n"))
;; (org-tree-to-indirect-buffer 'current-window)
(org-narrow-to-subtree)

  )

(defun conditionally-disable-abbrev ()
  ""
  (if (string-match "smex-" (format "%s" this-command))
      (abbrev-mode -1)))

(add-hook 'minibuffer-setup-hook 'conditionally-disable-abbrev)
(add-hook 'minibuffer-exit-hook (lambda () (abbrev-mode 1)))
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (abbrev-mode -1)))

(setq
read-buffer-completion-ignore-case t
read-file-name-completion-ignore-case t)

(use-package reveal-in-finder

  :bind)

(defun cycle-hyphenation-or-toggle-item ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'org-toggle-item)
    (cycle-hyphenation)))

;; ---------------------------------------------------------------------
;; Around-advice: tweak `org-archive-location`, then run the real command
;; ---------------------------------------------------------------------

(defun sed--org-archive-subtree (orig-fn &rest args)
  "Archive subtree under a heading named after its parent.
Keeps identical behaviour to the old `defadvice', but uses `advice-add'."
  (let ((org-archive-location
         (if (save-excursion
               (org-back-to-heading)
               (> (org-outline-level) 1))
             ;; archive into a drawer under the parent's headline
             (concat (car (split-string org-archive-location "::"))
                     "::* "
                     (car (org-get-outline-path)))
           org-archive-location)))
    (apply orig-fn args)))                 ; run the original command

(advice-add 'org-archive-subtree :around #'sed--org-archive-subtree)

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (
; ("k" . org-kill-note-or-show-branches)
                                      ("q" . bh/show-org-agenda)
                                      ("h" . org-agenda-schedule)
                                      ("d" . org-deadline)
                                      ("w" . org-refile)
                                      ("y" . org-archive-subtree-default-with-confirmation)
                                      ("a" . org-archive-subtree)
                                      ("s" . org-schedule)
                                      ("x" . org-mark-subtree)
                                      ("z" . org-add-note)
                                      ("m" . (lambda nil (interactive) (org-todo "MISSED")))

                                      ("A" . org-archive-subtree-default-with-confirmation)
                                      ("N" . org-forward-heading-same-level)
                                      ("P" . org-backward-heading-same-level)
                                      ("J" . org-clock-goto)
                                      ("Z" . ignore))))

(setq org-latex-image-default-width "370pt");; new value just for book export
;; (setq org-latex-image-default-width "180pt") good value, works for QIAGEN for exampl
(setq   org-export-allow-bind-keywords t)

(setq org-highlight-latex-and-related '(latex))

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

;; (add-hook 'after-init-hook 'org-agenda-list)
(use-package org-inlinetask
  :ensure nil ;; <- don't try to install it
  :defer)
;; Overwrite the current window with the agenda
;; (setq org-agenda-window-setup 'current-window)

;; Delete IDs When Cloning
(setq org-clone-delete-id t)

;; start org in folded mode
(setq org-startup-folded nil)

;; allow alphabetical list entries, i.e. "a. this b. that c. another"
(setq org-alphabetical-lists t)

;; fast TODO selection
(setq org-use-fast-todo-selection t)

;; more org settings
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-src-fontify-natively t)

;; (add-to-list 'load-path (expand-file-name "~/git/org-mode/lisp"))

(setq org-todo-keywords
   '((sequence "TODO" "STARTED" "|" "DONE")
    ;; (sequence "|" "SKIPPING")
(sequence "MISSED" "|" "DONE")
(sequence "MISSED ❌" "|" "DONE")
(sequence "STARTED" "|" "DONE ✅")
(sequence "STARTED 🏁" "|" "DONE ✅")
(sequence "Example:" "|")
    (sequence "NEED TO INVOICE" "INVOICED" "|" "PAID")
    (sequence "|" "CANCELED")
    ;; (sequence "EXPOSURE ACTIVITY" "|")
    ;; (sequence "MSG" "MESSAGE" "|" "CALLED")
    ;; (sequence "COMMITTED" "RESULTS" "|")
    ;; (sequence "WAITING" "DAILIES" "WEEKLIES" "MONTHLIES" "QUARTERLIES" "YEARLIES" "GOALS" "SOMEDAY" "|")
    ;; (sequence "QUESTION" "|" "ANSWERED")
    ;; (sequence "QUESTIONS" "|" "ANSWERS")
(sequence "Original:" "|" "Revised:")
    ;; (sequence "STRATEGY" "|")
    ;; (sequence "TIP" "|")
(sequence "REWARD" "|" "REWARDED")
    ;; (sequence "NOTES" "RESEARCH" "POINT" "NARRATIVE" "ANECDOTE" "WRITING" "|")
    ;; (sequence "PART" "HED" "HEDTK" "|")
    ;; (sequence "IF" "THEN" "|")
    ;; (sequence "COWRITE" "|" "DONE")
(sequence "TO PROCESS" "|" "PROCESSED")
;; (sequence "GOAL" "PLAN" "NOTE" "|" "DONE")
    ))

;; Set todo keyword colors
(setq org-todo-keyword-faces
 '(
   ("PROCESSED" :foreground "LavenderBlush" :background "darkgrey" :weight bold)
   ("NEXT" :background "medium sea green" :foreground "white" :weight bold)
  ("ACTION" :foreground "medium sea green" :weight bold)
  ("WAITING" :background "yellow" :foreground "purple" :weight bold)
  ("EVENT" :background "gray25" :foreground "white" :weight bold)
    ("PROJECT" :background "firebrick" :foreground "white" :weight bold)
  ("STARTED" :background "dodger blue" :foreground "white" :weight bold)
  ("DONE" :foreground "white" :background "#228B22"
          :box (:line-width (2 . 1) :color "#228B22")
          :weight bold)))

(use-package hl-todo
:defer t
; ensure t
:hook (org-mode . hl-todo-mode)
 :config

 (setq hl-todo-include-modes '(org-mode))
 (setq hl-todo-keyword-faces
    '(("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("REQUESTED" . "#dc8cc3")
     ("IMPORTANT" . "#dc8cc3")
     ("CONCLUSION" . "#dc8cc3")
     ("SENT" . "#dc8cc3")
     ("ANSWER" . "#dc8cc3")
     ("REJECTED" . "Red")
     ("TK" . "Red")
     ("tktk" . "Red")
     ("xyz" . "Red")
     ("tk" . "Red")
     ("tktks" . "Red")
   ("Q " . (:foreground "white" :background "#C92228"))
("QUESTION" . (:foreground "white" :background "#C92228"))
     ("GOAL" . "DarkGreen")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . (:foreground "white" :background "#228B22"
                            :box (:line-width (2 . 1) :color "#228B22")))
     ("ADMITTED" . "DarkGreen")
     ("NOTE" . "#d0bf8f")
     ("STARTED" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("MAYBE" . "#cc9393")
     ("ACTION" . "#cc9393")
     ("PROJECT" . "#cc9393")
     ("XXX" . "#cc9393")
     ("XXXX" . "#cc9393")
     ("\\?\\?\\?" . "#cc9393"))))

(setq org-priority-start-cycle-with-default nil)

(defun new-org-delete-backward-char (N)
  (interactive "p")
  (cond ((region-active-p)
         (delete-region
          (region-beginning)
          (region-end)))
        ((looking-back "^\\*+[ ]*") ;; one or more stars
         (previous-line)
         (end-of-line))
(t
(org-delete-backward-char N)
)))

'(initial-major-mode (quote org-mode))
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

'(org-indent-mode-turns-off-org-adapt-indentation nil)
'(org-indent-mode-turns-on-hiding-stars nil)
'(org-insert-mode-line-in-empty-file t)
'(org-list-indent-offset 3)
'(org-log-done (quote time))
'(org-n-level-faces 9)
'(org-odd-levels-only nil)
'(org-indent-mode 1)
'(org-priority-faces nil)
'(org-provide-checkbox-statistics t)

(defvar my-org-directory "~/Dropbox/writing/notationaldata/")
(defvar my-org-default-notes-file (concat my-org-directory "notes.txt"))

(setq org-directory my-org-directory)
(setq org-default-notes-file my-org-default-notes-file)

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
	(org-back-to-heading)
	(org-update-parent-todo-statistics)))))
;;; -----------------------------------------------------------
;;; Keep parent TODO cookies in sync after killing a line
;;; -----------------------------------------------------------

(defun sed--update-parent-cookie (&rest _)
  "Refresh parent checkbox / todo statistics after editing."
  (myorg-update-parent-cookie))   ; your existing helper

(advice-add 'org-kill-line   :after #'sed--update-parent-cookie)
(advice-add 'kill-whole-line :after #'sed--update-parent-cookie)

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
  (when (org-in-regexp org-link-bracket-re 1)
    (kill-new (org-link-unescape (org-match-string-no-properties 1)))))

;; (use-package mm-url) ; to include mm-url-decode-entities-string

;; (defun org-insert-link ()
;;   "Insert org link where default description is set to html title."
;;   (interactive)
;;   (let* ((url (read-string "URL: "))
;;          (title (get-html-title-from-url url)))
;;     (org-insert-link nil url title)))

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

(define-key key-minor-mode-map (kbd "C-s-0") 'show-all)
(define-key key-minor-mode-map (kbd "C-s-a") 'show-all)

(define-key key-minor-mode-map (kbd "<M-s-return>") 'org-inlinetask-insert-task)

(global-auto-revert-mode -1)

;; Ensure palimpsest is loaded before enabling the mode
(require 'palimpsest nil t)
(add-hook 'find-file-hook (lambda ()
                            (when (featurep 'palimpsest)
                              (palimpsest-mode 1))))

;; (setq org-pomodoro-format "Pomodoro: %s")
;; (setq org-pomodoro-killed-sound "~/sounds/autodestructsequencearmed_ep.mp3")
(setq org-pomodoro-length 25)
(setq org-pomodoro-short-break-length 0.5)
(setq org-pomodoro-long-break-length 30)
(setq org-pomodoro-long-break-sound "/Users/jay/Dropbox/audio/sounds/InkSoundStroke3.mp3")
 (setq org-pomodoro-play-ticking-sounds nil)
;; (setq org-pomodoro-short-break-format "Short Break: %s")
(setq org-pomodoro-short-break-sound "/Users/jay/Dropbox/audio/sounds/Metal_Gong-Dianakc-109711828.mp3")

;; (setq org-pomodoro-finished-sound "/Users/jay/Dropbox/audio/sounds/InkSoundStroke3.mp3")

(setq org-pomodoro-finished-sound "/Users/jay/Dropbox/audio/sounds/Horse-Gallop.mp3")

;; (setq org-pomodoro-ticking-sound "~/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/com.taptanium.thunderstorm.DreamQuest_preview.m4a")

(defun pomodoro-start ()
  (interactive)
(org-todo 'done)
(previous-line)
(condition-case nil
(org-pomodoro)
 (error nil))
)

(defun pomodoro-stop ()
  "Stop the current org-pomodoro timer."
  (interactive)
  (org-pomodoro-kill))

(add-hook 'org-pomodoro-finished-hook #'(lambda ()

(do-applescript (format "
ignoring application responses
	tell application \"System Events\"
keystroke \"B\" using {command down, shift down, option down, control down} -- start Pomodoro One
key code {118}
end tell
end ignoring

set now to current date
set nowTime to (hours of now) & \":\" & (minutes of now)
set pomodoroStart to (current date) - 25 * minutes
set pStartTime to (hours of pomodoroStart) & \":\" & (minutes of pomodoroStart)
set achieved to text returned of (display dialog \"What did you achieve in this Pomodoro?\" default answer \"\")
set entry_text to \"# Bookwriting:\" & pStartTime & \" - \" & time string of now & \"

\" & achieved & \"

#pomodoro \"

"))
))

(use-package org-mac-link
:defer t
  )

(add-hook 'find-file-hooks 'goto-address-prog-mode)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

'(cua-enable-cua-keys (quote shift))
'(cua-highlight-region-shift-only t)
'(cua-mode nil nil (cua-base))
'(send-mail-function (quote sendmail-send-it))
'(shift-select-mode nil)
'(transient-mark-mode t)

'(message-send-mail-function (quote message-send-mail-with-sendmail))
'(mail-send-mail-function (quote message-send-mail-with-sendmail))
'(setq mail-user-agent 'message-user-agent)
'(global-set-key [(A-W)]  'buffer-stack-bury-and-kill)
'(ns-right-command-modifier (quote meta))
'(ns-tool-bar-display-mode (quote both) t)
'(ns-tool-bar-size-mode nil t)
;; '(standard-indent 3)
'(ns-function-modifier (quote meta))
(transient-mark-mode t)
(tooltip-mode -1)
(setq ns-function-modifier 'hyper)
;; open files in an existing frame instead of a new frame
(setq ns-pop-up-frames nil)

;; ------------------------------------------------------------------
;; File-extension → major-mode rules  (prepend to keep highest priority)
;; ------------------------------------------------------------------
(setq auto-mode-alist
      (append
       '(("\\.txt\\'"                       . org-mode)
         ("\\.calca\\'"                     . org-mode)
         ("\\.tmode\\'"                     . text-mode)
         ("\\.msg\\'"                       . message-mode)
         ("\\.org\\'"                       . org-mode)
         ("\\.org_archive\\'"               . org-mode)
         ("\\.txt_archive\\'"               . org-mode)
         ("README\\'"                       . org-mode)
         ("shared-functions\\'"             . emacs-lisp-mode)
         ("gnu-emacs-startup\\'"            . emacs-lisp-mode)
         ("\\.css\\'"                       . css-mode)
         ("\\.rb\\'"                        . ruby-mode)
         ("Rakefile\\'"                     . ruby-mode)
         ("\\.js\\(?:on\\)?\\'"             . js2-mode)
         ("\\.xml\\'"                       . nxml-mode)
         ("\\.fountain\\'"                  . fountain-mode)
         ("COMMIT_EDITMSG\\'"               . diff-mode)
         ("\\.md\\'"                        . markdown-mode)
         ("\\.mdx\\'"                        . markdown-mode)
         ("\\.abbrev_defs\\'"               . emacs-lisp-mode)
         ("\\.html?\\'"                     . web-mode)
         ("\\.astro?\\'"                    . web-mode)
)
       auto-mode-alist))                    ; keep earlier rules, too

'(org-support-shift-select (quote always))

(setq default-directory "~/Dropbox/writing/" )

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/emacs/backup/per-save")))

(setq make-backup-files t        ; backup of a file the first time it is saved.
   backup-by-copying t        ; don't clobber symlinks
   version-control t         ; version numbers for backup files
   delete-old-versions t       ; delete excess backup files silently
   delete-by-moving-to-trash t
   kept-old-versions 6        ; oldest versions to keep when a new numbered backup is made (default: 2)
   kept-new-versions 9        ; newest versions to keep when a new numbered backup is made (default: 2)
   auto-save-default t        ; auto-save every buffer that visits a file
   auto-save-timeout 20       ; number of seconds idle time before auto-save (default: 30)
   auto-save-interval 200      ; number of keystrokes between auto-saves (default: 300)
vc-make-backup-files t ; Make backups of files, even when they're in version control
   )

(defun force-backup-of-buffer ()
 ;; Make a special "per session" backup at the first save of each
 ;; emacs session.
 (when (not buffer-backed-up)
  ;; Override the default parameters for per-session backups.
  (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
     (kept-new-versions 3))
   (backup-buffer)))
 ;; Make a "per save" backup on each save. The first save results in
 ;; both a per-session and a per-save backup, to keep the numbering
 ;; of per-save backups consistent.
 (let ((buffer-backed-up nil))
  (backup-buffer)))

(add-hook 'before-save-hook 'force-backup-of-buffer)

;; (setenv "PATH" (shell-command-to-string "source ~/.profile; echo -n $PATH"))

(setenv "PATH"
        (mapconcat
         #'identity
         '("/opt/homebrew/bin" "/opt/homebrew/sbin"
;; "/Users/jay/work/bin"
;; "/Users/jay/Dropbox/GitHub/terminal_velocity/bin"
;; "/Users/jay/.pyenv/shims" "/Users/jay/.pyenv/bin"
           "/usr/local/bin"                ; Homebrew Intel fallback
           "/usr/bin" "/bin" "/usr/sbin" "/sbin"
           "/Library/TeX/texbin"
           "/Applications/iTerm.app/Contents/Resources/utilities"
           "/Users/jay/Dropbox/apps/bash-scripts"
;; "/Users/jay/.rvm/bin"
)
         ":"))

;; (use-package eshell-autojump)

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

(setq ido-save-directory-list-file "~/emacs/.savefile/ido.hist")
(setq projectile-known-projects-file "~/emacs/.savefile/projectile-bookmarks.eld")

(use-package recentf
:defer t
:config
(setq recentf-save-file "/Users/jay/emacs/recentf/recentf")
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(run-with-idle-timer 60 t 'recentf-save-list) ; save recentf automatically so recent files are stored even in the case of abnormal exit
)

(setq buffer-stack-show-position nil)

(setq buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Archive*" "*Agenda*" "*fontification*" "*Warnings*" "Calendar" "*Tree:*" "*spacemacs*" "*scratch*" "*Backtrace*" "todo" "TODO" "*org-roam*")))
;; the above I don't think works or at least I don't know how to add to it

;; The below definitely works.
(defun buffer-stack-filter-regexp (buffer)
  "Non-nil if buffer is in buffer-stack-tracked."
  (not (or (string-match "April\\|Help\\|helpful\\|minibuf\\|echo\\|conversion\\|converting\\|agenda\\|server\\|Messages\\|tex\\|Output\\|autoload\\|Customize\\|address\\|clock\\|Backtrace\\|Completions\\|grep\\|Calendar\\|archive\\|Compile-Log\\|tramp\\|helm\\|Alerts\\|Minibuf\\|Agenda\\|Echo\\|gnugol\\|RNC\\|ediff\\|widget\\|melpa\\|git\\|hydra\\|which\\|fontification\\|Helm\\|popwin\\|Custom\\|Warnings\\|tags\\|hours\\|gnugol\\|guide-key\\|scratch\\|vc\\|Compile\\|mm\\|nntpd\\|spacemacs\\|Gnorb\\|quelpa\\|eldoc\\|tar\\|wordnik\\|escape\\|trace\\|debug\\|emacs\\|Re-Builder\\|Ilist\\|orgmode\\|todo\\|loaddefs\\|gnu\\|elpa\\|version\\|alert\\|counsel\\|consult\\|*info*\\|*affe*\\|pixel\\|org-roam\\|width\\|jka\\|123244\\|ediff\\|Org parse\\|EGLOT\\|later" (buffer-name buffer))
     (member buffer buffer-stack-untracked))))
(setq buffer-stack-filter 'buffer-stack-filter-regexp)

;; ------------------------------------------------------------------
;; recentf — paths & patterns we never want to record
;; ------------------------------------------------------------------
(setq recentf-exclude
      '(
        ;; folders / globs
        "/var/"          "elpa"          "cache"          "archive"
        "contacts"       "gnugol"        "helm"           "ido\\.hist"
        "ido\\.last"     "koma"          "message"        "mu2"
        "org-clock-save\\.el" "paths"    "persp-auto"     "recent-addresses"
        "recentf"        "roam/notes"    "rollback-info"  "ssh:"  "shared"
        "agenda"         "bookmark"      "bookmarks"      "Applications"
        "Before"         "Calendar"      "LaTeX"          "Shared"
        "System"         "cache"         "*Org tags*"     "*emacs-settings*"
        "*sent mail*"

        ;; VCS / config dirs
        "\\.git" "\\.emacs\\.d"

        ;; binary or bulky file types
        "\\.\\(?:avi\\|bmk\\|bmp\\|flv\\|gif\\|gz\\|ics\\|jabber\\|jpe?g\\|mkv\\|mov\\|mp[34g]\\|mpeg\\|ogg\\|ogm\\|pdf\\|png\\|pptx?\\|tif\\|wav\\|wmv\\|xls[x]?\\)\\'"
        ;; office / markup you never reopen from recentf
        "\\.\\(?:docx?\\|od[tspg]\\|tex\\|html?\\)\\'"
        ;; src you skip
        "\\.Icon" "\\.css" "\\.xml"
        ))

(setq grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink"))

(setq grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot"
    "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl"
    "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg"
    "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js"
    "*.doc" "*.pdf" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "#*"))

(setq dired-omit-files "^\\.[^.]\\|\\.tex$\\|Icon*"

dired-omit-extensions
  (quote
  (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".html" ".tex" ".git" ".skim" "docx" "mp4" "pptx" "jpeg"))
)

;; (setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$\\|\\.DS_Store$\\|\\.doc$\\|\\.docx$\\|\\.ini$\\|\\.rtf$\\|\\Icon*\\|\\*.html")

;; Enable hiding details by default in all Dired buffers
(with-eval-after-load 'dired
  (require 'dired-x)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

;; Show only filenames (one per line) in Dired
(setq dired-listing-switches "-1a")

;; Optional: Ensure it also works for dired buffers created in other ways
(with-eval-after-load 'dired
  (setq dired-hide-details-hide-symlink-targets nil)  ; Show symlink targets
  (setq dired-hide-details-hide-information-lines t)) ; Hide the "total" line

(use-package wc-mode
:defer t
:config
(add-hook 'org-mode-hook 'wc-mode)

(defun word-count (arg)
(interactive "r")
(count-words arg)
 )

(setq wc-modeline-format "[Words: %tw")

(defun count-characters ()
(interactive)
(setq wc-modeline-format "[Words: %tw, Chars: %tc]")
 )
  )

(defun org-mac-link-chrome-insert-frontmost-url-with-quotes ()
  "with quotes"
  (interactive)
  (insert "\"")
  (org-mac-link-chrome-insert-frontmost-url)
  (insert ",\"")
  )

(defun web-research ()
  (interactive)
  (insert "#+BEGIN_QUOTE\n")
  (let ((p (point)))
    (insert "\n#+END_QUOTE\nSource: ")
    (org-mac-link-chrome-insert-frontmost-url)
    (goto-char p))
  (pasteboard-paste)
  (next-line)
  (next-line)
  (next-line)
    (insert "\n"))

(defun web-research-quotes ()
  (interactive)
  (insert "\"")
    (org-mac-link-chrome-insert-frontmost-url)
       (insert "\,\" "))

(defalias 'html2org-clipboard 'chatgpt2org)

(defun html2text-clipboard ()
 "Convert clipboard contents from HTML to plain text and paste."
 (interactive)
 (setq cmd "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t plain")
 (kill-new (shell-command-to-string cmd))
 (yank))

(defun jd-org-current-time ()
  "foo"
  (interactive)
(insert (format-time-string "[%H:%M]"))
  )

(defun jd-org-today ()
  "insert a new heading with today's date"
  (interactive)
(smart-org-meta-return-dwim)
  (org-insert-time-stamp (current-time))
(insert "\n")
)

(defun jd-org-approach ()
 "insert a new heading with today's date"
 (interactive)
(insert "\n** ")
 (org-insert-time-stamp (current-time))
(insert "\n")
(insert "*** TODO morning pages\n")
(insert "*** TODO meditate\n")
(insert "*** TODO work on book\n")
(insert "**** TODO pomodoro #1\n")
(insert "**** TODO pomodoro #2\n")
(insert "*** TODO an approach\n")
)

(defun jd-org-today-and-accountability ()
  "insert a new heading with today's date"
  (interactive)
(insert "\n** committed actions: ")
  (org-insert-time-stamp (current-time))
  (insert " [0%]\n")

(insert "*** TODO wake up by 8:30am\n")
(insert "*** TODO blue light\n")

(insert "*** TODO morning pages\n")
(insert "*** TODO bookwriting [0/8]\n")
(insert "**** TODO bookwriting #1\n")
(insert "**** TODO bookwriting #2\n")
(insert "**** TODO bookwriting #3\n")
(insert "**** TODO bookwriting #4\n")
(insert "**** TODO bookwriting #5\n")
(insert "**** TODO bookwriting #6\n")
(insert "**** TODO bookwriting #7\n")
(insert "**** TODO bookwriting #8\n")
(insert "*** TODO ")
; (left-char)
  )

(defun jd-org-2-book-and-accountability ()
 "insert a new heading with today's date"
 (interactive)
(insert "\n** ")
 (org-insert-time-stamp (current-time))
(insert "\n")
(insert "*** TODO bookwriting [0/2]\n")
(insert "**** TODO bookwriting #1\n")
(insert "**** TODO bookwriting #2\n")
(left-char)
 )

(defun jd-org-today-and-book ()
 "Insert a new heading with today's date and start org-pomodoro timer."
 (interactive)
 (insert "\n** ")
 (org-insert-time-stamp (current-time))
 (insert "\n")
 (insert "*** TODO bookwriting\n")
 (pomodoro-start))

(defun pomidor-start ()
 "Insert a new heading with today's date and start pomidor timer."
 (interactive)
 (insert "\n** ")
 (org-insert-time-stamp (current-time))
 (insert "\n")
 (insert "*** TODO bookwriting\n")
 (pomidor))

(defun jd-pomodoro-sprint ()
 "Insert a new heading with today's date and pomodoro sprint subheading, then start timer."
 (interactive)
 (insert "\n** ")
 (org-insert-time-stamp (current-time))
 (insert "\n")
 (insert "*** TODO pomodoro sprint\n")
 (pomodoro-start))

(defun jd-clock-in ()
 "Insert a new heading with current time and clock in."
 (interactive)
 (org-insert-heading)
 (org-insert-time-stamp (current-time))
 (org-clock-in)
 (next-line 2))

(defalias 'lowercase-region 'downcase-region)



(defvar lawlist-movement-syntax-table
  (let ((st (make-syntax-table)))
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
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))))

(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-completion-use-ido nil)
(setq org-refile-use-outline-path t) ; Show full paths for refiling


(defun my-org-files-list ()
 (delq nil
  (mapcar (lambda (buffer)
   (buffer-file-name buffer))
   (org-buffer-list 'files t))))

(setq org-refile-targets '((my-org-files-list :maxlevel . 4)))

;; allow refile to create parent tasks with confirmation:
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; (setq org-goto-interface 'outline-path-completion org-goto-max-level 3)

(defvar refile-region-format "\n%s\n")

(defvar refile-region-position 'top
  "Where to refile a region. Use 'bottom to refile at the
end of the subtree. ")

(defun jay-refile-region (beg end copy)
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
          (org-end-of-meta-data))
        (insert (format refile-region-format text))))))

(defun move-region-or-subtree-to-other-window ()
 (interactive)
 (when (and (eq 'org-mode major-mode)
       (not (region-active-p)))
  (org-mark-subtree))
 (let ((text (buffer-substring (region-beginning) (region-end))))
  (delete-region (region-beginning) (region-end))
  (other-window 1)
  (insert text)))

(defun copy-region-to-other-window ()
 (interactive)
 (when (region-active-p)
  (let ((text (buffer-substring (region-beginning) (region-end))))
   (other-window 1)
   (insert text)
   (other-window -1))))

(defun remove-links/jay (beg end)
  "Replace an Org bracket link at point with its description (or address if no description).
If a region is active, do this for *all* bracket links in the region.

Bracket links look like [[url][desc]] or [[url]]."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (use-region-p)
      ;; Region: process every match inside the region safely.
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward org-link-bracket-re nil t)
            (let ((desc (if (match-end 2)
                            (match-string-no-properties 2)
                          (match-string-no-properties 1))))
              (replace-match desc t t))))
        (message "Removed links in highlighted region"))
    ;; No region: behave like your original function at point.
    (if (org-in-regexp org-link-bracket-re 1)
        (save-excursion
          (let* ((remove (list (match-beginning 0) (match-end 0)))
                 (description (if (match-end 2)
                                  (org-match-string-no-properties 2)
                                (org-match-string-no-properties 1))))
            (apply #'delete-region remove)
            (insert description))
          (message "Removed link at point"))
      (message "No link at point"))))

(defun remove-hyperlinks ()
  "Remove Org bracket links depending on context:
- If region is active, remove only links in the region.
- Else if point is on a link, remove just that link.
- Otherwise, prompt to remove all links in the buffer."
  (interactive)
;;  (require 'org)  ; Make sure org functions/variables are loaded
  (cond
   ;; 1) If region is highlighted, remove links only in that region:
   ((use-region-p)
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (replace-match (match-string 2) t t))))

   ;; 2) Else, if point is on a link, remove just that link:
   ((org-in-regexp org-link-bracket-re 1)
    (let ((visible-text (match-string-no-properties 2)))
      (replace-match visible-text t t)))

   ;; 3) Otherwise, prompt and remove all links in buffer:
   (t
    (when (y-or-n-p "Remove all hyperlinks in the buffer? ")
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (replace-match (match-string 2) t t))))))

(defun remove-org-links-and-newlines ()
 "Remove all Org mode style links from the current buffer, also removing any newlines from the description."
 (interactive)
 (save-excursion
  (goto-char (point-min))
  (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" nil t)
   (let ((description (match-string 2)))
    ;; Replace newline characters with a space in the description
    (setq description (replace-regexp-in-string "\n" " " description))
    (replace-match description)))))

(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
	 (tocpl (mapcar (function
			 (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
	 (prompt (append '("File name: ") tocpl))
	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-string fname tocpl)))))

(global-set-key [(control x)(control r)] 'recentf-open-files-compl)

(defun visit-most-recent-file ()
  "Visits the most recently open file in `recentf-list' that is not already being visited."
  (interactive)
  (let ((buffer-file-name-list
         (mapcar 'file-truename
                 (remove nil (mapcar 'buffer-file-name (buffer-list)))))
        (recent-files-names (delete-dups (mapcar 'file-truename recentf-list)))
        most-recent-filename)
    (dolist (filename recent-files-names)
      (unless (member filename buffer-file-name-list)
        (setq most-recent-filename filename)
        (cl-return)))
    (ignore-errors (find-file most-recent-filename))))

(defun path-copy-path-to-clipboard ()
 "Copy the full current filename and path to the clipboard."
 (interactive)
 ;; Define a local variable 'filename' to store the path of the current file or directory.
 (let ((filename (if (equal major-mode 'dired-mode)
           default-directory ; If in 'dired-mode', use the current directory.
          (buffer-file-name)))) ; Otherwise, use the file path of the current buffer.
  ;; Check if 'filename' is non-nil and is not the latest entry in the kill ring.
  (when (and filename (not (equal filename (car kill-ring))))
   ;; Add 'filename' to the kill ring without duplicating it if it's already there.
   (kill-new filename)
   ;; Use a temporary buffer to copy 'filename' to the system clipboard.
   (with-temp-buffer
    (insert filename)
    (clipboard-kill-region (point-min) (point-max))))
  ;; Provide feedback to the user.
  (when filename
   (message "Copied to clipboard: %s" filename)))
 ;; Use custom function to ensure the clipboard content is also in the kill ring.
 (push-kill-ring-pasteboard-to-MacOS-clipboard))

(defun path-copy-path-to-clipboard-with-quotes ()
  "Copy the full current filename and path to the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename)))
  (push-kill-ring-pasteboard-to-MacOS-clipboard)
)

;; -*- lexical-binding: t; -*-

(defun jay/save-all-buffers ()
  "Silently save every buffer that is visiting a file.
If the file does not yet exist on disk, create it without
confirmation.  Non–file‑visiting buffers are ignored."
  (interactive)
  (let ((confirm-nonexistent-file-or-buffer nil)) ; suppress “create file?” prompt
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when buffer-file-name
          (when (or (buffer-modified-p)
                    (not (file-exists-p buffer-file-name)))
            (save-buffer)))))))

(global-set-key (kbd "M-C-N") 'outline-next-visible-heading)
(global-set-key (kbd "M-C-P") 'outline-previous-visible-heading)
(define-key key-minor-mode-map (kbd "M-C-N") 'outline-next-visible-heading)
(define-key key-minor-mode-map (kbd "M-C-P") 'outline-previous-visible-heading)

(global-set-key (kbd "M-N") 'org-forward-heading-same-level)
(global-set-key (kbd "M-n") 'org-next-visible-heading)
(global-set-key (kbd "M-P") 'org-backward-heading-same-level)
(global-set-key (kbd "M-p") 'org-previous-visible-heading)
(define-key key-minor-mode-map (kbd "M-N") 'org-forward-heading-same-level)
(define-key key-minor-mode-map (kbd "M-P") 'org-backward-heading-same-level)

(define-key key-minor-mode-map (kbd "M-{") 'org-backward-heading-same-level)
(define-key key-minor-mode-map (kbd "M-}") 'org-forward-heading-same-level)
(define-key key-minor-mode-map (kbd "M-[") 'org-backward-heading-same-level)
(define-key key-minor-mode-map (kbd "M-]") 'org-forward-heading-same-level)

(global-set-key (kbd "s-p") 'org-export-dispatch)
(define-key key-minor-mode-map (kbd "s-p") 'org-export-dispatch)

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

(define-key key-minor-mode-map (kbd "<M-S-backspace>") 'backward-kill-sexp)
(define-key key-minor-mode-map (kbd "<M-S-backspace>") 'backward-kill-sexp)
(define-key key-minor-mode-map (kbd "<M-S-backspace>") 'backward-kill-sexp)
(define-key key-minor-mode-map (kbd "<M-S-backspace>") 'backward-kill-sexp)

;; (global-set-key (kbd "C-h") 'delete-backward-char)

(global-set-key (kbd "M-h") 'help-command)

(defun flyspell-auto-correct-word-correct-space ()
  (interactive)
  (when (looking-back " " nil)
    (left-char 1))
  (flyspell-auto-correct-word)
  )

(global-set-key "\C-ce" 'eval-buffer)
(global-set-key "\C-cr" 'eval-region)
;; (define-key org-mode-map (kbd "`") 'flyspell-auto-correct-word)
(define-key org-mode-map (kbd "`") 'flyspell-auto-correct-word-correct-space)
;; (define-key org-mode-map (kbd "`") 'flyspell-auto-correct-previous-word)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-y") 'redo)

(define-key key-minor-mode-map (kbd "M-s-k") 'org-cut-subtree)
(define-key key-minor-mode-map (kbd "C-s-k") 'org-cut-subtree)

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

(use-package vertico
  :init
  (vertico-mode))

(use-package projectile
:defer t
  :diminish projectile-mode
  :init
  (projectile-mode +1)

  :custom
  (projectile-completion-system 'default) ;; Vertico integrates via default
  (projectile-enable-caching t)

  (projectile-globally-ignored-file-suffixes '("docx" "jpg" "png" "tmp" "tex" "html" "pdf" "pptx" "tmp"))

  (projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".tmp" "archived-work" "images" "excel-mindnode-omni" "archive"))

  (projectile-globally-ignored-files '("TAGS" ".DS_Store" ".projectile" ".dropbox"))

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :bind
  (:map projectile-mode-map
        ("s-o" . nil)))

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
;;  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
;; (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<tab>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<S-tab>") 'isearch-repeat-backward) ; single key, useful

;  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
 )

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

;; Load palimpsest package
(use-package palimpsest
  :ensure t
  :config
  ;; Enable globally after init
  (add-hook 'after-init-hook
            (lambda ()
              (palimpsest-mode 1)
              (global-hl-todo-mode 1)
              (projectile-mode  1)))
  ;; Define org-mode specific keybinding
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") 'palimpsest-move-region-to-bottom)))

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

;(setq set-mark-command-repeat-pop t)

(setq custom-safe-themes t)

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
)))) ; add TODO#+END_SRC

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
        (org-todo 'nextset))))) ; add TODO#+END_SRC

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

(defun xah-convert-entities-to-html-chars-region (begin end)
  (interactive "r")
  (xah-replace-pairs-region begin end
 '(
 ["&" "&amp;"]
 ["<" "&lt;"]
 [">" "&gt;"]
 )))

(defun xah-convert-html-chars-to-entities-region (begin end)
 (interactive "r")
 (xah-replace-pairs-region begin end
 '(
 ["&amp;" "&"]
 ["&lt;" "<"]
 ["&gt;" ">"]
 )))

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

(defun yas-expand-and-copy-to-clipboard (&optional field)
  "Expand snippet at point and copy the expansion to the clipboard.

Optional argument FIELD is for non-interactive use and is an
object satisfying `yas--field-p' to restrict the expansion to."
  (interactive)
  (let ((yas-triggers-in-field t)
        (yas-fallback-behavior nil))
    (setq field (yas-expand-from-trigger-key field))
    (when field
      (when (eq field t)
        (let ((snippet (car (yas-active-snippets))))
          (setq field (yas--snippet-exit snippet))))
      (gui-select-text (yas--field-start field)
                       (yas--field-end field))
      (clipboard-kill-ring-save (yas--field-start field)
                                (yas--field-end field)))))

(use-package yasnippet
  :defer t
  :ensure t
  :bind
  (("C-c e" . yas-load-snippet-buffer)
   ("." . insert-period))
  :hook ((org-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  ;; Load yasnippet directories
  (setq yas-snippet-dirs '("~/emacs/interesting-snippets" "~/emacs/snippets"))

  ;; Add hooks
  (add-hook 'yas-before-expand-snippet-hook (lambda () (key-minor-mode nil)))
  (add-hook 'yas-after-exit-snippet-hook (lambda () (key-minor-mode 1)))

  ;; Don't insert random spaces in my prose
  (setq yas-indent-line 'none)

  ;; Take input word including hyphen
  (setq yas/key-syntaxes '("w_" "w_." "^ ")) ; default is '("w" "w_" "w_." "^ ")

  ;; Suppress backquote warnings
  (defun suppress-backquote-warnings ()
    (interactive)
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))))

(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; after installing yasnippet but before you open any files
(with-eval-after-load 'yasnippet
  (yas-global-mode 1))

(with-eval-after-load 'org
  (add-hook 'org-tab-first-hook #'yas-org-very-safe-expand))

(defun yas-org-very-safe-expand ()
 "Expand the snippet at point and copy the expansion to the clipboard safely in org-mode."
 (let ((yas-fallback-behavior 'return-nil))
  (yas-expand)))

;; put this near the top of your yas config, outside any hook
(setq yas-trigger-key "<tab>")   ; string form is what yas expects

(defun new-yasnippet ()
  "Create a new Org-mode snippet file."
  (interactive)
  (let ((snippet-dir "/Users/jay/emacs/interesting-snippets/org-mode/")
        (snippet-name (read-string "Snippet name: "))
        (snippet-key (read-string "Snippet key: "))
        full-path)

    ;; Construct the full file path
    (setq full-path (concat snippet-dir snippet-name ".yasnippet"))

    ;; Ensure the snippet directory exists
    (unless (file-exists-p snippet-dir)
      (make-directory snippet-dir t))

    ;; Check if the file already exists
    (if (file-exists-p full-path)
        (if (yes-or-no-p "Snippet file already exists. Overwrite? ")
            (create-and-insert-snippet full-path snippet-name snippet-key)
          (message "Snippet creation canceled."))
      (create-and-insert-snippet full-path snippet-name snippet-key))))

(defun create-and-insert-snippet (full-path snippet-name snippet-key)
  "Helper function to create and insert snippet content."
  ;; Create and open the new snippet file
  (find-file full-path)
  ;; Insert the snippet structure
  (insert "# -*- mode: snippet -*-\n")
  (insert "# name: " snippet-name "\n")
  (insert "# key: " snippet-key "\n")
  (insert "# --\n")
  (insert "$0\n"))

(define-key key-minor-mode-map (kbd "s-k y a") 'new-yasnippet)

(use-package tiny
  :defer t
:config
(tiny-setup-default)
)
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

(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

;; --------------------------------------------------------------------
;; 2) Olivetti for a centered text area
;; --------------------------------------------------------------------
(use-package olivetti
  :ensure t
  :hook
  (org-mode        . olivetti-mode)
  (markdown-mode   . olivetti-mode)
  (text-mode       . olivetti-mode)
  (message-mode    . olivetti-mode)
  (emacs-lisp-mode . olivetti-mode)
  (css-mode        . olivetti-mode)
  (Custom-mode     . olivetti-mode)
    (sh-mode         . olivetti-mode)
  (web-mode        . olivetti-mode)
  (fundamental-mode . olivetti-mode)
  (help-mode       . olivetti-mode)
  (prog-mode       . olivetti-mode)
  (magit-status-mode . olivetti-mode)
  (dired-mode      . olivetti-mode)
  (Info-mode       . olivetti-mode)
  (eww-mode        . olivetti-mode)
  ;; Turn on visual-line-mode whenever we enter olivetti-mode:
  (olivetti-mode   . turn-on-visual-line-mode)
  :config
  ;; Set your desired body width here:
  (setq olivetti-body-width 80)

  ;; Unbind these if you like:
  (unbind-key (kbd "C-c [") olivetti-mode-map)
  (unbind-key (kbd "C-c ]") olivetti-mode-map)

  ;; Optional: keep org-tags aligned
  (setq org-tags-column 40))

(defun load-shared-functions ()
  (interactive)
(find-file "/Users/jay/emacs/emacs-settings/shared-functions.org"))

(defun load-gnu-startup ()
  (interactive)
(find-file "/Users/jay/emacs/emacs-settings/gnu-emacs-startup.org"))

(defun load-spacecraft-mode ()
 (interactive)
(find-file "~/emacs/emacs-settings/spacecraft-mode.org"))

(defun load-spacemacs-config ()
 (interactive)
(find-file "/Users/jay/emacs/emacs-settings/spacemacs-new-config.el"))

(defun load-roam-config ()
 (interactive)
(find-file "/Users/jay/emacs/emacs-settings/org-roam-config.el"))

(defun load-search-config ()
 (interactive)
(find-file "/Users/jay/emacs/emacs-settings/search-commands.org"))

(defun open-abbrevs ()
  (interactive)
(find-file "/Users/jay/emacs/aquamacs-jay/.abbrev_defs")
;; (olivetti-mode 1)
)

(defun embolden-region-or-point ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "*")
        (goto-char (region-beginning))
        (insert "*"))
    (insert "**")
    (backward-char)))

(define-key key-minor-mode-map (kbd "M-s-b") 'embolden-region-or-point)

(defun italicize-region-or-point ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "/")
        (goto-char (region-beginning))
        (insert "/"))
(insert "//")
    (backward-char)))

(define-key key-minor-mode-map (kbd "<C-i>") 'italicize-region-or-point)

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
Each element of ADLIST should look like:
  (FUNCTION WHERE AD-FN)
which is suitable for passing to `advice-add'."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ;; Generate the (advice-add ...) calls
     ,@(mapcar (lambda (adform)
                 (let ((fn    (car  adform))
                       (where (cadr adform))
                       (afn   (caddr adform)))
                   `(advice-add ',fn ,where ,afn)))
               adlist)
     ;; Wrap BODY in unwind-protect so advice is always removed
     (unwind-protect (progn ,@body)
       ;; Generate the (advice-remove ...) calls
       ,@(mapcar (lambda (adform)
                   (let ((fn  (car adform))
                         (afn (caddr adform)))
                     `(advice-remove ',fn ,afn)))
                 adlist))))

(defun my/org-checkbox-toggle-advice (orig-fn &rest args)
  "Advice to move to next list item on checkbox toggle."
  (my/with-advice
   ((org-update-checkbox-count-maybe
     :after (lambda () (ignore-errors (progn (org-next-item) (end-of-line))))))
   (apply orig-fn args)))

(advice-add #'org-ctrl-c-ctrl-c   :around #'my/org-checkbox-toggle-advice)
(advice-add #'org-toggle-checkbox :around #'my/org-checkbox-toggle-advice)

;; wrap-region
(use-package wrap-region
  :defer
  :ensure t
  :config
  ;; Fix cl deprecation warning
  (require 'cl-lib)

  ;; Add your custom wrapper
  (wrap-region-add-wrapper "\n#+BEGIN_QUOTE\n" "\n#+END_QUOTE\n" ";")

  ;; Define your wrapper function
  (defun wrap-region-define-wrappers ()
    "Defines defaults wrappers."
    (mapc
     (lambda (pair)
       (apply 'wrap-region-add-wrapper pair))
     '(
       ;; ("\"" "\"")
       ;; ("'"  "'")
       ;; ("("  ")")
       ("{"  "}")
       ;; ("["  "]")
       ;; ("<"  ">")
       ;; ("<"  ">")
       )))

  ;; Add mode-specific wrappers
  (wrap-region-add-wrappers
   '(
     ;;     ("*" "*" nil org-mode)
     ;;     ("/" "/" nil org-mode)
     ;; ("\"" "\"" nil org-mode)
     ("~" "~" nil org-mode)
     ("_" "_" nil org-mode)
     ("*" "*" nil (org-mode message-mode))
     ("/" "/" nil (org-mode message-mode))
     ("$" "$" nil (org-mode latex-mode))
     )))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:inherit font-lock-warning-face :weight bold))))
)

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-c\C-c" #'org-edit-src-exit))

;; Smart DWIM narrowing/widening command for Org, LaTeX, and code buffers.
;; No package boilerplate — just the function and helpers.

;;; Code:

(defgroup narrow-or-widen-dwim nil
  "Smart narrowing/widening helper."
  :group 'editing)

(defcustom narrow-or-widen-dwim-single-window t
  "If non-nil, close other windows when entering `org-edit-src-code'."
  :type 'boolean
  :group 'narrow-or-widen-dwim)

(defun narrow-or-widen-dwim--maybe-widen (prefix)
  "If the buffer is narrowed and PREFIX is nil, widen it.
Return non-nil when widening happened."
  (when (and (buffer-narrowed-p) (not prefix))
    (widen)
    t))

(defun narrow-or-widen-dwim--in-subtree-p ()
  "Return non-nil when point is inside an Org subtree (heading or its body)."
  (save-excursion
    (condition-case nil
        (progn (org-back-to-heading t) t)
      (error nil))))

;;;###autoload
(defun narrow-or-widen-dwim (p)
  "Do‑what‑I‑mean narrowing or widening.

Without a prefix argument, widen if the buffer is already narrowed.
Otherwise narrow intelligently to:
  • the active region,
  • the current Org src block (opening an indirect edit buffer),
  • the current Org block (quote/example/etc.),
  • the current Org subtree,
  • the Org element at point (drawer, property drawer, table, footnote),
  • the current LaTeX environment, or
  • the current defun in programming modes.

With a prefix argument P (\[universal-argument]), *always* narrow even if the
buffer is already narrowed; never widen."
  (interactive "P")
  (declare (interactive-only))
  ;; First, widen if appropriate. If we did, we're done.
  (unless (narrow-or-widen-dwim--maybe-widen p)
    (cond
     ;; 1) Active region in ANY mode ⇒ narrow to that region.
     ((region-active-p)
      (narrow-to-region (region-beginning) (region-end)))

     ;; 2) Inside an *Org src editing* indirect buffer -- exit it.
     ((bound-and-true-p org-src-mode)
      (org-edit-src-exit))

     ;; 3) In Org mode -- choose the right narrowing target.
     ((derived-mode-p 'org-mode)
      (cond
       ;; Inside a src block ⇒ open indirect edit buffer.
       ((org-in-src-block-p)
        (org-edit-src-code)
        (when narrow-or-widen-dwim-single-window
          (delete-other-windows)))

       ;; Inside any Org block (quote/example/etc.) ⇒ narrow to block.
       ((org-in-block-p nil)
        (org-narrow-to-block))

       ;; Within a subtree ⇒ narrow to subtree.
       ((narrow-or-widen-dwim--in-subtree-p)
        (save-excursion
          (org-back-to-heading t)
          (org-narrow-to-subtree)))

       ;; Other Org element at point (drawer, table, footnote) ⇒ element.
       ((let* ((elem (org-element-at-point)))
          (when (memq (org-element-type elem)
                      '(table footnote-definition property-drawer drawer))
            elem))
        (org-narrow-to-element))

       (t (user-error "Point is not in a narrowable Org context"))))

     ;; 4) LaTeX mode ⇒ narrow to current environment.
     ((derived-mode-p 'latex-mode)
      (LaTeX-narrow-to-environment))

     ;; 5) Fallback ⇒ defun.
     (t
      (narrow-to-defun)))))

(provide 'narrow-or-widen-dwim)
;;; narrow-or-widen-dwim.el ends here

(setq org-structure-template-alist
 '(("a" . "export ascii")
  ("c" . "center")
  ("C" . "comment")
  ("e" . "example")
  ("x" . "example")
  ("le" . "example")
  ("E" . "export")
  ("h" . "export html")
  ("l" . "src emacs-lisp")
  ("el" . "src emacs-lisp")
  ("la" . "export latex")
  ("q" . "src quote")
  ("s" . "src")
  ("sh" . "src sh")
  ("f" . "example fountain")
  ("v" . "example verse")))

;; ------------------------------------------------------------------
;; Org structure templates ­– <sr TAB> etc.
;; ------------------------------------------------------------------
(use-package org ; pulls in org-tempo automatically in modern Org
  :init
  ;; org-tempo must load *before* we touch the alist
  (require 'org-tempo)
  :config
  ;; Prepend our shortcuts once, not 5×
  (setq org-structure-template-alist
        (append
         '(("sr" . "src")                 ; generic <sr
           ("sh" . "src sh")              ; shell
           ("el" . "src emacs-lisp")      ; elisp
           ("u" . "src user")             ; user
           ("le" . "example")             ; example block
           ("v"  . "verse"))              ; verse block
         org-structure-template-alist)))

(put 'if 'lisp-indent-function nil)

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

(dotimes (n 10)
  (global-unset-key (kbd (format "M-%d" n))))

;; define list-title face
(defface list-title-face
  '((t (:foreground "red" :weight bold)))
  "fontify list titles")

;; define heading-title face
(defface heading-title-face
  '((t (:foreground "red" :weight bold)))
  "fontify heading titles")

;; define anki-clozure face
(defface anki-clozure-face
  '((t (:foreground "red" :weight bold)))
  "fontify anki clozures")

(font-lock-add-keywords 'org-mode
                        '(
			  ;; ("^.*:[ ]*$" . 'list-title-face) ; fontify any line that ends with a colon
                          ("^Q\\(UESTION\\|uestion\\):" . 'list-title-face) ; fontify "Question:" and "QUESTION:"

			  ("^[A-Za-z]+:" . 'list-title-face); fontify any word followed by a colon if it begins the line

			  ;; ("^\\*+[ ]*[a-Za-z]+:" . 'heading-title-face); fontify any heading that starts with a word followed by a colon. But how to make this face supersede the other one?
                          )
                        )

;; old code (works for HTML export, breaks http links)
;; (add-hook 'org-export-before-parsing-hook (lambda (backend) (replace-regexp "^[A-Za-z]+:" "*\\&*")))

(add-hook 'org-export-before-parsing-hook (lambda (backend) (replace-regexp "^\\([A-Za-z]+:\\)\\([^/]\\|/[^/]\\|$\\)" "*\\1*\\2")))

;; (add-hook 'org-export-before-parsing-hook (lambda (backend) (replace-regexp "^\\(.*:\\)[ ]*$" "*\\1*")))
 ;; any line that ends with a colon

(defun sort-lines-case-insensitive ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun typewriter-mode ()
  (interactive)
  (setq centered-cursor-mode t)
  (setq global-centered-cursor-mode t)
  )

(defun save-file-as-new ()
  "Force modification of current file, unless already modified."
  (interactive)
  (if (and (verify-visited-file-modtime (current-buffer))
           (not (buffer-modified-p)))
      (progn
        (set-buffer-modified-p t)
        (save-buffer 0))))

(defun touch-file (file)
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
   (dired-move-to-filename))))

(use-package ox-twbs)
; '(org-twbs-head-include-default-style nil)
;; '(org-twbs-htmlize-output-type (quote inline-css))
; '(org-twbs-indent t)

(if (eq window-system 'mac)
    (add-to-list 'exec-path "/usr/local/texlive/2025/bin/universal-darwin")
  )

(setq  ; org-export-dispatch-use-expert-ui t non-intrusive export dispatch
 org-latex-pdf-process               ; for regular export

 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; don't add extra lines to numbered lists and bulleted lists (set to nil)

;; add padding to numbered lists and bulleted lists (set to t)

(defun jay-load-latex ()
  (interactive)
  (let ((base "/Users/jay/emacs/emacs-settings/latex-templates/")
        (files '("elegant.el"
                 "beautiful-racket.el"
                 "elegant-garamond.el"
                 "blue-invoice.el"
                 "blue-invoice-with-letterhead.el"
                 "jay-latex-yosemite-setup.el")))
    (dolist (file files)
      (let ((full-path (expand-file-name file base)))
        (when (file-exists-p full-path)
          (load full-path))))
    ;; Also load the subtitle processing configuration
    (load "/Users/jay/emacs/emacs-settings/latex-subtitles/latex-subtitles.el")))

;; (load "/Users/jay/emacs/emacs-settings/latex-templates/blue-ruin.el")
;; (load "/Users/jay/emacs/emacs-settings/latex-templates/modest-ruin.el")

;; (load "/Users/jay/emacs/emacs-settings/latex-templates/inelegant.el")
;; (load "/Users/jay/emacs/emacs-settings/latex-templates/elegant-wider.el")

;; (load "/Users/jay/emacs/emacs-settings/latex-templates/resonate.el")

;; (load "/Users/jay/emacs/emacs-settings/latex-templates/blue-ruin_no_cover.el")

;; (use-package blue-ruin)
;; (use-package blue-invoice)
;; (use-package blue-ruin-no-cover)

(defun org-latex-filter-fancyvrb (text backend info)
 "Convert begin/end{verbatim} to begin/end{Verbatim}.
Allows use of the fancyvrb latex package."
 (when
   (org-export-derived-backend-p backend 'latex)
 (replace-regexp-in-string
  "\\\\\\(begin\\|end\\){verbatim}"
  "\\\\\\1{quote}"
  text)))

(add-to-list 'org-export-filter-final-output-functions
    'org-latex-filter-fancyvrb)

(setq org-startup-with-inline-images nil)

(defun delete-duplicate-lines-keep-blanks ()
 (interactive)
 (delete-duplicate-lines (region-beginning) (region-end) nil nil t))

(setq scroll-margin 25)
;; (setq recenter-positions (quote (top middle bottom)))

(setq org-clock-auto-clock-resolution t)
 (setq org-clock-idle-time 30)
 (setq org-clock-in-resume t)
 (setq org-clock-persist-query-resume nil)
 (setq org-clock-report-include-clocking-task t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-into-drawer nil)
(setq org-clocktable-defaults
 (quote
 (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))

(setq org-time-clocksum-format
 (quote
 (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

;; Utility functions:

(defun sensible-defaults/comment-or-uncomment-region-or-line ()
 "Comments or uncomments the region or the current line if there's no active region."
 (interactive)
 (let (beg end)
  (if (region-active-p)
    (setq beg (region-beginning) end (region-end))
   (setq beg (line-beginning-position) end (line-end-position)))
  (comment-or-uncomment-region beg end)))

(global-set-key (kbd "M-;")
         'sensible-defaults/comment-or-uncomment-region-or-line)


(setq vc-follow-symlinks t)

"When saving a file that starts with `#!', make it executable."
 (add-hook 'after-save-hook
      'executable-make-buffer-file-executable-if-script-p)

(defun repeat-last-command ()
"repeats the last command called via M-x"
(interactive)
(let ((history extended-command-history)
cmd)
;; remove any occurence of this-command at the head of `history'.
(while (string= this-command (setq cmd (pop history))))
(message "Running cmd: %s" cmd)
(call-interactively (intern cmd))))

(defun org-select-line ()
 "Select the current line"
 (interactive)
 (org-beginning-of-line) ; move to end of line
 (set-mark (line-end-position)))

(defun org-copy-line ()
 (interactive)
(org-select-line)
(pasteboard-copy)
(set-mark nil))

(defun kill-paragraph-from-beginning (b e)
  (interactive)
  (mark-paragraph)
  (kill-region)
  )

(defun eval-subtree ()
 (interactive)
 (org-edit-src-code)
 (eval-buffer)
 (org-edit-src-exit)
 )

(defun eval-adaptive ()
  "Smart eval:
- If region is active, eval the region.
- If in an Org-mode Emacs Lisp source block, eval the block.
- If in a Yasnippet buffer, load the snippet buffer as an org-mode snippet.
- If in an Emacs Lisp buffer, eval the buffer.
- Otherwise, notify the user."
  (interactive)
  (cond
   ;; If region is active, eval the region
   ((use-region-p)
    (eval-region (region-beginning) (region-end))
    (message "✅ Evaluated region"))

   ;; If in an Org-mode Emacs Lisp src block, eval that block
   ((and (derived-mode-p 'org-mode)
         (org-in-src-block-p))
    (let* ((context (org-element-context))
           (lang (org-element-property :language context))
           (body (org-element-property :value context)))
      (if (string= lang "emacs-lisp")
          (progn
            (eval (read (concat "(progn\n" body "\n)")))
            (message "✅ Evaluated Emacs Lisp src block"))
        (message "⚠️ Not an Emacs Lisp src block"))))

   ;; If in a Yasnippet buffer, load the snippet buffer as org-mode
   ((derived-mode-p 'snippet-mode)
    (yas-load-snippet-buffer 'org-mode)
    (message "✅ Loaded Yasnippet buffer for org-mode"))

   ;; If in an Emacs Lisp buffer, eval the whole buffer
   ((derived-mode-p 'emacs-lisp-mode)
    (eval-buffer)
    (message "✅ Evaluated buffer"))

   ;; Otherwise, do nothing
   (t
    (message "⚠️ No matching eval context"))))

(global-set-key (kbd "C-c e") #'eval-adaptive)

(defun kill-to-buffer-end-or-beginning (arg)
  (interactive "p")
  (if (and arg (= 0 (mod arg 4)))
      (beginning-of-buffer)
    (end-of-buffer))
  (kill-region (mark) (point))
  (recenter-top-bottom))

(define-key key-minor-mode-map (kbd "M-w") 'kill-to-buffer-end-or-beginning)

(defun up-by-degrees ()
 (interactive)
       (previous-line 6)
 )

(defun down-by-degrees ()
 (interactive)
       (next-line 6)
 )

(defun org-next-subtree-same-level-and-narrow ()
 (interactive)
 (widen)
 (org-forward-heading-same-level 1)
 (org-narrow-to-subtree)
 )

(defun org-previous-subtree-same-level-and-narrow ()
 (interactive)
(org-previous-visible-heading 1)
 (widen)
(org-backward-heading-same-level 1)
 (org-narrow-to-subtree)
 )

(defun org-next-subtree-and-narrow ()
 (interactive)
 (widen)
 (org-next-visible-heading 1)
 (org-narrow-to-subtree)
 )

(defun org-previous-subtree-and-narrow ()
 (interactive)
(org-previous-visible-heading 1)
 (widen)
 (org-previous-visible-heading 1)
 (org-narrow-to-subtree)
 )

(defun refile-region-or-subtree ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'jay-refile-region)
    (org-refile)))

(define-minor-mode org-config-files-local-mode
  "Minor mode for editing configuration files in org-mode."
  :init-value nil
  :lighter " OrgCfg"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<SPC>") 'insert-space)
            map)
  (message "org-config-files-local-mode is now %s" org-config-files-local-mode)
  (if org-config-files-local-mode
      (progn
        ;; When enabling, set any additional buffer-local variables
        (setq-local abbrev-mode nil))
    (kill-local-variable 'abbrev-mode)))

(defun yas/pasteboard-raw ()
 "Return content of OS X system pasteboard via `pbpaste'."
 (shell-command-to-string "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'"))

(defun yas/org-get-time-stamp (&rest args)
 "Return the string that `org-insert-time-stamp' would insert."
 (with-temp-buffer
  (apply #'org-insert-time-stamp args)
  (buffer-string)))

(defun yas/tiny-expand (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-max)) ; tiny-expand works on text preceding point
    (tiny-expand)
    (buffer-string)))

(defun yas/suppress-errors ()
(interactive)
  (ignore-errors (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
)

(defun loadup-gen ()
 "Generate the lines to include in the lisp/loadup.el file
to place all of the libraries that are loaded by your InitFile
into the main dumped emacs"
 (interactive)
 (defun get-loads-from-*Messages* ()
  (save-excursion
   (let ((retval ()))
	(set-buffer "*Messages*")
	(beginning-of-buffer)
	(while (search-forward-regexp "^Loading " nil t)
	 (let ((start (point)))
	  (search-forward "...")
	  (backward-char 3)
	  (setq retval (cons (buffer-substring-no-properties start (point)) retval))))
	retval)))
 (map 'list
    (lambda (file) (princ (format "(load \"%s\")\n" file)))
    (get-loads-from-*Messages*)))

(use-package crux
:defer t
:bind
( "M-`" . crux-swap-windows)
( "s-k rf" .  crux-rename-file-and-buffer)
( "s-k df" . crux-delete-file-and-buffer)
( "C-c d" . crux-duplicate-current-line-or-region)
( "C-c i" . crux-find-user-init-file)
)

(defun em-dash ()
(interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
(expand-abbrev)
(insert "---")
  )

(defun true-em-dash ()
(interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
(expand-abbrev)
(insert "—")
  )

(defun true-en-dash ()
(interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
(expand-abbrev)
(insert "-")
  )

(defun insert-one-double-quote ()
(interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
(insert "\""))

(defun insert-right-bracket ()
  (interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
  (insert "\]")
  )

(defun insert-equals-sign ()
  (interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
  (insert "=")
  )

(use-package undo-fu-session
  :init
  (setq undo-fu-session-compression nil)   ;; nil = never compress
  (undo-fu-session-global-mode))

(defun undo-fu-only-redo-fail-silently ()
 "Redo the last undone change if possible, silently fail if no more redo steps."
 (interactive)
 (ignore-errors
  (undo-fu-only-redo)))

(defun undo-fu-only-redo-fail-with-heart ()
 "Redo the last undone change if possible."
 (interactive)
 (condition-case nil
   (undo-fu-only-redo)
  (user-error (message "❤️"))))

(setq undo-tree-auto-save-history nil)

(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

(setq display-buffer-alist (quote (("" ignore (nil . reusable-frames)))))

(defun edit-this-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
   (when file-name
    (find-alternate-file (concat "/sudo::" file-name)))))

(defun sudo-find-file ()
 (interactive)
 (let ((file-name (buffer-file-name)))
  (when file-name
  (find-file (concat "/sudo::" file-name)))))

(defun sudo-edit-paths ()
 (interactive)
(find-file "/etc/paths")
(sudo-edit)
)

(defun sudo-edit-hosts ()
 (interactive)
(find-file "/private/etc/hosts")
(sudo-edit)
)

(add-hook 'prog-mode-hook #'prettify-symbols-mode)

(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

 (global-set-key (kbd "C-c v") 'projectile-ag)
 (global-set-key (kbd "C-c C-v") 'hrs/search-project-for-symbol-at-point)
;; (setq projectile-indexing-method 'native)

(setq org-src-window-setup 'current-window)

(defun insert-todays-date (arg)
 (interactive "P")
 (insert (if arg
    (format-time-string "%d-%m-%Y")
   (format-time-string "%Y-%m-%d"))))

(defun toggle-between-src-and-example-block ()
  (interactive)
  (save-excursion
    (let* ((elt (org-element-at-point))
           (elt-type (org-element-type elt))
           (bgn (org-element-property :begin elt)))
      (cond
       ((eq elt-type 'src-block)
        (goto-char bgn)
        (re-search-forward "#\\+BEGIN_SRC\\s-*\\(\\S-+\\)?")
        (replace-match "#+BEGIN_EXAMPLE \\1")
        (re-search-forward "#\\+END_SRC" nil t)
        (replace-match "#+END_EXAMPLE"))
       ((eq elt-type 'example-block)
        (goto-char bgn)
        (re-search-forward "#\\+BEGIN_EXAMPLE\\s-*\\(\\S-+\\)?")
        (replace-match "#+BEGIN_SRC \\1")
        (re-search-forward "#\\+END_EXAMPLE" nil t)
        (replace-match "#+END_SRC"))
       (t (message "Not in a src or example block goddammit!"))))))

(setq ediff-diff-options "-w")

(setq mac-wheel-button-is-mouse-2 nil)
;; so that the middle button works

(setq scroll-conservatively 1000) ; seems nice

(use-package ox-clip
  :defer t)

(defun org-def ()
(interactive)
(save-excursion
(beginning-of-line)
 (insert "- "))
(insert " :: ")
)

(defun delete-html-blocks ()
(interactive)
(replace-regexp "#\\+BEGIN_HTML\\(?:.*\\|\n\\)*#\\+END_HTML" "")
)

(use-package ox-tufte
  :defer t)

(use-package ox-tufte-LaTeX
:defer t
  :ensure nil
  :init (load "/Users/jay/emacs/emacs-settings/tufte-org-mode-master/ox-tufte-latex.el")
  )

(defvar yt-iframe-format
 ;; You may want to change your width and height.
 (concat "<iframe width=\"440\""
     " height=\"335\""
     " src=\"https://www.youtube.com/embed/%s\""
     " frameborder=\"0\""
     " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
  (browse-url
  (concat "https://www.youtube.com/embed/"
      handle)))
 (lambda (path desc backend)
  (cl-case backend
   (html (format yt-iframe-format
          path (or desc "")))
   (latex (format "\href{%s}{%s}"
          path (or desc "video"))))))

(defun spacemacs-version-display-and-copy ()
 "Echo the current version of Spacemacs, Emacs, and org-mode, and copy it."
 (interactive)
(setq system-version
 (substring
 (shell-command-to-string "defaults read loginwindow SystemVersionStampAsString")
 0 -1))
 (kill-new
   (message "Mac OSX Sierra version %s, Spacemacs version %s, Emacs version %s, org-mode version %s"
       system-version spacemacs-version emacs-version org-version))
 (push-kill-ring-pasteboard-to-MacOS-clipboard)
)

(defun emacs-version-display-and-copy ()
 "Echo the current version of Spacemacs, Emacs, and org-mode, and copy it."
 (interactive)
(setq system-version
 (substring
 (shell-command-to-string "defaults read loginwindow SystemVersionStampAsString")
 0 -1))
 (kill-new
   (message "Mac OSX Sierra version %s, Emacs version %s, org-mode version %s"
       system-version emacs-version org-version))
 (push-kill-ring-pasteboard-to-MacOS-clipboard)
)

(defun tidy-html ()
 "Tidies the HTML content in the buffer using `tidy'"
 (interactive)
 (shell-command-on-region
  ;; beginning and end of buffer
  (point-min)
  (point-max)
  ;; command and parameters
  "tidy -i -w 120 -q"
  ;; output buffer
  (current-buffer)
  ;; replace?
  t
  ;; name of the error buffer
  "*Tidy Error Buffer*"
  ;; show error buffer?
  t))

(defun qm-maybe ()
(interactive)
(when
(not
(looking-back "$")
)
(smart-question-mark)
)
)

;; (define-key key-minor-mode-map (kbd "?") 'qm-maybe)

(use-package shell-pop
:defer t
 :bind (("C-t" . shell-pop))
 :config
 (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 (setq shell-pop-term-shell "/bin/zsh")
 ;; need to do this manually or not picked up by `shell-pop'
 (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(defun ignore-case-in-searches ()
 (interactive)
 (setq case-fold-search t)
 )

(defun isearch-forward-ignore-case ()
(interactive)
(ignore-case-in-searches)
(isearch-forward)
)

(defun ap/org-count-words ()
  "If region is active, count words in it; otherwise count words in current subtree."
  (interactive)
  (if (use-region-p)
    (funcall-interactively #'count-words-region (region-beginning) (region-end))
   (org-with-wide-buffer
    (cl-loop for (lines words characters)
        in (org-map-entries
          (lambda ()
           (ap/org-forward-to-entry-content 'unsafe)
           (let ((end (org-entry-end-position)))
            (list (count-lines (point) end)
               (count-words (point) end)
               (- end (point)))))
          nil 'tree)
        sum lines into total-lines
        sum words into total-words
        sum characters into total-characters
        finally do (message "Subtree \"%s\" has %s lines, %s words, and %s characters."
                  (org-get-heading t t) total-lines total-words total-characters)))))

(defun ap/org-forward-to-entry-content (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
   ;; To improve performance in loops (e.g. with `org-map-entries')
   (org-back-to-heading))
  (cl-loop for element = (org-element-at-point)
       for pos = (pcase element
             (`(headline . ,_) (org-element-property :contents-begin element))
             (`(,(or 'planning 'property-drawer 'drawer) . ,_) (org-element-property :end element)))
       while pos
       do (goto-char pos)))

(setq
 time-stamp-active t     ; do enable time-stamps
time-stamp-line-limit 50; check first 50 lines
time-stamp-format "%04y-%02m-%02d"; date format
time-stamp-pattern "50//*Invoice date:\\*+\s%:y-%02m-%02d\\\\?$"
)

;; (add-hook 'write-file-hooks 'time-stamp) ; update when saving

(add-hook 'before-save-hook 'time-stamp)

;; (add-hook 'org-mode-hook
;;         (lambda ()
;;         (variable-pitch-mode 1)))

;; (set-face-attribute 'variable-pitch nil :family "Baskerville")
(set-face-attribute 'variable-pitch nil :family "Triplicate T3")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))

;;  (typo-mode 1)      ;; Good for symbols like em-dash
(add-hook 'poetry-mode-hook 'olivetti-mode 1)
;; (add-hook 'poetry-mode-hook 'olivetti-mode 1)

(defun poet-mode ()
(interactive)
(set-face-attribute 'default nil :family "Iosevka" :height 130)
(set-face-attribute 'fixed-pitch nil :family "Iosevka")
        (variable-pitch-mode 1)
(poetry-mode)
(olivetti-mode 1)
;; (turn-off-auto-capitalize-mode)

)

(defvar my-org-selected-css "/Users/jaydixit/Dropbox/web-design/custom-css/sans-serif.css"
  "Path of the selected CSS file or 'none' for no stylesheet. Defaults to sans-serif.css if not manually selected.")

(defvar my-org-css-options
  '("/Users/jay/Dropbox/web-design/custom-css/email.css"
    "/Users/jay/Dropbox/web-design/custom-css/gmail.css"
    "/Users/jay/Dropbox/github/incandescentman/css/neocortex.css"
"/Users/jay/Library/CloudStorage/Dropbox/github/incandescentman.github.io/css/bold.css"
"/Users/jay/Library/CloudStorage/Dropbox/web-design/custom-css/chatgpt-images.css"
    "/Users/jaydixit/Dropbox/web-design/custom-css/sakura-1.50css"
    "/Users/jaydixit/Dropbox/web-design/custom-css/sans-serif.css"
    "/Users/jay/Dropbox/github/org-html-themes/src/bigblow_theme/css/bigblow.css"
"/Users/jay/Library/CloudStorage/Dropbox/github/incandescentman.github.io/css/iphone.css"
    "none")
  "List of CSS file options for Org HTML export.")

(defun select-org-export-css ()
  "Prompt the user to select a CSS file or choose none."
  (interactive)
  (setq my-org-selected-css
        (completing-read "Choose a CSS file or none: " my-org-css-options))
  (when (string-equal my-org-selected-css "none")
    (setq my-org-selected-css nil)))

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline CSS for Org HTML export."
  (when (eq exporter 'html)
    (when (eq my-org-selected-css 'unset)
      (select-org-export-css))
    (if (and my-org-selected-css (not (eq my-org-selected-css 'unset)))
        (let ((final my-org-selected-css))
          (when (file-exists-p final)
            (setq org-html-head-include-default-style nil)
            (setq org-html-head (concat
                                 "<style type=\"text/css\">\n"
                                 "<!--/*--><![CDATA[/*><!--*/\n"
                                 (with-temp-buffer
                                   (insert-file-contents final)
                                   (buffer-string))
                                 "/*]]>*/-->\n"
                                 "</style>\n"))))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head nil))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

(defun show-duplicate-words (&optional alphabetical)
 "Collect all of the unique words in the current buffer and
display them in a new buffer. With prefix, alphabetize the
list."
 (interactive "P")
 (let ((buf (buffer-name))
    (new (get-buffer-create "*Unique Words*"))
    (txt (delete-dups (mapcar #'downcase
                 (split-string (buffer-string)
                        nil nil
                        "[^[:alnum:]]+")))))
  (with-current-buffer new
   (delete-region (point-min) (point-max))
   (insert (format "%d unique words in the <%s> buffer:\n\n"
           (length txt) buf))
   (cl-dolist (word (if alphabetical (sort txt #'string<) txt))
    (insert (concat word "\n"))))
  (pop-to-buffer new)))

(defun org-tempo-add-templates ()
 "Update all Org Tempo templates.

Go through `org-structure-template-alist' and
`org-tempo-keywords-alist' and update tempo templates."
 (mapc 'org--check-org-structure-template-alist '(org-structure-template-alist
						  org-tempo-keywords-alist))
 (let ((keys (org-tempo--keys)))
  ;; Check for duplicated snippet keys and warn if any are found.
  (when (> (length keys) (length (delete-dups keys)))
)
  ;; Remove any keys already defined in case they have been updated.
  (setq org-tempo-tags
	 (cl-remove-if (lambda (tag) (member (car tag) keys)) org-tempo-tags))
  (mapc #'org-tempo-add-block org-structure-template-alist)
  (mapc #'org-tempo-add-keyword org-tempo-keywords-alist)))

(use-package multiple-cursors
  :defer t)
(setq mc/list-file "/Users/jay/emacs/emacs-settings/mc-lists.el")

(define-key key-minor-mode-map (kbd "C-8") 'endless/mc-map)
(define-prefix-command 'endless/mc-map)
(define-key ctl-x-map "m" 'endless/mc-map)
(define-key key-minor-mode-map (kbd "C-8") 'endless/mc-map)

;;; Really really nice!
(define-key endless/mc-map "i" #'mc/insert-numbers)
(define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
(define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
(define-key endless/mc-map (kbd "<backspace>") 'delete-backward-char)
(define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
(define-key endless/mc-map "r" #'mc/reverse-regions)
(define-key endless/mc-map "s" #'mc/sort-regions)
(define-key endless/mc-map "l" #'mc/edit-lines)
(define-key endless/mc-map (kbd "<return>") #'newline-and-indent)
(define-key endless/mc-map "\C-a"
 #'mc/edit-beginnings-of-lines)
(define-key endless/mc-map "\C-e"
 #'mc/edit-ends-of-lines)
(define-key endless/mc-map (kbd ">") #'mc/cycle-forward)
(define-key endless/mc-map (kbd "<") #'mc/cycle-backward)

(use-package hydra
:defer t
  :ensure t)

(defhydra hydra-mc (:hint nil)
  "
Multiple cursors:

 Mark:   _n_ext   _p_revious   _a_ll   _l_ines      Unmark: _u_ next  _U_ prev
 Edit:   _i_nsert nums   edit _b_egin   edit _e_nd   _s_ort   _r_everse
 Cycle:  _>_ forward   _<_ backward
 Other:  _h_ide unmatched   _q_uit
"
  ("n" mc/mark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("a" mc/mark-all-like-this)
  ("l" mc/edit-lines)
  ("u" mc/unmark-next-like-this)
  ("U" mc/unmark-previous-like-this)
  ("i" mc/insert-numbers)
  ("b" mc/edit-beginnings-of-lines)
  ("e" mc/edit-ends-of-lines)
  ("s" mc/sort-regions)
  ("r" mc/reverse-regions)
  (">" mc/cycle-forward)
  ("<" mc/cycle-backward)
  ("h" mc-hide-unmatched-lines-mode)
  ("q" nil))

;; Bind hydra to your existing keymap
(define-key endless/mc-map (kbd "H") #'hydra-mc/body)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook #'dubcaps-mode)
(add-hook 'org-mode-hook #'dubcaps-mode)

(add-hook 'text-mode-hook #'olivetti-mode 1)
;; (add-hook 'org-mode-hook 'turn-off-autocomplete-mode )

(add-hook 'org-mode-hook 'turn-on-flyspell)
;; (add-hook 'org-mode-hook (lambda () (flyspell-lazy-mode 1)))

;; (add-hook 'org-mode-hook #'flycheck-mode)
;; (add-hook 'org-mode-hook (lambda () (org-sticky-header-mode 1)))

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook #'(lambda () (auto-fill-mode -1)))

;; (add-hook 'org-mode-hook 'turn-on-auto-capitalize-mode)

(add-hook 'org-mode-hook 'wrap-region-mode)
 (add-hook 'latex-mode-hook 'wrap-region-mode)

(add-hook
 'org-mode-hook
 (lambda ()
 (define-key org-mode-map (kbd "DEL")
  'new-org-delete-backward-char)))

(defun wide-screen ()
(interactive)
(setq olivetti-body-width 72)
)

(setq org-cycle-separator-lines 0)

(setq org-list-bullet-list '("-"))
(setq org-list-demote-modify-bullet nil)

(defun embiggen-text ()
 (interactive)
 (text-scale-increase 1)
 )

(defun ensmallen-text ()
 (interactive)
 (text-scale-decrease 1)
 )

(defun yesterday ()
  (interactive)
    (insert (shell-command-to-string "echo -n $(date -v -1d +%F)"))
  )

(defun today ()
  (interactive)
    (insert (shell-command-to-string "echo -n $(date +%F)"))
  )

(custom-set-faces
 '(org-ellipsis ((t (:family "Iosevka Nerd Font" :foreground "LightGray" :underline nil)))))
(setq org-ellipsis " ")
;; (setq org-ellipsis " ▶")

(defun org-render-table-at-point ()
 (interactive)
 (save-excursion
  (beginning-of-line)
  (if (overlays-at (point))
    ;; this is a rough solution, because there can
    ;; be other overlays at point
    (delete-overlay (car (overlays-at (point))))

   (let* ((element-type (org-element-type (org-element-at-point))))
    (if (and (not (eq element-type 'table))
         (not (eq element-type 'table-row)))
      (error "not at an org table")

     (while (not (eq 'table (org-element-type (org-element-at-point))))
      (forward-line -1))

     (org-render-table (org-element-at-point)))))))

(defun org-render-table (table)
 (interactive)
 (let* ((begin (org-element-property :begin table))
     (end (let ((pos (org-element-property :end table)))
        (goto-char pos)
        (beginning-of-line)
        ;; skip possible space after table
        (while (not (looking-at " *[|#]"))
         (setq pos (point))
         (forward-line -1))
        pos))
     (tabletxt (buffer-substring-no-properties begin end))
     (img (with-temp-buffer
        (insert tabletxt)
        (mark-whole-buffer)
        (org-latex-convert-region-to-latex)
        (org-preview-latex-fragment)
        (goto-char (point-min))
        (overlay-get (car (overlays-at (point))) 'display)))
     (overlay (make-overlay begin end)))
  (overlay-put overlay 'display img)
  (forward-line -1)))

(defun org-render-tables-in-buffer ()
 (save-excursion
  (org-element-map (org-element-parse-buffer) 'table 'org-render-table)))

(defun org-show-inline-images ()
 (interactive)
(org-toggle-inline-images))

(use-package ido-vertical-mode
  :defer t)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; optionally
(setq ido-use-faces nil)

;; (setq ido-use-faces t)
;; (set-face-attribute 'ido-vertical-first-match-face nil :foreground "orange")

(defvar mode-line-cleaner-alist
 `((auto-complete-mode . " α")
  (yas/minor-mode . " υ")
  (paredit-mode . " π")
  (eldoc-mode . "")
  (abbrev-mode . "")
  (rmail-mode . "")
  (counsel-mode . ".")
  (palimpsest-mode . "")
  ;; Major modes
  (lisp-interaction-mode . "λ")
  (hi-lock-mode . "")
  (python-mode . "Py")
  (emacs-lisp-mode . "EL")
  (nxhtml-mode . "nx"))
 "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
 (interactive)
 (cl-loop for cleaner in mode-line-cleaner-alist
    do (let* ((mode (car cleaner))
         (mode-str (cdr cleaner))
         (old-mode-str (cdr (assq mode minor-mode-alist))))
       (when old-mode-str
         (setcar old-mode-str mode-str))
        ;; major mode
       (when (eq mode major-mode)
        (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
 "Show \"slim\" flymake status in mode line."
 (when e-w
  (setq flymake-mode-line-e-w e-w))
 (when status
  (setq flymake-mode-line-status status))
 (let* ((mode-line " Φ"))
  (when (> (length flymake-mode-line-e-w) 0)
   (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
  (setq mode-line (concat mode-line flymake-mode-line-status))
  (setq flymake-mode-line mode-line)
  (force-mode-line-update)))

(setq flymake-mode-line-format "")

(add-to-list 'mode-line-cleaner-alist '(org-ai-mode . ""))

(require 'project nil t)

(defun jay/mode-line--project-name ()
 "Return a short project name for the current buffer, if available."
 (let (name)
  (cond
   ((and (fboundp 'projectile-project-p)
         (ignore-errors (projectile-project-p)))
    (setq name (ignore-errors (projectile-project-name))))
   ((fboundp 'project-current)
    (let* ((project (ignore-errors (project-current)))
           (root (cond
                  ((and project (fboundp 'project-root))
                   (ignore-errors (project-root project)))
                  ((car-safe project))))
           (trimmed (and root
                         (file-name-nondirectory
                          (directory-file-name root)))))
     (setq name trimmed)))
   ((ignore-errors (vc-root-dir))
    (setq name (file-name-nondirectory
                (directory-file-name (vc-root-dir))))))
  (when (and name
             (not (equal name ""))
             (not (equal name "-")))
   name)))

(defun jay/mode-line-project-segment ()
 "Mode line segment showing the current project name."
 (let ((name (jay/mode-line--project-name)))
  (when name
   (concat " · " name))))

(setq-default mode-line-format
       (let* ((current (remove '(vc-mode vc-mode) mode-line-format))
              (segment '(:eval (jay/mode-line-project-segment)))
              (result nil)
              (inserted nil))
        (dolist (elem current)
         (push elem result)
         (when (and (not inserted)
                    (eq elem 'mode-line-buffer-identification))
          (push segment result)
          (setq inserted t)))
        (setq result (nreverse result))
        (if inserted
         result
        (append result (list segment)))))

(setq evil-want-keybinding nil)
(setq evil-mode-line-format nil)

(setq gcmh-mode-line nil)

(defun timesheet_insert-custom-clock-entry ()
 (interactive)
 (insert "CLOCK: ")
 (org-time-stamp-inactive)
 (insert "--")
 ;; Inserts the current time by default.
 (let ((current-prefix-arg '(4))) (call-interactively 'org-time-stamp-inactive))
 (org-ctrl-c-ctrl-c))

(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex" "odt" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

(add-to-list 'org-latex-classes
    '("my-letter"
     "\\documentclass\[%
 DIV=14,
 fontsize=12pt,
 parskip=half,
 subject=titled,
 backaddress=false,
 fromalign=left,
 fromemail=true,
 fromphone=true\]\{scrlttr2\}
 \[DEFAULT-PACKAGES]
 \[PACKAGES]
 \[EXTRA]"))

(defun load-koma-letter ()
 (interactive)
 (load "/Users/jay/emacs/emacs-settings/ox-koma-letter.el")
 (load "/Users/jay/emacs/emacs-settings/fiverr-koma.el")
 )

(setq enable-local-eval t)

;; Add near the TOP of your config, before other packages
(use-package saveplace
  :ensure nil  ; built-in
  :init
  (setq save-place-file (expand-file-name ".cache/places" user-emacs-directory))
  ;; Exclude org-roam files from save-place to avoid startup spam
  (setq save-place-ignore-regexps
        (list (rx (or "/roam/" "/org-roam/"))))
  :config
  (save-place-mode -1))

(setq org-export-preserve-breaks t)

(define-key key-minor-mode-map (kbd "M-=") 'calc-grab-region)

(setq delete-trailing-lines nil)

(setq create-lockfiles nil)
(run-with-idle-timer 300 t 'org-save-all-org-buffers)

(defun iloveyou (args)
 (interactive "P")
(message "%s" (propertize "I love you! ❤️" 'Face '(:foreground "red")))
 )

(defun get-openai-api-key ()
 "Retrieve the OpenAI API key from an external file."
 (let ((api-key-file "~/.openai-api-key"))
 (when (file-exists-p api-key-file)
  (with-temp-buffer
  (insert-file-contents api-key-file)
  (buffer-string)))))

(setq openai-api-key (get-openai-api-key))

(defun start-pomodoro ()
 "Starts a 25-minute pomodoro timer and logs the completed pomodoro in a file called `~/pomodori.txt`."
 (interactive)
 (run-at-time "25 min" nil
        (lambda ()
         (with-current-buffer (find-file-noselect "~/pomodori.txt")
          (goto-char (point-max))
          (insert (concat (format-time-string "%Y-%m-%d %H:%M:%S")
                  ": Completed pomodoro\n"))
          (save-buffer))))
 (message "Pomodoro timer started. Will log completed pomodoro in `~/pomodori.txt` at %s"
      (format-time-string "%Y-%m-%d %H:%M:%S" (+ (float-time) (* 60 25)))))

(use-package warnings
  :defer t)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(setq org-odt-preferred-output-format "docx")

(defun org-titlecase-level-1 ()
 "Convert all Level 1 org-mode headings to title case."
 (interactive)
 (save-excursion
 (goto-char (point-min))
 (while (re-search-forward "^\\* " nil t)
  (titlecase-line))))

(defun sentencecase-region (start end)
 "Convert the region to sentence case."
 (interactive "r")
 (let ((case-fold-search nil)) ; Make search case-sensitive
 (save-excursion
  (goto-char start)
  (downcase-region start end) ; Convert everything to lowercase first
  (while (< (point) end)
  (capitalize-word 1)  ; Capitalize the first word
  (forward-sentence))  ; Move to the next sentence
  (goto-char start)    ; Go back to the start to fix special cases
  (while (re-search-forward "\\bi\\b" end t)
  (replace-match "I"))
  (goto-char start)
  (while (re-search-forward "\\bi'" end t)
  (replace-match "I'"))
  (goto-char start)
  (while (re-search-forward "\\bjay\\b" end t)
  (replace-match "Jay"))
  (goto-char start)
  (while (re-search-forward "\\bsunjay\\b" end t)
  (replace-match "Sunjay"))
  (goto-char start)
  (while (re-search-forward "\\bdixit\\b" end t)
  (replace-match "Dixit")))))

(defun extract-hyperlinks-from-file (file)
 "Extracts all hyperlinks from the text file FILE."
 (interactive "fEnter file to extract hyperlinks from: ")
 (let ((hyperlinks '()))
  (with-temp-buffer
   (insert-file-contents file)
   (goto-char (point-min))
   (while (re-search-forward "\\(http\\|https\\|id\\)://[^[:space:]]+" nil t)
    (push (match-string-no-properties 0) hyperlinks)))
  (message "Hyperlinks extracted: %s" hyperlinks)
  hyperlinks))

(defun extract-hyperlinks-from-buffer ()
 "Extracts all hyperlinks from the current buffer."
 (interactive)
 (let ((hyperlinks '()))
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "\\[\\[id:[^]]+\\]\\]" nil t)
    (push (match-string-no-properties 0) hyperlinks)))
  (message "Hyperlinks extracted: %s" hyperlinks)
  hyperlinks))

(use-package key-chord
  :defer t)
(key-chord-mode 1)

(defun number-region (beg end)
 "Add numbering to a highlighted region."
 (interactive "r")
 (let ((counter 1)
    (end-marker (copy-marker end)))
  (save-excursion
   (goto-char beg)
   (beginning-of-line)
   (while (< (point) end-marker)
    (insert (format "%d. " counter))
    (setq counter (1+ counter))
    (forward-line 1))
   (set-marker end-marker nil))))

;; org-roam-config.el is already loaded in spacemacs-new-config.el
;; (load "/Users/jay/emacs/emacs-settings/org-roam-config.el")

;; Example configuration for Consult
(use-package consult
:defer t
 :after (consult-eglot)
 ;; Replace bindings. Lazily loaded due by `use-package'.
 :bind (
;; ("C-s s" . consult-line)
;; 	 ("C-s e" . consult-eglot-symbols)
;; 	 ("C-s r" . consult-ripgrep)
;; ("s-G" . consult-ripgrep)
;; 	 ("C-s f" . consult-find)
;; 	 ("C-s l" . consult-flymake)
;; 	 ("C-s n" . consult-focus-lines)
;; 	 ("C-s I" . consult-project-imenu)
;; 	 ("C-s o" . consult-outline)
;; 	 ("C-s b" . consult-bookmark)
;; 	 ("C-s m" . consult-man)

   )

 ;; Enable automatic preview at point in the *Completions* buffer. This nt when you use the default completion UI.
 :hook (completion-list-mode . consult-preview-at-point-mode)

 ;; The :init configuration is always executed (Not lazy)
 :init

 (global-unset-key (kbd "C-s"))

 ;; Optionally configure the register formatting. This improves the register
 ;; preview for `consult-register', `consult-register-load',
 ;; `consult-register-store' and the Emacs built-ins.
 (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)

 ;; Optionally tweak the register preview window.
 ;; This adds thin lines, sorting and hides the mode line of the window.
 (advice-add #'register-preview :override #'consult-register-window)

 ;; Use Consult to select xref locations with preview
 (setq xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)

 ;; Configure other variables and modes in the :config section,
 ;; after lazily loading the package.
 :config

 ;; Optionally configure preview. The default value
 ;; is 'any, such that any key triggers the preview.
 ;; (setq consult-preview-key 'any)
 ;; (setq consult-preview-key (kbd "M-."))
 ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
 ;; For some commands and buffer sources it is useful to configure the
 ;; :preview-key on a per-command basis using the `consult-customize' macro.
 (consult-customize
  consult-theme
  :preview-key '(:debounce 0.2 any)
  consult-ripgrep consult-git-grep consult-grep
  consult-bookmark consult-recent-file consult-xref
  consult--source-bookmark consult--source-recent-file
  consult--source-project-recent-file
  ;; :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
 ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
 ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
 ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
 ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ))

(key-chord-define-global "SS" 'consult-line)

(use-package org-roam-ui
:defer t
  :after org-roam
;;     normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;     a hookable mode anymore, you're advised to pick something yourself
;;     if you don't care about startup time, use
;; :hook (after-init . org-roam-ui-mode)

  :config
  (setq org-roam-ui-sync-theme t
     org-roam-ui-follow t
     org-roam-ui-update-on-save t
     org-roam-ui-open-on-start t))

(use-package orderless

 :defer t
 :ensure t
 :custom
 (completion-styles '(orderless))
 (completion-category-overrides '((command (styles . (partial-completion))))))

(use-package marginalia
:defer t
:ensure t
 :delight
 :custom
 (marginalia-max-relative-age 0)
 (marginalia-align 'right)
 :init
 (marginalia-mode))

 (use-package vertico
:defer t
:ensure t
   :init
 (vertico-mode)

 ;; Different scroll margin
 ;; (setq vertico-scroll-margin 0)

 ;; Show more candidates
 ;; (setq vertico-count 20)

 (setq vertico-resize t)

 ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
 ;; (setq vertico-cycle t)
 )

(defun show-duplicate-lines ()
 "Display all duplicate lines in the current buffer."
 (interactive)
 (let ((lines (split-string (buffer-string) "\n" t))
    (seen-lines '())
    (dup-lines '()))
  (dolist (line lines)
   (if (member line seen-lines)
     (setq dup-lines (cons line dup-lines))
    (setq seen-lines (cons line seen-lines))))
  (if dup-lines
    (with-output-to-temp-buffer "*Duplicate Lines*"
     (dolist (line (reverse dup-lines))
      (princ (concat line "\n"))))
   (message "No duplicate lines found."))))

(setq org-tag-alist '(("important" . ?i)
           ("urgent"  . ?u)))

(use-package openwith
  :defer t)
(setq openwith-associations '(("\\.pdf\\'" "open" (file))))
(openwith-mode t)

;; Prevent OpenWith from interfering with non-interactive file reads
;; during Org exports/publishing (e.g., resolving file: links to PDFs).
;; Some Org backends briefly open linked files to inspect them; OpenWith
;; would otherwise hijack that and raise an error by launching an external app.
;;
;; We advise the main export entry points to temporarily disable
;; `openwith-mode` for the duration of the export, restoring it afterward.
(defun jay/with-openwith-disabled (orig-fun &rest args)
  (let ((was-enabled (and (boundp 'openwith-mode) openwith-mode)))
    (when was-enabled (openwith-mode -1))
    (unwind-protect
        (apply orig-fun args)
      (when was-enabled (openwith-mode 1)))))

(with-eval-after-load 'org
  (advice-add 'org-export-to-file :around #'jay/with-openwith-disabled)
  (advice-add 'org-export-as :around #'jay/with-openwith-disabled))

(add-hook 'org-export-before-processing-hook 'my-load-org-latex-export-config)

(defun my-load-org-latex-export-config (&rest args)
  "Load the Org-mode LaTeX export configuration file and remove itself from the hook."
  (jay-load-latex)
  (remove-hook 'org-export-before-processing-hook 'my-load-org-latex-export-config))

(setq org-autolist-enable-delete nil)

(defun define-word--convert-html-tag-to-face (str)
 "Replace semantical HTML markup in STR with the relevant faces."
 (with-temp-buffer
  (insert str)
  ;; Remove empty <i></i> tags
  (goto-char (point-min))
  (while (re-search-forward "<i></i>" nil t)
   (replace-match ""))
  (cl-loop for (regexp face) in define-word--tag-faces do
     (define-word--regexp-to-face regexp face))
  (buffer-string)))

(use-package sdcv
:defer t
:custom
sdcv-popup-function 'showtip)

(defun consult-recenter-advice (orig-fn &rest args)
 "Advice for `recenter' to avoid errors when the window is not displaying the current buffer."
 (when (eq (window-buffer) (current-buffer))
  (apply orig-fn args)))

(advice-add 'recenter :around #'consult-recenter-advice)

(defun org-outdent-or-promote ()
 "Run either org-outdent-item-tree or org-promote-subtree,
depending on which one is appropriate based on the context."
 (interactive)
 (cond
 ;; If the cursor is on a plain list item, run org-outdent-item-tree
 ((org-at-item-p) (org-outdent-item-tree))
 ;; If the cursor is on a headline, run org-promote-subtree
 ((org-at-heading-p) (org-promote-subtree))
 ;; Otherwise, do nothing and show a message
 (t (message "Not at an item or a headline"))))

(defun org-indent-or-demote ()
 "Run either org-indent-item-tree or org-demote-subtree,
depending on which one is appropriate based on the context."
 (interactive)
 (cond
  ;; If the cursor is on a plain list item, run org-indent-item-tree
  ((org-at-item-p) (org-indent-item-tree))
  ;; If the cursor is on a headline, run org-demote-subtree
  ((org-at-heading-p) (org-demote-subtree))
  ;; Otherwise, do nothing and show a message
  (t (message "Not at an item or a headline"))))

; (define-key Info-mode-map (kbd "s-o") 'copy-region-to-other-window)
; (define-key org-mode-map (kbd "s-o") 'move-region-to-other-window)
(define-key Info-mode-map (kbd "s-[") 'Info-backward-node)
(define-key Info-mode-map (kbd "s-]") 'Info-forward-node)

(defun toggle-window-2-3 ()
 "Toggle between window 2 and window 3 using winum."
 (interactive)
 (let ((current-window (winum-get-number)))
  (cond ((= current-window 2)
      (winum-select-window-by-number 3))
     ((= current-window 3)
      (winum-select-window-by-number 2))
     (t
      (message "Point is not in window 2 or 3.")))))
;; (define-key key-minor-mode-map (kbd "s-`") 'toggle-window-2-3)

;;;; 0.  make Emacs start with your full shell environment  ;;;;


;; (use-package exec-path-from-shell
;;   :demand t
;;   :custom
;;   (exec-path-from-shell-arguments '("-l"))   ; no "-i"
;;   :config
;;   (exec-path-from-shell-initialize)
;; )

(setenv "FASD_DATA" (expand-file-name "~/.fasd"))

  ;; import PATH and other vars
  ;; (dolist (var '("PATH" "FASD_OPTS"))
  ;;   (exec-path-from-shell-copy-env var)))

;;;; 1.  fasd ----------------------------------------------------

(use-package ivy :ensure t)
(use-package fasd
  :ensure t
  :after ivy)

(use-package fasd
  :commands (fasd-find-file)
  :init
  ;; do NOT auto-prompt --- the command runs with an empty query
  (setq fasd-enable-initial-prompt nil)
  :config
  ;; ------------------------------------------------------------
  ;;  one single dispatcher
  ;; ------------------------------------------------------------
(defun fasd-find-file (&optional prefix query)
  "Jump to a file/dir with fasd.
PREFIX >1 ⇒ dirs only,  PREFIX <0 ⇒ files only,  none ⇒ both."
  (interactive "P")
  (unless (executable-find "fasd")
    (user-error "fasd executable not found in PATH"))
  ;; --- build the CLI we know works for us -------------------
  (let* ((type-opt (pcase (prefix-numeric-value prefix)
                     ((pred (< 0)) " -f")     ; negative = files only
                     ((pred (> 1)) " -d")     ; >1 = dirs only
                     (_ "")))                 ; default = both
         ;; Here we use  -s (score) + -l (list)
         (cmd (string-trim
               (format "fasd -sl%s %s"
                       type-opt
                       (shell-quote-argument (or query "")))))
         (cands (split-string (shell-command-to-string cmd) "\n" t)))
    (unless cands
      (user-error "fasd returned nothing (cmd was: %s)" cmd))
    (let ((choice (completing-read "fasd: " cands nil t)))
      (if (file-directory-p choice)
          (dired choice)
        (find-file choice)))))
)

;; ------------------------------------------------------------
;;  Global function to add paths to fasd
;; ------------------------------------------------------------
(defun my/fasd--add (path)
  "Register PATH in the fasd database, if fasd is available."
  (when (and path                       ; a real file/dir
             (executable-find "fasd")   ; the binary is in PATH
             (file-exists-p path))
    ;; run asynchronously - never blocks Emacs
    (start-process "fasd-add" nil "fasd" "--add" (expand-file-name path))))

;; ------------------------------------------------------------
;;  Hook it up to file visits
;; ------------------------------------------------------------
(add-hook 'find-file-hook          ; every real file you visit
          (lambda () (my/fasd--add buffer-file-name)))

(add-hook 'dired-after-readin-hook ; every directory you enter in Dired
          (lambda () (my/fasd--add
                      (if (stringp dired-directory)
                          dired-directory
                        (car dired-directory)))))

(use-package google-this
:defer t
:custom
(google-this-browse-url-function 'eww-browse-url)
  )

(defun google-word-and-display-in-EWW ()
 "Google the word at point and display the results in the minibuffer using EWW."
 (interactive)
 (let ((word (thing-at-point 'word t)))
  (if word
    (progn
     (eww-browse-url (concat "https://www.google.com/search?q=" word))
     (message "Showing results for %s in EWW" word))
   (message "No word at point"))))

(define-key key-minor-mode-map (kbd "M-g M-g") 'google-this)

(defun er-google ()
 "Google the selected region if any, display a query prompt otherwise."
 (interactive)
 (browse-url
  (concat
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  (url-hexify-string (if mark-active
     (buffer-substring (region-beginning) (region-end))
    (read-string "Google: "))))))

(use-package eww
:defer t
:bind
  (
   :map eww-mode-map
        ("o" . eww-browse-with-external-browser)
        ("r" . eww-reload)
        ("s-l" . eww)
        ("s-[" . eww-back-url)
        ("s-]" . eww-forward-url)
        ("s-o" . eww-open-file)
   ))

;; (setq consult-git-grep-args )

(use-package org-mac-link
  :ensure t        ;; auto‑install if needed (remove if you vendor it)
  :defer nil       ;; load immediately so your redefs take effect
  :config
  ;; (Anything that depends on org-mac-link being loaded can live here.)
  )

(defun org-mac-paste-applescript-links (as-link-list)
  "Convert AS-LINK-LIST returned by AppleScript to Org links and yank it.

Input format is one link per line, each line:
   \"URL::split::Title\""
  (let* ((raw (string-trim as-link-list "\"" "\""))
         (lines (split-string raw "[\r\n]+" t))
         (links (cl-loop for l in lines
                         for parts = (split-string l "::split::")
                         for url  = (car parts)
                         for desc = (cadr parts)
                         unless (string-empty-p url)
                         collect (org-make-link-string url desc))))
    (kill-new (string-join links "\n"))
    (car kill-ring)))

(defun org-mac-link-skim-open (uri _)
  "Open a Skim link URI.
Accepted formats:
  skim://path/to/file.pdf::123   ; jump to page 123
  skim://path/to/file.pdf        ; just open the PDF"
  (let* ((pos   (string-match "::\\([0-9]+\\)\\'" uri)) ; position of "::"
         (page  (if pos (match-string 1 uri) "1"))      ; default to page 1
         (doc   (if pos (substring uri 0 pos) uri)))
    (do-applescript
     (format
 "tell application \"Skim\"
    activate
    open (quoted form of POSIX file \"%s\")
    go document 1 to page %s of document 1
  end tell" doc page)
     )))

;;;###autoload
(defun org-mac-bing-insert-frontmost-url () …)
;;;###autoload
(defun org-mac-bing-get-frontmost-url () …)
;;;###autoload
(defun org-mac-link-skim-open (uri _) …)

(defun org-mac-link--clean-title (title)
  "Remove unwanted prefixes like '(2) ' from TITLE."
  (replace-regexp-in-string "^([0-9]+)\\s-+" "" title))

(defun org-mac-link-chrome-copy-frontmost-url ()
  "Retrieve the URL and title from the frontmost Google Chrome tab and copy as an Org-mode link."
  (interactive)
  (let* ((as-command
          "tell application \"Google Chrome\"
               if not (exists front window) then return \"NO_ACTIVE_TAB\"
               set theURL to URL of active tab of front window
               set theTitle to title of active tab of front window
               return theURL & \"§§§\" & theTitle
           end tell")
         (as-result (string-trim (shell-command-to-string (concat "osascript -e " (shell-quote-argument as-command))))))
    (if (string-equal as-result "NO_ACTIVE_TAB")
        (message "No active tab found in Google Chrome.")
      (let* ((components (split-string as-result "§§§"))
             (url (string-trim (nth 0 components)))
             (title (org-mac-link--clean-title (string-trim (nth 1 components))))
             (org-link (format "[[%s][%s]]" url title)))
        (kill-new org-link)
(message "Copied Org link to clipboard: %s" org-link)
))))

(defun org-mac-link-chrome-insert-frontmost-url ()
  "Insert the URL and title from the frontmost Google Chrome tab as an Org-mode link."
  (interactive)
  (let* ((as-command
          "tell application \"Google Chrome\"
               set theURL to URL of active tab of front window
               set theTitle to title of active tab of front window
               return theURL & \"§§§\" & theTitle
           end tell")
         (as-result (string-trim (shell-command-to-string (concat "osascript -e " (shell-quote-argument as-command)))))
         (components (split-string as-result "§§§"))
         (url (string-trim (nth 0 components)))
         (title (org-mac-link--clean-title (string-trim (nth 1 components))))
         (org-link (format "[[%s][%s]]" url title)))
    (insert org-link)
;; (message "Inserted Org link: %s" org-link)
))

;; or in Safari format
(defun org-mac-link-chrome-insert-frontmost-url (arg)
  "Insert URL and title from frontmost Chrome tab as Org-mode link.
With prefix ARG, insert using 'safari' custom link type."
  (interactive "P")
  (let* ((as-command
          "tell application \"Google Chrome\"
               set theURL to URL of active tab of front window
               set theTitle to title of active tab of front window
               return theURL & \"§§§\" & theTitle
           end tell")
         (as-result (string-trim (shell-command-to-string (concat "osascript -e " (shell-quote-argument as-command)))))
         (components (split-string as-result "§§§"))
         (url (string-trim (nth 0 components)))
         (title (org-mac-link--clean-title (string-trim (nth 1 components))))
         (org-link (if arg
                       (format "[[safari:%s][%s]]" url title) ; Safari format if universal arg
                     (format "[[%s][%s]]" url title))))       ; Default format otherwise
    (insert org-link)))

(defalias 'affe-grep-current-project 'affe-grep)
(defalias 'consult-grep-current-project 'consult-grep)

(defalias 'affe-find-filenames-and-folder-names 'affe-find)
(defalias 'consult-find-filenames-and-folder-names 'consult-find)

(defalias 'counsel-google 'counsel-search)
(defalias 'google-counsel-Google-with-autosuggest 'counsel-search)

(use-package org-roam-export
  :defer)

(defun org-export-id-link-removal (backend)
 "Inspired by 'org-attach-expand-links' ，which is in 'org-export-before-parsing-functions' "
 (save-excursion
  (while (re-search-forward "id:" nil t)
   (let ((link (org-element-context)))
    (if (and (eq 'link (org-element-type link))
         (string-equal "id"
                (org-element-property :type link)))
      (let ((description (and (org-element-property :contents-begin link)
                  (buffer-substring-no-properties
                   (org-element-property :contents-begin link)
                   (org-element-property :contents-end link))))
         )
       (goto-char (org-element-property :end link))
       (skip-chars-backward " \t")
       (delete-region (org-element-property :begin link) (point))
       (insert description))
     )))))

(add-to-list 'org-export-before-parsing-functions #'org-export-id-link-removal)

;; (setq org-agenda-files '("/Users/jay/dropbox/roam/notes"))

(setq org-attach-auto-tag "ATTACHMENTS")
(setq org-attach-id-dir "/Users/jay/dropbox/roam/attachments")
(defun org-roam-show-attachments (node)
  (when-let ((id (org-roam-node-id node))
             (folder (org-attach-dir-from-id id t))
             (attached-files (org-attach-file-list folder)))
    (magit-insert-section (org-roam-show-attachments)
      (magit-insert-heading "Attached Files")
      (dolist (file attached-files)
        (insert (org-roam-fontify-like-in-org-mode
                 (format " - [[file:%s/%s][%s]]\n" folder file file))))
      (insert "\n"))))

(setq org-id-method 'ts)
(setq org-attach-id-to-path-function-list
 '(org-attach-id-ts-folder-format
  org-attach-id-uuid-folder-format))

(winner-mode +1)
(define-key winner-mode-map (kbd "s-[") #'winner-undo)
(define-key winner-mode-map (kbd "s-]") #'winner-redo)

;; (rg-enable-default-bindings)

(use-package deadgrep
  :defer)
(define-key key-minor-mode-map (kbd "s-k d g") 'deadgrep-current-directory)

(defun timu/search-org-files ()
  "Grep for a string in the `~/org' using `rg'."
  (interactive)
(consult-ripgrep "~/org" ""))

 (defun timu/search-project-files ()
  "Grep for a string in the `~/projects' using `rg'."
  (interactive)
(consult-ripgrep "~/projects" ""))

(defun timu/org-go-to-heading (&optional arg)

"Go to an outline heading with `consult-org-heading'. Also move the heading to the top of the buffer with `evil-scroll-line-to-top'"

  (interactive)

(consult-org-heading)
(evil-scroll-line-to-top arg))

; dired Settings
(setq dired-clean-up-buffers-too nil
      dired-kept-versions 8)


;; (use-package dired-details+)


;; (defadvice dired-readin
;;     (after dired-after-updating-hook first () activate)
;;   "Sort dired listings with directories first before adding marks."
;;   (mydired-sort)
;;   (let ((dired-details-internal-overlay-list  ())) (dired-details-hide)))

(defcustom dired-details-hidden-string ""
  "*This string will be shown in place of file details and symbolic links."
  :group 'dired-details
  :type 'string)

(defcustom dired-details-initially-hide t
  "*Hide dired details on entry to dired buffers."
  :group 'dired-details
  :type 'boolean)

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
             (dired-move-to-filename))))

(defun timu-baseline-async-shell-command-no-window (command)

 "Do not display the `async-shell-command' COMMAND output buffer.

Credit: https://stackoverflow.com/a/60333836

Credit: https://stackoverflow.com/a/47910509."

 (interactive)

 (let ((display-buffer-alist

     (list (cons

        "\\*Async Shell Command\\*.*"

        (cons #'display-buffer-no-window nil)))))

  (async-shell-command command)))

(defun shell-open-dir ()
 "Open current directory at point with shell command \"open\".
This will open \"Finder.app\" at current location."
 (interactive)
 (timu-baseline-async-shell-command-no-window "open ./" ))

(defun dired-up-directory ()
 "Go up a directory in `dired'."
 (interactive)
 (find-alternate-file ".."))

(defun jay-up-directory ()
  "If in Dired, go up.  Otherwise open parent dir in Dired."
  (interactive)
  (cond
   ;; Already in Dired → behave like the native command
   ((derived-mode-p 'dired-mode)
    (find-alternate-file ".."))
   ;; Visiting a real file → open its parent directory
   (buffer-file-name
    (dired (file-name-directory buffer-file-name)))
   ;; Fallback: prompt for a directory
   (t
    (call-interactively #'dired))))

(defun dired-copy-path-at-point ()
 "Copy the full path of the at `point' to the `kill-ring'.
Credit: https://emacs.stackexchange.com/a/36851/30874"
 (interactive)
 (dired-copy-filename-as-kill 0))


(defun dired-shell-open-dir ()
 "Open current directory at point with shell command \"open\".
This will open \"Finder.app\" at current location."
 (interactive)
 (timu-baseline-async-shell-command-no-window "open ./" ))

(defun quicklook ()
 "Open the files at point with shell command \"qlmanage\".
This will display a Quicklook of the file at point in macOS."
 (interactive)
 (setq file (dired-get-file-for-visit))
 (timu-baseline-async-shell-command-no-window
  (concat "qlmanage -p " (shell-quote-argument file) " > /dev/null 2>&1")))

(add-to-list 'org-file-apps '(directory . emacs))

(defun touch-file (file)
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
   (dired-move-to-filename))))

(use-package dired-quick-sort
  :defer)

;; (define-key dired-mode-map (kbd "<return>") 'dired-find-file-other-window)

(defun timu-dired-open-in-external-app (&optional file)

 "Open the current FILE or Dired marked files in external app.

The app is chosen from your OS's preference.

Credit: http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html."

 (interactive)

 (let (doIt (myFileList

       (cond

        ((eq major-mode 'dired-mode)

        (dired-get-marked-files))

        ((not file) (list (buffer-file-name)))

        (file (list file)))))

  (setq doIt (if (<= (length myFileList) 30) t

         (y-or-n-p "Open more than 30 files? ")))

  (when doIt

   (cond

    ((string-equal system-type "windows-nt")

    (mapc (lambda (fPath)

        (w32-shell-execute

         "open" (replace-regexp-in-string "/" "\\" fPath t t)))

       myFileList))

    ((string-equal system-type "darwin")

    (mapc (lambda (fPath)

        (shell-command (format "open \"%s\"" fPath)))

       myFileList))

    ((string-equal system-type "gnu/linux")

    (mapc (lambda (fPath)

        (let ((process-connection-type nil))

         (start-process "" nil "xdg-open" fPath))) myFileList))))))

(defun nvalt ()
 "Open the directory in dired and run dired-preview."
 (interactive)

 (let ((directory "/Users/jay/Library/Mobile Documents/27N4MQEA55~pro~writer/Documents/"))
  (dired directory)
  (dired-preview-mode)))

(use-package counsel-fd
  :defer t)

(setq consult-locate-args "fasd -t -l -R")
(define-key key-minor-mode-map (kbd "C-x C-l") 'consult-locate)

(defun source-current-file ()
 "Source the current file in a shell."
 (interactive)
 (let* ((filename (buffer-file-name))
     (cmd (format "source %s" (shell-quote-argument filename)))
     (output (shell-command-to-string cmd)))
  (message "Output: %s" output)))

;; The fitness.org file calls this function to eval the configuration source
;; block. This is the only function that needs to be defined outside of
;; fitness.org.

(defun ap/org-call-src-block (name)
 ;; Based on <http://kitchingroup.cheme.cmu.edu/blog/2014/08/11/Using-org-mode-outside-of-Emacs-sort-of/>
 ;; This works better than the org-sbe (aka sbe) macro, because it
 ;; calls the block upon expansion, making it difficult to bind to
 ;; a command to run later
 ;; TODO: Use `org-babel-goto-named-src-block'! I guess it's new...or not, it's from 2010!
 (org-with-wide-buffer
  (-when-let (src (org-element-map (org-element-parse-buffer) 'src-block
           (lambda (element)
            (when (string= name (org-element-property :name element))
             element))
           nil ;info
           t ))
   (goto-char (org-element-property :begin src))
   (let ((org-confirm-babel-evaluate nil))
    (org-babel-execute-src-block)))))

(defvar move-or-copy-mode nil)

(defun select-move-mode-or-copy-mode ()
 (interactive)
 (setq move-or-copy-mode
    (completing-read "Would you like to move this region or just copy it? (move or copy): " '("Move" "Copy"))))

(defun move-or-copy-region-to-other-window (start end)
 "Move or copy selected text to other window based on move-or-copy-mode."
 (interactive "r")
 (unless move-or-copy-mode
  (select-move-mode-or-copy-mode))
 (let ((count (count-words-region start end)))
 (save-excursion
  (if (string= move-or-copy-mode "Move")
   (progn
   (kill-region start end)
   (message "Moved %s words" count))
  (progn
   (copy-region-as-kill start end)
   (message "Copied %s words" count)))
  (other-window 1)
  (newline)
  (yank)
  (newline)
  (other-window -1))))

(defvar refile-or-roam-refile-mode nil)

(defun select-refile-mode ()
 (interactive)
 (setq refile-or-roam-refile-mode
    (completing-read "Would you like to refile this region or subtree to Org file or Org-roam file? (org-refile or org-roam-refile): " '("org-refile" "org-roam-refile"))))

(defun choose-refile-method-and-refile ()
 "Choose the refile method and refile the current subtree or region based on refile-or-roam-refile-mode."
 (interactive)
 (unless refile-or-roam-refile-mode
  (select-refile-mode))
 (if (use-region-p)
   (save-excursion
    (if (string= refile-or-roam-refile-mode "org-refile")
      (progn
       (refile-region-or-subtree)
       (message "Refiled to Org file"))
     (progn
      (org-roam-refile-region-or-subtree)
      (message "Refiled to Org-roam file"))))
  (let ((current-heading (org-get-heading t t t t)))
   (save-excursion
    (if (string= refile-or-roam-refile-mode "org-refile")
      (progn
       (refile-region-or-subtree)
       (message "Refiled to Org file %s" current-heading))
     (progn
      (org-roam-refile-region-or-subtree)
      (message "Refiled to Org-roam file %s" current-heading)))))))

(defun activate-olivetti-in-split ()
 (when (not (bound-and-true-p disable-olivetti-auto-toggle))
  (if (< (window-width) 80)
    (olivetti-mode -1)
   (olivetti-mode 1))))

(add-hook 'org-mode-hook (lambda ()
              (add-hook 'window-configuration-change-hook 'activate-olivetti-in-split nil t)))

;; (global-yascroll-bar-mode 1)

(use-package ctrlf
  :defer)
;; (ctrlf-mode +1)
;; (define-key key-minor-mode-map (kbd "s-f") 'ctrlf-forward-default)
(define-key ctrlf-mode-map (kbd "s-g") 'ctrlf-next-match)
;; (define-key ctrlf-mode-map (kbd "s-g") 'ctrlf-next-match)

(ace-link-setup-default)

(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

(defun delete-visited-file (buffer-name)
 "Delete the file visited by the buffer named BUFFER-NAME."
 (interactive "Delete file visited by buffer ")
 (let* ((buffer (get-buffer buffer-name))
     (filename (buffer-file-name buffer)))
  (when buffer
   (when (and filename
         (file-exists-p filename))
    (delete-file filename))
   (kill-buffer buffer))))
(defalias 'crux-delete-buffer-and-file #'delete-visited-file)

(use-package ox-timeline

:config
 (setq org-timeline-source-url "/Users/jay/Dropbox/github/incandescentman.github.io/timeline/dist"))

(defun org-timeline-export-to-html-and-open ()
 "Export the current Org file to HTML using `org-timeline-export-to-html', then open the HTML file."
 (interactive)
 (let ((html-file (org-timeline-export-to-html)))
  (when html-file
   (browse-url (concat "file://" (expand-file-name html-file))))))

'(ox-clip-osx-cmd
  "pandoc -f html -t markdown - | grep -v \"^:::\" | sed 's/{#.*}//g' | pbcopy")
(defalias 'copy-as-markdown 'ox-clip-formatted-copy)

(defadvice recentf-save-list (around debug-recentf-save-list activate)
 (message "Saving recentf list...")
 ad-do-it
 (message "Saved recentf list"))

(defun my-update-recentf ()
 (copy-file "/Users/jay/emacs/recentf/recentf" "/Users/jay/.emacs.d/.cache/recentf" t))

(defadvice configuration-layer/update-packages
  (before my-update-recentf-before-update-packages activate)
 (copy-file "/Users/jay/emacs/recentf/recentf" "/Users/jay/.emacs.d/.cache/recentf" t))

(define-globalized-minor-mode olivetti-global-mode olivetti-mode
 (lambda ()
  (unless (minibufferp)
   (olivetti-mode 1))))

(olivetti-global-mode)

(setq org-time-stamp-custom-formats '("<%a %m/%e/%Y>" . "<%a %B %e %l:%M %p>"))
(setq-default org-display-custom-times nil)

;; Ensure the library is loaded before keybinding
(use-package smartparens
  :defer nil                       ; <--- load immediately
  :config
(smartparens-mode 1)
(smartparens-global-mode 1)

(setq sp-escape-quotes-after-insert nil)

)

;; (setq org-archive-location
;;  (concat "/Users/jay/Dropbox/roam/notes/archive/"
;;   (format-time-string "%Y-%m" (current-time)) "-%s::* "(format-time-string "%Y-%m-%d" (current-time))))

(defun my/org-archive-file ()
 "Dynamically set `org-archive-location' based on the current file directory."
 (let* ((current-file-dir (file-name-directory (buffer-file-name)))
     (archive-dir (concat current-file-dir "archive/"))
     (archive-file (concat archive-dir
                (format-time-string "%Y-%m" (current-time))
                "-%s::"
                (format-time-string "%Y-%m-%d" (current-time)))))
  (setq org-archive-location archive-file)))

;; Run the above function before every org-archive operation
(add-hook 'org-archive-hook 'my/org-archive-file)

(defun unfill-paragraph-keep-formatting (start end)
 "Unfill the region, but preserve plain-text lists and org-mode SCHEDULED tasks."
 (interactive "*r")
 (let ((fill-column (point-max)))
  (unfill-paragraph start end)))

(defun should-unfill-p (start end)
 "Determine whether the region should be unfilled."
 (save-excursion
  (goto-char start)
  (not (or (looking-at "^\\s-*\\([-*+]\\|[0-9]+[.)]\\)\\s-+") ;; plain-text list
       (looking-at "^\\s-*SCHEDULED:")           ;; org-mode SCHEDULED task
       (looking-at "^\\s-*DEADLINE:")            ;; org-mode DEADLINE
       ))))

(defun unfill-region-smart (start end)
 "Unfill the region, but preserve plain-text lists and org-mode SCHEDULED tasks."
 (interactive "*r")
 (let ((pos start))
  (while (< pos end)
   (let ((next-pos (or (next-single-property-change pos 'hard) end)))
    (when (should-unfill-p pos next-pos)
     (unfill-region-keep-formatting pos next-pos))
    (setq pos next-pos)))))

(defun region-to-numbered-list (start end)
  "Turn a region into a numbered list."
  (interactive "r")
  (let* ((s (buffer-substring-no-properties start end))
         (split-strings (split-string s "\\([0-9]+\\. \\)" t))
         (trimmed-strings (mapcar 'string-trim split-strings))
         (filtered-strings (cl-remove-if (lambda (x) (string= x "")) trimmed-strings))
         (numbered-strings
          (cl-loop for str in filtered-strings and i from 1
                   collect (concatenate 'string (number-to-string i) ". " str))))
    (delete-region start end)
    (insert (mapconcat 'identity numbered-strings "\n"))))

(defun clone-indirect-buffer-new-window-without-focus ()
  "Clone the current buffer to another window *but keep focus* in the original window."
  (interactive)
  (let ((original-window (selected-window)))
    ;; This built-in command automatically creates an indirect buffer
    ;; for the current buffer, displays it in another window,
    ;; and SELECTS that new window.
    (clone-indirect-buffer-other-window nil t)

    ;; If you want to narrow in Org mode:
    (when (derived-mode-p 'org-mode)
      (org-narrow-to-subtree))

    ;; Now go back to the original window:
    (select-window original-window)))

(defun clone-indirect-buffer-new-window-and-focus ()
  "Clone the current buffer to another window and KEEP focus in the new window."
  (interactive)
  ;; This will automatically create and select the new indirect buffer in another window
  (clone-indirect-buffer-other-window nil t)

  ;; Narrow if you wish:
  (when (derived-mode-p 'org-mode)
    (org-narrow-to-subtree))
  ;; Because clone-indirect-buffer-other-window already selects the new buffer,
  ;; you don't need to do anything else to maintain focus there.
)

(defface org-inline-tags-face
 '((t (:foreground "orange" :weight bold)))
 "Face for custom inline tags in plain list items.")

;; Simple font-lock keyword approach
(defun org-inline-tags-setup-font-lock ()
  "Setup font-lock for inline tags in org-mode."
  ;; Use a simple regex that matches tags
  (font-lock-add-keywords
   nil
   '(("\\(#\\w+\\(?:-\\w+\\)*\\)"
      (1 'org-inline-tags-face t)))
   t))

;; Function to fontify all tags in current buffer
(defun org-inline-tags-fontify-buffer ()
  "Manually fontify all inline tags in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "#\\w+\\(?:-\\w+\\)*" nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (put-text-property start end 'face 'org-inline-tags-face)
        (put-text-property start end 'font-lock-face 'org-inline-tags-face)))))

;;;###autoload
(defun org-inline-tags-mode-enable ()
  "Enable org-inline-tags highlighting and functionality."
  (interactive)
  ;; Add to hook for future buffers
  (add-hook 'org-mode-hook 'org-inline-tags-setup-font-lock)
  ;; Apply to current buffer if in org-mode
  (when (derived-mode-p 'org-mode)
    (org-inline-tags-setup-font-lock)
    ;; First apply font-lock rules
    (font-lock-mode 1)
    (font-lock-flush)
    (font-lock-ensure)
    ;; Then manually fontify existing tags
    (org-inline-tags-fontify-buffer))
  (message "Org inline tags mode enabled"))

;; Safe initialization - only run if org-mode is loaded
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'org-inline-tags-setup-font-lock))

;;;***autoload
(defun org-inline-tags-search (tag)
 "Search for TAG in the current org file."
 (org-search-view nil (concat "\\" "#" tag)))

;;;***autoload
(defun org-inline-tags-search-buffer (tag)
 "Search for TAG in the current buffer."
 (consult-line (concat "\\" "#" tag)))

;;;***autoload
(defun org-inline-tags-search-project-wide ()
 "Search for inline TAG project-wide using consult-ripgrep if available, otherwise use occur."
 (interactive)
 (if (fboundp 'consult-ripgrep)
   (progn
    (message "Enter tag to search for (Please go to the beginning of the line and add a \\ before the # sign)")
    (sit-for 0.5) ; pause for 0.5 to 2 seconds, whatever you waaant
    (consult-ripgrep nil))
  (occur (read-string "Enter tag to search for (Please go to the beginning of the line and add a \\ before the # sign): #"))))

(defun org-inline-tags-return (&optional indent)
 "Check if point is on an inline tag, and if so, search for that tag.
Otherwise, call `org-return'."
 (interactive)
 (let ((tag (with-syntax-table (let ((st (make-syntax-table)))
                 (modify-syntax-entry ?# "_" st)
                 st)
        (thing-at-point 'symbol))))
  (if (and tag (string-prefix-p "#" tag))
    (org-inline-tags-search-project-wide)
   (if (fboundp 'smart-return)
     (smart-return indent)
    (org-return indent)))))


;;;***autoload
(defun org-inline-tags-insert ()
 "Prompt the user to choose a tag and insert it at the current cursor position."
 (interactive)
 (let* ((tag-alist '((?r . "review")
           (?b . "book")
           (?t . "todo")
           (?u . "urgent")
           (?p . "tweet")
           (?i . "insight")
           (?c . "cook-ideas-over-time")))
     (selected-key (read-char "Choose a tag:\n
r: review
b: book
t: todo
u: urgent
p: tweet
i: insight
c: cook-ideas-over-time\n"))
     (selected-tag (cdr (assoc selected-key tag-alist))))
  (if selected-tag
    (insert (format " #%s" selected-tag))
   (error "Invalid tag selection"))))

;; for org-html-export
(defun my-org-add-class-to-hash-tags (text backend info)
  "Add class to words starting with a '#' in HTML export."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "\\<#\\w+\\>"
                              "<span class=\"hash-tag\">\\&</span>"
                              text)))

(add-to-list 'org-export-filter-plain-text-functions
    'my-org-add-class-to-hash-tags)

;; Strip emoji from LaTeX exports
(defun my-org-latex-strip-emoji (text backend info)
  "Remove emoji from LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "[🀀-🫿]" "" text)))

(add-to-list 'org-export-filter-plain-text-functions
    'my-org-latex-strip-emoji)

;; Also apply to headlines where emoji often appear
(add-to-list 'org-export-filter-headline-functions
    'my-org-latex-strip-emoji)



;; (define-key org-mode-map (kbd "<return>") 'org-inline-tags-return)

;; (define-key key-minor-mode-map (kbd "<return>") 'org-inline-tags-return)

;; (define-key key-minor-mode-map (kbd "s-:") 'insert-inline-tag)
;; (define-key key-minor-mode-map (kbd "s-;") 'search-for-inline-tag-project-wide)

(defun org-roam-auto-link-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((word (thing-at-point 'word 'no-properties))
             (node (car (org-roam-db-query [:select [id] :from nodes :where (= title $s1)] word))))
        (if node
            (progn
              (push-mark)
              (forward-word)
              (let ((region (list (mark) (point))))
                (org-roam-link-set-region (car region) (cadr region) node))))))))

(defun kb/find-node-backlink (arg &optional node choices)
 "Navigate notes by link. With universal ARG try to use only to navigate the tags of the current note. Optionally takes a selected NODE and filepaths CHOICES."
 (interactive "P")
 (let* ((depth (if (numberp arg) arg 1))
     (choices
     (or choices
       (when arg
        (-map #'org-roam-backlink-target-node (org-roam-backlinks-get (org-roam-node-from-id (or (ignore-errors (org-roam-node-id node))
                                                     (org-id-get-create))))))))
     (all-notes (org-roam-node--completions))
     (completions
     (or (--filter (-contains-p choices (cdr it)) all-notes) all-notes))
     (next-node
     ;; taken from org-roam-node-read
     (let* ((nodes completions)
         (node (completing-read
            "Node: "
            (lambda (string pred action)
             (if (eq action 'metadata)
               '(metadata
                (annotation-function . (lambda (title)
                             (funcall org-roam-node-annotation-function
                                 (get-text-property 0 'node title))))
                (category . org-roam-node))
              (complete-with-action action nodes string pred))))))
      (or (cdr (assoc node nodes))
        (org-roam-node-create :title node)))
     )
     )
  (if (equal node next-node)
    (org-roam-node-visit node)
   (kb/find-node-backlink nil next-node (cons next-node (-map #'org-roam-backlink-source-node (org-roam-backlinks-get next-node))))
   )))

(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
 '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
   (off . "<span class=\"task-todo\">&#x2610;</span>")
   (trans . "<span class=\"task-in-progress\">&#x25eb;</span>"))))

(defun org-mac-link-skim-insert-page ()
  "Insert the link to the page in the Skim.app. Include the page number.

  This function calls the org-mac-link-skim-get-page function to get
  a formatted org-mode link to the current page in the Skim.app PDF file.
  The resulting link is then inserted at the current point."
  (interactive)
  (insert (org-mac-link-skim-get-page)))



(defun org-mac-link-applescript-get-skim-page-link ()
 "AppleScript to get the link to the page in the Skim.app."
 (do-applescript
 (concat
 "tell application \"Skim\"\n"
 "set theDoc to front document\n"
 "set theTitle to (name of theDoc) as string\n"
 "set thePath to (path of theDoc) as string\n"
 "set thePage to (get index of current page of theDoc) as string\n"
 "set theSelection to selection of theDoc\n"
 "set theContent to contents of (get text of theSelection) as string\n"
 "if theContent is missing value or theContent is \"\" then\n" ;; Check for missing value or empty string
 " set theContent to theTitle & \", p. \" & thePage\n"
 "end if\n"
 "if theTitle ends with \".pdf\" then\n" ;; Check for .pdf at the end of the title
 " set theTitle to text 1 thru -5 of theTitle\n" ;; Remove the last 4 characters (.pdf)
 "end if\n"
 "set theLink to thePath & \"::\" & thePage\n" ;; Removed extra space before ::split::

 "end tell\n"
 "return theLink & \" ::split::\" & theTitle & \", p. \" & thePage as string\n"))) ;; Separated link and content

(defun org-mac-link-skim-get-page ()
 "Get the link to the page in the Skim.app."
 (interactive)
 (message "Applescript: Getting Skim page link...")
 (let* ((result (org-mac-link-applescript-get-skim-page-link))
   (result-parts (split-string result "::split::"))
   (link (car result-parts))
   (description (cadr result-parts)))
 ;; Remove starting and ending quotes from the link
 (when (string-prefix-p "\"" link)
  (setq link (substring link 1)))
 (when (string-suffix-p "\"" link)
  (setq link (substring link 0 -1)))
 ;; Remove leading and trailing whitespace from the link
  (setq link (string-trim link))
 ;; Remove starting and ending quotes from the description
 (when (string-prefix-p "\"" description)
  (setq description (substring description 1)))
 (when (string-suffix-p "\"" description)
  (setq description (substring description 0 -1)))
;; Trim leading and trailing whitespace from the description
 (setq description (string-trim description))
 ;; Remove .pdf from the description
 (when (string-suffix-p ".pdf" description)
  (setq description (substring description 0 -4)))
 ;; Replace underscores with spaces in the description
(setq description (replace-regexp-in-string "_" " " description))

 ;; Format the link
 (format "[[%s][%s]]" link description))) ;; Org-mode link format

(defun org-mac-link-skim-get-document ()
 "Get the link to the document in the Skim.app, without page number."
 (interactive)
 (message "Applescript: Getting Skim document link...")
 (let* ((result (org-mac-link-applescript-get-skim-page-link))
     (result-parts (split-string result "::split::"))
     (link (car result-parts))
     (description (cadr result-parts)))
  ;; Remove starting and ending quotes from the link
  (when (string-prefix-p "\"" link)
   (setq link (substring link 1)))
  (when (string-suffix-p "\"" link)
   (setq link (substring link 0 -1)))
  ;; Remove leading and trailing whitespace from the link
  (setq link (string-trim link))
  ;; Remove starting and ending quotes from the description
  (when (string-prefix-p "\"" description)
   (setq description (substring description 1)))
  (when (string-suffix-p "\"" description)
   (setq description (substring description 0 -1)))
;; Trim leading and trailing whitespace from the description
 (setq description (string-trim description))
  ;; Remove .pdf from the description
  (when (string-suffix-p ".pdf" description)
   (setq description (substring description 0 -4)))
  ;; Replace underscores with spaces in the description
(setq description (replace-regexp-in-string "_" " " description))
  ;; Remove page number from the description
  (when (string-match ", p\\. \\([0-9]+\\)" description)
   (setq description (replace-match "" nil nil description)))
  ;; Format the link
  (format "[[%s][%s]]" link description)))

(defun org-mac-link-skim-insert-document ()
 "Insert the link to the document in the Skim.app, without page number."
 (interactive)
 (insert (org-mac-link-skim-get-document)))

;; Skim insert link to PDF (or with C-u prefix, insert link to PDF including page number)
(defun org-mac-link-skim-insert (&optional arg)
 "Insert a link to the current Skim document or page.
With a universal argument, insert a link to the current page including page number."
 (interactive "P")
 (if arg
   (org-mac-link-skim-insert-page)
  (org-mac-link-skim-insert-document)))

(defun later-list ()
 (interactive)
 (find-file "/Users/jay/Dropbox/roam/notes/20230728235900-later_list.org"))

(defun redbold ()
  (interactive)
  (custom-set-faces
'(bold ((t (:inherit font-lock-warning-face :foreground "red" :weight bold)))))
  )

(add-hook 'org-mode-hook (lambda ()
       (redbold)))

(defun claire-share-export-subtree-to-html ()
 (interactive)
 (let* ((title (file-name-sans-extension (buffer-name)))
     (path "/users/jay/dropbox/roam/claire/")
     (export-line (concat "#+EXPORT_FILE_NAME: " path title ".html")))
  (save-excursion
   (goto-char (point-min))
   (forward-line 3)
   (unless (looking-at "#\\+EXPORT_FILE_NAME:")
    (insert (concat export-line "\n"))))
  (org-export-dispatch)))

(with-eval-after-load 'vertico
(define-key key-minor-mode-map (kbd "C-M-S-s-o") 'embark-act)
;; (define-key vertico-map (kbd "M-o") 'embark-act)
)

(defun copy-region-as-kill-and-push-to-clipboard (beg end)
  "Copy the region as kill and push it to the macOS clipboard.
BEG and END define the region to copy."
  (interactive "r")
  (copy-region-as-kill beg end)
  (push-kill-ring-pasteboard-to-MacOS-clipboard))

;; Simpler version - directly copies clipboard to kill ring
(defun clipboard-to-kill-ring ()
"Copies clipboard contents directly to kill ring."
(interactive)
(let ((clipboard-content (shell-command-to-string "pbpaste")))
(unless (string-equal clipboard-content "")
(kill-new clipboard-content))))

;; More complex version - strips trailing newline
(defun clipboard-to-kill-ring-and-strip-trailing-newlines ()
"Strips trailing newline from clipboard before copying to kill ring."
(interactive)
(let ((clipboard-content (shell-command-to-string "pbpaste")))
;; Check for non-empty clipboard contents
(when clipboard-content

  ;; Strip trailing newline if present
  (setq clipboard-content
        (if (string-match "\n\\'" clipboard-content)
            (replace-match "" t t clipboard-content)
          clipboard-content))

   ;; Copy to kill ring unless empty
   (unless (string-equal clipboard-content "")
     (kill-new clipboard-content)))))

(define-key key-minor-mode-map (kbd "M-%") 'eval-expression)
(define-key key-minor-mode-map (kbd "M-:") 'query-replace)

(defun insert-image-link-from-clipboard ()
  "Inserts an org-mode image link using the clipboard content as the image URL."
  (interactive)
  (let* ((mac-clipboard-content (gui-selection-value))
         (clip-content (if mac-clipboard-content
                           (progn
                             (kill-new mac-clipboard-content)
                             mac-clipboard-content)
                         (substring-no-properties (current-kill 0)))))
    (if clip-content
        (if (string-match-p "^http" clip-content)
            (progn
              (insert (format "[[img%s]]" clip-content))
              (org-display-inline-images))
          (message "Clipboard content does not start with 'http'."))
      (message "Clipboard is empty."))))

(defun insert-local-file-link-from-clipboard ()
  "Inserts an org-mode link using the clipboard content as the file path."
  (interactive)
  (clipboard-to-kill-ring)
  (let* ((clip-content (substring-no-properties (current-kill 0)))
         (file-name (file-name-nondirectory clip-content))
         (file-name-base (file-name-sans-extension file-name))
         (hyphen-and-underscore-replaced (replace-regexp-in-string "[-_]" " " file-name-base))
         (no-leading-numbers (replace-regexp-in-string "^[0-9]*" "" hyphen-and-underscore-replaced))
         (start-pos (point)))
    (insert (format "[[file+emacs:%s][📄 %s]]" clip-content (string-trim no-leading-numbers)))
    (goto-char start-pos)
    (search-forward "[[" nil t)
    (search-forward "[" nil t)
    (let ((title-start (point)))
      (search-forward "]" nil t)
      (backward-char)
      (capitalize-region title-start (point)))
    (goto-char (+ 2 start-pos))
    (search-forward "[" nil t)
    (search-forward "]" nil t))
  nil)  ; Explicitly return nil

(defun insert-directory-link-from-clipboard ()
 "Inserts an org-mode directory link using the clipboard content as the directory path."
 (interactive)
 (clipboard-to-kill-ring)
 (let* ((clip-content (substring-no-properties (current-kill 0)))
     (dir-name (file-name-nondirectory (directory-file-name clip-content)))
     (hyphen-and-underscore-replaced (replace-regexp-in-string "[-_]" " " dir-name))
     (start-pos (point)))
  (insert (format "[[%s][📁 %s]]" clip-content (string-trim hyphen-and-underscore-replaced)))
  (goto-char start-pos)
  (search-forward "[[" nil t)
  (search-forward "[" nil t)
  (let ((title-start (point)))
   (search-forward "]" nil t)
   (backward-char)
   (capitalize-region title-start (point)))
  (goto-char (+ 2 start-pos))
  (search-forward "[" nil t)
  (search-forward "]" nil t))
 nil) ; Explicitly return nil

(setq org-indent-indentation-per-level 2)

(add-hook 'text-mode-hook 'context-menu-mode)
(add-hook 'shell-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)
(add-hook 'org-mode-hook 'context-menu-mode)

; (add-hook 'context-menu-functions #'my-context-menu)



(defun redundant-delete-redundant-asterisks-in-org-headings ()
  "Remove redundant asterisks in Org-mode headings within the current buffer or selected region."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^\\(*+\\) \\(.*?\\)\\(*+\\)?\\s-*$" end t)
        (let ((stars (match-string 1))
              (heading (match-string 2)))
          (replace-match (concat stars " " (replace-regexp-in-string "\\*+" "" heading))))))))

(defun open-weeklies ()
  (interactive)
  (find-file "/Users/jay/Dropbox/roam/accountability/weeklies.txt")
  )

(defun split-window-left (&optional size)
 "Like split-window-right, with selected window on the right."
 (interactive "P")
 (split-window-right size)
 (other-window 1))

(defun export-to-pdf-and-prepend-cover-page ()
 "Export the current Org document to PDF and prepend a cover page located in the same directory. Then open the resulting PDF."
 (interactive)
 (let* ((output-pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
     (cover-page (concat (file-name-directory (buffer-file-name)) "cover-page.pdf"))
     (final-pdf (concat (file-name-sans-extension (buffer-file-name)) "_with-cover.pdf")))
  (org-latex-export-to-pdf)
  (shell-command (format "pdftk %s %s cat output %s"
              (shell-quote-argument cover-page)
              (shell-quote-argument output-pdf)
              (shell-quote-argument final-pdf)))
  (message "PDF exported and combined with cover page: %s" final-pdf)
  ;; Open the resulting PDF
  (cond
   ((string-equal system-type "darwin") ; macOS
   (shell-command (format "open %s" (shell-quote-argument final-pdf))))
   ((string-equal system-type "gnu/linux") ; Linux
   (shell-command (format "xdg-open %s" (shell-quote-argument final-pdf))))
   ((string-equal system-type "windows-nt") ; Windows
   (shell-command (format "start %s" (shell-quote-argument final-pdf)))))))

;; (setq org-preview-latex-default-process 'verbatim)

(defun open-labeled-address-in-google-maps ()
  "Find and open a labeled address in Google Maps, displaying the address in the minibuffer."
  (interactive)
  (save-excursion
    (let (address start end)
      (if (re-search-backward "^\\s-*\\(?:address:\\|Address:\\)\\s-*" nil t)
          (progn
            ;; Move past the label
            (goto-char (match-end 0))
            ;; Skip empty lines after the label
            (while (looking-at-p "^\\s-*$")
              (forward-line 1))
            (setq start (point))
            ;; Search forward for a ZIP code line ending
            (if (re-search-forward "\\b[0-9]\\{5\\}\\b\\s-*$" nil t)
                (progn
                  (setq end (match-end 0))
                  (setq address (buffer-substring-no-properties start end))
                  (setq address (replace-regexp-in-string "[ \t\n]+" " " (string-trim address)))
                  (message "Opening address: %s" address)
                  (browse-url (concat "https://www.google.com/maps/search/?api=1&query="
                                      (url-hexify-string address))))
              (message "ZIP code not found after address label!")))
        (message "No address label found!")))))

(use-package org-transclusion
  :config
  (setq org-transclusion-exclude-elements '(property-drawer))
  (setq org-transclusion-include-first-section nil)
  '(org-transclusion-extensions
    '(org-transclusion-src-lines org-transclusion-font-lock org-transclusion-indent-mode))
(custom-set-faces
 '(org-transclusion-fringe ((t (:background "yellow" :foreground "yellow")))))

;; Ensure that load-path to org-transclusion is already added
;; If you installed it with the built-in package.el, this should be already done.
;; (add-to-list 'load-path "path/to/org-transclusion/")
(add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
(require 'org-transclusion-indent-mode)
)

;; Customize the appearance of transclusion fringes
(set-face-attribute
 'org-transclusion-fringe nil
 :foreground "yellow"
 :background "yellow")

(load "/Users/jay/emacs/external-packages/org-transclusion-power-pack/org-transclusion-power-pack.el")

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)  ; Enable Flycheck globally
  :config
  ;; Turn off Flycheck in Org mode:
  (add-hook 'org-mode-hook (lambda () (flycheck-mode -1))))

(setq flycheck-mode-line nil)

;; smartparens configuration
(use-package smartparens-config
:defer t)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; web-mode configuration
;;(require 'web-mode)
;;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (add-hook 'web-mode-hook
;;      (lambda ()
;;       (setq web-mode-enable-auto-pairing nil)
;;       (setq web-mode-enable-auto-closing nil)))

;; Disable smartparens in web-mode
(add-hook 'web-mode-hook 'turn-off-smartparens-mode)

(defun pasteboard-copy-large-file ()
 "Copy the current region to the pasteboard using a temporary file."
 (interactive)
 (let* ((txt (buffer-substring (region-beginning) (region-end)))
     (temp-file (make-temp-file "emacs-pasteboard")))
  (with-temp-file temp-file
   (insert txt))
  (shell-command-to-string (format "cat %s | pbcopy" (shell-quote-argument temp-file)))
  (delete-file temp-file)))

(global-set-key (kbd "C-c C-c") 'pasteboard-copy)

(defun crux-delete-file-and-buffer ()
 "Kill the current buffer and deletes the file it is visiting."
 (interactive)
 (let ((filename (buffer-file-name)))
  (when filename
   (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
    (let ((vc-backend (vc-backend filename)))
     (when vc-backend
      (vc-delete-file filename))
     (call-process "/opt/homebrew/Cellar/trash/0.9.2/bin/trash" nil nil nil filename)
     (message "Moved file %s to your MacOS Trash 🗑️" filename)
     (kill-buffer))))))

(defun replace-in-all-files-in-current-directory (from-string to-string &optional regexp-flag)
  "Replace FROM-STRING with TO-STRING in every file under `default-directory'.

With a *prefix argument* (\\[universal-argument]) treat FROM-STRING as a
regular expression; otherwise perform a literal search/replace.

The command visits each regular file recursively, performs a
non-interactive query replace (i.e. no prompts while it works), saves
the buffer, and kills it when done."
  (interactive
   (let ((from (read-string "Replace: "))
         (to   (read-string "With: "))
         (regexp current-prefix-arg))      ;; C-u → regexp mode
     (list from to regexp)))

  (let* ((dir   default-directory)
         (files (directory-files-recursively dir ".*" nil))
         ;; Temporarily disable openwith-mode to prevent external program interference
         (openwith-mode-enabled (and (boundp 'openwith-mode) openwith-mode)))

    ;; Turn off openwith-mode if it's active
    (when openwith-mode-enabled
      (openwith-mode -1))

    (unwind-protect
        (dolist (file files)
          (when (file-regular-p file)
            ;; Use find-file-literally to avoid file handlers and mode hooks
            (let ((buf (find-file-noselect file t t)))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  ;; Read the file contents properly
                  (erase-buffer)
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (let ((replacements 0))
                    (if regexp-flag
                        (while (re-search-forward from-string nil t)
                          (replace-match to-string)
                          (setq replacements (1+ replacements)))
                      (while (search-forward from-string nil t)
                        (replace-match to-string)
                        (setq replacements (1+ replacements))))
                    (when (> replacements 0)
                      (message "Replaced %d occurrences in %s" replacements file)
                      (write-region nil nil file nil 'quiet)))
                  (kill-buffer buf))))))
      ;; Re-enable openwith-mode if it was active
      (when openwith-mode-enabled
        (openwith-mode 1)))))

;; Optional convenient alias:

;; (define-key dired-mode-map (kbd "s-H") 'query-replace-all-files-in-current-directory)

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (define-key dired-mode-map (kbd "s-H") 'query-replace-all-files-in-current-directory)))

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(defun force-split-window-vertically (&optional window)
 "Force splitting windows vertically."
 (let ((window (or window (selected-window))))
  (or (and (window-splittable-p window t)
       (with-selected-window window
        (split-window-right)))
    (split-window-sensibly window))))

(setq split-window-preferred-function #'force-split-window-vertically)

(setq org-roam-buffer-display-properties '((side . right) (slot . 1) (window-width . 0.3)))

(purpose-mode -1)

(setq max-lisp-eval-depth 10000)

(defun expand-outreach-snippet ()
 "Expand the yasnippet with key 'outreach'."
 (interactive)
 (yas-expand-snippet (yas-lookup-snippet "outreach")))

(defun expand-outreach-snippet-paste-copy-all ()
 "Expand the 'outreach' snippet, paste clipboard text into the buffer using `pasteboard-paste-adaptive`, and then select the entire buffer and copy it."
 (interactive)
 ;; Expand the snippet
 (expand-outreach-snippet)
 ;; Wait a moment to ensure snippet expansion completes
 (run-at-time "0.1 sec" nil
        (lambda ()
         ;; Paste clipboard text into buffer
         (pasteboard-paste-adaptive)
         ;; Select the entire buffer
         (goto-char (point-min))
         (push-mark (point-max) nil t)
         (activate-mark)
         ;; Copy the entire buffer to clipboard
         (pasteboard-copy))))

(defun expand-outreach-snippet-paste-copy-all-and-submit-ChatGPT ()
 "Expand the 'outreach' snippet, paste clipboard text into the buffer using `pasteboard-paste-adaptive`, and then select the entire buffer and copy it."
 (interactive)
 ;; Expand the snippet
 (expand-outreach-snippet)
 ;; Wait a moment to ensure snippet expansion completes
 (run-at-time "0.1 sec" nil
        (lambda ()
         ;; Paste clipboard text into buffer
         (pasteboard-paste-adaptive)
         ;; Select the entire buffer
         (goto-char (point-min))
         (push-mark (point-max) nil t)
         (activate-mark)
         ;; Copy the entire buffer to clipboard
         (pasteboard-copy)
         ;; Execute AppleScript to paste into ChatGPT
         (do-applescript
         "tell application \"ChatGPT\" to activate
          tell application \"System Events\"
           keystroke \"n\" using command down
           delay 1
           keystroke \"v\" using command down
           delay 1
           keystroke return
          end tell"))))

(define-key key-minor-mode-map (kbd "s-k e m") 'expand-outreach-snippet-paste-copy-all-and-submit-ChatGPT)

(with-eval-after-load 'yasnippet
 (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(defun replace-dashes-with-em-dashes ()
  "Replace all occurrences of '---' or '--' with em dashes in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; First, replace all occurrences of '---' with em dashes
    (while (search-forward "---" nil t)
      (replace-match "—" nil t))
    ;; Then, replace all occurrences of '--' with em dashes
    (goto-char (point-min))
    (while (search-forward "--" nil t)
      (replace-match "—" nil t))))

;; Define a list of additional paths
(defvar my-additional-paths
  '("/usr/local/bin"
    "/opt/homebrew/bin"
    "/Applications/Firefox.app/Contents/MacOS"))

;; Update PATH environment variable
(setenv "PATH"
        (concat (getenv "PATH") ":"
                (mapconcat 'identity my-additional-paths ":")))

;; Update exec-path
(dolist (path my-additional-paths)
  (add-to-list 'exec-path path))


;; Rest of your configuration
(message "Final PATH: %s" (getenv "PATH"))

(defun org-to-html-to-clipboard-hardcoded ()
  "Convert hardcoded org-mode text to HTML and copy it to the MacOS clipboard as HTML."
  (interactive)
  (let* ((org-content "Feel free to suggest some times that work for you or book time with me directly [[https://calendar.app.google/aSu61Nzz3PmxeYVaA][here]].")
         (html-content (org-export-string-as org-content 'html t))
         (temp-file (make-temp-file "clipboard-html-" nil ".html")))
    ;; Write the HTML content to a temporary file
    (with-temp-file temp-file
      (insert html-content))
    ;; Use AppleScript to read the file and set the clipboard
    (let ((script (format "set the clipboard to (read (POSIX file \"%s\")) as «class HTML»" temp-file)))
      (do-applescript script))
    ;; Delete the temporary file
    (delete-file temp-file)))

(add-to-list 'org-export-filter-timestamp-functions
       #'endless/filter-timestamp)
(defun endless/filter-timestamp (trans back _comm)
 "Remove <> around time-stamps."
 (pcase back
  ((or `jekyll `html)
   (replace-regexp-in-string "&[lg]t;" "" trans))
  (`latex
   (replace-regexp-in-string "[<>]" "" trans))))

(setq-default org-display-custom-times t)
;; Before you ask: No, removing the <> here doesn't work.
(setq org-time-stamp-custom-formats '("%B %d, %Y" . "%d/%m/%Y %a %H:%M"))

(setq debug-on-message "org-element--cache: Added org-data parent to non-headline element")

(with-eval-after-load 'smartparens
  (sp-local-pair 'org-mode "'" nil :actions nil)  ;; example: disable quote pairing
  (remove-hook 'org-mode-hook #'show-smartparens-mode))

; or more drastically:
; (remove-hook 'post-command-hook 'sp-show--pair-function)

(setq org-html-toplevel-hlevel 1)

(define-minor-mode my-org-config-mode
  "Minor mode for my custom Org file config."
  :init-value nil
  :lighter " myOrgCfg"
  (if my-org-config-mode
      (progn
        (define-key org-mode-map (kbd "<SPC>") #'insert-space)
        (make-local-variable 'abbrev-mode)
        (setq abbrev-mode nil)
        (setq-local org-config-files-local-mode t))
    ;; Optionally, unset org-config-files-local-mode when disabling:
    (kill-local-variable 'org-config-files-local-mode)))

(provide 'my-org-config-mode)

(defun jay-info-emacs-manual ()
  "Open the custom-built Emacs info manual."
  (interactive)
  (info "/Users/jay/emacs/emacs-fresh-source/info/emacs.info"))

;; (remove-hook 'before-save-hook #'org-roam-link-replace-all)

(defun isearch-forward-word-at-point ()
  "Search forward for the next occurrence of the word at point.
If invoked again, move to the next instance of the word."
  (interactive)
  (let ((word (thing-at-point 'word t)))  ;; Get word at point, non-nil for empty words
    (if (not word)
        (message "No word at point")
      (if (eq last-command this-command)
          (isearch-repeat-forward)  ;; Continue searching if repeated
        (progn
          (isearch-forward nil 1)
          (isearch-yank-string word))))))

(require 'timeline)

;;; Markdown and Calendar Setup

;; --------------------------------------------------------------------
;; 1) markdown-mode + bullet behavior
;; --------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  ;; Indentation for lists
  (setq markdown-list-indent-width 2)
  (define-key markdown-mode-map (kbd "RET") #'my-markdown-newline-with-bullet)
  (define-key markdown-mode-map (kbd "<tab>") #'my-markdown-cycle))

(defvar my-markdown--last-command-removed-bullet nil
  "Non-nil if the last call to `my-markdown-newline-with-bullet'
removed an empty bullet line.")

(defun my-markdown-newline-with-bullet ()
  "Insert or remove Markdown bullets intelligently.
- If line is an empty bullet (`  - `), remove it completely and leave one blank line.
- If line is a bullet plus text, insert another bullet on the next line.
- If the previous command removed a bullet and we're now on a blank line,
  do nothing extra (second Enter press).
- Otherwise, call `markdown-enter-key'."
  (interactive)
  (cond
   ;; -----------------------------
   ;; (1) Empty bullet line: remove entirely
   ;; -----------------------------
   ((save-excursion
      (beginning-of-line)
      (looking-at "^  -\\s*$"))
    ;; Remove the line including its trailing newline
    (delete-region (line-beginning-position)
                   (progn (forward-line 1) (point)))
    ;; If the next line is not empty, insert a blank line
    (unless (or (eobp) (looking-at "^\\s-*$"))
      (newline))
    (setq my-markdown--last-command-removed-bullet t))

   ;; -----------------------------
   ;; (2) Bullet plus text: continue the list
   ;; -----------------------------
   ((save-excursion
      (beginning-of-line)
      (looking-at "^  - +[^[:space:]]"))
    (end-of-line)
    (newline)
    (insert "  - ")
    (setq my-markdown--last-command-removed-bullet nil))

   ;; -----------------------------
   ;; (3) Second Enter press on the newly blank line: do nothing
   ;; -----------------------------
   ((and (or (bobp) (save-excursion
                      (beginning-of-line)
                      (looking-at "^\\s-*$")))
         my-markdown--last-command-removed-bullet)
    (setq my-markdown--last-command-removed-bullet nil)
    (message "Escaped bullet list."))

   ;; -----------------------------
   ;; (4) Fallback
   ;; -----------------------------
   (t
    (setq my-markdown--last-command-removed-bullet nil)
    (markdown-enter-key))))

  ;; Wrap lines visually (no hard breaks)
  (add-hook 'markdown-mode-hook #'visual-line-mode)

  ;; Outline settings: treat #, ##, ### as headings
  (setq outline-regexp "^\$begin:math:text$#+\\$end:math:text$ "
        outline-level
        (lambda ()
          (save-excursion
            (looking-at "^\$begin:math:text$#+\\$end:math:text$")
            (length (match-string 1)))))
  (add-hook 'markdown-mode-hook #'outline-minor-mode)

  ;; Highlight dates like "3/30/2025" in a special face
  (defface markdown-date-face
    '((t (:inherit font-lock-keyword-face :height 1.1 :weight bold)))
    "Face for date entries in markdown calendar.")
  (font-lock-add-keywords
   'markdown-mode
   '(("^[0-9]+/[0-9]+/[0-9]+" . 'markdown-date-face)))

;; --------------------------------------------------------------------
;; 3) (Optional) Visual Indentation with Overlays
;; --------------------------------------------------------------------
(defun my-markdown-current-level ()
  "Return the heading level of the nearest markdown heading above this line.
If none found, returns 1."
  (save-excursion
    (let ((level 1))
      (while (and (not (bobp)) (>= (point) (point-min)))
        (when (looking-at "^\$begin:math:text$#+\\$end:math:text$ ")
          (setq level (length (match-string 1)))
          (goto-char (point-min)))  ;; Stop after finding one
        (forward-line -1))
      level)))

(defun my-markdown-visual-indent ()
  "Visually indent lines under Markdown headings using overlays.
Indents each line by 2*(heading-level-1) spaces if inside a heading."
  (interactive)
  (let ((inhibit-read-only t))
    (remove-overlays (point-min) (point-max) 'my-markdown-indent t)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((level (my-markdown-current-level)))
          (when (> level 1)
            (let* ((spaces (make-string (* 2 (1- level)) ?\s))
                   (ov (make-overlay (point) (point))))
              (overlay-put ov 'my-markdown-indent t)
              (overlay-put ov 'display spaces)))
          (forward-line 1))))))

(defun my-markdown-visual-indent-refresh (_beg _end _len)
  (when (eq major-mode 'markdown-mode)
    (my-markdown-visual-indent)))

(add-hook 'markdown-mode-hook #'my-markdown-visual-indent)
(add-hook 'after-change-functions #'my-markdown-visual-indent-refresh)

;; --------------------------------------------------------------------
;; 4) Optional: Custom <tab> folding
;; --------------------------------------------------------------------
(defun my-markdown-cycle ()
  "Toggle visibility of current heading's children in markdown."
  (interactive)
  (if (outline-on-heading-p)
      (outline-toggle-children)
    (outline-back-to-heading)))

(defun jay/org-export-date-format-chooser ()
  "Interactively choose an Org-mode date format, set it globally, copy file-local definition to clipboard, and optionally insert as file-local variable."
  (interactive)
  (let* ((formats '("U.S. short: 12/25"
                    "U.S. numeric: 12/25/2025"
                    "ISO standard: 2025-12-25"
                    "Short: Dec 25, 2025"
                    "Medium: Mon Dec 25, 2025"
                    "Full: Monday Dec 25, 2025"
                    "Long: Monday December 25, 2025"))
         (choice (completing-read "Choose date format: " formats nil t))
         (format-string (pcase choice
                          ("U.S. short: 12/25" "%m/%d")
                          ("U.S. numeric: 12/25/2025" "%m/%d/%Y")
                          ("ISO standard: 2025-12-25" "%Y-%m-%d")
                          ("Short: Dec 25, 2025" "%b %d, %Y")
                          ("Medium: Mon Dec 25, 2025" "%a %b %d, %Y")
                          ("Full: Monday Dec 25, 2025" "%A %b %d, %Y")
                          ("Long: Monday December 25, 2025" "%A %B %d, %Y")))
         (file-local (format "# %s Variables:\n# org-export-date-timestamp-format: \"%s\"\n# %s:\n"
                             "Local" format-string "End")))

    ;; Set global variable
    (setq org-export-date-timestamp-format format-string)
    (message "Global Org export date format set to '%s'" format-string)

    ;; Copy file-local definition to clipboard
    (call-process-shell-command (format "echo -n '%s' | pbcopy" file-local))
    (message "File-local variable copied to clipboard.")

    ;; Prompt to insert into buffer
    (when (string= (completing-read "Insert file-local variable into buffer? " '("no" "yes") nil t nil nil "no") "yes")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n" file-local)))
      (message "Inserted file-local variable for date format '%s'" format-string))))

;; Stop org-expiry from stamping a CREATED property on new headings
(setq org-expiry-insert-created nil)
(remove-hook 'org-insert-heading-hook #'org-expiry-insert-created)

(defconst jay/org-date-formats
  '(("U.S. short: 12/25"              . "%m/%d")
    ("U.S. numeric: 12/25/2025"       . "%m/%d/%Y")
    ("ISO standard: 2025-12-25"       . "%Y-%m-%d")
    ("Short: Dec 25, 2025"            . "%b %d, %Y")
    ("Medium: Mon Dec 25, 2025"       . "%a %b %d, %Y")
    ("Full: Monday Dec 25, 2025"      . "%A %b %d, %Y")
    ("Long: Monday December 25, 2025" . "%A %B %d, %Y"))
  "Menu labels (with examples) → `format-time-string' specs.")

(defun jay/org-date-format-chooser ()
  "Set Org date formats for export, overlays, or both—always showing
time as \"7:00pm\" when the timestamp includes a clock."
  (interactive)
  (let* ((target (completing-read
                  "Configure: " '("export" "overlays" "both") nil t nil nil "both"))
         (need-export  (member target '("export" "both")))
         (need-overlay (member target '("overlays" "both")))
         (exp-fmt (and need-export
                       (cdr (assoc (completing-read
                                    "Export format: "
                                    (mapcar #'car jay/org-date-formats) nil t)
                                   jay/org-date-formats))))
         (ov-fmt  (and need-overlay
                       (cdr (assoc (completing-read
                                    "Overlay format: "
                                    (mapcar #'car jay/org-date-formats) nil t)
                                   jay/org-date-formats))))
         (block "# Local Variables:\n"))
    ;; ── apply selections ───────────────────────────────────────────
    (when need-export
      (setq org-export-date-timestamp-format exp-fmt)
      (setq block (concat block
                          (format "# org-export-date-timestamp-format: \"%s\"\n"
                                  exp-fmt))))
    (when need-overlay
      (setq org-display-custom-times t)
      (let* ((date-only ov-fmt)
             (date-time (concat ov-fmt " %-I:%M%P"))) ; 7:00pm
        (setq org-time-stamp-custom-formats
              (cons (format "<%s>" date-only)
                    (format "<%s>" date-time)))
        (setq block (concat block
                            (format "# org-time-stamp-custom-formats: '(\"<%s>\" . \"<%s>\")\n"
                                    date-only date-time)))))
    (setq block (concat block "# End:\n"))
    ;; ── clipboard + optional insert ───────────────────────────────
    (kill-new block)
    (when (yes-or-no-p "Insert Local Variables block here? ")
      (save-excursion
        (goto-char (point-max))
        (insert "\n" block)))
    (message "Updated %s format%s. Block copied (M-y to yank)."
             target (if (string= target "both") "s" ""))))

(org-link-set-parameters "safari"
                         :follow (lambda (url)
                                   (start-process "open-safari" nil "open" "-a" "Safari" url))
                         :export (lambda (url description backend)
                                   (format "<a href=\"%s\">%s</a>" url (or description url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow local variables to be set without prompting:
(setq enable-local-variables :all)

;; 1) Mark `enable-local-eval` as safe (boolean).
(put 'enable-local-eval 'safe-local-variable #'booleanp)

;; 2) Mark `org-config-files-local-mode` as safe (boolean).
(put 'org-config-files-local-mode 'safe-local-variable #'booleanp)

;; 3) Mark `lexical-binding` as safe (boolean).
(put 'lexical-binding 'safe-local-variable #'booleanp)

;; 4) Whitelist exact eval forms (Emacs compares them using `equal`).
(add-to-list 'safe-local-eval-forms '(org-config-files-local-mode 1))
(add-to-list 'safe-local-eval-forms '(org-config-files-local-mode t))
(add-to-list 'safe-local-eval-forms '(org-config-files-local-mode))
(add-to-list 'safe-local-eval-forms '(when (fboundp 'rainbow-mode) (rainbow-mode 1)))

;; 5) If you still want to mark other local variables as safe by value:
(dolist (var '((org-config-files-local-mode . t)
               (enable-local-eval . t)
               (lexical-binding . t)
               (org-export-allow-bind-keywords . t)
               (org-timestamp-custom-formats "%a %b %d:" . "<%d/%m/%Y %a %H:%M>")
               (org-timestamp-custom-formats "<%B %d" . "<%d/%m/%Y %a %H:%M>")))
  (add-to-list 'safe-local-variable-values var))

;; 6) Mark remote Org resources as safe (unrelated to eval, but good to keep).
(setq org-safe-remote-resources
      '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-bigblow\\.setup\\'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jay/fix-org-footnotes ()
  "Convert ‘([[fn:: URL][LABEL]])’ → ‘[fn:: [[URL][LABEL]]]’ in the
current Org buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; 1 = URL, 2 = label
    (let ((re "(\\[\\[fn::\\s-*\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]\\))"))
      (while (re-search-forward re nil t)
        (replace-match "[fn:: [[\\1][\\2]]]" t nil)))))

(setq debug-on-quit nil)
(add-to-list 'debug-ignored-errors 'minibuffer-quit)

(add-to-list 'debug-ignored-errors 'outline-before-first-heading)

;; Let C-g always abort in minibuffer, even under Evil
(with-eval-after-load 'evil
  (define-key minibuffer-local-map (kbd "C-g") #'keyboard-quit)
  (define-key evil-normal-state-map (kbd "C-g") #'keyboard-quit)) ; optional

(defun jay/debug-find-corrupted-file (&optional file)
  "Prompt for an Org file (default ~/Downloads) and try to parse it.

If `org-element-parse-buffer` signals an error, print the error.
Otherwise report success and, on confirmation, move the file to
~/Dropbox/roam/notes/ (directory must already exist)."
  (interactive
   (list
    (read-file-name
     "Org file to test: "
     "~/Downloads/"            ;; start here
     nil t                     ;; must exist, no default
     nil                       ;; no initial input
     (lambda (f) (string-match-p "\\.org\\'" f))))) ;; only .org files
  (let* ((debug-on-error nil)            ;; don't drop into debugger
         (result
          (with-temp-buffer
            (insert-file-contents file)
            (condition-case err
                (progn (org-element-parse-buffer) 'ok)
              (error err)))))
    (cond
     ;; ✅ parsed fine - maybe move it
     ((eq result 'ok)
      (message "✅  %s parsed with no errors" file)
      (when (yes-or-no-p "Move it to ~/Dropbox/roam/notes/? ")
        (let* ((dest-dir  "~/Dropbox/roam/notes/")
               (dest-file (expand-file-name (file-name-nondirectory file)
                                            dest-dir)))
          (rename-file file dest-file 1) ;; 1 = overwrite with confirmation
          (message "Moved to %s" dest-file))))
     ;; ❌ parse error - show the error object
     (t
      (message "❌  %s --- %S" file result)))))

(defun jay/debug-scan-org-tree (root)
  "Recursively scan ROOT for *.org files that choke `org-element-parse-buffer'.
Print a quick report in *Messages* and return a list of problematic files."
  (interactive "DDirectory to scan: ")
  (require 'org)                         ;; make sure Org is loaded
  (let ((bad nil)
        (total 0))
    (dolist (file (directory-files-recursively root "\\.org\\'"))
      (setq total (1+ total))
      (with-temp-buffer
        (insert-file-contents file nil nil nil 'replace)
        (condition-case err
            (org-element-parse-buffer)
          (error
           (push (cons file err) bad)))))
    (if bad
        (progn
          (message "❌  %d / %d Org files failed to parse:" (length bad) total)
          (dolist (p (reverse bad))
            (message "    %s  ---  %S" (car p) (cdr p))))
      (message "✅  All %d Org files parsed cleanly" total))
    bad))

(defun jay/debug-current-buffer ()
  "Parse the current Org buffer and echo success or first error."
  (interactive)
  (condition-case err
      (progn (org-element-parse-buffer) (message "✅ no parse errors"))
    (error (message "❌ %S" err))))

(defun jay/find-suspect-lines (&optional max-indent max-len)
  "Echo line numbers with very deep bullet indent or very long length.
Defaults: MAX-INDENT = 40 spaces, MAX-LEN = 800 chars."
  (interactive)
  (let* ((max-indent (or max-indent 40))
         (max-len   (or max-len   800))
         (re-bullet "^[ \t]*[-+*]\\|^[ \t]*[0-9]+[.)]"))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bol (point))
               (eol (line-end-position))
               (len (- eol bol))
               (indent (progn
                         (skip-chars-forward " \t")
                         (current-column))))
          (when (or (and (looking-at re-bullet) (>= indent max-indent))
                    (> len max-len))
            (message "⚠️  line %d: indent %d, length %d"
                     (line-number-at-pos) indent len)))
        (forward-line 1)))))

(use-package amx
  :ensure t
  :init
  ;; Tell AMX *before it loads* which back-ends to use.
  (setq amx-backend 'standard; no Ido/Vertico
        amx-history-length 99                     ; keep 99 commands
        amx-save-file     (locate-user-emacs-file "amx-items"))
  :config
  (amx-mode 1)     ; replace default M-x
  (savehist-mode 1))
(setq amx-save-file "/Users/jay/emacs/local-emacs-config/amx-items")
(setq package-quickstart-file
     "/Users/jay/emacs/local-emacs-config/package-quickstart.el")

;; (setq debug-on-quit nil)

(dolist (map '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map
;;  ivy-minibuffer-map
))      ; drop this line if not using Ivy
  (when (boundp map)
    (define-key (symbol-value map) (kbd "C-g") #'minibuffer-keyboard-quit)))

(setq org-element--cache-self-verify 'backtrace) ; default is nil

(require 'seq)
(require 'subr-x)

(defvar jay/org-cache-reset-roots
  (mapcar #'file-truename
          '("~/Library/CloudStorage/Dropbox/roam/"
            "~/Library/CloudStorage/Dropbox/github/roam-life/"))
  "Roots whose Org buffers should force-refresh the element cache on focus.")

(defun jay/org--buffer-needs-cache-reset-p ()
  "Return non-nil when the current buffer is an Org file under a tracked root."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (let ((truename (file-truename (buffer-file-name))))
      (seq-some (lambda (root)
                  (string-prefix-p root truename))
                jay/org-cache-reset-roots))))

(defun jay/org-reset-cache-on-focus ()
  "Force-refresh Org element caches for Dropbox-backed roam buffers."
  (when (fboundp 'org-element-cache-reset)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (jay/org--buffer-needs-cache-reset-p)
          (org-element-cache-reset 'force))))))

(add-hook 'focus-in-hook #'jay/org-reset-cache-on-focus)

;; ────────────────────────────────────────────────────────────
;; Guard org-indent's idle timer against rare parser errors
;; ────────────────────────────────────────────────────────────
(with-eval-after-load 'org-indent
  (defun my/org-indent-safe-agent (orig-fn &rest args)
    "Run ORIG-FN but ignore the occasional parser error."
    (ignore-errors
      (apply orig-fn args)))

  (ignore-errors                    ; remove duplicate wrapper, if any
    (advice-remove 'org-indent-initialize-agent
                   #'my/org-indent-safe-agent))

  (advice-add 'org-indent-initialize-agent
              :around #'my/org-indent-safe-agent))

(setq org-element-cache-diagnostic-output nil)

(run-with-timer 1 nil
  (lambda ()
    (when (display-graphic-p)
      (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
      (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'prepend))))

;; Speed up macOS open events
(setq ns-pop-up-frames nil)  ; Don't create new frames

;; Native comp quality-of-life
(setq native-comp-async-report-warnings-errors 'silent  ; hide noisy byte-native diffs
      comp-deferred-compilation           t           ; JIT-compile packages in the background
      comp-deferred-compilation-black-list '("/mu4e.*\\.el$")) ; skip anything poky

(global-display-line-numbers-mode 1)

(setq native-comp-jit-compilation t
package-native-compile    t)

;;; --- Emergency exit --------------------------------------------------------
;; Panic button: C-c C-! to save and exit
(global-set-key (kbd "C-c C-!")
                (lambda () (interactive)
                  (save-some-buffers t)
                  (kill-emacs)))

(setq debug-on-error 1)

(defun jd/maintain-quickstart ()
  "Regenerate `package-quickstart.el' whenever package list changes."
  (let* ((qs-file package-quickstart-file)
         (stamp   (file-attribute-modification-time
                   (file-attributes qs-file))))
    ;; Refresh if file is missing or older than 24 h
    (when (or (not (file-exists-p qs-file))
              (> (float-time (time-subtract (current-time) stamp))
                 (* 24 60 60)))
      (message "⟳  Refreshing package-quickstart...")
      (package-quickstart-refresh))))

;; Run once per boot, after packages are loaded:
(add-hook 'after-init-hook #'jd/maintain-quickstart)

;; Auto-refresh quickstart after package operations
(advice-add 'package-install :after
            (lambda (&rest _) (package-quickstart-refresh)))
(advice-add 'package-delete :after
            (lambda (&rest _) (package-quickstart-refresh)))

(use-package counsel :defer t)

;;; footnotes--convert-markdown-footnotes-to-org.el --- convert reference footnotes → Org  -*- lexical-binding: t; -*-
;; Author: you
;; Usage: mark region or use whole buffer, then  M-x footnotes--convert-markdown-footnotes-to-org
(require 'subr-x)                            ; for `string-trim'
(defun footnotes--convert-markdown-footnotes-to-org (&optional beg end)
  "Convert reference-style Markdown footnotes to Org-mode footnotes.
Inline examples:
   ([anthropic.com][8], [anthropic.com][7])  →  [fn:8], [fn:7]
   ( [foo][1] )                              →  [fn:1]
Definition examples:
   [8]: https://example.com \"Title\"        →  [fn:8] [[https://example.com][Title]]
With an active region, operate only on that region; otherwise
process the whole buffer."
  (interactive (when (use-region-p)
                 (list (region-beginning) (region-end))))
  (save-excursion
    (save-restriction
      (when beg (narrow-to-region beg end))
      ;; ------------------------------------------------------------
      ;; 1. INLINE MARKERS   [text][N]  →  [fn:N]
      ;; ------------------------------------------------------------
      (goto-char (point-min))
      (while (re-search-forward
              (rx "[" (+? (not "]")) "]"
                  "[" (group (+ digit)) "]")
              nil t)
        (replace-match (format "[fn:%s]" (match-string 1)) t t))
      ;; ------------------------------------------------------------
      ;; 2. STRIP PARENTHESES AROUND FOOTNOTE REFERENCES
      ;;    ([fn:8])  →  [fn:8]
      ;;    ([fn:8], [fn:7])  →  [fn:8], [fn:7]
      ;; ------------------------------------------------------------
      (goto-char (point-min))
      (while (re-search-forward
              ;; Match parentheses containing one or more [fn:N] references
              (rx "("
                  (group (0+ space)
                         "[fn:" (+ digit) "]"
                         (0+ (seq (0+ space) "," (0+ space) "[fn:" (+ digit) "]"))
                         (0+ space))
                  ")")
              nil t)
        (replace-match (match-string 1) t t))
      ;; ------------------------------------------------------------
      ;; 3. FOOTNOTE DEFINITIONS   [N]: URL \"Title\"  →  Org form
      ;; ------------------------------------------------------------
      (goto-char (point-min))
      (while (re-search-forward
              (rx line-start
                  "[" (group (+ digit)) "]:"
                  (+ blank)
                  (group (+ (not (any blank))))          ; URL
                  (? (+ blank) "\"" (group (+? (not "\""))) "\""))
              nil t)
        (let* ((num   (match-string 1))
               (url   (match-string 2))
               (title (string-trim (or (match-string 3) url))))
          (replace-match
           (format "[fn:%s] [[%s][%s]]" num url title) t t))))
    (message "Markdown reference footnotes → Org footnotes complete.")))
(provide 'footnotes--convert-markdown-footnotes-to-org)
;;; footnotes--convert-markdown-footnotes-to-org.el ends here

(defun asterisk-to-dash ()
  "Replace every newline-star sequence with newline-dash in the
current buffer.  Safe (save-excursion) and repeatable."
  (interactive)
  (save-excursion
;; (goto-char (point-min))
    (while (search-forward "\n* " nil t)
      (replace-match "\n- "))))

(defun asterisk-to-dash-and-convert-code-blocks-to-org ()
  "From point to end of buffer:
   • Change '\\n* ' to '\\n- '.
   • Change a line starting with two spaces + '* ' to two spaces + '- '.
   • Normalize dashes (add spaces around --- and convert ' - ' to ' --- ').
   Then run `convert-markdown-to-org-code-blocks-simple` if it exists."
  (interactive)
  ;; 1. newline *  → newline -
  (save-excursion
    (while (search-forward "\n* " nil t)
      (replace-match "\n- ")))
  ;; 2. line-start two-spaces *  → two-spaces -
  (save-excursion
    (while (re-search-forward "^  \\* " nil t)
      (replace-match "  - ")))
  ;; 3. normalize dashes
  (normalize-dashes)
  ;; 4. optional post-processing
  (when (fboundp 'convert-markdown-to-org-code-blocks-simple)
    (convert-markdown-to-org-code-blocks-simple)))

(defun normalize-dashes ()
  "• Add spaces around a naked em-dash marker (---).
• Promote a space-hyphen-space ( - ) to a spaced em-dash ( --- ).
• Never convert bullet point dashes (at start of lines or after only whitespace).
• Preserve Markdown table separator lines that rely on repeated hyphens."
  (interactive)
  (let ((count 0)
        (markdown-table-line-p
         (lambda (pos)
           (save-excursion
             (goto-char pos)
             (beginning-of-line)
             (looking-at-p "^[ \t]*|.*|[ \t]*$")))))
    (save-excursion
      ;; Pass 1 -- space out triple-hyphens between *any* non-whitespace chars
      (goto-char (point-min))
      (while (re-search-forward "\\(\\S-\\)---\\(\\S-\\)" nil t)
        (let ((table-line (funcall markdown-table-line-p (match-beginning 0))))
          (if table-line
              (goto-char (match-end 0))
            (replace-match "\\1 --- \\2" nil nil))))

      ;; Pass 2 -- turn space-hyphen-space into spaced em-dash
      ;; BUT only when NOT at beginning of line or after whitespace only
      (goto-char (point-min))
      (while (re-search-forward "\\(.\\) - " nil t)
        ;; Check if the character before space-hyphen-space is not whitespace
        ;; and we're not at the beginning of a line
        (let* ((char-before (match-string 1))
               (table-line (funcall markdown-table-line-p (match-beginning 0))))
          (unless (or table-line
                      (string-match-p "^[ \t\n]$" char-before))
            (replace-match "\\1 --- " nil nil)
            (setq count (1+ count)))
          (unless table-line
            (goto-char (match-end 0)))))

      (message "Replaced %d space-hyphen-space patterns" count))))

(defun my/org-wrap-bare-image-paths (_backend)
  "Turn any line that consists only of a pathname ending in
   .png/.jpg/.jpeg/.gif into  [[file:...]].
   Handles absolute paths (/...),  ./relative/...,  ../relative/...,  or
   plain relative like  mob-wife/foo.png."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            ;; line with optional spaces, *any* path that contains a slash and ends
            ;; in the right extension, then only spaces + EOL
            "^\\s-*\\([^[:space:]]*/[^[:space:]]*\\.\\(?:png\\|jpe?g\\|gif\\)\\)\\s-*$"
            nil t)
      (let* ((raw (match-string 1))
             ;; encode spaces only
             (encoded (replace-regexp-in-string " " "%20" raw t t)))
        (replace-match (format "[[file:%s]]" encoded) t t))))
  (buffer-string))

(add-hook 'org-export-before-processing-hook #'my/org-wrap-bare-image-paths)

(defun my/org-insert-image-links-from-dir (dir &optional as-bare-paths)
  "Insert Org links for every image in DIR.
If AS-BARE-PATHS is non-nil (or called with a prefix key),
insert the *bare relative pathname* instead of [[file:...]].
Supported extensions: png, jpg, jpeg, gif, svg, webp."
  (interactive "DDirectory with images: \nP")
  (let* ((img-ext '("png" "jpg" "jpeg" "gif" "svg" "webp"))
         (here    (file-name-directory (or (buffer-file-name) default-directory)))
         (files   (seq-filter
                   (lambda (f)
                     (and (file-regular-p f)
                          (member (downcase (file-name-extension f)) img-ext)))
                   (directory-files dir t "^[^.].*"))))   ; skip . and ..
    (unless files
      (user-error "No image files found in %s" dir))
    (dolist (f (sort files #'string<))
      (let ((rel (file-relative-name f here)))
        (insert (if as-bare-paths
                    rel
                  (format "[[file:%s]]" rel)))
        (insert "\n")))))

;;; --- batch exporter: one HTML file per heading -----------------------------

(defun my/slugify (s)
  "Turn a heading title S into a filesystem-safe slug."
  (let* ((down  (downcase
                 (replace-regexp-in-string "[^[:alnum:]]+" "-" s)))
         (clean (replace-regexp-in-string "^-\\|-\\'" "" down)))
    (replace-regexp-in-string "-+" "-" clean)))

(defun my/org-export-headings-to-html ()
  "Export each level-2 subtree to its own HTML file, plus index.html.
   Adds ../css/chatgpt-images.css to every page."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Save this buffer first - exporter needs a filename"))
  (let* ((base-dir (file-name-directory (buffer-file-name)))
         ;; The CSS lives one directory above base-dir
         (css-path (file-relative-name
                    (expand-file-name "../css/chatgpt-images.css" base-dir)
                    base-dir))
         (css-tag  (format "<link rel=\"stylesheet\" href=\"%s\"/>" css-path))
         (index-links '()))
    ;; ---------- export each level-2 heading ----------
    (org-map-entries
     (lambda ()
       (when (= (org-outline-level) 2)
         (let* ((title (nth 4 (org-heading-components)))
                (slug  (concat (my/slugify title) ".html"))
                (outfile (expand-file-name slug base-dir)))
           (save-restriction
             (org-narrow-to-subtree)
             (let ((org-html-head-extra css-tag))
               (org-export-to-file 'html outfile nil nil nil nil)))
           (push (format "- [[file:%s][%s]]" slug title) index-links))))
     nil 'file)
    ;; ---------- build the index ----------
    (with-temp-buffer
      (insert "* ChatGPT Use Cases for Mamie - Index\n\n")
      (dolist (l (nreverse index-links)) (insert l "\n"))
      (write-file (expand-file-name "index.org" base-dir))
      (let ((org-html-head-extra css-tag))
        (org-export-to-file 'html (expand-file-name "index.html" base-dir))))
    (message "✅ Exported %d pages + index" (length index-links))))

(with-eval-after-load 'info
  (set-face-attribute 'info-index-match nil
                      :background "#3a3a3a"
                      :foreground 'unspecified))

;; Better winner-mode fix
(setq winner-dont-bind-my-keys t)  ; Optional: if you have key conflicts
(with-eval-after-load 'winner
  (setq winner-boring-buffers-regexp
        "\\*[hH]elm.*\\*\\|\\*Compile-Log\\*\\|\\*Messages\\*"))

;; 1. Silence winner-mode bug (put in user-config)
(with-eval-after-load 'winner
  (advice-add 'winner-save-old-configurations
              :around (lambda (orig &rest args)
                        (ignore-errors (apply orig args)))))

;; Core major-mode foundation
(use-package web-mode
  :ensure t)

;; Minimal major mode for .astro files (inherits from web-mode)
(define-derived-mode astro-mode web-mode "Astro")
(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-mode))

(use-package eglot
  :ensure t
  :config
  ;; Use project.el's built-in vc-project type
  (require 'project)

  ;; Add package.json as a project root marker
  (add-to-list 'project-vc-extra-root-markers "package.json")

  ;; IMPORTANT: Clear any existing astro-mode entries first
  (setq eglot-server-programs
        (assq-delete-all 'astro-mode eglot-server-programs))

(add-to-list
 'eglot-server-programs
 `(astro-mode
   . ("npx" "@astrojs/language-server" "--stdio"
      :initializationOptions
      (:typescript
       (:tsdk ,(expand-file-name
                "/Users/jay/Library/CloudStorage/Dropbox/github/socratic-astro/node_modules/typescript/lib"))))))

  ;; Start the LS automatically when you open an .astro file
  (add-hook 'astro-mode-hook #'eglot-ensure))

;; Format-on-save only if Eglot is already managing the buffer
(defun my/astro-format-before-save ()
  (when (and (derived-mode-p 'astro-mode)
             (eglot-managed-p))
    (eglot-format-buffer)))
(add-hook 'before-save-hook #'my/astro-format-before-save)

;; overwrite mdx files
(setq revert-without-query '(".*\\.mdx$"))

(defun convert-markdown-to-org-code-blocks-simple ()
  "Simple version that replaces based on presence/absence of language specifier."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      ;; First pass: Replace opening fences (those with language specifiers)
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(````?\\)\\([[:alnum:]]+\\)[[:space:]]*$" nil t)
        (replace-match "\\1#+begin_src \\3" t))

      ;; Second pass: Replace closing fences (those without language specifiers)
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)\\(````?\\)[[:space:]]*$" nil t)
        (replace-match "\\1#+end_src \n" t))

      ;; Handle backticks in language specification
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:space:]]*\\)#\\+begin_src[[:space:]]+`\\([[:alnum:]]+\\)" nil t)
        (replace-match "\\1#+begin_src \\2" t)))))

;; Optional: Create keybindings
;; (global-set-key (kbd "C-c m o") 'convert-markdown-to-org-code-blocks)
;; (global-set-key (kbd "C-c m s") 'convert-markdown-to-org-code-blocks-simple)
;; (global-set-key (kbd "C-c m S") 'convert-markdown-to-org-code-blocks-stateful)

;; Display-aware tab bar management to hide hardware notches (built-in or external).
(defgroup jay/display nil
  "Display detection helpers for Jay's Emacs configuration."
  :group 'environment)

(defcustom jay/notch-display-identifiers '("Built-in" "Retina Display")
  "List of regex patterns identifying displays that need notch concealment.
Each string is matched case-sensitively against the monitor name returned by
`display-monitor-attributes-list'.  Add your external monitor's name here to
enable tab-bar-based notch hiding on that display as well."
  :type '(repeat (string :tag "Display name regexp"))
  :group 'jay/display)

(use-package tab-bar
  :defer t
  :config
  (tab-bar-mode -1))

(defun jay/get-frame-position ()
  "Get the actual numeric position of the frame, evaluating any symbolic expressions."
  (let ((left (frame-parameter nil 'left))
        (top (frame-parameter nil 'top)))
    (cons (if (numberp left) left (eval left))
          (if (numberp top) top (eval top)))))

(defun jay/current-display-attributes ()
  "Return the attribute alist for the display containing the current frame."
  (when (eq system-type 'darwin)
    (let* ((pos (jay/get-frame-position))
           (frame-x (car pos))
           (frame-y (cdr pos))
           (displays (display-monitor-attributes-list))
           (current-display nil))
      (dolist (display displays)
        (let* ((geometry (cdr (assoc 'geometry display)))
               (x (nth 0 geometry))
               (y (nth 1 geometry))
               (width (nth 2 geometry))
               (height (nth 3 geometry)))
          (when (and geometry
                     (>= frame-x x)
                     (<= frame-x (+ x width))
                     (>= frame-y y)
                     (<= frame-y (+ y height)))
            (setq current-display display))))
      current-display)))

(defun jay/--display-needs-notch-p (display)
  "Return non-nil when DISPLAY matches `jay/notch-display-identifiers'."
  (let ((name (cdr (assoc 'name display))))
    (when name
      (cl-some (lambda (needle)
                 (string-match-p needle name))
               jay/notch-display-identifiers))))

(defun jay/on-notch-display-p ()
  "Check if current frame is on a display that should hide a notch.
Matches the current monitor against `jay/notch-display-identifiers'."
  (let ((current-display (jay/current-display-attributes)))
    (and current-display
         (jay/--display-needs-notch-p current-display))))

(defun jay/update-tab-bar-for-display ()
  "Enable the tab bar when the frame is on a configured notch display and fullscreen.
Otherwise, keep it politely out of sight."
  (interactive)
  (let* ((fullscreen-state (frame-parameter nil 'fullscreen))
         (on-notch-display (jay/on-notch-display-p))
         ;; We regard either 'fullboth (native fullscreen) or 'maximized
         ;; as warranting notch-defence duty.
         (should-show     (and on-notch-display
                               (memq fullscreen-state '(fullboth maximized)))))
    (if should-show
        (progn
          (tab-bar-mode 1)
          (message "🍙🚫 Notch hidden 🎩✨"))
      (progn
        (tab-bar-mode -1)
;; (message "🎩 Notch hiding disabled.")
))))

;; Auto-update functions
(defun jay/frame-move-hook (frame)
  "Hook to run when frame might have moved."
  (when (eq frame (selected-frame))
    (jay/update-tab-bar-for-display)))

(defun jay/setup-display-aware-tab-bar ()
  "Activate minimal hooks for notch-hiding logic."
  (when (eq system-type 'darwin)
    ;; Clear any old hooks
    (remove-hook 'focus-in-hook            #'jay/update-tab-bar-for-display)
    (remove-hook 'after-make-frame-functions
                 (lambda (f) (with-selected-frame f
                               (jay/update-tab-bar-for-display))))
    ;; Re-add just one lightweight hook
    (add-hook 'focus-in-hook #'jay/update-tab-bar-for-display)
    ;; Initial check at startup
    (jay/update-tab-bar-for-display)))

;; Modified fullscreen toggle that updates tab bar
(defun jay/toggle-fullscreen ()
  "Toggle fullscreen mode with automatic tab bar management."
  (interactive)
  (let ((fullscreen-p (frame-parameter nil 'fullscreen)))
    (set-frame-parameter
     nil 'fullscreen
     (if fullscreen-p nil 'fullboth))
    ;; Update tab bar after changing fullscreen state
    (run-with-timer 0.1 nil 'jay/update-tab-bar-for-display)
    ;; Adjust font size if available
    (when (fboundp 'jay/adjust-font-size)
      (jay/adjust-font-size))))

;; Debug function to check current status
(defun jay/debug-display-detection ()
  "Show current display detection status."
  (interactive)
  (let* ((on-notch-display (jay/on-notch-display-p))
         (pos (jay/get-frame-position))
         (displays (display-monitor-attributes-list))
         (display-info (mapconcat
                       (lambda (d)
                         (format "%s: %s"
                                 (or (cdr (assoc 'name d)) "Unknown")
                                 (cdr (assoc 'geometry d))))
                       displays ", ")))
    (message "🔍 Position: %s | Notch display: %s | Monitors: %s"
             pos (if on-notch-display "Yes" "No") display-info)))

(defun jay/add-current-display-to-notch-list ()
  "Add the current monitor's name to `jay/notch-display-identifiers'.
This updates the running Emacs session; use Customize to persist the change."
  (interactive)
  (if-let* ((display (jay/current-display-attributes))
            (name (cdr (assoc 'name display))))
      (progn
        (cl-pushnew name jay/notch-display-identifiers :test #'string=)
        (message "➕ Added \"%s\" to jay/notch-display-identifiers (not persisted)" name))
    (message "⚠️ Unable to detect current display name.")))

;; Initialize on load
(when (eq system-type 'darwin)
  (run-with-timer 1 nil 'jay/setup-display-aware-tab-bar))



(defun my/skip-flyspell-on-large-buffers ()
  (when (> (buffer-size) (* 1 1024 1024)) ; >1 MB
    (flyspell-mode -1)))
(add-hook 'flyspell-mode-hook #'my/skip-flyspell-on-large-buffers)

(use-package cape
  :defer
  :init
  (add-to-list 'completion-at-point-functions #'cape-emoji))

;; --- choose your modifier once --------------------------
(defconst my/emoji-tone "🏽")     ;; U+1F3FD  (medium skin tone)
;; --------------------------------------------------------

(defun my/emoji-tonify (emoji)
  "If EMOJI can take a skin-tone, append MY/EMOJI-TONE to it."
  (if (string-match-p "\\p{Emoji_Modifier_Base}" emoji)
      (concat emoji my/emoji-tone)
    emoji))

;; Advice Cape's inserter so every completion runs through our filter.
(advice-add #'cape--completion-insert
            :filter-args (lambda (args)
                           (setcar args (my/emoji-tonify (car args)))
                           args))

(setq org-astro-known-posts-folders
      '(("actions" . "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/actions/src/content/blog")
        ("jaydocs" . "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/jaydocs/src/content/blog")
        ("socratic" . "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/socratic/src/content/blog")

        ("kanban" . "/Users/jay/Library/CloudStorage/Dropbox/github/rising-action-kanban/content")
))

;; Completely disable native compilation warnings/errors logging
(setq native-comp-async-report-warnings-errors 'silent) 

;; Prevent *Async-native-compile-log* from popping up
(setq native-comp-async-report-warnings-errors nil)

(defvar my-auto-kill-buffers
  '("*Async-native-compile-log*")
  "List of buffers to kill after startup.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (dolist (buf my-auto-kill-buffers)
              (when (get-buffer buf)
                (kill-buffer buf)))))

;; Then configure
(use-package fsrs
  :ensure nil  ;; already installed via package-vc
  :defer t)

(use-package org-srs
  :ensure nil  ;; already installed via package-vc
  :after fsrs
  :hook (org-mode . org-srs-embed-overlay-mode)
  :bind (:map org-mode-map
         ("<f5>" . org-srs-review-rate-easy)
         ("<f6>" . org-srs-review-rate-good)
         ("<f7>" . org-srs-review-rate-hard)
         ("<f8>" . org-srs-review-rate-again)))

(use-package url
  :defer)

(add-to-list 'load-path "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/")
(use-package ox-astro
  :load-path "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/"
  :after ox
  :ensure nil)
(setq org-astro-debug-images t)
;; Configure source root folder for ox-astro
(setq org-astro-source-root-folder
      "/Users/jay/Library/CloudStorage/Dropbox/roam")
;; Configure known posts folders with folder structure preservation
(setq org-astro-known-posts-folders
      '(("blog" . (:path "~/projects/my-astro-site/src/content/blog"))
        ("jaydocs" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/jaydocs/src/content/blog"))
        ("socraticai" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/socratic/src/content/blog"))
        ("astro-roam" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-roam/src/content"))
        ("my-life" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/me/my-life/src/content"))
        ("my-mind" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/me/my-mind/src/content"
                      :preserve-folder-structure t))))

;; --- Org-roam contexts --- --- -- --- -- --- -- --- -- --- -- --- -- --- --

(defun jay/org-roam--use (dir)
  "Switch org-roam to DIR and resync DB."
  (setq org-roam-directory (file-truename dir))
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  ;; Optional per-context dailies folder:
  ;; (setq org-roam-dailies-directory "daily/")
  (org-roam-db-sync)
  (message "org-roam → %s" org-roam-directory))

(defun jay/org-roam-use-main ()
  "Main notes."
  (interactive)
  (jay/org-roam--use "/Users/jay/Library/CloudStorage/Dropbox/roam/"))

(defun jay/org-roam-use-life ()
  "Life archive."
  (interactive)
  (jay/org-roam--use "/Users/jay/Library/CloudStorage/Dropbox/github/roam-life/"))

(require 'ox-json)

;; Auto-reload files changed on disk
(global-auto-revert-mode 1)

;; Nice-to-haves
(setq auto-revert-use-notify t            ; use file system events when possible
      auto-revert-interval 1              ; fallback poll interval (seconds)
      auto-revert-verbose nil             ; no echo area messages
      global-auto-revert-non-file-buffers t) ; also refresh Dired, etc.

(defun kill-unwanted-buffers ()
  "Kill common unwanted buffers like *spacemacs*, *info*, and *Async-native-compile-log*."
  (interactive)
  (dolist (buf '("*spacemacs*" "*info*" "*Async-native-compile-log*" "*Shell Command Output*"))
    (when (get-buffer buf)
      (kill-buffer buf))))

(add-hook 'emacs-startup-hook #'kill-unwanted-buffers)



(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "s-[") 'Info-backward-node)
  (define-key Info-mode-map (kbd "s-]") 'Info-forward-node)
  (define-key Info-mode-map (kbd "<s-up>") 'Info-up)
  (define-key Info-mode-map (kbd "s-l") 'Info-goto-node))

(defun jay/info-local-overrides ()
  "Ensure Jay's super key aliases work inside `Info-mode'."
  (let* ((override (make-sparse-keymap))
         (alist (or minor-mode-overriding-map-alist '())))
    (define-key override (kbd "s-[") #'Info-backward-node)
    (define-key override (kbd "s-]") #'Info-forward-node)
    (define-key override (kbd "<s-up>") #'Info-up)
    (define-key override (kbd "s-l") #'Info-goto-node)
    (setq alist (assq-delete-all 'key-minor-mode alist))
    (setq alist (assq-delete-all 'winner-mode alist))
    (push (cons 'key-minor-mode override) alist)
    (push (cons 'winner-mode override) alist)
    (set (make-local-variable 'minor-mode-overriding-map-alist) alist)))

(add-hook 'Info-mode-hook #'jay/info-local-overrides)
