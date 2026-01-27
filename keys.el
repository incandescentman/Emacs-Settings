;;; keys.el --- all my key bindings  -*- lexical-binding: t; -*-

;;;; 0.  Override Minor Mode Definition
(defvar key-minor-mode-map (make-sparse-keymap)
  "Keymap that should win against major‑mode maps.")

;; NOTE → make it *global* so one activation covers every buffer.
(define-minor-mode key-minor-mode
  "Enable my global keys in spite of major‑mode maps."
  :init-value t                ; turned on once we load this file
  :global     t               ; <<< important – fixes missing bindings
  :lighter     " key"
  :keymap     key-minor-mode-map)

;; Disable the override map inside the minibuffer, re-enable on exit
(add-hook 'minibuffer-setup-hook (lambda () (key-minor-mode -1)))
(add-hook 'minibuffer-exit-hook  (lambda () (key-minor-mode  1)))

;;;; 1. Helper Functions ------------------------------------------------

(defun visit-messages-buffer ()
  "Switch to the *Messages* buffer in another window."
  (interactive)
  (view-echo-area-messages)
  (other-window 1))

(defun visit-messages-buffer-full-screen ()
  "Visit the *Messages* buffer in full screen."
  (interactive)
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (switch-to-buffer (current-buffer))))

(defun copy-minibuffer-contents (&optional _arg)
  "Copy the entire contents of the minibuffer to the kill‑ring."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char (point-max))
    (copy-region-as-kill (region-beginning) (region-end))))

(defun my/org-show-level-1 () "Show org headings to level 1." (interactive) (org-show-level 1))
(defun my/org-show-level-2 () "Show org headings to level 2." (interactive) (org-show-level 2))
(defun my/org-show-level-3 () "Show org headings to level 3." (interactive) (org-show-level 3))
(defun my/org-show-level-4 () "Show org headings to level 4." (interactive) (org-show-level 4))
(defun my/org-show-level-5 () "Show org headings to level 5." (interactive) (org-show-level 5))
(defun my/org-show-level-6 () "Show org headings to level 6." (interactive) (org-show-level 6))
(defun my/org-show-level-7 () "Show org headings to level 7." (interactive) (org-show-level 7))
(defun my/org-show-level-8 () "Show org headings to level 8." (interactive) (org-show-level 8))

(defun my/org-agenda-day ()
  "Open the org agenda day view."
  (interactive)
  (org-agenda nil "d"))

(defun my/panic-save-and-quit ()
  "Save all buffers and kill Emacs immediately."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(defun my/scroll-up ()
  "Scroll up (for mouse wheel binding)."
  (interactive)
  (scroll-up-command))

(defun my/scroll-down ()
  "Scroll down (for mouse wheel binding)."
  (interactive)
  (scroll-down-command))

;;;; 2. Keybinding Tables ----------------------------------------------

(defconst my/global-key-bindings
  '(;; ==================================================================
    ;; File Finding & Navigation
    ;; ==================================================================
    ("s-R"           . fasd-find-file)
    ("s-r"           . counsel-recentf)
    ("C-s-r"         . consult-find)
    ("M-s-d"         . counsel-find-file)
    ("s-k e e"       . fasd-find-file)
    ("s-k f z"       . counsel-fzf)
    ("s-P"           . projectile-find-file)
    ("s-\\"         . visit-most-recent-file)
    ("C-x C-d"       . dired) ; keep dired here – consult-dir is on "C-x C-j"
    ("C-x C-f"       . find-file-at-point-or-affe-find)
    ("C-x C-j"       . consult-dir)
    ("s-/ f p"       . search-filename-proposal-directory)
    ("s-/ f B"       . search-filename-book-directory)
    ("s-/ f b"       . search-filename-both-book-and-proposal-directories)

    ;; ==================================================================
    ;; Search & Replace
    ;; ==================================================================
    ("C-s"           . consult-line)
    ("s-f"           . isearch-forward-ignore-case)
    ("s-F"           . pasteboard-search-for-clipboard-contents)
    ("s-h"           . replace-string)
    ("s-g"           . isearch-repeat-forward)
    ("C-s-g "        . consult-ripgrep-current-directory)
    ("s-G"           . counsel-projectile-ag)
    ("C-s-f"         . isearch-forward-word-at-point)
    ("s-k ag"        . affe-grep)
    ("s-k d g"       . deadgrep-current-directory)
    ("s-k rg"        . consult-ripgrep-current-directory)
    ("s-k g l"       . affe-grep-gnulisp-directory)
    ("s-k y a"       . affe-grep-gnulisp-directory)
    ("s-k w s"       . isearch-forward-word)
    ("M-s b"         . book-search)
    ("M-s c"         . current-buffers-search)
    ("s-/ gr"        . consult-grep)
    ("s-/ g l"       . affe-grep-gnulisp-directory)
    ("s-/ g ub"      . affe-grep-bash-scripts)
    ("s-/ g up"      . affe-grep-bash-profile)
    ("C-x C-l"       . consult-locate)
    ("s-8"           . search-open-buffers)
    ("M-g M-g"       . google-this)
    ("M-:"           . query-replace)
    ("s-k rr"        . replace-regexp)

    ;; ==================================================================
    ;; Window Management
    ;; ==================================================================
    ("M-1"           . winum-select-window-1)
    ("M-2"           . winum-select-window-2)
    ("M-3"           . winum-select-window-3)
    ("s-w"           . delete-window)
    ("s-`"           . other-window-or-frame)
    ("s-n"           . make-frame)
    ("s-k s w"       . crux-swap-windows)
    ("s-0"           . move-region-to-other-window)
    ("s-o"           . move-or-copy-region-to-other-window)
    ("s-O"           . reveal-in-finder)
    ("M-0"           . copy-region-to-other-window)
    ("s-+"           . copy-region-to-other-window)
    ("s-k f e"       . restore-frame-to-external-minotaur-two-thirds-size-and-position)
    ("s-k f l"       . restore-frame-to-laptop-two-thirds-size-and-position)

    ;; ==================================================================
    ;; Org‑mode Core
    ;; ==================================================================
    ("s-d"           . org-todo)
    ("M-d"           . org-todo)
    ("M-s-9"         . org-todo)
    ("s-j"           . org-todo)
    ("s-k o l"       . olivetti-mode)
    ("] ol"          . olivetti-mode)
    ("s-k o e"       . olivetti-expand)
    ("s-_"           . olivetti-shrink)
    ("s-k o m"       . org-mode)
    ("s-k o a"       . org-agenda)
    ("s-k o s"       . org-schedule)
    ("s-k o d"       . org-deadline)
    ("s-k o t"       . org-timeline-export-to-html-and-open)
    ("s-k o c"       . org-wc-display)
    ("s-K"           . org-cut-subtree)
    ("C-s-k"         . org-cut-subtree)
    ("M-s-k"         . org-cut-subtree)
    ("s-k v"         . org-paste-subtree)
    ("s-k x"         . org-cut-subtree)
    ("s->"           . load-gnu-startup) ; keep this final binding for s->
    ("s-k c s"       . org-schedule)
    ("C-c C-s"       . org-schedule)
    ("s-k t d"       . org-todo-list)
    ("s-k a f"       . org-attach)
    ("s-L"           . org-mac-link-chrome-insert-frontmost-url)
    ("s-S"           . org-mac-link-skim-insert)
    ("C-c C-x C-r"   . org-clock-report)
    ("C-c C-x <C-i>" . org-clock-in)
    ("s-k c i"       . jd-clock-in)
    ("C-c v i"       . org-insert-src-block)
    ("C-;"           . org-def)
    ("C-S-<left>"    . org-outdent-or-promote)
    ("C-S-<right>"   . org-indent-or-demote)
    ("C-<tab>"       . org-cycle-force-archived)
    ("s-k t s"       . org-toggle-time-stamp-overlays)
    ("s-p"           . org-export-dispatch)
    ("s-("           . org-velocity)
    ("<M-s-return>"  . org-inlinetask-insert-task)
    ("] i t"         . org-inlinetask-insert-task)
    ("<s-S-return>"  . smart-org-insert-todo-heading-dwim)
    ("s-k a d"       . my/org-agenda-day)

    ;; Org Outline Visibility -------------------------------------------
    ("C-s-0"         . show-all)
    ("C-s-a"         . show-all)
    ("C-s-1"         . my/org-show-level-1)
    ("C-s-2"         . my/org-show-level-2)
    ("C-s-3"         . my/org-show-level-3)
    ("C-s-4"         . my/org-show-level-4)
    ("C-s-5"         . my/org-show-level-5)
    ("C-s-6"         . my/org-show-level-6)
    ("C-s-7"         . my/org-show-level-7)
    ("C-s-8"         . my/org-show-level-8)

    ;; Navigation -------------------------------------------------------
    ("M-]"           . org-next-visible-heading)
    ("M-["           . org-previous-visible-heading)
    ("M-n"           . org-next-visible-heading)
    ("M-p"           . org-previous-visible-heading)
    ("M-N"           . org-forward-heading-same-level)
    ("M-P"           . org-backward-heading-same-level)
    ("M-{"           . org-backward-heading-same-level)
    ("M-}"           . org-forward-heading-same-level)
    ("M-C-N"         . outline-next-visible-heading)
    ("M-C-P"         . outline-previous-visible-heading)
    ("C-M-]"         . org-next-subtree-and-narrow)
    ("C-M-["         . org-previous-subtree-and-narrow)
    ("C-]"           . org-next-subtree-same-level-and-narrow)
    ("ESC ESC"       . org-previous-subtree-same-level-and-narrow)
    ("M-("           . backward-word)
    ("M-)"           . forward-word)
    ("s-;"           . consult-outline)
    ("s-5"           . point-stack-push)
    ("s-6"           . point-stack-pop)
    ("s-7"           . point-stack-forward-stack-pop)
    ("s-k g c"       . goto-char)

    ;; Tables -----------------------------------------------------------
    ("s-k t c"       . org-table-create)
    ("s-k r t"       . org-render-table-at-point)
    ("s-k d c"       . org-table-delete-column)
    ("s-k i c"       . org-table-insert-column)
    ("s-k i r"       . org-table-insert-row)

    ;; Formatting -------------------------------------------------------
    ("C-M-8"         . org-toggle-heading)
    ("M-8"           . org-toggle-heading-same-level)
    ("M-*"           . org-toggle-todo-heading)
    ("M-a"           . org-priority-up)
    ("s-k t t"       . toggle-between-src-and-example-block)
    ("<C-i>"         . italicize-region-or-point)
    ("C-o"           . embolden-or-bold)
    ("M-o"           . embolden-or-bold)
    ("M-s-b"         . embolden-region-or-point)

    ;; Text & Clipboard -------------------------------------------------
    ;; ("C-c d"         . insert-todays-date) ; CONFLICT: C-c d is crux-duplicate-current-line-or-region
    ("M-t"           . titlecase-dwim)
    ("C-w"           . copy-region-as-kill-and-push-to-clipboard)
    ("s-c"           . pasteboard-copy-adaptive) ; <–– Fix: now active everywhere
    ("s-v"           . pasteboard-paste-adaptive)
    ("s-V"           . pasteboard-paste-adjusted-subtrees-adaptive)
    ("s-x"           . pasteboard-cut-adaptive)
    ("C-v"           . html2org-clipboard)
    ("C-s-v"         . html2org-clipboard)
    ("M-s-v"         . html2org-clipboard)
    ("C-s-c"         . ox-clip-formatted-copy)
    ("s-k g b"       . gist-buffer-to-pasteboard)
    ("s-k u p"       . unfill-paragraph)
    ("M--"           . cycle-hyphenation-or-toggle-item)
    ("M-_"           . em-dash)
    ("s-k r l"       . remove-link)
    ("C-s-SPC"       . cape-emoji)

    ;; macOS‑style bindings --------------------------------------------
    ("s-a"           . mark-whole-buffer)
    ("s-s"           . jay/save-all-buffers)
    ("s-z"           . undo-fu-only-undo)
    ("s-y"           . undo-fu-only-redo-fail-with-heart)
    ("<s-up>"        . beginning-of-buffer)
    ("<s-down>"      . end-of-buffer)   ; <–– Fix: correct key syntax
    ("s-="           . embiggen-text)
    ("s--"           . ensmallen-text)
    ("s-,"           . customize-group)

    ;; Buffers ----------------------------------------------------------
    ("<s-left>"      . buffer-stack-up)
    ("<s-right>"     . buffer-stack-down)
    ("s-b"           . narrow-or-widen-dwim)
    ("s-B"           . consult-buffer)
    ("s-k RET"       . kill-current-buffer)
    ("s-k s-k"       . kill-current-buffer)
    ("M-s-<right>"   . switch-to-next-buffer)
    ("M-s-<left>"    . previous-buffer)
    ("M-q"           . prelude-switch-to-previous-buffer)
    ("s-t"           . new-buffer)
    ("s-I"           . clone-indirect-buffer-new-window-and-focus)

    ;; Help & Docs ------------------------------------------------------
    ("M-h"           . help-command)
    ("M-h M-k"       . describe-key)
    ("s-D"           . define-word-at-point)
    ("s-T"           . mw-thesaurus-lookup-dwim)
    ("s-k g t"       . google-translate-at-point)
    ("C-s-]"         . help-go-forward)
    ("M-s-t"         . mw-thesaurus-lookup-at-point)

    ;; Editing Helpers --------------------------------------------------
    ("C-d"           . kill-word-correctly-and-capitalize)
    ("C-k"           . my/kill-line-dwim)
    ("C-l"           . reflash-indentation)
    ("M-C"           . capitalize-word)
    ("M-L"           . downcase-sentence)
    ("<backspace>"   . my/delete-backward-and-capitalize)
    ("C-<backspace>" . delete-char)
    ("M-<backspace>" . backward-kill-word-correctly-and-capitalize)
    ("<s-backspace>" . kill-region)
    ("M-/"           . completion-at-point)
    ("C-M-/"         . hippie-expand)
    ("M-="           . er/expand-region)
    ("C-="           . er/expand-region)
    ("M-'"           . insert-one-double-quote)
    ("M-\""        . open-abbrevs)
    ("M-."           . insert-period)
    ("M-,"           . insert-comma)
    ("M-?"           . insert-question-mark)
    ("M-+"           . add-word-to-personal-dictionary)
    ("M-e"           . smart-forward-sentence)
    ("C-9"           . goto-last-change-reverse)
    ("C--"           . goto-last-change)
    ("M-j"           . aide-openai-complete-buffer-insert)
    ("<M-S-backspace>" . backward-kill-sexp)
    ("M-w"           . kill-to-buffer-end-or-beginning)
    ("] ]"           . insert-right-bracket)
    ("s-k d l"       . double-line-breaks-in-region)
    ("s-k dd"        . delete-duplicate-lines-keep-blanks)

    ;; =================================================================
    ;; Mark, Region & Selection
    ;; =================================================================
    ("S-s-SPC"       . set-mark-command)
    ("C-M-SPC"       . set-mark-command)
    ("C-M-x"         . exchange-point-and-mark)
    ("C-x p"         . pop-to-mark-command)
    ("M-s-."         . mark-paragraph)
    ("s-m"           . mc/mark-all-like-this)
    ("s-k m c"       . multiple-cursors-reflash)
    ("s-k r e"       . set-rectangular-region-anchor)
    ("C-8"           . endless/mc-map)

    ;; =================================================================
    ;; Mode Switching
    ;; =================================================================
    ("s-k c m"       . css-mode)
    ("s-k s h"       . sh-mode)
    ("s-k s m"       . sh-mode)
    ("s-k f m"       . text-mode)
    ("s-k e l"       . emacs-lisp-mode)
    ("s-k p m"       . poetry-mode)
    ("s-k w c"       . wc-mode)
    ("s-k w m"       . whitespace-mode)
    ("s-k m b"       . menu-bar-mode)

    ;; =================================================================
    ;; System & Miscellaneous
    ;; =================================================================
    ("="             . amx)
    ("s-e"           . embark-act)
    ("S-<return>"    . visit-messages-buffer)
    ("s-k e b"       . ediff-buffers)
    ("s-k r b"       . revert-buffer)
    ("s-k s b"       . scrollbar-mode-turn-off-scrollbar)
    ("s-i"           . imenu)
    ("s-k i l"       . imenu-list)
    ("s-k i m"       . imenu-list)
    ("s-k h l"       . spacemacs/toggle-highlight-current-line-globally-off)
    ("s-k c p"       . path-copy-path-to-kill-ring)
    ("s-|"           . path-copy-path-to-clipboard)
    ("<s-return>"    . toggle-fullscreen)
    ("C-\\"          . palimpsest-move-region-to-bottom)
    ("C-'"           . palimpsest-move-region-to-bottom)
    ("C-c C-v"       . refile-region)
    ("s-'"           . choose-refile-method-and-refile)
    ("s-E"           . new-email-from-subtree-no-signature)
    ("C-c m"         . compose-mail) ; Moved from global-set-key
    ("s-M"           . tr-toggle-transclusion)
    ("s-k n s"       . yas/new-snippet)
    ("C-s-\\"        . source-current-file)
    ("s-W"           . open-weeklies)
    ("M-s-="         . calc-eval-region)
    ("C-c r"         . eval-region)
    ("M-x"           . execute-extended-command)
    ("[mouse-2]"     . context-menu-open)
    ("C-M-3"         . number-region)
    ("C-M-\\"        . palimpsest-move-region-to-top)
    ("M-%"           . eval-expression)
    ("s-{"           . path-copy-path-to-clipboard)
    ("s-}"           . path-copy-path-to-clipboard)
    ("s-k bl"        . blue-light)
    ("s-k cf"        . customize-face)
    ("s-k cw"        . count-words)
    ("s-k e m"       . expand-outreach-snippet-paste-copy-all-and-submit-ChatGPT)
    ("s-k kb"        . keybinding-read-and-insert)
    ("s-k mk"        . keybinding-read-and-insert)

    ;; =================================================================
    ;; Pomodoro
    ;; =================================================================
    ("<f20>"         . pomodoro-start)
    ("s-k p s"       . pomodoro-start)
    ("C-c C-x p p"   . pomodoro-start)
    ("C-c C-x pi"    . pomodoro-start)
    ("C-c C-x po"    . pomidor-stop)

    ;; =================================================================
    ;; Book Related
    ;; =================================================================
    ("M-b M-d"       . book-dired)
    ("M-b r"         . read-a-book)
    ("M-b j"         . read-jd)
    ("M-b M-b"       . work-on-book)
    ("M-b M-w"       . work-on-book)
    ("M-b lc"        . book-load-current)
    ("M-b M-p"       . book-proposal-directory)
    ("M-b M-m"       . book-mistakes-directory)
    ("<C-s-left>"    . work-on-book)

    ;; =================================================================
    ;; Load Functions & Docs
    ;; =================================================================
    ("s-k l a"       . jay-load-latex)
    ("s-k l t"       . jay-load-latex)
    ("s-k k a"       . load-koma-letter)
    ("s-k k o"       . load-koma-letter)
    ("s-<"           . load-shared-functions)
    ("s-?"           . load-spacecraft-mode)
    ("s-."           . calendar)
    ("] ci"          . load-spacemacs-config)
    ("] cr"          . load-roam-config)
    ("] cs"          . load-search-config)
    ))

(defconst my/global-map-bindings
  '(;; ==================================================================
    ;; Global map bindings (preserve global-set-key semantics)
    ;; ==================================================================
    ("s-/ dg"        . deadgrep-current-directory)
    ("s-/ pa"        . counsel-projectile-ag)
    ("s-/ rg"        . consult-ripgrep-current-directory)
    ("s-/ gg"        . consult-git-grep)
    ("s-/ rr"        . roam-rg-search)
    ("s-/ g b"       . grep-both-directories)
    ("M-c"           . capitalize-or-endless/capitalize)
    ("M-l"           . downcase-or-endless-downcase)
    ("M-u"           . endless/upcase)
    ("M-U"           . caps-lock-mode)
    ("M-SPC"         . insert-space)
    ("s-1"           . delete-other-windows)
    ("s-2"           . split-window-vertically)
    ("s-3"           . split-window-left)
    ("s-9"           . sticky-window-keep-window-visible)
    ("C-a"           . mwim-beginning)
    ("C-e"           . mwim-end)
    ("C-x m"         . endless/mc-map)
    ("C-c C-c"       . pasteboard-copy)
    ("C-c C-!"       . my/panic-save-and-quit)
    ("C-c <mouse-3>" . right-click-context-menu)
    ("C-c v"         . projectile-ag)
    ("C-x C-b"       . ibuffer)
    ("<C-wheel-up>"  . my/scroll-up)
    ("<C-wheel-down>" . my/scroll-down)
    ))

(defconst my/minibuffer-bindings
  '(("s-v"           . pasteboard-paste-verbatim)
    ("s-x"           . pasteboard-cut)
    ("s-c"           . copy-minibuffer-contents)
    ("s-a"           . copy-minibuffer-contents)))

(defun my/install-minibuffer-escape-keys ()
  "Ensure C-g always aborts in minibuffer maps."
  (dolist (map '(minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map))
    (when (boundp map)
      (define-key (symbol-value map) (kbd "C-g") #'minibuffer-keyboard-quit))))


;;;; 3. Keybinding Installation
(defun my/install-global-keys ()
  "Turn `key-minor-mode' on and register all global bindings."
  (key-minor-mode 1)

  ;; Install all global keybindings from the list
  (dolist (binding my/global-key-bindings)
    (define-key key-minor-mode-map (kbd (car binding)) (cdr binding)))

  ;; Install global-map bindings
  (dolist (binding my/global-map-bindings)
    (global-set-key (kbd (car binding)) (cdr binding)))

  ;; Install minibuffer keybindings
  (dolist (binding my/minibuffer-bindings)
    (define-key minibuffer-local-map (kbd (car binding)) (cdr binding)))
  (my/install-minibuffer-escape-keys)

  (define-key help-map (kbd "i") 'jay-info-emacs-manual)

  ;; This binding uses a package; handle separately or ensure package is loaded.
  ;; (bind-key "C-c <mouse-3>" 'right-click-context-menu)

  ;; Global unsets
  (global-unset-key (kbd "C-S-r"))
  (define-key key-minor-mode-map (kbd "C-S-r") nil))

;; Run it once after init, or immediately if init already finished.
(if after-init-time
    (my/install-global-keys)
  (add-hook 'after-init-hook #'my/install-global-keys))

;;;; 4. Prefix map bindings
(when (boundp 'endless/mc-map)
  (define-key endless/mc-map "i" #'mc/insert-numbers)
  (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
  (define-key endless/mc-map "a" #'mc/mark-all-like-this)
  (define-key endless/mc-map (kbd "<backspace>") #'delete-backward-char)
  (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
  (define-key endless/mc-map "r" #'mc/reverse-regions)
  (define-key endless/mc-map "s" #'mc/sort-regions)
  (define-key endless/mc-map "l" #'mc/edit-lines)
  (define-key endless/mc-map (kbd "<return>") #'newline-and-indent)
  (define-key endless/mc-map (kbd "C-a") #'mc/edit-beginnings-of-lines)
  (define-key endless/mc-map (kbd "C-e") #'mc/edit-ends-of-lines)
  (define-key endless/mc-map (kbd ">") #'mc/cycle-forward)
  (define-key endless/mc-map (kbd "<") #'mc/cycle-backward)
  (define-key endless/mc-map (kbd "H") #'hydra-mc/body))

;;;; 5. Mode-local binding alists ------------------------------------------
;;
;; Declarative alists so the dump function in keybinding-docs.el can
;; iterate them.  Installation still uses with-eval-after-load.

(defconst my/org-mode-bindings
  '(("<return>"       . smart-return)
    ("C-k"            . my/kill-line-dwim)
    ("<SPC>"          . smart-space)
    ("s-v"            . pasteboard-paste-adaptive)
    ("s-l"            . org-insert-link)
    ("s-k c s"        . org-clone-subtree)    ; overrides global in Org
    ("C-c e"          . eval-adaptive)
    ("C-c C-s"        . org-schedule)
    ("C-c C-r"        . palimpsest-move-region-to-bottom)
    ("`"              . flyspell-auto-correct-word-correct-space)
    ("."              . smart-period)
    (","              . comma-or-smart-comma)
    ("?"              . smart-question-mark)
    ("!"              . smart-exclamation-point)
    ("-"              . smart-hyphen)
    (";"              . smart-semicolon)
    (":"              . colon-or-smart-colon)
    ("<C-S-right>"    . org-shiftmetaright)
    ("<C-S-left>"     . org-shiftmetaleft)
    ("<C-right>"      . org-metaright)
    ("<C-left>"       . org-metaleft)
    ("<C-up>"         . org-metaup)
    ("<C-down>"       . org-metadown)
    ("M-<return>"     . smart-org-meta-return-dwim)
    ("M-S-<return>"   . smart-org-insert-todo-heading-dwim)
    ("C-M-<return>"   . smart-org-insert-subheading)
    ("<C-S-M-return>" . smart-org-insert-todo-subheading)
    ("<C-s-return>"   . smart-org-insert-todo-subheading)
    ("<C-return>"     . return-insert-blank-line-before)
    ("<C-S-return>"   . smart-org-insert-todo-heading-dwim)
    ("<M-up>"         . up-by-degrees)
    ("<M-down>"       . down-by-degrees)
    ("<left>"         . jay/left-char)
    ("<right>"        . jay/right-char)
    ("DEL"            . new-org-delete-backward-char)
    ("M-K"            . kill-sentence-maybe-else-kill-line))
  "Bindings installed into `org-mode-map'.")

(defconst my/org-mode-unbindings
  '("<M-S-left>" "<M-S-right>" "<M-S-up>" "<M-S-down>"
    "<M-left>" "<M-right>" "C-S-r")
  "Keys unbound (set to nil) in `org-mode-map'.")

(defconst my/org-override-bindings
  '(("<C-M-left>"  . org-outdent-or-promote)
    ("<C-M-right>" . org-indent-or-demote)
    ("<M-S-up>"    . org-shiftup)
    ("<M-S-down>"  . org-shiftdown)
    ("<M-down>"    . down-by-degrees)
    ("<M-up>"      . up-by-degrees))
  "Bindings installed into `key-minor-mode-map' after org loads.")

(defconst my/org-src-mode-bindings
  '(("C-c C-c" . org-edit-src-exit))
  "Bindings installed into `org-src-mode-map'.")

(defconst my/flyspell-bindings
  '(("C-;" . org-def))
  "Bindings installed into `flyspell-mode-map'.")

(defconst my/isearch-bindings
  '(("<tab>"   . isearch-ring-advance)
    ("<S-tab>" . isearch-repeat-backward))
  "Bindings installed into `isearch-mode-map'.")

(defconst my/help-mode-bindings
  '(("C-s-]" . help-go-back))
  "Bindings installed into `help-mode-map'.")

(defconst my/text-mode-bindings
  '(("s-v" . pasteboard-paste-clean))
  "Bindings installed into `text-mode-map'.")

(defconst my/info-mode-bindings
  '(("s-[" . Info-backward-node)
    ("s-]" . Info-forward-node)
    ("<s-up>" . Info-up)
    ("s-l" . Info-goto-node))
  "Bindings installed into `Info-mode-map'.")

(defconst my/winner-bindings
  '(("s-[" . winner-undo)
    ("s-]" . winner-redo))
  "Bindings installed into `winner-mode-map'.")

(defconst my/ctrlf-bindings
  '(("s-g" . ctrlf-next-match))
  "Bindings installed into `ctrlf-mode-map'.")

(defconst my/markdown-mode-bindings
  '(("RET"   . my-markdown-newline-with-bullet)
    ("<tab>" . my-markdown-cycle))
  "Bindings installed into `markdown-mode-map'.")

(defconst my/evil-bindings
  '(("C-g" . keyboard-quit))
  "Bindings installed into `evil-normal-state-map'.")

(defconst my/evil-unbindings '("s-k")
  "Keys unbound in `evil-normal-state-map'.")

(defconst my/vertico-override-bindings
  '(("C-M-S-s-o" . embark-act))
  "Bindings installed into `key-minor-mode-map' after vertico loads.")

(defconst my/emacs-lisp-mode-bindings
  '(("s-v"   . pasteboard-paste-verbatim)
    ("C-c e" . eval-buffer)
    ("M-K"   . kill-sexp))
  "Bindings installed into `emacs-lisp-mode-map'.")

;; Master registry — used by keybinding-docs.el dump function
(defconst my/mode-binding-registry
  '(("org-mode-map"          . my/org-mode-bindings)
    ("org-src-mode-map"       . my/org-src-mode-bindings)
    ("flyspell-mode-map"      . my/flyspell-bindings)
    ("isearch-mode-map"       . my/isearch-bindings)
    ("help-mode-map"          . my/help-mode-bindings)
    ("text-mode-map"          . my/text-mode-bindings)
    ("Info-mode-map"          . my/info-mode-bindings)
    ("winner-mode-map"        . my/winner-bindings)
    ("ctrlf-mode-map"         . my/ctrlf-bindings)
    ("markdown-mode-map"      . my/markdown-mode-bindings)
    ("evil-normal-state-map"  . my/evil-bindings)
    ("emacs-lisp-mode-map"    . my/emacs-lisp-mode-bindings)
    ;; Override maps (installed into key-minor-mode-map)
    ("key-minor-mode-map (org)"    . my/org-override-bindings)
    ("key-minor-mode-map (vertico)" . my/vertico-override-bindings))
  "Registry mapping map names to their binding alists.
Used by `my/keybinding-dump' to generate documentation.")

;;;; 5b. Install mode-local bindings --------------------------------------

(defun my/install-mode-bindings (keymap bindings)
  "Install BINDINGS alist into KEYMAP."
  (dolist (b bindings)
    (define-key keymap (kbd (car b)) (cdr b))))

(defun my/unbind-keys (keymap keys)
  "Set each key string in KEYS to nil in KEYMAP."
  (dolist (k keys)
    (define-key keymap (kbd k) nil)))

(with-eval-after-load 'org
  (my/unbind-keys  org-mode-map my/org-mode-unbindings)
  (my/install-mode-bindings org-mode-map my/org-mode-bindings)
  (my/install-mode-bindings key-minor-mode-map my/org-override-bindings))

(with-eval-after-load 'org-src
  (my/install-mode-bindings org-src-mode-map my/org-src-mode-bindings))

(with-eval-after-load 'flyspell
  (my/install-mode-bindings flyspell-mode-map my/flyspell-bindings))

(with-eval-after-load 'isearch
  (my/install-mode-bindings isearch-mode-map my/isearch-bindings))

(with-eval-after-load 'help-mode
  (my/install-mode-bindings help-mode-map my/help-mode-bindings))

(with-eval-after-load 'text-mode
  (my/install-mode-bindings text-mode-map my/text-mode-bindings))

(with-eval-after-load 'info
  (my/install-mode-bindings Info-mode-map my/info-mode-bindings))

(with-eval-after-load 'winner
  (my/install-mode-bindings winner-mode-map my/winner-bindings))

(with-eval-after-load 'ctrlf
  (my/install-mode-bindings ctrlf-mode-map my/ctrlf-bindings))

(with-eval-after-load 'markdown-mode
  (my/install-mode-bindings markdown-mode-map my/markdown-mode-bindings))

(with-eval-after-load 'evil
  (my/unbind-keys evil-normal-state-map my/evil-unbindings)
  (my/install-mode-bindings evil-normal-state-map my/evil-bindings))

(with-eval-after-load 'vertico
  (my/install-mode-bindings key-minor-mode-map my/vertico-override-bindings))

(with-eval-after-load 'emacs-lisp-mode
  (my/install-mode-bindings emacs-lisp-mode-map my/emacs-lisp-mode-bindings))

(provide 'keys)
;;; keys.el ends here
