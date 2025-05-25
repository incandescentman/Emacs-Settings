;;; Keys.el --- all my key bindings  -*- lexical-binding: t; -*-

;;;; 0. Override Minor Mode Definition
(defvar key-minor-mode-map (make-sparse-keymap)
  "Keymap that should win against major-mode maps.")

(define-minor-mode key-minor-mode
  "Enable my global keys in spite of major-mode maps."
  :init-value nil
  :lighter " key"
  :keymap key-minor-mode-map)

;; Disable the override map inside the minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (key-minor-mode -1)))

;;;; 1. Helper Functions
(defun visit-messages-buffer ()
  "Switch to the *Messages* buffer in another window."
  (interactive)
  (view-echo-area-messages)
  (other-window 1))

(defun copy-minibuffer-contents ()
  "Copy the contents of the minibuffer."
  (interactive)
  (beginning-of-visual-line)
  (end-of-buffer)
  (copy-region-as-kill (mark) (point))
  ;; This custom function must be defined elsewhere in your configuration
  ;; (push-kill-ring-pasteboard-to-MacOS-clipboard)
  )

;;;; 2. Keybinding Definitions
(defconst my/global-key-bindings
  '(;; =================================================================
    ;; File Finding & Navigation
    ;; =================================================================
    ("s-R"           . fasd-find-file)
    ("s-r"           . counsel-recentf)
    ("C-s-r"         . consult-find)
    ("M-s-d"         . counsel-find-file)
    ("s-k e e"       . fasd-find-file)
    ("s-k f z"       . counsel-fzf)
    ("s-P"           . projectile-find-file)
    ("s-\\"          . visit-most-recent-file)
    ("C-x C-j"       . dired-up-directory)
    ("C-x C-d"       . consult-dir) ; Using consult-dir as the final choice

    ;; =================================================================
    ;; Search & Replace
    ;; =================================================================
    ("C-s"           . consult-line)
    ("s-f"           . isearch-forward-ignore-case)
    ("s-F"           . pasteboard-search-for-clipboard-contents)
    ("s-h"           . replace-string)
    ("s-g"           . isearch-repeat-forward)
    ("C-s-g "        . consult-ripgrep-current-directory) ; Note the trailing space
    ("s-G"           . counsel-projectile-ag)
    ("C-s-f"         . isearch-forward-word-at-point)
    ("s-k ag"        . affe-grep)
    ("s-k w s"       . isearch-forward-word)
    ("M-s b"         . book-search)
    ("M-s c"         . current-buffers-search)

    ;; =================================================================
    ;; Window Management
    ;; =================================================================
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

    ;; =================================================================
    ;; Org-mode Core
    ;; =================================================================
    ("s-d"           . org-todo)
    ("M-d"           . org-todo)
    ("M-s-9"         . org-todo)
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
    ("s-k v"         . org-paste-subtree)
    ("s-k x"         . org-cut-subtree)
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

    ;; =================================================================
    ;; Org Navigation
    ;; =================================================================
    ("M-]"           . org-next-visible-heading)
    ("M-["           . org-previous-visible-heading)
    ("C-M-]"         . org-next-subtree-and-narrow)
    ("C-M-["         . org-previous-subtree-and-narrow)
    ("C-]"           . org-next-subtree-same-level-and-narrow)
    ("ESC ESC"       . org-previous-subtree-same-level-and-narrow)

    ;; =================================================================
    ;; Org Tables
    ;; =================================================================
    ("s-k t c"       . org-table-create)
    ("s-k r t"       . org-render-table-at-point)
    ("s-k d c"       . org-table-delete-column)
    ("s-k i c"       . org-table-insert-column)
    ("s-k i r"       . org-table-insert-row)

    ;; =================================================================
    ;; Org Formatting
    ;; =================================================================
    ("C-M-8"         . org-toggle-heading)
    ("M-8"           . org-toggle-heading-same-level)
    ("M-*"           . org-toggle-todo-heading)
    ("M-a"           . org-priority-up)
    ("s-k t t"       . toggle-between-src-and-example-block)

    ;; =================================================================
    ;; Text Manipulation & Clipboard
    ;; =================================================================
    ("M-t"           . titlecase-dwim)
    ("C-w"           . copy-region-as-kill-and-push-to-clipboard) ; Note: Overrides default cut
    ("s-c"           . pasteboard-copy-adaptive)
    ("s-v"           . pasteboard-paste-adaptive)
    ("s-V"           . pasteboard-paste-adjusted-subtrees-adaptive)
    ("s-x"           . pasteboard-cut-and-capitalize-and-replace-em-dashes-maybe)
    ("C-v"           . html2org-clipboard)
    ("C-s-v"         . html2org-clipboard)
    ("M-s-v"         . html2org-clipboard)
    ("C-s-c"         . ox-clip-formatted-copy)
    ("s-k g b"       . gist-buffer-to-pasteboard)
    ("s-k u p"       . unfill-paragraph)
    ("M--"           . cycle-hyphenation-or-toggle-item)
    ("M-_"           . em-dash)
    ("s-k r l"       . remove-link)

    ;; =================================================================
    ;; macOS Standard Keybindings
    ;; =================================================================
    ("s-a"           . mark-whole-buffer)
    ("s-s"           . jay/save-all-buffers)
    ("s-z"           . undo-fu-only-undo)
    ("s-y"           . undo-fu-only-redo-fail-with-heart)
    ("[s-up]"        . beginning-of-buffer)
    ("[s-down]"      . end-of-buffer)
    ("s-="           . embiggen-text)
    ("s--"           . ensmallen-text)
    ("s-,"           . customize-group)

    ;; =================================================================
    ;; Buffer Management
    ;; =================================================================
    ("s-b"           . narrow-or-widen-dwim)
    ("s-B"           . consult-buffer)
    ("s-k RET"       . kill-current-buffer)
    ("s-k s-k"       . kill-current-buffer)
    ("M-s-<right>"   . switch-to-next-buffer)
    ("M-s-<left>"    . previous-buffer)
    ("M-q"           . prelude-switch-to-previous-buffer)
    ("s-t"           . new-buffer)
    ("s-I"           . clone-indirect-buffer-new-window-and-focus)

    ;; =================================================================
    ;; Editing & Movement
    ;; =================================================================
    ("C-d"           . kill-word-correctly-and-capitalize)
    ("C-k"           . my/kill-line-dwim)
    ("C-l"           . reflash-indentation)
    ("<backspace>"   . my/delete-backward-and-capitalize)
    ("C-<backspace>" . delete-char)
    ("M-<backspace>" . backward-kill-word-correctly-and-capitalize)
    ("<s-backspace>" . kill-region)
    ("M-/"           . completion-at-point)
    ("C-M-/"         . hippie-expand)
    ("M-="           . er/expand-region)
    ("C-="           . er/expand-region)
    ("M-'"           . insert-one-double-quote)
    ("M-\""          . open-abbrevs)
    ("M-."           . insert-period)
    ("M-,"           . insert-comma)
    ("M-?"           . insert-question-mark)
    ("M-+"           . add-word-to-personal-dictionary)
    ("M-e"           . smart-forward-sentence)
    ("C-9"           . goto-last-change-reverse)
    ("C--"           . goto-last-change)
    ("M-j"           . aide-openai-complete-buffer-insert)

    ;; =================================================================
    ;; Mark, Region & Selection
    ;; =================================================================
    ("S-s-SPC"       . set-mark-command)
    ("C-M-SPC"       . set-mark-command)
    ("C-M-x"         . exchange-point-and-mark)
    ("C-x p"         . pop-to-mark-command)
    ("M-s-."         . mark-paragraph)
    ("s->"           . org-mark-subtree) ; Using this as the final choice for s->
    ("s-m"           . mc/mark-all-like-this)
    ("s-k m c"       . multiple-cursors-reflash)
    ("s-k r e"       . set-rectangular-region-anchor)

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
    ("="             . smex)
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

    ;; =================================================================
    ;; Pomodoro
    ;; =================================================================
    ("<f20>"         . pomodoro-start)
    ("s-k p s"       . pomodoro-start)
    ("C-c C-x p p"   . pomodoro-start)
    ("C-c C-x pi"    . pomodoro-start)
    ("C-c C-x po"    . pomodoro-stop)

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
    ("M-h"           . help-command)
    ("M-h M-k"       . describe-key)
    ("s-D"           . define-word-at-point)
    ("s-T"           . mw-thesaurus-lookup-dwim)
    ("s-k g t"       . google-translate-at-point)
    ("C-s-]"         . help-go-forward)
    ))

(defconst my/minibuffer-bindings
  '(("s-v"           . pasteboard-paste-raw)
    ("s-x"           . pasteboard-cut)
    ("s-c"           . copy-minibuffer-contents)
    ("s-a"           . copy-minibuffer-contents)))


;;;; 3. Keybinding Installation
(defun my/install-global-keys ()
  "Turn `key-minor-mode' on and register all global bindings."
  (key-minor-mode 1)

  ;; Install all global keybindings from the list
  (dolist (binding my/global-key-bindings)
    (define-key key-minor-mode-map (kbd (car binding)) (cdr binding)))

  ;; Install minibuffer keybindings
  (dolist (binding my/minibuffer-bindings)
    (define-key minibuffer-local-map (kbd (car binding)) (cdr binding)))

  ;; This binding uses a package; handle separately or ensure package is loaded.
  ;; (bind-key "C-c <mouse-3>" 'right-click-context-menu)

  ;; Global unsets
  (global-unset-key (kbd "C-S-r"))
  (define-key key-minor-mode-map (kbd "C-S-r") nil))

;; Run it once after init
(add-hook 'after-init-hook #'my/install-global-keys)

;;;; 4. Mode-local bindings (lazy loading)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<return>") #'smart-return)
  (define-key org-mode-map (kbd "C-k") #'my/kill-line-dwim)
  (define-key org-mode-map (kbd "s-v") #'pasteboard-paste-adaptive)
  (define-key org-mode-map (kbd "s-l") #'org-insert-link)
  (define-key org-mode-map (kbd "s-k c s") #'org-clone-subtree) ; Overrides global binding in Org
  (define-key org-mode-map (kbd "C-c e") #'eval-adaptive)
  (define-key org-mode-map (kbd "C-c C-s") #'org-schedule)
  (define-key org-mode-map (kbd "C-S-r") nil)
  (define-key org-mode-map (kbd "M-K") #'kill-sentence-maybe-else-kill-line))

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") #'org-def))

(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "C-s-]") #'help-go-back))

(with-eval-after-load 'text-mode
  (define-key text-mode-map (kbd "s-v") #'pasteboard-paste-clean))

(with-eval-after-load 'emacs-lisp-mode
  (define-key emacs-lisp-mode-map (kbd "s-v") #'pasteboard-paste-raw)
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'eval-buffer)
  (define-key emacs-lisp-mode-map (kbd "M-K") #'kill-sexp))

(provide 'keys)
;;; keys.el ends here
