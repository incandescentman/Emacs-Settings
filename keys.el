;;; Keys.el --- personal key bindings             -*- lexical-binding: t; -*-
;;
;;  ▸ Drop this file somewhere in ‘load-path’
;;  ▸  (require 'keys)   ;; in init.el
;;  ▸ M-x byte-compile-file RET keys.el  ⇒ fastest load-time

;;;; 0.  Override minor-mode – beats all major-modes ────────────────────
(defvar key-minor-mode-map (make-sparse-keymap)
  "Keymap that should win against major-mode maps.")

(define-minor-mode key-minor-mode
  "Enable my global keys in spite of major-mode maps."
  :init-value nil
  :lighter     " key"
  :keymap      key-minor-mode-map)

;; Don’t pollute the minibuffer.
(add-hook 'minibuffer-setup-hook (lambda () (key-minor-mode -1)))

;;;; 1.  Helper commands used by bindings ───────────────────────────────
(defun visit-messages-buffer ()
  "Open *Messages* in another window, already scrolled to bottom."
  (interactive)
  (with-current-buffer (messages-buffer)
    (goto-char (point-max)))
  (pop-to-buffer (messages-buffer)))

(defun copy-minibuffer-contents ()
  "Copy whole minibuffer input to the kill-ring (and clipboard)."
  (interactive)
  (let ((inhibit-read-only t))
    (copy-region-as-kill (point-min) (point-max)))
  ;; (push-kill-ring-pasteboard-to-MacOS-clipboard) ; if you have it
  )

;;;; 2.  Truly global bindings – installed once at startup ──────────────
(defconst my/global-key-bindings
  '(
    ;; ─── Search / navigation ─────────────────────────────────────────
    ("M-s-d"          counsel-find-file)
    ("s-r"            counsel-recentf)
    ("C-s"            consult-line)
    ("C-s-g "         consult-ripgrep-current-directory)
    ("s-g"            isearch-repeat-forward)
    ("s-f"            isearch-forward-ignore-case)
    ("s-F"            pasteboard-search-for-clipboard-contents)
    ("s-\\\\"         visit-most-recent-file)
    ("M-s-<right>"    switch-to-next-buffer)
    ("M-s-<left>"     previous-buffer)

    ;; ─── Editing / text ops ──────────────────────────────────────────
    ("s-c"            pasteboard-copy-adaptive)
    ("s-x"            pasteboard-cut-and-capitalize-and-replace-em-dashes-maybe)
    ("s-v"            pasteboard-paste-adaptive)
    ("s-V"            pasteboard-paste-adjusted-subtrees-adaptive)
    ("C-s-v"          html2org-clipboard)
    ("C-s-c"          ox-clip-formatted-copy)
    ("s-h"            replace-string)
    ("M-+"            add-word-to-personal-dictionary)
    ("M-?"            insert-question-mark)
    ("M-\""           open-abbrevs)
    ("M-."            insert-period)
    ("M-,"            insert-comma)
    ("M-_"            em-dash)
    ("C-M-/"          hippie-expand)
    ("C-d"            kill-word-correctly-and-capitalize)
    ("C-<backspace>"  delete-char)
    ("M-<backspace>"  backward-kill-word-correctly-and-capitalize)
    ("<backspace>"    my/delete-backward-and-capitalize)

    ;; ─── Window / buffer management ──────────────────────────────────
    ("s-w"            delete-window)
    ("M-1"            winum-select-window-1)
    ("M-2"            winum-select-window-2)
    ("M-3"            winum-select-window-3)
    ("s-n"            make-frame)
    ("s-`"            other-window-or-frame)
    ("s-b"            narrow-or-widen-dwim)
    ("C-x C-d"        dired)                     ; shadowed later by consult-dir
    ("C-x C-j"        dired-up-directory)
    ("s-0"            move-region-to-other-window)
    ("s-o"            move-or-copy-region-to-other-window)
    ("s-O"            reveal-in-finder)
    ("<s-backspace>"  kill-region)
    ("s-z"            undo-fu-only-undo)
    ("s-y"            undo-fu-only-redo-fail-with-heart)
    ("s-="            embiggen-text)
    ("s--"            ensmallen-text)

    ;; ─── Org helpers & agenda ────────────────────────────────────────
    ("s-d"            org-todo)
    ("s-k t d"        org-todo-list)
    ("s-k o a"        org-agenda)
    ("s-k o s"        org-schedule)
    ("s-k o d"        org-deadline)
    ("s-k r t"        org-render-table-at-point)
    ("s-k r l"        remove-link)
    ("s-k v"          org-paste-subtree)
    ("s-k x"          org-cut-subtree)
    ("s-k a f"        org-attach)
    ("s-k n s"        yas/new-snippet)
    ("s-k t c"        org-table-create)
    ("s-k c i"        jd-clock-in)
    ("s-k r e"        set-rectangular-region-anchor)
    ("s-k w s"        isearch-forward-word)
    ("s-k t s"        org-toggle-time-stamp-overlays)
    ("s-k o l"        olivetti-mode)
    ("] ol"           olivetti-mode)
    ("s-k o e"        olivetti-expand)
    ("s-_"            olivetti-shrink)

    ;; ─── Utility / misc  ─────────────────────────────────────────────
    ("="              smex)
    ("S-<return>"     visit-messages-buffer)
    ("C-c <mouse-3>"  right-click-context-menu)
    ("[mouse-2]"      context-menu-open)
    ("s-P"            projectile-find-file)
    ("s-E"            new-email-from-subtree-no-signature)
    ("s-T"            mw-thesaurus-lookup-dwim)
    ("s-D"            define-word-at-point)
    ("s-W"            open-weeklies)
    ("s-, "           customize-group)
    ("s-<"            load-shared-functions)
    ("s->"            load-gnu-startup)
    ("s-?"            load-spacecraft-mode)
    ("s-."            calendar)
    ("s-'"            choose-refile-method-and-refile)
    ("s-B"            consult-buffer)
    ("C-c m"          compose-mail)
    ("C-c C-v"        refile-region)
    ("C-x p"          pop-to-mark-command)
    ("C-s-r"          consult-find)
    ("C-s-f"          isearch-forward-word-at-point)
    ("C-s-v"          html2org-clipboard)

    ;; ─── Programming helpers ─────────────────────────────────────────
    ("s-I"            clone-indirect-buffer-new-window-and-focus)
    ("C-c r"          eval-region)
    ("C-9"            goto-last-change-reverse)
    ("C--"            goto-last-change)
    ("M-="            er/expand-region)
    ("C-="            er/expand-region)

    ;; ─── Misc minor-modes / toggles ──────────────────────────────────
    ("s-k w m"        whitespace-mode)
    ("s-k h l"        spacemacs/toggle-highlight-current-line-globally-off)
    ("s-k s b"        scrollbar-mode-turn-off-scrollbar)
    ("s-k r b"        revert-buffer)
    ("s-K"            org-cut-subtree)
    ("s-k m c"        multiple-cursors-reflash)
    ("s-k f z"        counsel-fzf)
    ("M-s-="          calc-eval-region)
    ("<f20>"          pomodoro-start)
    ("s-k p s"        pomodoro-start)
    ("s-k p m"        poetry-mode)
    ("s-k g b"        gist-buffer-to-pasteboard)
    ;; … add further bindings here as needed …
    )
  "Pairs of (key . command) for `key-minor-mode-map`.")

(defun my/install-global-keys ()
  "Enable `key-minor-mode` and install the bindings in
`my/global-key-bindings`."
  (key-minor-mode 1)
  (dolist (pair my/global-key-bindings)
    (define-key key-minor-mode-map (kbd (car pair)) (cdr pair)))

  ;; Minibuffer extras
  (define-key minibuffer-local-map (kbd "s-v") #'pasteboard-paste-raw)
  (define-key minibuffer-local-map (kbd "s-c") #'copy-minibuffer-contents))

(add-hook 'after-init-hook #'my/install-global-keys)

;;;; 3.  Mode-local bindings – lazy, zero impact on startup ─────────────
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<return>") #'smart-return)
  (define-key org-mode-map (kbd "C-k")      #'my/kill-line-dwim)
  (define-key org-mode-map (kbd "s-v")      #'pasteboard-paste-adaptive)
  (define-key org-mode-map (kbd "s-k c s")  #'org-clone-subtree)
  (define-key org-mode-map (kbd "C-c C-s")  #'org-schedule)
  (define-key org-mode-map (kbd "s-l")      #'org-insert-link)
  (define-key org-mode-map (kbd "M-K")      #'kill-sentence-maybe-else-kill-line))

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") #'org-def))

(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "C-s-]") #'help-go-back))

(with-eval-after-load 'text-mode
  (define-key text-mode-map (kbd "s-v") #'pasteboard-paste-clean))

(with-eval-after-load 'emacs-lisp-mode
  (define-key emacs-lisp-mode-map (kbd "s-v") #'pasteboard-paste-raw)
  (define-key emacs-lisp-mode-map (kbd "M-K") #'kill-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'eval-buffer))

(provide 'keys)
;;; keys.el ends here
