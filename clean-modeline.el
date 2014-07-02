;;; modeline-pack.el --- Modeline setup

;;; Commentary:

;;; Code:

(defvar mode-line-cleaner-alist
  `((auto-complete-mode       . " α")
    (yas-minor-mode           . " γ")
    (paredit-mode             . " Φ")
    (eldoc-mode               . "")
    (pabbrev-mode              . " π")
    (undo-tree-mode           . " τ")
    (visual-line-mode         . "")
    (palimpsest-mode         . " ↓")

    (volatile-highlights-mode . " υ")
    (elisp-slime-nav-mode     . " δ")
    (nrepl-mode               . " ηζ")
    (nrepl-interaction-mode   . " ηζ")
    (cider-mode               . " ηζ")
    (cider-interaction        . " ηζ")
    ;; Major modes
    (clojure-mode             . "λ")
    (python-mode              . "Py")
    (emacs-lisp-mode          . "EL")
    (markdown-mode            . "md")
    (magit                    . "ma")
    (haskell-mode             . "ha")
    (tuareg-mode              . "ml")
    (flymake-mode             . "fm"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)


;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ

;;; modeline-pack ends here
