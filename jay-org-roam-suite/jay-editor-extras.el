;;; jay-editor-extras.el --- Editor environment tweaks and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; This module collects non–org-roam-specific configuration that supports your
;; writing and export environment.
;;
;; - Shell and PATH setup for consistent exec-path.
;; - XeLaTeX export pipeline for org-mode.
;; - Ispell dictionary corrections.
;; - Captain predicate hooks for auto-capitalization logic.
;;
;; Can be safely loaded independently or alongside `jay-org-roam-core.el`.

;;; Code:

;; -----------------------------------------------------------------------------
;; Ispell dictionary configuration
;; -----------------------------------------------------------------------------
(defun jay/fix-ispell-contraction ()
  "Fix ispell handling of contractions like shouldn't."
  (add-to-list 'ispell-dictionary-alist
               '("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))

(with-eval-after-load 'ispell
  (jay/fix-ispell-contraction)
  (add-to-list 'ispell-dictionary-alist
               '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
  (add-to-list 'ispell-dictionary-alist
               '("american" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US-large") nil utf-8))
  (add-to-list 'ispell-dictionary-alist
               '("english"  "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US-large") nil utf-8)))

;; -----------------------------------------------------------------------------
;; Shell and export environment
;; -----------------------------------------------------------------------------
(setq shell-file-name "/bin/bash")
(let* ((login-path (replace-regexp-in-string
                    "[ \t\n\r]+\\'" ""
                    (shell-command-to-string
                     "bash -l -c 'printf %s \"$PATH\"'")))
       (login-path-list (split-string login-path ":" t))
       (preferred-front '("/usr/local/bin" "/opt/homebrew/bin"))
       (preferred-tail  '("/usr/local/texlive/2024/bin/universal-darwin"))
       (combined-paths (delete-dups
                        (append preferred-front
                                login-path-list
                                preferred-tail
                                (copy-sequence exec-path))))
       (combined-string (mapconcat #'identity combined-paths ":")))
  (setenv "PATH" combined-string)
  (setenv "SHELL" "/bin/bash")
  (setq exec-path combined-paths))

(setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"))

;; -----------------------------------------------------------------------------
;; Captain predicate hooks (for auto-capitalization)
;; -----------------------------------------------------------------------------
(add-hook 'text-mode-hook (lambda () (setq captain-predicate (lambda () t))))
(add-hook 'org-mode-hook  (lambda () (setq captain-predicate (lambda () t))))

(provide 'jay-editor-extras)
;;; jay-editor-extras.el ends here
