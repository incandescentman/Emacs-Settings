;;; timeline-search.el --- Search helpers for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Ripgrep integration and buffer-local bindings for diary search.

;;; Code:

(require 'timeline-core)

(defun my-diary-search ()
  "Quickly search all diary entries using consult-ripgrep."
  (interactive)
  (unless (require 'consult nil 'noerror)
    (user-error "consult is not available; install it to use diary search"))
  (let* ((target (expand-file-name diary-file))
         (dir (file-name-directory target))
         (file (file-name-nondirectory target))
         (base-args (if (boundp 'consult-ripgrep-args)
                        consult-ripgrep-args
                      "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number -- "))
         (consult-ripgrep-args (concat base-args (shell-quote-argument file))))
    (consult-ripgrep dir)))

(defun my-diary--setup-search-shortcuts ()
  "Bind `/` and `s` to `my-diary-search` in the diary buffer."
  (when (and buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name diary-file)))
    (local-set-key (kbd "/") #'my-diary-search)
    (local-set-key (kbd "s") #'my-diary-search)))

(add-hook 'markdown-mode-hook #'my-diary--setup-search-shortcuts)

(with-eval-after-load 'calendar
  (define-key calendar-mode-map (kbd "/") #'my-diary-search)
  (define-key calendar-mode-map (kbd "s") #'my-diary-search))

(provide 'timeline-search)

;;; timeline-search.el ends here
