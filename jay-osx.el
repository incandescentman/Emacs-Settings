(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


(setq mac-command-key-is-meta nil)
(setq mac-command-modifier 'super)
(setq mac-option-key-is-meta t)
(setq mac-option-modifier 'meta)

(define-key key-minor-mode-map (kbd "<home>") 'beginning-of-buffer)
(define-key key-minor-mode-map (kbd "<end>") 'end-of-buffer)

(define-key key-minor-mode-map (kbd "s-q") 'save-buffers-kill-terminal)
(define-key key-minor-mode-map (kbd "s-f") 'isearch-forward)
(define-key key-minor-mode-map (kbd "s-h") 'replace-string)


(define-key key-minor-mode-map (kbd "s-o") 'replace-string)

(setq mac-pass-command-to-system nil)
