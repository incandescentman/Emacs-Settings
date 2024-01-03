;; create a custom minor mode to override other keybindings and use mine instead
(defvar key-minor-mode-map (make-sparse-keymap) "key-minor-mode keymap.")

(define-minor-mode key-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " key"
  :keymap key-minor-mode-map)

(key-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (key-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)



(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


(setq mac-command-key-is-meta nil)
(setq mac-command-modifier 'super)
(setq mac-option-key-is-meta t)
(setq mac-option-modifier 'meta)


;; (setq mac-function-modifier 'hyper)
;; (I disabled fn as the hyper modifier because it interferes with Mac defaults fn-left and fn-right for home and end on a MacBook internal keyboard)

(define-key key-minor-mode-map (kbd "<home>") 'beginning-of-buffer)
(define-key key-minor-mode-map (kbd "<end>") 'end-of-buffer)

(define-key key-minor-mode-map (kbd "s-q") 'save-buffers-kill-terminal)
(define-key key-minor-mode-map (kbd "s-f") 'isearch-forward)
(define-key key-minor-mode-map (kbd "s-h") 'replace-string)
(define-key key-minor-mode-map (kbd "s-a") 'mark-whole-buffer)

(delete-selection-mode 1)


(setq mac-pass-command-to-system nil)


;; (setq mac-right-command-modifier 'hyper); right command is hyper key
