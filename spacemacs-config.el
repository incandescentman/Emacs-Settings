(spacemacs/set-leader-keys "gx" 'forward-char)
;; works

;; global, I think?
(evil-leader/set-key
"gp" 'forward-char
)
;; works


(evil-leader/set-key-for-mode 'org-mode
    "gl"  'ledger-delete-current-transaction
)
;; doesn't work

(spacemacs/set-leader-keys-for-major-mode 'org-mode "gd" 'forward-char)
;; doesn't work

(define-key key-minor-mode-map (kbd "}rf") 'prelude-rename-file-and-buffer)
(define-key key-minor-mode-map (kbd "}vi") 'org-insert-src-block)
(define-key key-minor-mode-map (kbd "}nl") 'new-lisp-buffer)
(use-package wn-org)
;; (define-key key-minor-mode-map (kbd "]d") 'wn-org)

;; (spacemacs/set-leader-keys "d" 'forward-char)

(evil-leader/set-key
"c[" 'load-shared-functions
"c]" 'load-gnu-startup
"gb" 'gist-buffer-to-pasteboard
"]" 'insert-right-bracket
)
