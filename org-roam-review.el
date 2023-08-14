;; (add-to-list 'load-path "/Users/jay/emacs/nursery/lisp/")

;; (use-package org-roam-review
;;       :commands (org-roam-review
;;                  org-roam-review-list-by-maturity
;;                  org-roam-review-list-recently-added)

;;       ;; ;; Optional - tag all newly-created notes as seedlings.
;;       ;; :hook (org-roam-capture-new-node . org-roam-review-set-seedling)

;;       ;; ;; Optional - keybindings for applying Evergreen note properties.
;;       ;; :general
;;       ;; (:keymaps 'org-mode-map
;;       ;; "C-c r r" '(org-roam-review-accept :wk "accept")
;;       ;; "C-c r u" '(org-roam-review-bury :wk "bury")
;;       ;; "C-c r x" '(org-roam-review-set-excluded :wk "set excluded")
;;       ;; "C-c r b" '(org-roam-review-set-budding :wk "set budding")
;;       ;; "C-c r s" '(org-roam-review-set-seedling :wk "set seedling")
;;       ;; "C-c r e" '(org-roam-review-set-evergreen :wk "set evergreen"))

;;       ;; ;; Optional - bindings for evil-mode compatability.
;;       ;; :general
;;       ;; (:states '(normal) :keymaps 'org-roam-review-mode-map
;;       ;; "TAB" 'magit-section-cycle
;;       ;; "g r" 'org-roam-review-refresh)
;;       )

;;     (use-package org-roam-search
;;       :commands (org-roam-search))

;;     (use-package org-roam-links
;;       :commands (org-roam-links))

;;     (use-package org-roam-dblocks
;;       :hook (org-mode . org-roam-dblocks-autoupdate-mode))

;;     (use-package org-roam-rewrite
;;       :commands (org-roam-rewrite-rename
;;                  org-roam-rewrite-remove
;;                  org-roam-rewrite-inline
;;                  org-roam-rewrite-extract))

;;     (use-package org-roam-slipbox
;;       :after org-roam
;;       :demand t
;;       :config
;;       (org-roam-slipbox-buffer-identification-mode +1)
;;       (org-roam-slipbox-tag-mode +1))
