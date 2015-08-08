;;; package --- Summary
; org-serenity-mode


;;; Commentary:
; This provides for a cleaner view when using org-mode to manage to-do task lists. 
                                        ; When using org-mode's outlining features, this is perhaps less necessary, since in that use case the leading stars provide more useful visual feedback akin to Markdown syntax.

; You can turn this mode on and off by calling the command org-bullets-mode. 
;;; Code:

; org-bullets-mode already has a mechanism for changing org-mode leading stars into characters of your choice
(require 'org-bullets)
(global-visual-line-mode 1)

; for org-mode headings, instead of leading stars we'll use spaces
(custom-set-variables '(org-bullets-bullet-list (quote (" ")))
                      '(org-hide-emphasis-markers t)
                      '(org-hide-leading-stars t)
         '(org-fontify-done-headline t)
                      '(org-fontify-emphasized-text t)
;; '(org-hidden-keywords (quote (author title)) nil nil "#+BEGIN_QUOTE")
 '(org-indent-indentation-per-level 2)
)

; turn on org-bullets mode in org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; display : as > for block quotes :-)
(font-lock-add-keywords
 'org-mode '(("^\\(:+\\) " 1 (compose-region (match-beginning 1) (match-end 1) ?> ) nil)))


; for prettier plain-text lists, change hyphens to bullets
;; (font-lock-add-keywords
;; 'org-mode '(("^\\(-+\\) " 1 (compose-region (match-beginning 1) (match-end 1) ?• ) nil)))


;; cross out tasks when I do them!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline nil))))
 '(org-headline-done ((t (:strike-through t)))))


(provide 'org-serenity-mode)

;;; org-serenity-mode ends here
