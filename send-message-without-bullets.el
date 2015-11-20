(defun send-message-without-bullets ()
  (interactive)
  (remove-hook 'org-mode-hook 'org-bullets-mode)
  (message-send)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'send-message-without-bullets)))
