(defvar *sent-emails-org-file* "~/sent-emails.org")
(defun save-buffer-to-sent-emails-org-file ()
  ;; header
  (write-region
   (concat "\n\n\n* ===============================\n* -- "
           (current-time-string)
           " --\n* -------------------------------\n\n")
   0 *sent-emails-org-file* t)
  ;; buffer
  (write-region nil 0 *sent-emails-org-file* t))

(defun send-message-without-bullets ()
  (interactive)
  (remove-hook 'org-mode-hook 'org-bullets-mode)
  (save-buffer-to-sent-emails-org-file)
  (message-send)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'send-message-without-bullets)))
