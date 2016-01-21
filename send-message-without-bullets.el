<<<<<<< HEAD
(defvar *sent-emails-org-file* "/Users/jay/Dropbox/writing/notationaldata/sent-emails.org")
=======
(defvar *mail-signature* "\n---\nJay Dixit\n(646) 355-8001\njaydixit.com")
(defun sign-current-email ()
  (save-excursion
    (end-of-buffer)
    (insert *mail-signature*)))

(defvar *sent-emails-org-file* "~/sent-emails.org")
>>>>>>> aa92f6bed2c6c6190fed3703be60cef95ecc3617
(defun save-buffer-to-sent-emails-org-file ()
  ;; header
  (write-region
   (concat "\n\n\n* "
           (current-time-string)
           "\n\n")
   0 *sent-emails-org-file* t)
  ;; buffer
  (write-region nil 0 *sent-emails-org-file* t))

(defun send-message-without-bullets ()
  (interactive)
  (remove-hook 'org-mode-hook 'org-bullets-mode)
  (notmuch-mua-send)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(defun custom-send-message (arg)
  (interactive "p")
  (when (and arg (= 0 (mod arg 4)))
    (sign-current-email))
  (save-buffer-to-sent-emails-org-file)
  (send-message-without-bullets))

(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" 'custom-send-message)))

