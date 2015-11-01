(defvar search-link-cmd "/Users/jay/emacs/prelude/personal/searchlink/searchlink.rb")

(defun searchlink (start end)
  (interactive "r")
  (let* ((txt (delete-and-extract-region start end))
		 (shell-cmd (format "echo %s | %s" txt search-link-cmd))
		 (res (shell-command-to-string shell-cmd)))
	(message "Sh: %s , res: %s" shell-cmd res)
	(insert res)))

