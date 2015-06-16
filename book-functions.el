
(push "~/.emacs.d/helm-cmd-t" load-path)
(require 'helm-config)
(require 'helm-cmd-t)
(global-set-key (kbd "M-t") 'helm-cmd-t)

(setq book-source (helm-cmd-t-get-create-source-dir "~/Dropbox/writing/book"))

(setq jays-library-source-1 (helm-cmd-t-get-create-source-dir "/Users/jay/Music/iTunes/iTunes Media/Books"))
(setq jays-library-source-2 (helm-cmd-t-get-create-source-dir "~/iBooks/Books"))

(setq jays-library-public (helm-cmd-t-get-create-source-dir "~/Dropbox/Public/library"))




(setq helm-ff-lynx-style-map nil helm-input-idle-delay 0.1 helm-idle-delay 0.1)




(defun helm-book-plus ()
  "Choose file from book folder."
  (interactive)
  (helm :sources (list

                  helm-source-buffers-list
                  helm-source-recentf
                  helm-source-bookmarks
                  book-source)))

(defun book-helm-strict ()
  "Choose file from book folder."
  (interactive)
  (helm :sources (list
                  book-source)))


(defun read-a-book ()
  "Choose file from book folder."
  (interactive)
  (helm :sources (list
                  jays-library-source-1
                  jays-library-source-2
)))



(defvar my-org-folders (list  "/Users/jay/Music/iTunes/iTunes Media/Books" "~/iBooks/Books")
  "my permanent folders for helm-mini")

(defun read-jd (&optional arg)
  ""
  (interactive "P")
  (if (consp arg)
      (call-interactively 'helm-cmd-t-repos)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources (mapcar (lambda (dir)
                               (helm-cmd-t-get-create-source-dir dir))
                             my-org-folders)
            :candidate-number-limit 20
            :buffer "*helm-my-org:*"
            :input "_JD "))))


(defun book-dired()
  (interactive)
 (cyberpunk-jay)
 ;; (toggle-fullscreen)
 (org-mode)
  (dired "~/b/"))



(defun work-on-book ()
"Work on my book! :-)"
(interactive)
(find-file "/Users/jay/b/booktime.org")
(end-of-buffer)
(jd-clock-in)
(book-dired))




(defun book-proposal-directory()
  (interactive)
 (cyberpunk-jay)
;; (toggle-fullscreen)
  (dired "~/b/writing-in-progress/proposal"))
