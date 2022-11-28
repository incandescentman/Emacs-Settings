(defun book-load-current ()
  (interactive)
  (jay/save-some-buffers)
(condition-case nil
  (kill-buffer "top-performer.org")
  (error nil))

;; (find-file "/Users/jay/b/proposal/12-mistakes-real-headings.txt")
(find-file "/Users/jay/Dropbox/writing/book/12-mistakes/02_hookup-mode_using-st-criteria-for-finding-lt-mates---conceptual-outline.txt")
(text-scale-increase 2)
;; (find-file "/Users/jay/b/proposal/12-mistakes-conceptual-outline.txt")
;; (find-file "/Users/jay/Dropbox/writing/book/12-mistakes/12-mistakes-conceptual-outline.txt")
(find-file "/Users/jay/Dropbox/writing/book/feb-18/intro.txt")
(text-scale-increase 2)
(split-window-horizontally)
(find-file "/Users/jay/Dropbox/writing/book/12-mistakes/02_hookup-mode_using-st-criteria-for-finding-lt-mates---chapter.txt")
(text-scale-increase 2)
;; (other-window 1)

)


(push "/Users/jay/.emacs.d/helm-cmd-t" load-path)
(push "/Users/jay/emacs/spacemacs/elpa/helm-cmd-t-20150823.1157/" load-path)

(use-package helm-config)
(use-package helm-cmd-t)
(global-set-key (kbd "M-t") 'helm-cmd-t)

(setq book-source (helm-cmd-t-get-create-source-dir "/Users/jay/Dropbox/writing/book"))
(setq jays-library-source-1 (helm-cmd-t-get-create-source-dir "/Users/jay/devonthink-databases/PDF-books.dtBase2/Files.noindex/pdf/"))

;; (setq jays-library-source-1 (helm-cmd-t-get-create-source-dir "/Users/jay/Music/iTunes/iTunes Media/Books"))
;; (setq jays-library-source-2 (helm-cmd-t-get-create-source-dir "/Users/jay/iBooks2"))

;; (setq jays-library-public (helm-cmd-t-get-create-source-dir "/Users/jay/Dropbox/Public/library"))

(setq emacs-settings (helm-cmd-t-get-create-source-dir "/Users/jay/gnulisp"))



(setq helm-ff-lynx-style-map nil helm-input-idle-delay 0.1 helm-idle-delay 0.1)


(defun search-within-emacs-settings ()
  (interactive)
  (helm :sources (list
emacs-settings)))

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



(defvar my-org-folders (list  "/Users/jay/Music/iTunes/iTunes Media/Books" "/Users/jay/iBooks2")
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
;; (cyberpunk-jay)
 ;; (toggle-fullscreen)
 (org-mode)
;;  (dired "/Users/jay/b/"))
(dired "/Users/jay/b/12-mistakes/"))



(defun work-on-book ()
"Work on my book! :-)"
(interactive)
(jay/save-some-buffers)
;; (book-dired)
(condition-case nil
  (kill-buffer "top-performer.org")
  (error nil))
;; (adobe-garamond-pro)
(org-cycle-agenda-files)
)


(defun book-clock-in ()
  (interactive)
  (find-file "/Users/jay/b/booktime.org")
(end-of-buffer)
(jd-clock-in)
)

(defun book-proposal-directory()
  (interactive)
;; (cyberpunk-jay)
;; (toggle-fullscreen)
  (dired "/Users/jay/b/proposal"))

(defun book-mistakes-directory()
  (interactive)
;; (cyberpunk-jay)
;; (toggle-fullscreen)
  (dired "/Users/jay/b/12-mistakes"))


(defun book-search (args)
  (interactive "P")
(helm-org-agenda-files-headings)
  )

(defun current-buffers-search (args)
  (interactive "P")
(helm-multi-swoop-org)
  )

(defun book-load-current ()
  (interactive)
  (jay/save-some-buffers)
(condition-case nil
  (kill-buffer "top-performer.org")
  (error nil))

(org-cycle-agenda-files)
;; (find-file "/Users/jay/b/proposal/12-mistakes-real-headings.txt")
;; (find-file "/Users/jay/Dropbox/writing/book/feb-18/intro.txt")
;; (find-file "/Users/jay/Dropbox/writing/book/feb-18/ovulatory-shift.txt")
;; (find-file "/Users/jay/Dropbox/writing/book/feb-18/bias.txt")
;; (find-file "/Users/jay/Dropbox/writing/book/feb-18/00-the-pervy-professor-effect---an-intro-to-love-biases.txt")
;; (find-file "/Users/jay/Dropbox/writing/book/feb-18/bias-to-categorize.txt")

;; (other-window 1)
)
