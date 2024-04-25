(define-key key-minor-mode-map (kbd "s-/ gr") 'consult-grep)

;; Search
(global-set-key (kbd "s-/ dg") 'deadgrep-current-directory) ; not incremental. but nicely formatted. lays it all out nicely in a buffer.
(global-set-key (kbd "s-/ pa") 'counsel-projectile-ag) ; as an alternative to deadgrep check out ag so maybe it's better

(global-set-key (kbd "s-/ rg") 'consult-ripgrep-current-directory) ; pretty slick, shows you the actual file context

(global-set-key (kbd "s-k rg") 'consult-ripgrep-current-directory) ; pretty slick, shows you the actual file context


(global-set-key (kbd "s-/ gg") 'consult-git-grep) ; pretty great, like projectile, doesn't respect .projectile

;; (global-set-key (kbd "s-/ st") 'consult-) ;
;; (global-set-key (kbd "s-/ l") 'counsel-) ;

(global-set-key (kbd "s-/ rr") 'roam-rg-search)

(defun roam-rg-search ()
 "Search org-roam directory using consult-ripgrep. With live-preview."
 (interactive)
 (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
 (consult-ripgrep org-roam-directory)))

(defun counsel-ag-roam ()
 "Do counsel-ag on the org roam directory"
 (interactive)
 (counsel-ag nil org-roam-directory))

(defun consult-ripgrep-current-directory ()
 "Search current directory using consult-ripgrep."
 (interactive)
 (consult-ripgrep default-directory))


(defun deadgrep-current-directory ()
 "Search current directory using deadgrep."
 (interactive)
 (let ((search-term (read-string "Search term: ")))
  (deadgrep search-term default-directory)))

(defalias 'deadgrep 'deadgrep-project-directory)

(defalias 'rg-current-directory 'rg-dwim-current-dir)
(defalias 'rg-project-directory 'rg-dwim-project-dir)
(defalias 'rg-current-file 'rg-dwim-current-file)

 (use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
  consult-org-roam-forward-links
  :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  ;; ("C-c n e" . consult-org-roam-file-find)
  ;; ("C-c n b" . consult-org-roam-backlinks)
  ;; ("C-c n l" . consult-org-roam-forward-links)
  ;; ("C-c n r" . consult-org-roam-search)
)

;; (define-key key-minor-mode-map (kbd "s-G") 'counsel-projectile-grep); this fails to ignore files specified in .projectile
;; (define-key key-minor-mode-map (kbd "s-G") 'projectile-grep) ; this successfully ignores those files but isn't incremental

(define-key key-minor-mode-map (kbd "C-s-g ") 'consult-ripgrep-current-directory)
(define-key key-minor-mode-map (kbd "s-G") 'counsel-projectile-ag)

(use-package affe
 :config
 ;; Manual preview key for `affe-grep'
 (consult-customize affe-grep :preview-key "M->"))

(use-package ffap)
(defun find-file-at-point-or-affe-find ()
 "Open the file at point if one exists, otherwise run affe-find."
 (interactive)
 (let ((filename (ffap-file-at-point))) ; Check if there is a file at point
 (if filename
  (find-file filename) ; Open file at point if it exists
  (affe-find)))) ; Otherwise, run affe-find

(define-key key-minor-mode-map (kbd "C-x C-f") 'find-file-at-point-or-affe-find)

(defun affe-grep-gnulisp-directory (&optional initial)
 "Fuzzy grep in the /Users/jay/gnulisp directory with optional INITIAL input."
 (interactive "P")
 (affe-grep "/Users/jay/gnulisp" initial))

(defun affe-grep-yasnippets-directory (&optional initial)
 "Fuzzy grep in the /Users/jay/gnulisp directory with optional INITIAL input."
 (interactive "P")
 (affe-grep "/Users/jay/emacs/interesting-snippets/org-mode" initial))

(define-key key-minor-mode-map (kbd "s-k g l") 'affe-grep-gnulisp-directory)

(define-key key-minor-mode-map (kbd "s-/ g l") 'affe-grep-gnulisp-directory)

(define-key key-minor-mode-map (kbd "s-/ g ub") 'affe-grep-bash-scripts)
(define-key key-minor-mode-map (kbd "s-/ g up") 'affe-grep-bash-profile)

;; (define-key key-minor-mode-map (kbd "s-/ g r") 'consult-org-roam-search)
;; (define-key key-minor-mode-map (kbd "s-/ g r") 'affe-grep-org-roam)

(defun counsel-find-file-in-yasnippets ()
 "Use counsel-find-file to search for files in the org-mode snippets directory."
 (interactive)
 (let ((default-directory "/Users/jay/emacs/interesting-snippets/org-mode/"))
 (counsel-find-file)))

(define-key key-minor-mode-map (kbd "s-k y a") 'affe-grep-gnulisp-directory)

(defun fzf-find-file--proposal-directory ()
 "Use counsel-fzf to search for files in the /Users/jay/Dropbox/writing/proposal/ directory."
 (interactive)
 ;; Use counsel-fzf with the specified directory as the root for searching.
 (counsel-fzf nil "/Users/jay/Dropbox/writing/proposal/"))

(defalias 'search-filename-proposal-directory 'fzf-find-file--proposal-directory)

(define-key key-minor-mode-map (kbd "s-/ f p") 'search-filename-proposal-directory)


(defun fzf-find-file--book-directory ()
 "Use counsel-fzf to search for files in the /Users/jay/Dropbox/writing/book/ directory."
 (interactive)
 ;; Use counsel-fzf with the specified directory as the root for searching.
 (counsel-fzf nil "/Users/jay/Dropbox/writing/book/"))

(defalias 'search-filename-book-directory 'fzf-find-file--book-directory)


(define-key key-minor-mode-map (kbd "s-/ f B") 'search-filename-book-directory)

(defun counsel-fzf-both-proposal-and-book-dirs ()
  "Search in both /Users/jay/Dropbox/writing/proposal/ and /Users/jay/Dropbox/writing/book/ directories."
  (interactive)
  ;; Define the directories to search in as dir1 and dir2.
  (let* ((dir1 "/Users/jay/Dropbox/writing/proposal/")
         (dir2 "/Users/jay/Dropbox/writing/book/")
         ;; Use directory-files-recursively to list all files in dir1.
         ;; This function recursively lists files in a directory and its subdirectories.
         (files1 (directory-files-recursively dir1 ""))
         ;; Similarly, list all files in dir2.
         (files2 (directory-files-recursively dir2 ""))
         ;; Combine the file lists from both directories into all-files.
         (all-files (append files1 files2)))
    ;; Use ivy-read to present an interactive interface with the combined file list.
    ;; ivy-read is a part of the ivy completion framework, providing a simple and
    ;; efficient way to select an item from a list.
    (ivy-read "Find file: " all-files
              ;; Define an action to be performed when a file is selected.
              ;; In this case, it opens the selected file with find-file.
              :action (lambda (f) (when f (find-file f))))))


(defalias 'search-filename-both-book-and-proposal-directories 'counsel-fzf-both-proposal-and-book-dirs)

(define-key key-minor-mode-map (kbd "s-/ f B") 'search-filename-book-directory)

(define-key key-minor-mode-map (kbd "s-/ f b") 'search-filename-both-book-and-proposal-directories)

;; works but is overly complex. Do not use. use counsel-projectile-ag instead.
(defun rg-search-book-and-proposal-dirs (&optional initial-input)
 "Search using ripgrep in the book and proposal directories with optional INITIAL-INPUT."
 (interactive "sEnter search pattern: ")
 (let* ((input (if (and initial-input (not (equal initial-input "")))
          initial-input
         ".*"))
     (rg-cmd (concat "rg --vimgrep --color=always --hidden -g '!.git' '"
             input "' "
             "/Users/jay/Dropbox/writing/book "
             "/Users/jay/Dropbox/writing/proposal")))
  (compilation-start rg-cmd 'grep-mode)))

;; don't need any of this. I put both directories in the same projectile project ("both"), so just use counsel-projectile-ag


(defalias 'grep-both-directories 'counsel-projectile-ag)
(defalias 'grep-book-directory 'consult-ripgrep-current-directory)
(defalias 'grep-proposal-directory 'consult-ripgrep-current-directory)


(define-key global-map (kbd "s-/ g b") 'grep-both-directories)

(defun projectile-display-project-root ()
 "Show the root directory of the current Projectile project."
 (interactive)
 (if (projectile-project-p)
   (message "Current project root: %s" (projectile-project-root))
  (message "Not in a project")))
