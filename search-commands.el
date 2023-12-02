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

(define-key key-minor-mode-map (kbd "s-/ g l") 'affe-grep-gnulisp-directory)

(define-key key-minor-mode-map (kbd "s-/ b s") 'affe-grep-bash-scripts)
(define-key key-minor-mode-map (kbd "s-/ b p") 'affe-grep-bash-profile)


(define-key key-minor-mode-map (kbd "s-/ b p") 'affe-grep-bash-profile)

(define-key key-minor-mode-map (kbd "s-/ o r") 'consult-org-roam-search)
(define-key key-minor-mode-map (kbd "s-/ b p") 'affe-grep-org-roam)

(use-package affe
 :config
 ;; Manual preview key for `affe-grep'
 (consult-customize affe-grep :preview-key "M->"))

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


(define-key key-minor-mode-map (kbd "s-k p r") 'fzf-find-file--proposal-directory)

(defun fzf-find-file--book-directory ()
 "Use counsel-fzf to search for files in the /Users/jay/Dropbox/writing/book/ directory."
 (interactive)
 ;; Use counsel-fzf with the specified directory as the root for searching.
 (counsel-fzf nil "/Users/jay/Dropbox/writing/book/"))


(define-key key-minor-mode-map (kbd "s-k b o") 'fzf-find-file--book-directory)

(defun fzf-find-file-both-proposal-and-book-dirs ()
 "Search in both /Users/jay/Dropbox/writing/proposal/ and /Users/jay/Dropbox/writing/book/ directories using fzf."
 (interactive)
 (let ((cmd "find '/Users/jay/Dropbox/writing/proposal/' '/Users/jay/Dropbox/writing/book/' -type f | fzf"))
  (ivy-read "Find file: " (process-lines "sh" "-c" cmd)
       :action (lambda (f) (when f (find-file f)))
       :caller 'counsel-fzf-proposal-and-book-dirs)))


(define-key key-minor-mode-map (kbd "s-k b b") 'fzf-find-file-both-proposal-and-book-dirs)
