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

;; (define-key key-minor-mode-map (kbd "s-G") 'counsel-projectile-grep); this fails to ignore files specified in .projectile
;; (define-key key-minor-mode-map (kbd "s-G") 'projectile-grep) ; this successfully ignores those files but isn't incremental

(define-key key-minor-mode-map (kbd "s-G") 'consult-ripgrep-current-directory)
(define-key key-minor-mode-map (kbd "C-s-g") 'counsel-projectile-ag)
