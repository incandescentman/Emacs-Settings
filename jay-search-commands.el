(define-key key-minor-mode-map (kbd "s-/ gr") 'consult-grep)

;; Search
(global-set-key (kbd "s-/ dg") 'deadgrep) ; not incremental. but nicely formatted. lays it all out nicely in a buffer.
(global-set-key (kbd "s-/ pa") 'counsel-projectile-ag) ; as an alternative to deadgrep check out ag so maybe it's better
(global-set-key (kbd "s-/ rg") 'consult-ripgrep) ; pretty slick, shows you the actual file context
(global-set-key (kbd "s-/ gg") 'consult-git-grep) ; pretty great, like projectile, doesn't respect .projectile

;; (global-set-key (kbd "s-/ st") 'consult-) ;
;; (global-set-key (kbd "s-/ l") 'counsel-) ;
