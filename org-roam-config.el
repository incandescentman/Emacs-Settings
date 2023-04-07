(use-package org-roam
  :defer
  :delight
  :custom
  (org-roam-directory (file-truename "/Users/jay/Dropbox/roam"))
  :config
  (org-roam-setup)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :journal:
- tags :: \n\n
* %<%Y-%m-%d>\n
-
"))))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-export)
  (setq org-roam-capture-templates
	      '(

	        ("t" "topic" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :topic:
- tags :: \n\n
* ${title}
- ")
	         :unnarrowed t)

          ("p" "person" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :person:\n\
- tags :: \n\n
* ${title}
- ")
	         :unnarrowed t)

	        ("d" "definition" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :concept:
- tags :: \n\n
* ${title}
")
           :unnarrowed t)

	        ("s" "sentence" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :sentence:
- tags :: \n\n
* ${title}
")
	         :unnarrowed t)

	        ("i" "idea" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :idea:
- tags :: \n\n
* ${title}
-
")
	         :unnarrowed t)

	        ("c" "conversation" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :conversation:
- tags :: \n\n
* ${title}
")
	         :unnarrowed t)


	        ("d" "default" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :note:
- tags :: \n\n
* ${title}
-
")
	         :unnarrowed t)
          ))
  :bind

  ;; s-u (define-key key-minor-mode-map (kbd "s-u ") 'roam)

  (
   ("s-u f" . org-roam-find-node)
   ("S-s-<up>" . org-roam-backlinks-buffer)
   ;; ("S-s-<down>" . projectile-ripgrep)
   ("S-s-<down>" . consult-git-grep)
   ;;   ("S-s-<down>" . deadgrep)
   ("s-u l" . org-roam-buffer-toggle)
   ("s-u i" . org-roam-node-insert)
   ("s-u c" . org-roam-capture)
   ;; ("C-c r d a" . org-agenda)
   ;; ("C-c r d s" . org-schedule)
   ("s-j j" . org-roam-dailies-goto-today)
   ("C-S-d" . org-roam-dailies-goto-today)
;;   ("s-u t" . org-roam-dailies-goto-today)
;; org-roam-ui-mode
   ("S-s-<left>" .  org-roam-node-insert)
   ("S-s-<right>" . org-roam-node-find)
   ("s-u d" . org-roam-dailies-goto-date)

   (":" . insert-colon)

   ("s-u h" . org-roam-heading-add) ;; org-roam create heading

   ("s-u t" . org-roam-dailies-capture-today)
("s-j y" . org-roam-dailies-yesterday)
   ;; ("s-u Y" . org-roam-dailies-goto-yesterday)
   ("s-j t" . org-roam-dailies-goto-tomorrow)
   ("s-j c" . org-roam-dailies-capture-today)
   ("s-u c" . org-roam-capture)
   ("s-u r" . org-roam-refile)
   ("s-u a" . org-roam-alias-add)
   ("s-u o" . org-roam-dailies-find-date)
   ("s-u p" . org-roam-dailies-goto-previous-note)
   ("s-u n" . org-roam-dailies-goto-next-note)
   ("s-u k" . org-roam-dailies-capture-date)
   ("s-u m" . org-roam-dailies-goto-date)
;;   ("s-T" . org-roam-tags)


   ("s-/ sn" . org-roam-search-nodes)
;   ("s-/ st" . consult-)
; ("s-/ l" . council-)

("s-/ dg" . deadgrep) ; not incremental. but nicely formatted
("s-/ cp" . counsel-projectile-ag) ; as an alternative to deadgrep check out ag so maybe it's better
("s-/ rg" . consult-ripgrep) ; pretty slick, shows you the actual file context
("s-/ gg" . consult-git-grep) ; pretty great, like projectile, doesn't respect .projectile
   ))

;; (global-page-break-lines-mode 0)
(advice-add #'org-roam-fontify-like-in-org-mode :around (lambda (fn &rest args) (save-excursion (apply fn args))))



(setq org-roam-completion-everywhere t)
;; doesn't work for some reason

;; so that org-roam links can be followed
;; source: [[https://github.com/org-roam/org-roam/issues/1732][clicking on any link within *org-roam* buffer fails with an error message · Issue #1732 · org-roam/org-roam]]
;; if necessary, consider using org-roam-buffer-refresh



(defun org-roam-backlinks-buffer ()
(interactive)
(org-roam-buffer-toggle)
(other-window 1)
)


(defun org-roam-search-nodes ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
;;  (require 'org-roam)
(counsel-rg nil org-roam-directory nil nil))


(defalias 'org-roam-heading-add 'org-id-get-create)
(defalias 'org-roam-find-node 'org-roam-node-find)
(defalias 'org-roam-insert-node 'org-roam-node-insert)
