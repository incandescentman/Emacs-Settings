(use-package org-roam
  :delight
  :custom
  (org-roam-directory (file-truename "~/Dropbox/roam/"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :journal:\n\n"))))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-export)
  (setq org-roam-capture-templates
	      '(

	        ("t" "topic" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :topic:\n\n")
	         :unnarrowed t)

          ("c" "contact" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :person:\n\n\n")
	         :unnarrowed t)

	        ("d" "definition" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :topic:\n\n")
           :unnarrowed t)

	        ("s" "sentence" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :topic:\n\n")
	         :unnarrowed t)

	        ("i" "idea" plain "%?"
	         :target (file+head "blog/${slug}.org"
			                        "#+title: ${title}\n#+filetags: :idea:\n\n")
	         :unnarrowed t)

	        ("a" "association" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :association:\n\n")
	         :unnarrowed t)

	        ("r" "research" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :research:\n\n")
	         :unnarrowed t)

	        ("k" "Kings class" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :class:\n\n")
	         :unnarrowed t)

	        ("d" "default" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :note:\n\n")
	         :unnarrowed t)
          ))
  :bind

  ;; s-u (define-key key-minor-mode-map (kbd "s-u ") 'roam)

  (
   ("s-u f" . org-roam-node-find)
   ("S-s-<up>" . org-roam-buffer-toggle)
   ;; ("S-s-<down>" . projectile-ripgrep)
   ("S-s-<down>" . consult-git-grep)
   ;;   ("S-s-<down>" . deadgrep)
   ("s-u l" . org-roam-buffer-toggle)
   ("s-u i" . org-roam-node-insert)
   ("s-u c" . org-roam-capture)
   ;; ("C-c r d a" . org-agenda)
   ;; ("C-c r d s" . org-schedule)
   ("s-D" . org-roam-dailies-goto-today)
   ("s-u t" . org-roam-dailies-goto-today)
   ("s-u t" . org-roam-dailies-capture-today)
   ("S-s-<left>" .  org-roam-node-insert)
   ("S-s-<right>" . org-roam-node-find)
   ("s-u d" . org-roam-dailies-goto-date)

   (":" . insert-colon)

   ("s-u t" . org-roam-dailies-capture-today)
   ;; ("s-u y" . org-roam-dailies-yesterday)
   ;; ("s-u T" . org-roam-dailies-goto-today)
   ;; ("s-u Y" . org-roam-dailies-goto-yesterday)
   ("s-u +" . org-roam-dailies-goto-tomorrow)
   ("s-u v" . org-roam-dailies-goto-tomorrow)
   ("s-u j" . org-roam-dailies-capture-today)
   ("s-u c" . org-roam-capture)
   ("s-u r" . org-roam-refile)
   ;; ("s-u a" . org-roam-alias-add)
("s-u o" . org-roam-dailies-find-date)
   ("s-u p" . org-roam-dailies-goto-previous-note)
   ("s-u n" . org-roam-dailies-goto-next-note)
   ("s-u k" . org-roam-dailies-capture-date)
   ("s-u m" . org-roam-dailies-goto-date)
   ))
