(use-package org-roam
  :delight
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org-roam/"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :journal:\n"))))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-export)
  (setq org-roam-capture-templates
	'(("i" "individual" plain "%?"
	   :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :person:\n\n\n
* Description
  - Position:
  - Associations:
  - Topics:
  - Website(s):

* Meetings

* Notes

* Projects

* Publications

")
	   :unnarrowed t)
	  ("t" "topic" plain "%?"
	   :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :topic:\n")
	   :unnarrowed t)
	  ("b" "blog" plain "%?"
	   :target (file+head "blog/${slug}.org"
			      "#+title: ${title}\n#+filetags: :blog:\n")
	   :unnarrowed t)
	  ("a" "association" plain "%?"
	   :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :association:\n")
	   :unnarrowed t)
	  ("r" "research" plain "%?"
	   :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :research:\n")
	   :unnarrowed t)
	  ("c" "class" plain "%?"
	   :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :class:\n
* Week 1
** Readings

** Lecture

** Assignments

")
	   :unnarrowed t)
	  ("d" "default" plain "%?"
	   :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :note:\n")
	   :unnarrowed t)))
  :bind

  ;; s-u (define-key key-minor-mode-map (kbd "s-u ") 'roam)

  (("s-u f" . org-roam-node-find)
   ("s-u l" . org-roam-buffer-toggle)
   ("s-u i" . org-roam-node-insert)
   ("s-u c" . org-roam-capture)
   ;; ("C-c r d a" . org-agenda)
   ;; ("C-c r d s" . org-schedule)
   ;; ("C-c r d c" . org-roam-dailies-goto-today)
   ;; ("C-c r d d" . org-roam-dailies-goto-date)
   ))

  (use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :bind (("C-c r s" . org-roam-ui-mode))
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


 (use-package marginalia
  :delight
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

 (use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

