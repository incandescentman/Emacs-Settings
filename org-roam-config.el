(use-package org-roam
  :defer
  :after org
  :delight
  :custom
  (org-roam-directory (file-truename "/Users/jay/Dropbox/roam"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))
  (org-roam-dailies-directory "journal/")

  ;; Capture templates
  (org-roam-dailies-capture-templates
   '(("j" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+filetags: :journal:
- tags :: \n\n
* %<%Y-%m-%d>\n
-
"))))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
 (setq org-roam-capture-templates
	      '(

	        ("t" "topic" plain "- tags :: \n
* ${title}
- %?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :topic:")
	         :unnarrowed t)

          ("p" "person" plain "%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :person:\n\
- tags :: \n\n
* ${title}
- ")
	         :unnarrowed t)

	        ("d" "definition" plain "- tags :: \n
* ${title}
- %?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :definitiont:")
           :unnarrowed t)

	        ("s" "sentence" plain "- tags :: \n
* ${title}
- %?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :sentence:")
	         :unnarrowed t)

	        ("i" "idea" plain "- tags :: \n
* ${title}
- %?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :idea:")
	         :unnarrowed t)

	        ("c" "conversation" plain "- tags :: \n
* ${title}
- %?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :conversation:")
	         :unnarrowed t)


	        ("d" "default" plain "- tags :: \n
* ${title}
- %?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+title: ${title}\n#+filetags: :note:")
	         :unnarrowed t)


          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)


          ))

  :bind
  (
   ("s-u f" . org-roam-find-node)
   ("S-s-<up>" . org-roam-backlinks-buffer)
   ;; ("S-s-<down>" . projectile-ripgrep)
   ("S-s-<down>" . consult-git-grep)
   ;;   ("S-s-<down>" . deadgrep)
   ("s-u l" . org-roam-buffer-toggle)
   ("s-u i" . org-roam-node-insert)
   ("s-u c" . org-roam-capture)
   ("S-s-<left>" .  org-roam-node-insert)
   ("S-s-<right>" . org-roam-node-find)
   ("s-u r" . org-roam-refile)

   ;; goto
   ("s-u o" . org-roam-dailies-find-date)
   ("s-u d" . org-roam-dailies-goto-date)
   ("s-u p" . org-roam-dailies-goto-previous-note)
   ("s-u n" . org-roam-dailies-goto-next-note)
   ("s-j"   . org-roam-dailies-goto-today)
   ("C-S-d" . org-roam-dailies-goto-today)
   ("s-u y" . org-roam-dailies-goto-yesterday)
   ("s-u T" . org-roam-dailies-goto-tomorrow)
   ("s-u Y" . org-roam-dailies-yesterday)

   ;; capture
   ("s-u t" . org-roam-dailies-capture-today)
   ("s-u c" . org-roam-dailies-capture-today)
   ("s-u k" . org-roam-dailies-capture-date)

   ;; search
   ("s-/ sn" . org-roam-search-nodes)
   (":" . insert-colon)

   ("s-u h" . org-roam-heading-add) ;; org-roam create heading
   ("s-u a" . org-roam-alias-add)

   ;;   ("s-T" . org-roam-tags)

   ;; ("C-c r d a" . org-agenda)
   ;; ("C-c r d s" . org-schedule)

   ))

;; Add custom functions and advice
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

;; rename some org-roam functions
(defalias 'org-roam-heading-add 'org-id-get-create)
(defalias 'org-roam-find-node 'org-roam-node-find)
(defalias 'org-roam-insert-node 'org-roam-node-insert)

;; Search
(global-set-key (kbd "s-/ dg") 'deadgrep) ; not incremental. but nicely formatted
(global-set-key (kbd "s-/ cp") 'counsel-projectile-ag) ; as an alternative to deadgrep check out ag so maybe it's better
(global-set-key (kbd "s-/ rg") 'consult-ripgrep) ; pretty slick, shows you the actual file context
(global-set-key (kbd "s-/ gg") 'consult-git-grep) ; pretty great, like projectile, doesn't respect .projectile

;; (global-set-key (kbd "s-/ st") 'consult-) ;
;; (global-set-key (kbd "s-/ l") 'counsel-) ;

;; Include org-roam-protocol and org-roam-export after org-roam
(use-package org-roam-protocol
  :after org-roam)

(use-package org-roam-export
  :after org-roam)


;; inline tags for plain list items
(defface inline-tag-face
  '((t (:foreground "orange" :weight bold)))
  "Face for custom inline tags in plain list items.")

(font-lock-add-keywords 'org-mode
  '(("#\\(\\w+\\)" 0 'inline-tag-face)))

(defun search-for-inline-tag (tag)
  "Search for inline TAG in org-mode files."
  (interactive "sEnter tag to search for: ")
  (org-search-view nil (concat "#" tag)))

(defun insert-inline-tag ()
  (interactive)
  (let* ((tag-alist '((?r . "review")
                      (?b . "book")
                      (?t . "todo")
                      (?u . "urgent")
                      (?p . "tweet")
                      (?i . "insight")
                      (?c . "cook-ideas-over-time")))
         (selected-key (read-char "Choose a tag:\n
r: review
b: book
t: todo
u: urgent
p: tweet
i: insight
c: cook-ideas-over-time\n")))
    (setq selected-tag (cdr (assoc selected-key tag-alist)))
    (if selected-tag
        (insert (format " #%s" selected-tag))
      (message "Invalid tag selection"))))


(define-key org-mode-map (kbd "s-:") 'insert-inline-tag)
(define-key org-mode-map (kbd "s-;") 'search-for-inline-tag)
(define-key key-minor-mode-map (kbd "s-;") 'search-for-inline-tag)

(add-to-list 'org-agenda-custom-commands
             '("r" "Review items"
               agenda ""
               (
                ;; (org-agenda-files '("~/path/to/your/org/files/"))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "::review::"))
                (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                (org-agenda-overriding-header "Items to review"))))


