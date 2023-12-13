(use-package org-roam
  :after org
  :delight
  :custom
  (org-roam-directory (file-truename "/Users/jay/Dropbox/roam"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:15}" 'face 'org-tag)))
  (org-roam-dailies-directory "journal/")

  ;; Capture templates

;; %A, %B %d, %Y
  (org-roam-dailies-capture-templates
   '(("j" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: :journal:
- Links :: \n
* %<%A, %B %d, %Y>
\n\n\n"))))
;; - %<Week %w, day %j>\n
  :config
;;  (org-roam-setup)
  (org-roam-db-autosync-mode)



  (setq org-roam-capture-templates
	      '(

	        ("n" "note" plain "- Links :: \n- Source :: \n

* ${title}
%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :note:")
	         :unnarrowed t)



	        ("m" "memoir" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "memoir/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :memoir:")
	         :unnarrowed t)

	        ("q" "quotes for the book" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "quotes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :quote:")
	         :unnarrowed t)


	        ("$" "consumer" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "consumer/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :memoir:")
	         :unnarrowed t)


          ("S" "Storytelling and Writing" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "storytelling/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :storytelling:")
	         :unnarrowed t)

          ("O" "Outline / Structure / Schelling Points" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "structure/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :structure:")
	         :unnarrowed t)



	        ("k" "kanban" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "kanban/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :kanban:")
	         :unnarrowed t)



          	        ("v" "Venture out of comfort zone / mantras" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "mantras/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :mantras:")
	         :unnarrowed t)




          ("p" "person" plain "%?"
	         :target (file+head "person/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :person:\n\
- Links :: \n- Source :: \n\n
* ${title}
- ")
	         :unnarrowed t)

	        ("b" "book" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "book/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :book:")
	         :unnarrowed t)

          ("B" "Plans" plain "%?"
	         :target (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :project:\n\
- Links :: \n- Source :: \n\n
* ${title}
- ")
	         :unnarrowed t)


          ("P" "photography" plain "%?"
	         :target (file+head "photography/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :photography:\n\
- Links :: [[id:C3FA6627-02AB-46B2-BC9B-2651AEBFCAC9][🌐 photography]]\n- Source :: \n\n
* ${title}
- ")
	         :unnarrowed t)


	        ("X" "exemplars" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "exemplars/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :exemplars:")
	         :unnarrowed t)



                    ("i" "incomegen" plain "%?"
	                   :target (file+head "incomegen/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :business:\n\
- Links :: \n- Source :: \n\n
* ${title}
- ")
	         :unnarrowed t)



                    ("W" "Wanderlust and travel" plain "%?"
	                   :target (file+head "travel/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :business:\n\
- Links :: \n- Source :: \n\n
* ${title}
- ")
	         :unnarrowed t)



	        ("x" "cuts" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "cuts/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :cuts:")
           :unnarrowed t)

	        ("f" "finances and housekeeping" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "finances/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :finances:")
	         :unnarrowed t)


	        ("A" "accountability and task capture" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "accountability/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :accountability:")
	         :unnarrowed t)



	        ("R" "Recipes and Food" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "recipes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :recipes:")
	         :unnarrowed t)



	        ("l" "definition" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "definitions/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :definition:")
           :unnarrowed t)


          ("L" "Learning, lectures, and classes" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "lectures/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :learning:")
           :unnarrowed t)



	        ("s" "sentence" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "sentences/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :sentence:")
	         :unnarrowed t)

	        ("c" "creative idea" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "ideas/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :idea:")
	         :unnarrowed t)

	        ("C" "Conversation" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "conversations/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :conversation:")
	         :unnarrowed t)


	        ("d" "default" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :note:")
	         :unnarrowed t)


          ("w" "lectures and public talks" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "lectures/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :lectures:")
           :unnarrowed t)




	        ("e" "emacs" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "emacs/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :emacs:")
	         :unnarrowed t)



	        ("T" "temporary" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "temp/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :temp:")
	         :unnarrowed t)


          	        ("t" "therapy" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "therapy/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :therapy:")
	         :unnarrowed t)


	        ("I" "intelligence" plain "- Links :: \n- Source :: \n

* ${title}
%?"
	         :target (file+head "AI/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :intelligence:")
	         :unnarrowed t)



	        ("a" "article notes or books and articles" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "literature-notes/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :literaturenote:")
	         :unnarrowed t)


	        ("L" "Library of articles poems and essays" plain "- Links :: \n- Source :: \n
* ${title}
%?"
	         :target (file+head "library/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :library:")
	         :unnarrowed t)


          ("w" "work ie NLI" plain "%?"
	         :target (file+head "nli-roam/%<%Y%m%d%H%M%S>-${slug}.org"
			                        "#+TITLE: ${title}\n#+FILETAGS: :work:\n\
- Links :: \n- Source :: \n\n
* ${title}
- ")
	         :unnarrowed t)



        ("z" "zork" plain "- Links :: \nSource :: \n

* ${title}
%?"
         :target (file+head (lambda () (concat (read-string "Enter file path: ") "/%<%Y%m%d%H%M%S>-${slug}.org"))
                            "#+TITLE: ${title}\n#+FILETAGS: :work:")
         :unnarrowed t)

          ))



  :bind
  (
   ("s-u f" . org-roam-find-node)
("S-s-<up>" . org-roam-backlinks-buffer)
   ;; ("S-s-<down>" . projectile-ripgrep)
   ("S-s-<down>" . org-roam-buffer-toggle)
   ;;   ("S-s-<down>" . deadgrep)
   ("s-u l" . org-roam-buffer-toggle)
   ("s-u i" . org-roam-node-insert)
   ("s-u c" . org-roam-capture)
   ("S-s-<left>" . org-roam-node-insert-with-emoji-and-comma)
   ("S-s-<right>" . org-roam-node-find)
   ("s-u r" . org-roam-refile)

   ;; goto
   ("s-u o" . org-roam-dailies-find-date)
   ("s-u ." . org-roam-dailies-goto-date)
   ("s-u p" . org-roam-yesterday)
   ("s-u n" . org-roam-dailies-goto-next-note)
   ("s-j"   . org-roam-dailies-goto-today)
   ("C-S-d" . org-roam-dailies-goto-today)
   ("s-u y" . org-roam-dailies-goto-yesterday)
   ("s-u t" . org-roam-dailies-goto-tomorrow)
   ("s-u Y" . org-roam-dailies-yesterday)

   ;; capture
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


(defun org-roam-yesterday ()
(interactive)
(jay/save-some-buffers)
(org-roam-dailies-goto-previous-note)
  )



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


;; Include org-roam-protocol and org-roam-export after org-roam
(use-package org-roam-protocol
  :after org-roam)

(use-package org-roam-export
  :after org-roam)




(add-to-list 'org-agenda-custom-commands
             '("r" "Review items"
               agenda ""
               (
                ;; (org-agenda-files '("~/path/to/your/org/files/"))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "::review::"))
                (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                (org-agenda-overriding-header "Items to review"))))





;; Fix org-roam-refile to avoid this error:
;; user-error: The kill is not a (set of) tree(s). Use 'C-y' to yank anyway

;; Problem: seems to be related to the org-paste-subtree function, which expects the content in the kill ring to be a valid Org subtree or a set of subtrees. If the content you're trying to refile as a region is not in this format, the function raises an error.
;; Solution: Modify the org-roam-refile function to copy the region as plain text instead of using org-copy-subtree
(defun org-roam-refile-region-or-subtree ()
 "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point."
 (interactive)
 (let* ((regionp (org-region-active-p))
     (region-start (and regionp (region-beginning)))
     (region-end (and regionp (region-end)))
     (node (org-roam-node-read nil nil nil 'require-match))
     (file (org-roam-node-file node))
     (nbuf (or (find-buffer-visiting file)
          (find-file-noselect file)))
     level reversed)
  (if (equal (org-roam-node-at-point) node)
    (user-error "Target is the same as current node")
   (if regionp
     (progn
      (kill-new (buffer-substring-no-properties region-start region-end))
      (org-save-markers-in-region region-start region-end))
    (progn
     (if (org-before-first-heading-p)
       (org-roam-demote-entire-buffer))
     (org-copy-subtree 1 nil t)))
   (with-current-buffer nbuf
    (org-with-wide-buffer
     (goto-char (org-roam-node-point node))
     (setq level (org-get-valid-level (funcall outline-level) 1)
        reversed (org-notes-order-reversed-p))
     (goto-char
     (if reversed
       (or (outline-next-heading) (point-max))
      (or (save-excursion (org-get-next-sibling))
        (org-end-of-subtree t t)
        (point-max))))
     (unless (bolp) (newline))
     (if regionp
       (insert (current-kill 0))
      (org-paste-subtree level nil nil t))
     (and org-auto-align-tags
       (let ((org-loop-over-headlines-in-active-region nil))
        (org-align-tags)))
     (when (fboundp 'deactivate-mark) (deactivate-mark))))
   (if regionp
     (progn
      (goto-char region-end)
      (delete-region region-start region-end))
    (org-preserve-local-variables
     (delete-region
     (and (org-back-to-heading t) (point))
     (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
   ;; If the buffer end-up empty after the refile, kill it and delete its
   ;; associated file.
   (when (eq (buffer-size) 0)
    (if (buffer-file-name)
      (delete-file (buffer-file-name)))
    (set-buffer-modified-p nil)
    ;; If this was done during capture, abort the capture process.
    (when (and org-capture-mode
          (buffer-base-buffer (current-buffer)))
     (org-capture-kill))
    (kill-buffer (current-buffer))))))



;; TODO learn how to use this
(use-package consult-org-roam)
;; https://github.com/jgru/consult-org-roam


;; maybe not OP but I tried this package a few days ago and gave up, it slowed considerably consult-buffer. I couldn't identify yet why.
;; https://www.reddit.com/r/emacs/comments/yy79pn/how_to_hideignore_orgroam_buffersfiles_when_using/
;; exclude certain tags
  ;; (add-to-list 'recentf-exclude ".*roam.org$")
  ;; (add-to-list 'consult-buffer-filter ".*roam.org$")



(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            'org-roam-unlinked-references-section
            ))




(defun org-roam-node-insert-with-emoji ()
  "Insert an Org-roam node link with a 🌐 emoji prepended to the description."
  (interactive)
  (let ((original-org-roam-post-node-insert-hook org-roam-post-node-insert-hook))
    (add-hook 'org-roam-post-node-insert-hook
              (lambda (id description)
                (save-excursion
                  ;; Use org-element-context to find the link
                  (let* ((context (org-element-context))
                         (start (org-element-property :contents-begin context))
                         (end (org-element-property :contents-end context)))
                    (when start
                      (goto-char start)
                      (insert "🌐 ")))))
              ;; Append to the front of the hook list
              0 t)
    (unwind-protect
        (org-roam-node-insert)
      ;; Restore the original hook after the function is done
      (setq org-roam-post-node-insert-hook original-org-roam-post-node-insert-hook))))

(defun org-roam-node-insert-with-emoji-and-comma ()
  "Insert an Org-roam node link with a 🌐 emoji prepended to the description.
If the point is already on an existing link, it prepends ', ' before inserting the new link."
  (interactive)
           ;; Check if point is looking back at "]]"
                  (when (looking-back "\\]\\]" 2)
                    (insert ", "))
  (let ((original-org-roam-post-node-insert-hook org-roam-post-node-insert-hook))
    (add-hook 'org-roam-post-node-insert-hook
              (lambda (id description)
                (save-excursion
                  ;; Use org-element-context to find the link
                  (let* ((context (org-element-context))
                         (start (org-element-property :contents-begin context))
                         (end (org-element-property :contents-end context)))
                    (when start
                      (goto-char start)
                      (insert "🌐 ")))))
              ;; Append to the front of the hook list
              0 t)
    (unwind-protect
        (org-roam-node-insert)
      ;; Restore the original hook after the function is done
      (setq org-roam-post-node-insert-hook original-org-roam-post-node-insert-hook))))
