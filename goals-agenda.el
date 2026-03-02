;; ============================================================================
;; JAY'S DAILY ACTION MENU --- Energy-Based Task View
;; ============================================================================

;; First, set up the projects directory
(setq jay-projects-dir "~/Library/CloudStorage/Dropbox/github/velocity/projects/")

;; Make sure org-agenda knows to look in the projects directory
(setq org-agenda-files
      (list "~/Library/CloudStorage/Dropbox/github/velocity/goals/20250519215301-master-task-list.org"
            jay-projects-dir))

;; Custom agenda command: Daily Action Menu
(setq org-agenda-custom-commands
      '(("d" "Daily Action Menu"
         ((tags-todo "#focus/TODO|#focus/NEXT"
                     ((org-agenda-overriding-header "\n🔥 FOCUS WORK (90+ min blocks, deep attention required)\n")
                      (org-agenda-sorting-strategy '(priority-down deadline-up))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'past))))
          
          (tags-todo "#admin/TODO|#admin/NEXT"
                     ((org-agenda-overriding-header "\n⚙️  ADMIN TASKS (5-30 min, light cognitive load)\n")
                      (org-agenda-sorting-strategy '(priority-down effort-up))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'past))))
          
          (tags-todo "#connect/TODO|#connect/NEXT"
                     ((org-agenda-overriding-header "\n🤝 CONNECT (Email, calls, relationship building)\n")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'past))))
          
          (tags-todo "#batch/TODO|#batch/NEXT"
                     ((org-agenda-overriding-header "\n📦 BATCH (Do several together, context switching penalty)\n")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'past))))
          
          (agenda ""
                  ((org-agenda-overriding-header "\n📅 SCHEDULED FOR TODAY\n")
                   (org-agenda-span 1)
                   (org-agenda-start-day nil))))
         
         ((org-agenda-compact-blocks t)
          (org-agenda-block-separator "")))))

;; Optional: Quick access binding moved to keys.el

;; ============================================================================
;; RELATED SETTINGS (Optional but recommended)
;; ============================================================================

;; Show inherited tags in agenda (so project context shows up)
(setq org-agenda-show-inherited-tags t)

;; Dim blocked tasks (tasks that depend on incomplete prerequisites)
(setq org-agenda-dim-blocked-tasks t)

;; Keep agenda visually stable (monospace + no soft wrapping).
(defun jay/org-agenda-make-it-not-deranged ()
  "Keep org-agenda aligned: fixed-pitch, no wrapping."
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (setq-local line-spacing 0)
  (setq-local org-agenda-tags-column -80)

  ;; Force monospace in agenda even if variable-pitch is enabled elsewhere.
  (when (fboundp 'buffer-face-mode)
    (setq-local buffer-face-mode-face 'fixed-pitch)
    (buffer-face-mode 1)))

(add-hook 'org-agenda-mode-hook #'jay/org-agenda-make-it-not-deranged)
(add-hook 'org-agenda-finalize-hook #'jay/org-agenda-make-it-not-deranged)

;; Use a more readable date format
(setq org-agenda-format-date
      (lambda (date)
        (concat "\n" 
                (make-string 79 ?─) "\n"
                (org-agenda-format-date-aligned date))))

;; Add effort estimates to your agenda view (optional)
;; Shows how long you estimated each task will take
(setq org-agenda-prefix-format
      '((agenda . " %?-12t% s")
        (todo . " %s")
        (tags . " %s")
        (search . " %s")))

;; Avoid tag spillover in narrow windows.
(setq org-agenda-remove-tags t)
