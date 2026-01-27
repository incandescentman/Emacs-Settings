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

;; Use a more readable date format
(setq org-agenda-format-date
      (lambda (date)
        (concat "\n" 
                (make-string 79 ?─) "\n"
                (org-agenda-format-date-aligned date))))

;; Add effort estimates to your agenda view (optional)
;; Shows how long you estimated each task will take
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))
