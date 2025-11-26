;;; jay-org-roam-profiles.el --- Multi-database org-roam profiles  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a clean system for switching between multiple independent org-roam databases.
;; Each profile has its own:
;; - Database file
;; - Directory structure
;; - Capture templates
;; - Configuration
;;
;; Usage:
;;   M-x jay/org-roam-switch-profile RET my-life RET
;;   M-x jay/org-roam-switch-profile RET default RET
;;
;; Or bind to a key:
;;   (global-set-key (kbd "C-c r p") #'jay/org-roam-switch-profile)

;;; Code:

(require 'org-roam nil t)
(require 'subr-x)
(declare-function org-astro-export-to-mdx "ox-astro" (&optional async subtreep visible-only body-only))
(defvar org-astro-source-root-folder nil
  "Root directory for ox-astro exports. Updated when switching jay/org-roam profiles.")

;; -----------------------------------------------------------------------------
;; Profile Storage
;; -----------------------------------------------------------------------------
(defvar jay/org-roam-current-profile nil
  "The name of the currently active org-roam profile.")

(defvar jay/org-roam-dailies-template-default
  '(("j" "Journal" entry "* %?"
     :target (file+head "%<%Y-%m-%d>.org"
                        ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: :journal:\n\n- Links ::\n\n* %<%A, %B %d, %Y>\n\n** Today [0/1]\n")))
  "Default org-roam dailies capture template with ID drawer and journal metadata.")

(defvar jay/org-roam-profiles
  '((default
     :name "High Velocity"
     :directory "/Users/jay/Dropbox/roam"
     :db-location nil  ; use default cache for performance
     :dailies-directory "journal/"
     :capture-templates jay/org-roam-capture-templates-default
     :dailies-capture-templates jay/org-roam-dailies-template-default
     :astro-source-root "/Users/jay/Dropbox/roam")

    (my-life
     :name "My Life (Personal)"
     :directory "/Users/jay/Dropbox/roam-life"
     :db-location "/Users/jay/Dropbox/roam-life/.org-roam.db"
     :dailies-directory "journal/"
     :capture-templates jay/org-roam-capture-templates-mylife
     :dailies-capture-templates jay/org-roam-dailies-template-default
     :astro-source-root "/Users/jay/Dropbox/roam-life"
     :mode-line-label "Life")

    (roam-social
     :name "Roam Social (Relationships)"
     :directory "/Users/jay/Dropbox/roam-social"
     :db-location "/Users/jay/Dropbox/roam-social/.org-roam.db"
     :dailies-directory "journal/"
     :capture-templates jay/org-roam-capture-templates-social
     :dailies-capture-templates jay/org-roam-dailies-template-default
     :astro-source-root "/Users/jay/Dropbox/roam-social"
     :mode-line-label "Social")

    (roam-parents
     :name "Roam Parents (Family HQ)"
     :directory "/Users/jay/Dropbox/roam-parents"
     :db-location "/Users/jay/Dropbox/roam-parents/.org-roam.db"
     :dailies-directory "journal/"
     :capture-templates jay/org-roam-capture-templates-parents
     :dailies-capture-templates jay/org-roam-dailies-template-default
     :astro-source-root "/Users/jay/Dropbox/roam-parents"
     :mode-line-label "Parents"))
  "Alist of org-roam profile configurations.
Each profile is a plist with keys:
  :name - Display name for the profile
  :directory - Root directory for org-roam files
  :db-location - Path to database file (nil = use default)
  :dailies-directory - Subdirectory for daily notes
  :capture-templates - Symbol or list of capture templates
  :dailies-capture-templates - Symbol or list of dailies capture templates")

;; -----------------------------------------------------------------------------
;; Profile Definitions
;; -----------------------------------------------------------------------------

;; Helper for building templates
(defun jay/roam-template (key label dir tag &rest args)
  "Helper to build an org-roam capture template.

KEY is the capture key, LABEL is displayed in capture prompts, DIR is
the directory (relative to the profile root), and TAG is used to derive
the default `#+FILETAGS:` line.

Additional keyword ARGS allow callers to override pieces of the template:

  :body            String used as the template body.
  :filetags        String inserted after `#+FILETAGS:` (default derives from TAG).
  :created-format  Time format string used for the `#+CREATED:` stamp.
  :type            Capture entry type (defaults to `plain`).
  :target          Custom capture target (skips standard directory logic).
  :unnarrowed      Whether to leave the final buffer unnarrowed (defaults to t).
  :props           Additional plist appended to the template definition."
  ;; Examples:
  ;;   (jay/roam-template "n" "note" "notes" "note")
  ;;   (jay/roam-template "p" "person" "person" "person"
  ;;                      :body "- Links :: [[id:xyz][People]]\n* ${title}\n%?")
  ;;   (jay/roam-template "z" "custom" "" nil
  ;;                      :target (file+head (lambda () ...) "#+TITLE: ${title}\n"))
  (let* ((body (or (plist-get args :body)
                   "- Links ::\n- Source ::\n\n* ${title}\n%?"))
         (filetags (if (plist-member args :filetags)
                       (plist-get args :filetags)
                       (when tag (format ":%s:" tag))))
         (created-format (or (plist-get args :created-format)
                             "%Y-%m-%d %a %H:%M"))
         (type (or (plist-get args :type) 'plain))
         (target (or (plist-get args :target)
                     `(file+head ,(format "%s/%%<%%Y%%m%%d%%H%%M%%S>-${slug}.org" dir)
                                 ,(concat "#+TITLE: ${title}\n"
                                          (format "#+CREATED: [%%<%s>]\n" created-format)
                                          (when filetags
                                            (format "#+FILETAGS: %s\n" filetags))))))
         (unnarrowed (if (plist-member args :unnarrowed)
                         (plist-get args :unnarrowed)
                         t))
         (props (plist-get args :props)))
    (append
     `(,key ,label ,type ,body
            :target ,target
            :unnarrowed ,unnarrowed)
     props)))

(defconst jay/org-roam-default-template-specs
  '(("A" "accountability and task capture" "accountability" "accountability")
    ("a" "article notes or books and articles" "literature-notes" "literaturenote")
    ("H" "Plans" "project" "project")
    ("g" "ChatGPT Outputs" "chatgpt-outputs" "gpt")
    ("I" "intelligence" "AI" "intelligence"
     :body "- Links ::\n- Source ::\n\n\n* ${title}\n%?")
    ("j" "job application" "job-applications" "job")
    ("l" "logistics of OpenAI" "logistics" "library")
    ("c" "Conversation" "conversations" "conversation")
    ("d" "documents and deliverables" "documents" "document")
    ("D" "Developing, vibecoding" "vibecoding" "document")
    ("E" "Exercise" "exercise" "exercise")
    ("f" "finances and housekeeping" "finances" "finances")
    ("M" "Momentum --- 2025 job hunt" "job-hunt-2025" "momentum")
    ("O" "Outline / Structure / Schelling Points" "structure" "structure")
    ("p" "person" "person" "person"
     :body "- Links :: [[id:20240426T130414.177117][🌐 People]]\n- Source ::\n\n* ${title}\n%?")
    ("W" "writers" "writers" "writers"
     :filetags ":writers:person:")
    ("k" "kanban" "kanban" "kanban")
    ("L" "Learning, lectures, and classes" "lectures" "learning")
    ("z" "CRM (custom path)" "" "crm"
     :body "- Links ::\nSource ::\n\n\n* ${title}\n%?"
     :target (file+head (lambda ()
                          (concat (read-string "Enter file path: ")
                                  "/%<%Y%m%d%H%M%S>-${slug}.org"))
                        "#+TITLE: ${title}\n#+CREATED: [%<%Y-%m-%d %a %H:%M>]\n#+FILETAGS: :crm:")))
  "Data-driven specs for the default profile's bespoke templates.")

(defconst jay/org-roam-default-factory-template-specs
  '(("B" "Business (Socratic AI)" "business-socratic" "project")
    ("b" "books" "books" "books")
    ("C" "corpus (transcripts and examples)" "corpus" "corpus")
    ("e" "emacs" "emacs" "emacs")
    ("m" "mantras and intentions" "mantras" "mantras")
    ("n" "note" "notes" "note")
    ("o" "OpenAI, i.e. work" "notes" "work")
    ("P" "photography" "photography" "photography")
    ("q" "quotes about AI" "quotes" "quote")
    ("S" "Socratic AI" "socratic" "socratic")
    ("s" "Storytelling and Writing" "storytelling" "storytelling")
    ("T" "Travel" "travel" "travel")
    ("w" "lectures and public talks" "lectures" "lectures")
    ("X" "exemplars" "exemplars" "exemplars")
    ("x" "cuts" "cuts" "cuts")
    ("$" "consumerist" "consumerist" "consumerist")
    ("r" "social / relationships" "social" "social"))
  "Factory-style template specs that also lean on `jay/roam-template`.")

;; DEFAULT PROFILE TEMPLATES (Your current work setup)
(defvar jay/org-roam-capture-templates-default
  (append
   (mapcar (lambda (spec) (apply #'jay/roam-template spec))
           jay/org-roam-default-template-specs)
   (mapcar (lambda (spec) (apply #'jay/roam-template spec))
           jay/org-roam-default-factory-template-specs))
  "Capture templates for the default (work) profile.")

;; MY-LIFE PROFILE TEMPLATES (Personal/life notes)
(defvar jay/org-roam-capture-templates-mylife
  (list
   ;; Storytelling & memoir templates
   '("s" "story / anecdote" plain
     "- Links ::\n- Date :: %<%Y-%m-%d>\n- Time Period :: \n- Location :: \n- People Involved :: \n\n* ${title}\n\n** The Story\n\n%?\n\n** Why This Matters\n\n** Details to Remember\n\n"
     :target (file+head "stories/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :story:")
     :unnarrowed t)

   '("o" "OpenAI story" plain
     "- Links ::\n- Date :: %<%Y-%m-%d>\n- Time Period :: \n- People :: \n\n* ${title}\n\n** What Happened\n\n%?\n\n** Context\n\n** Why It's Interesting\n\n"
     :target (file+head "openai-stories/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :openai:story:")
     :unnarrowed t)

   '("m" "memory / moment" plain
     "- Links ::\n- When :: \n- Where :: \n- Who :: \n\n* ${title}\n\n%?\n\n"
     :target (file+head "memories/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :memory:")
     :unnarrowed t)

   '("f" "family story" plain
     "- Links ::\n- About :: \n- Time Period :: \n- Source :: \n\n* ${title}\n\n** The Story\n\n%?\n\n** Context\n\n"
     :target (file+head "family/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :family:story:")
     :unnarrowed t)

   '("h" "high school / college story" plain
     "- Links ::\n- Date :: %<%Y-%m-%d>\n- Year :: \n- Place :: \n- People :: \n\n* ${title}\n\n%?\n\n"
     :target (file+head "school-days/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :school:story:")
     :unnarrowed t)

   '("p" "person (character sketch)" plain
     "- Links ::\n- Relationship :: \n- Time Period :: \n\n* ${title}\n\n** Memorable Qualities\n\n%?\n\n** Key Stories\n\n** Why They Matter\n\n"
     :target (file+head "people/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :person:memoir:")
     :unnarrowed t)

   '("M" "Moth story idea" plain
     "- Links ::\n- Date :: %<%Y-%m-%d>\n- Theme :: \n- Stakes :: \n- Arc :: \n\n* ${title}\n\n** Opening Hook\n\n%?\n\n** Middle / Turning Point\n\n** Ending / What I Learned\n\n** Stage Notes\n\n"
     :target (file+head "moth-stories/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :moth:performance:")
     :unnarrowed t)

   '("e" "essay / reflection piece" plain
     "- Links ::\n- Date :: %<%Y-%m-%d>\n- Theme :: \n\n* ${title}\n\n** Core Idea\n\n%?\n\n** Examples / Stories\n\n** So What?\n\n"
     :target (file+head "essays/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :essay:memoir:")
     :unnarrowed t)

   '("q" "quote / dialogue" plain
     "- Links ::\n- Who Said It :: \n- Context :: \n- Date :: %<%Y-%m-%d>\n\n* ${title}\n\n%?\n\n"
     :target (file+head "quotes/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :quote:dialogue:")
     :unnarrowed t)

   '("c" "character detail / observation" plain
     "- Links ::\n- About :: \n- Date Observed :: %<%Y-%m-%d>\n\n* ${title}\n\n%?\n\n"
     :target (file+head "observations/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :observation:")
     :unnarrowed t)

   '("t" "turning point / pivotal moment" plain
     "- Links ::\n- Date :: %<%Y-%m-%d>\n- Before :: \n- After :: \n\n* ${title}\n\n** What Happened\n\n%?\n\n** Why It Changed Things\n\n"
     :target (file+head "turning-points/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :turning-point:memoir:")
     :unnarrowed t)

   '("n" "note / fragment" plain
     "- Links ::\n- Date :: %<%Y-%m-%d>\n\n* ${title}\n\n%?\n\n"
     :target (file+head "fragments/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :fragment:")
     :unnarrowed t))
  "Capture templates for the my-life (storytelling/memoir) profile.")

;; ROAM-SOCIAL PROFILE TEMPLATES (Relationships, community, networking)
(defvar jay/org-roam-capture-templates-social
  (list
   ;; General-purpose templates
   (jay/roam-template
    "n" "note" "notes" "note")

   (jay/roam-template
    "d" "idea" "ideas" "idea"
    :body "- Links ::\n- Source ::\n\n* ${title}\n\n** The Idea\n%?\n\n** Why It Matters\n")

   (jay/roam-template
    "s" "insight" "insights" "insight"
    :body "- Links ::\n- Source ::\n- Context :: \n\n* ${title}\n\n%?\n")

   (jay/roam-template
    "t" "thought" "thoughts" "thought")

   ;; Relationship-specific templates
   (jay/roam-template
    "c" "connection log" "connections" "connection"
    :body "- Links ::\n- Person :: \n- Context :: \n- Next Touchpoint :: \n\n* ${title}\n\n** What We Talked About\n%?\n\n** Actions\n- [ ] ")

   (jay/roam-template
    "e" "event debrief" "events" "event"
    :body "- Links ::\n- Date :: %<%Y-%m-%d>\n- Location :: \n- Host :: \n\n* ${title}\n\n** Highlights\n%?\n\n** People Met\n- \n\n** Follow-ups\n- [ ] ")

   (jay/roam-template
    "f" "follow-up plan" "follow-ups" "followup"
    :body "- Links ::\n- Person :: \n- Trigger :: \n\n* ${title}\n\n** Message Draft\n%?\n\n** Reminder Window\n")

   (jay/roam-template
    "i" "introduction" "introductions" "introduction"
    :body "- Links ::\n- For :: \n- From :: \n- Why Now :: \n\n* ${title}\n\n** Bullet Points\n%?\n"
    :filetags ":introduction:social:")

   (jay/roam-template
    "p" "person snapshot" "people" "person"
    :body "- Links ::\n- Relationship Depth :: \n- Last Talked :: \n- Shared Projects :: \n\n* ${title}\n\n** Bio\n%?\n\n** Topics They Love\n- \n\n** I Can Help With\n- \n"
    :filetags ":person:social:")

   (jay/roam-template
    "r" "relationship map" "relationships" "relationship"
    :body "- Links ::\n- Anchor Person :: \n- Theme :: \n\n* ${title}\n\n** Constellation\n- %?\n\n** Warm Paths\n- \n\n** Energy Level\n"))
  "Capture templates for the roam-social (relationships/networking) profile.")

;; ROAM-PARENTS PROFILE TEMPLATES (Family HQ / caretaking)
(defvar jay/org-roam-capture-templates-parents
  (list
   (jay/roam-template
    "c" "care log" "care-log" "care"
    :body "- Links ::\n- Parent :: \n- Date :: %<%Y-%m-%d>\n- Time :: %<%H:%M>\n\n* ${title}\n\n** Summary\n%?\n\n** Mood / Energy\n- \n\n** Notes\n- \n")

   (jay/roam-template
    "m" "medical note" "medical" "medical"
    :body "- Links ::\n- Parent :: \n- Provider :: \n- Facility :: \n- Date :: %<%Y-%m-%d>\n\n* ${title}\n\n** Observations\n%?\n\n** Medications / Dosage\n- \n\n** Questions\n- \n"
    :filetags ":medical:parents:")

   (jay/roam-template
    "a" "appointment" "appointments" "appointment"
    :body "- Links ::\n- Parent :: \n- When :: %<%Y-%m-%d %H:%M>\n- Location :: \n\n* ${title}\n\n** Prep Checklist\n- [ ] %?\n\n** Outcomes\n- \n")

   (jay/roam-template
    "p" "parent dossier" "profiles" "profile"
    :body "- Links ::\n- Birthdate :: \n- Primary Doctor :: \n- Emergency Contacts :: \n\n* ${title}\n\n** Health Snapshot\n- Conditions :: \n- Allergies :: \n\n** Stories / Values\n%?\n"
    :filetags ":parents:profile:")

   (jay/roam-template
    "s" "support reflection" "reflections" "reflection"
    :body "- Links ::\n- Date :: %<%Y-%m-%d>\n- Theme :: \n\n* ${title}\n\n** What Went Well\n%?\n\n** Hard Moments\n\n** Next Actions\n- [ ] ")

   (jay/roam-template
    "t" "task / errand" "tasks" "task"
    :body "- Links ::\n- Parent :: \n- Deadline :: %<%Y-%m-%d>\n\n* ${title}\n\n** Steps\n- [ ] %?\n"
    :filetags ":parents:task:"))
  "Capture templates for the roam-parents (family operations) profile.")


;; -----------------------------------------------------------------------------
;; Core Profile Switching Logic
;; -----------------------------------------------------------------------------

(defun jay/org-roam-get-profile (profile-name)
  "Get the profile plist for PROFILE-NAME."
  (cdr (assq profile-name jay/org-roam-profiles)))

(defun jay/org-roam-profile-exists-p (profile-name)
  "Return t if PROFILE-NAME exists in `jay/org-roam-profiles'."
  (assq profile-name jay/org-roam-profiles))

(defun jay/org-roam-close-database ()
  "Safely close the current org-roam database connection."
  (when (and (boundp 'org-roam-db)
             org-roam-db
             (emacsql-live-p org-roam-db))
    (ignore-errors
      (emacsql-close org-roam-db)))
  (setq org-roam-db nil))

(defun jay/org-roam-apply-profile (profile-name &optional force-sync)
  "Apply the configuration for PROFILE-NAME.
Refreshes all org-roam variables and (re)initialises the database connection.
If FORCE-SYNC is non-nil, ensure the database is synced even when not switching profiles."
  (let* ((profile (jay/org-roam-get-profile profile-name))
         (directory (plist-get profile :directory))
         (db-location (plist-get profile :db-location))
         (dailies-dir (plist-get profile :dailies-directory))
         (templates (plist-get profile :capture-templates))
         (dailies-templates (plist-get profile :dailies-capture-templates))
         (switching-profiles (and jay/org-roam-current-profile
                                  (not (eq jay/org-roam-current-profile profile-name))))
         (needs-sync (or force-sync switching-profiles)))

    (unless profile
      (user-error "Profile '%s' not found" profile-name))

    ;; Ensure org-roam is loaded
    (unless (featurep 'org-roam)
      (require 'org-roam))

    ;; Only close database if we're actually switching
    (when switching-profiles
      (jay/org-roam-close-database))

    ;; Update org-roam configuration
    ;; Use directory path as-is to avoid resolving Dropbox symlink to CloudStorage path
    (setq org-roam-directory (file-name-as-directory
                              (if (file-name-absolute-p directory)
                                  directory
                                  (expand-file-name directory)))
          org-roam-dailies-directory dailies-dir
          org-roam-db-location (if db-location
                                   (if (file-name-absolute-p db-location)
                                       db-location
                                       (expand-file-name db-location))
                                   (expand-file-name "org-roam.db" (xdg-cache-home)))
          org-roam-capture-templates (if (symbolp templates)
                                         (symbol-value templates)
                                         templates))
    (message "DEBUG profiles: org-roam-directory set to %s" org-roam-directory)

    (let ((resolved-dailies (cond
                             ((symbolp dailies-templates)
                              (and (boundp dailies-templates)
                                   (symbol-value dailies-templates)))
                             ((consp dailies-templates) dailies-templates)
                             (t nil))))
      (when resolved-dailies
        (setq org-roam-dailies-capture-templates (copy-tree resolved-dailies))))

    ;; Keep ox-astro exports aligned with the active profile's notes root.
    (let ((astro-root (plist-get profile :astro-source-root)))
      (when astro-root
        (setq org-astro-source-root-folder (if (file-name-absolute-p astro-root)
                                               astro-root
                                               (expand-file-name astro-root)))))

    ;; Ensure the directory exists
    (unless (file-directory-p org-roam-directory)
      (make-directory org-roam-directory t)
      (message "Created org-roam directory: %s" org-roam-directory))

    ;; Reinitialize the database connection when switching profiles    ;; No clearing needed - we're just connecting to a different database file
    (when switching-profiles
      (setq org-roam-db nil)
      (org-roam-db))

    ;; Sync database if we explicitly switched profiles (or caller requested it)
    (when needs-sync
      (condition-case err
          (progn
            (unless (and (boundp 'org-roam-db) (emacsql-live-p org-roam-db))
              (org-roam-db))
            (org-roam-db-sync))
        (error (message "Org-roam profile sync failed: %s" (error-message-string err)))))

    ;; Update current profile
    (setq jay/org-roam-current-profile profile-name)

    ;; Save the current profile for next session
    (jay/org-roam-save-current-profile)

    (when switching-profiles
      (message "Switched to org-roam profile: %s (%s)"
               profile-name
               (plist-get profile :name)))))

(defun jay/org-roam-switch-profile (profile-name)
  "Switch to org-roam profile PROFILE-NAME.
Prompts for profile name with completion."
  (interactive
   (list (intern (completing-read "Switch to profile: "
                                  (mapcar (lambda (p)
                                            (format "%s - %s"
                                                    (car p)
                                                    (plist-get (cdr p) :name)))
                                          jay/org-roam-profiles)
                                  nil t nil nil
                                  (when jay/org-roam-current-profile
                                    (format "%s - %s"
                                            jay/org-roam-current-profile
                                            (plist-get (jay/org-roam-get-profile
                                                        jay/org-roam-current-profile)
                                                       :name)))))))

  ;; Extract just the profile symbol if they selected "symbol - name" format
  (when (stringp profile-name)
    (setq profile-name (intern (car (split-string profile-name " - ")))))

  (jay/org-roam-apply-profile profile-name))

;; -----------------------------------------------------------------------------
;; Persistence (remember last profile across sessions)
;; -----------------------------------------------------------------------------

(defvar jay/org-roam-profile-cache-file
  (expand-file-name "org-roam-current-profile" (xdg-cache-home))
  "File to store the current org-roam profile.")

(defun jay/org-roam-save-current-profile ()
  "Save the current profile to disk."
  (when jay/org-roam-current-profile
    (with-temp-file jay/org-roam-profile-cache-file
      (prin1 jay/org-roam-current-profile (current-buffer)))))

(defun jay/org-roam-load-saved-profile ()
  "Load and apply the last-used profile from disk."
  (when (file-exists-p jay/org-roam-profile-cache-file)
    (condition-case err
        (let ((saved-profile (with-temp-buffer
                               (insert-file-contents jay/org-roam-profile-cache-file)
                               (read (current-buffer)))))
          (when (jay/org-roam-profile-exists-p saved-profile)
            (jay/org-roam-apply-profile saved-profile)
            (message "Restored org-roam profile: %s" saved-profile)))
      (error (message "Failed to load saved org-roam profile: %s"
                      (error-message-string err))))))

;; -----------------------------------------------------------------------------
;; Mode line indicator
;; -----------------------------------------------------------------------------

(defun jay/org-roam-mode-line-indicator ()
  "Return a string for the mode line showing current profile."
  (when jay/org-roam-current-profile
    (let* ((profile (jay/org-roam-get-profile jay/org-roam-current-profile))
           (name (plist-get profile :name))
           (display-name (cond
                          ((plist-get profile :mode-line-label)
                           (plist-get profile :mode-line-label))
                          ((stringp name)
                           (let ((primary (string-trim (car (split-string name "(")))))
                             (unless (string-empty-p primary)
                               primary)))
                          (t nil)))
           (label (or display-name
                      (when jay/org-roam-current-profile
                        (let* ((raw (replace-regexp-in-string "-" " "
                                                              (symbol-name jay/org-roam-current-profile)))
                               (words (split-string raw "[[:space:]]+" t)))
                          (mapconcat #'capitalize words " ")))
                      "roam")))
      (propertize (format " [Roam:%s]"
                          label)
                  'face '(:foreground "cyan")
                  'help-echo (format "Org-roam profile: %s\nClick to switch" name)
                  'mouse-face 'mode-line-highlight
                  'local-map (let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                           #'jay/org-roam-switch-profile)
                               map)))))

;; Add to mode-line-format
(unless (memq '(:eval (jay/org-roam-mode-line-indicator)) mode-line-format)
  (setq-default mode-line-format
                (append mode-line-format
                        '((:eval (jay/org-roam-mode-line-indicator))))))

;; -----------------------------------------------------------------------------
;; Convenience commands
;; -----------------------------------------------------------------------------

(defun jay/org-roam-show-current-profile ()
  "Display information about the current org-roam profile."
  (interactive)
  (if jay/org-roam-current-profile
      (let* ((profile (jay/org-roam-get-profile jay/org-roam-current-profile))
             (name (plist-get profile :name))
             (dir (plist-get profile :directory))
             (db (or (plist-get profile :db-location)
                     (expand-file-name "org-roam.db" (xdg-cache-home)))))
        (message "Current profile: %s (%s)\nDirectory: %s\nDatabase: %s"
                 jay/org-roam-current-profile name dir db))
      (message "No org-roam profile is currently active")))

(defun jay/org-roam-switch-to-default ()
  "Quick switch to default profile."
  (interactive)
  (jay/org-roam-apply-profile 'default))

(defun jay/org-roam-switch-to-mylife ()
  "Quick switch to my-life profile."
  (interactive)
  (jay/org-roam-apply-profile 'my-life))

(defun jay/org-roam-switch-to-social ()
  "Quick switch to roam-social profile."
  (interactive)
  (jay/org-roam-apply-profile 'roam-social))

(defun jay/org-roam-switch-to-parents ()
  "Quick switch to roam-parents profile."
  (interactive)
  (jay/org-roam-apply-profile 'roam-parents))

;; -----------------------------------------------------------------------------
;; Astro export helpers
;; -----------------------------------------------------------------------------

(defun jay/org-astro--ensure-export-backend ()
  "Ensure ox-astro export command is available."
  (unless (fboundp 'org-astro-export-to-mdx)
    (require 'ox-astro nil t))
  (unless (fboundp 'org-astro-export-to-mdx)
    (user-error "org-astro-export-to-mdx is not available; load ox-astro first")))

(defun jay/org-astro--export-with-root (root &optional async subtreep visible-only body-only)
  "Export using ROOT as `org-astro-source-root-folder'."
  (jay/org-astro--ensure-export-backend)
  (let ((org-astro-source-root-folder (expand-file-name root)))
    (org-astro-export-to-mdx async subtreep visible-only body-only)))

(defun jay/org-astro-export-from-roam (&optional async subtreep visible-only body-only)
  "Run `org-astro-export-to-mdx' with the main roam tree as source root."
  (interactive)
  (jay/org-astro--export-with-root "/Users/jay/Dropbox/roam"
                                   async subtreep visible-only body-only))

(defun jay/org-astro-export-from-roam-life (&optional async subtreep visible-only body-only)
  "Run `org-astro-export-to-mdx' with the roam-life tree as source root."
  (interactive)
  (jay/org-astro--export-with-root "/Users/jay/Dropbox/roam-life"
                                   async subtreep visible-only body-only))

(defun jay/org-astro-export-from-roam-social (&optional async subtreep visible-only body-only)
  "Run `org-astro-export-to-mdx' with the roam-social tree as source root."
  (interactive)
  (jay/org-astro--export-with-root "/Users/jay/Dropbox/roam-social"
                                   async subtreep visible-only body-only))

(defun jay/org-astro-export-from-roam-parents (&optional async subtreep visible-only body-only)
  "Run `org-astro-export-to-mdx' with the roam-parents tree as source root."
  (interactive)
  (jay/org-astro--export-with-root "/Users/jay/Dropbox/roam-parents"
                                   async subtreep visible-only body-only))

;; -----------------------------------------------------------------------------
;; Integration with main config
;; -----------------------------------------------------------------------------

(defun jay/org-roam-profiles-init ()
  "Initialize the org-roam profile system.
Call this from your main config after org-roam is loaded."
  ;; Load the last-used profile, or default to 'default
  (if (file-exists-p jay/org-roam-profile-cache-file)
      (jay/org-roam-load-saved-profile)
      (jay/org-roam-apply-profile 'default)))

;; -----------------------------------------------------------------------------
(provide 'jay-org-roam-profiles)
;;; jay-org-roam-profiles.el ends here
