;; ------------- init.el (or early-load file) --------------------------
(require 'cl-lib)

(use-package ob-tangle
  :defer t)          ; 1️⃣ make sure tangling is available

(with-eval-after-load 'org    ; 2️⃣ define + register the helper
  (setq org-support-shift-select 'always)
  (defun jd/org-auto-tangle ()
    "Tangle this Org buffer on save when it has #+auto_tangle: t."
    (when (and (derived-mode-p 'org-mode)
               (member "t"
                       (cdr (assoc "auto_tangle"
                                   (org-collect-keywords '("auto_tangle"))))))
      (org-babel-tangle)))

  ;; Global hook is fine; predicate prevents work on non-Org files.
  (add-hook 'after-save-hook #'jd/org-auto-tangle))

(defvar jay/startup-module-timings nil
  "Collected startup timing entries as (LABEL . SECONDS).")

(defvar jay/startup-profile-file "/Users/jay/emacs/emacs-settings/docs/lab/startup-profile.org"
  "Org file that stores startup timing snapshots.")

(defun jay/measure-startup-step (label thunk)
  "Measure THUNK and store timing with LABEL."
  (let ((start (float-time)))
    (prog1 (funcall thunk)
      (push (cons label (- (float-time) start)) jay/startup-module-timings))))

(defun jay/load-with-timing (file)
  "Load FILE and record elapsed startup time."
  (jay/measure-startup-step
   (format "load %s" (file-name-nondirectory file))
   (lambda () (load file))))

(defun jay/org-babel-load-file-with-timing (file)
  "Run `org-babel-load-file' on FILE and record elapsed startup time."
  (jay/measure-startup-step
   (format "org-babel-load %s" (file-name-nondirectory file))
   (lambda () (org-babel-load-file file))))

(defun jay/require-with-timing (feature)
  "Require FEATURE and record elapsed startup time."
  (jay/measure-startup-step
   (format "require %s" feature)
   (lambda () (require feature))))

(defun jay/startup-write-profile ()
  "Append one startup timing snapshot to `jay/startup-profile-file'."
  (let* ((dir (file-name-directory jay/startup-profile-file))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (total (if (and (boundp 'before-init-time)
                         (boundp 'after-init-time)
                         before-init-time
                         after-init-time)
                    (float-time (time-subtract after-init-time before-init-time))
                  nil))
         (timings (sort (copy-sequence jay/startup-module-timings)
                        (lambda (a b) (> (cdr a) (cdr b))))))
    (make-directory dir t)
    (with-temp-buffer
      (if (file-exists-p jay/startup-profile-file)
          (insert-file-contents jay/startup-profile-file)
        (insert "#+TITLE: Startup Timing Profile\n#+STARTUP: showall\n"))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" timestamp))
      (insert (format "- Emacs: %s\n" emacs-version))
      (insert (format "- Org: %s\n" (if (boundp 'org-version) org-version "unknown")))
      (when total
        (insert (format "- Total init time: %.3fs\n" total)))
      (insert "- Measured module loads:\n")
      (insert "| Step | Seconds |\n|------+---------|\n")
      (dolist (entry timings)
        (insert (format "| %s | %.4f |\n" (car entry) (cdr entry))))
      (write-region (point-min) (point-max) jay/startup-profile-file nil 'silent))))

(defvar jay/startup-profile-written nil
  "Non-nil once the current Emacs session has written a startup profile.")

(defun jay/startup-write-profile-once ()
  "Write the startup timing profile once per Emacs session."
  (unless jay/startup-profile-written
    (setq jay/startup-profile-written t)
    (jay/startup-write-profile)))

(add-hook 'emacs-startup-hook #'jay/startup-write-profile-once)

;; `spacemacs-new-config.el' loads late enough that `emacs-startup-hook' may
;; already have fired in some startup paths. Schedule a one-shot fallback so
;; the profile file is still written for this session.
(when after-init-time
  (run-with-idle-timer 1 nil #'jay/startup-write-profile-once))

(defun jay/startup-health-check ()
  "Show a quick startup health report for Org and Org-roam glue code."
  (interactive)
  (require 'org)
  (let* ((root "/Users/jay/emacs/emacs-settings")
         (suite-dir (expand-file-name "jay-org-roam-suite" root))
         (fix-info '(("org-roam-id-fix.el" . org-roam-id-fix)
                     ("org-roam-db-fix.el" . org-roam-db-fix)))
         (lines (list
                 (format "Org version: %s" org-version)
                 (format "org-link-preview available: %s" (if (fboundp 'org-link-preview) "yes" "no"))
                 (format "org-support-shift-select: %S"
                         (if (boundp 'org-support-shift-select)
                             org-support-shift-select
                           'unbound)))))
    (dolist (entry fix-info)
      (let* ((filename (car entry))
             (feature (cdr entry))
             (suite-path (expand-file-name filename suite-dir))
             (parent-path (expand-file-name filename root))
             (found-at (cond
                        ((file-exists-p suite-path) suite-path)
                        ((file-exists-p parent-path) parent-path)
                        (t "missing")))
             (loaded (if (featurep feature) "yes" "no")))
        (setq lines
              (append lines
                      (list (format "%s found: %s" filename found-at)
                            (format "%s loaded: %s" feature loaded))))))
    (with-current-buffer (get-buffer-create "*Jay Startup Health*")
      (erase-buffer)
      (insert (mapconcat #'identity lines "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "Startup health report written to *Jay Startup Health*")))

(defun jay/doctor--recent-load-errors (&optional max-lines max-hits)
  "Return recent error-like lines from *Messages*."
  (let ((line-limit (or max-lines 600))
        (hit-limit (or max-hits 12))
        (hits '())
        (seen 0))
    (with-current-buffer (messages-buffer)
      (save-excursion
        (goto-char (point-max))
        (while (and (< seen line-limit)
                    (not (bobp)))
          (setq seen (1+ seen))
          (forward-line -1)
          (let ((line (string-trim (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
            (when (and (not (string-empty-p line))
                       (string-match-p
                        "\\(\\b[Ee]rror\\b\\|failed\\|void-function\\|void-variable\\|wrong-type-argument\\)"
                        line))
              (push line hits))))))
    (setq hits (delete-dups (nreverse hits)))
    (if (> (length hits) hit-limit)
        (cl-subseq hits 0 hit-limit)
      hits)))

(defun jay/doctor--binary-report ()
  "Return diagnostic rows for required external binaries."
  (mapcar (lambda (bin)
            (cons bin (or (executable-find bin) "MISSING")))
          '("rg" "pandoc" "hunspell")))

(defun jay/doctor--org-roam-db-report ()
  "Return Org-roam database diagnostics as an alist."
  (let* ((db-loc (and (boundp 'org-roam-db-location) org-roam-db-location))
         (db-file (and db-loc (expand-file-name db-loc)))
         (db-size (and db-file (file-exists-p db-file)
                       (file-attribute-size (file-attributes db-file))))
         (db-live (and (boundp 'org-roam-db)
                       (boundp 'org-roam-db-location)
                       (fboundp 'emacsql-live-p)
                       org-roam-db
                       (ignore-errors (emacsql-live-p org-roam-db)))))
    `(("org-roam-directory" . ,(if (boundp 'org-roam-directory) org-roam-directory "unbound"))
      ("org-roam-db-location" . ,(or db-file "unbound"))
      ("db-file-exists" . ,(if (and db-file (file-exists-p db-file)) "yes" "no"))
      ("db-file-size" . ,(if db-size (format "%s bytes" db-size) "n/a"))
      ("db-connection-live" . ,(if db-live "yes" "no")))))

(defun jay/doctor--dropbox-sanity-report ()
  "Return Dropbox path sanity diagnostics."
  (let* ((direct "/Users/jay/Dropbox")
         (cloud "/Users/jay/Library/CloudStorage/Dropbox")
         (roam (and (boundp 'org-roam-directory) org-roam-directory))
         (roam-expanded (and roam (expand-file-name roam))))
    `(("direct-dropbox-exists" . ,(if (file-directory-p direct) "yes" "no"))
      ("cloudstorage-dropbox-exists" . ,(if (file-directory-p cloud) "yes" "no"))
      ("org-roam-directory" . ,(or roam-expanded "unbound"))
      ("org-roam-uses-cloudstorage-path"
       . ,(if (and roam-expanded (string-prefix-p cloud roam-expanded)) "yes" "no")))))

(defun jay/doctor--keybinding-collisions ()
  "Return key collisions where key-minor bindings shadow different global commands."
  (let ((collisions '()))
    (when (boundp 'key-minor-mode-map)
      (map-keymap
       (lambda (event binding)
         (when (and (commandp binding)
                    (not (integerp event)))
           (let* ((key (vector event))
                  (global (lookup-key global-map key)))
             (when (and (commandp global)
                        (not (eq global binding)))
               (push (list (key-description key) binding global) collisions)))))
       key-minor-mode-map))
    (nreverse collisions)))

(defun jay/doctor--collect-fix-hints (bins roam dropbox collisions errors)
  "Return actionable one-line fix hints from doctor inputs."
  (let ((hints '()))
    ;; Missing binaries
    (dolist (row bins)
      (let ((bin (car row))
            (path (cdr row)))
        (when (string= path "MISSING")
          (push (format "Missing %s: run `brew install %s`" bin bin) hints))))

    ;; Org-roam directory/db checks
    (when (string= (cdr (assoc "db-file-exists" roam)) "no")
      (push "Org-roam DB missing: run `M-x org-roam-db-sync`" hints))
    (when (string= (cdr (assoc "db-connection-live" roam)) "no")
      (push "Org-roam DB connection is not live: run `M-x org-roam-db-clear-all` then `M-x org-roam-db-sync`" hints))
    (let ((roam-dir (cdr (assoc "org-roam-directory" roam))))
      (when (and (stringp roam-dir)
                 (not (string= roam-dir "unbound"))
                 (not (file-directory-p roam-dir)))
        (push (format "Org-roam directory missing: run `mkdir -p %s`" (shell-quote-argument roam-dir))
              hints)))

    ;; Dropbox path sanity
    (when (string= (cdr (assoc "direct-dropbox-exists" dropbox)) "no")
      (push "Direct Dropbox path missing: verify Dropbox desktop is installed and `/Users/jay/Dropbox` exists" hints))
    (when (string= (cdr (assoc "org-roam-uses-cloudstorage-path" dropbox)) "yes")
      (push "Org-roam points to CloudStorage path: set `org-roam-directory` to `/Users/jay/Dropbox/roam` in `jay-org-roam-core.el`" hints))

    ;; Keybinding collisions
    (when (> (length collisions) 0)
      (push (format "Found %d key collisions: review with `M-x describe-keymap RET key-minor-mode-map RET` and resolve the highest-frequency keys first"
                    (length collisions))
            hints))

    ;; Recent error-like messages
    (when errors
      (push "Startup/runtime errors seen: run `M-x view-echo-area-messages`, then `M-x toggle-debug-on-error` and restart to capture a backtrace" hints))

    (if hints
        (delete-dups (nreverse hints))
      '("No obvious fixes needed based on current checks."))))

(defun jay/doctor (&optional fix-hints)
  "Run one-screen diagnostics for startup/runtime health.
With prefix arg FIX-HINTS, append actionable remediation commands."
  (interactive "P")
  (require 'org)
  (let* ((errors (jay/doctor--recent-load-errors))
         (bins (jay/doctor--binary-report))
         (roam (jay/doctor--org-roam-db-report))
         (dropbox (jay/doctor--dropbox-sanity-report))
         (collisions (jay/doctor--keybinding-collisions))
         (hints (and fix-hints
                     (jay/doctor--collect-fix-hints bins roam dropbox collisions errors))))
    (with-current-buffer (get-buffer-create "*Jay Doctor*")
      (erase-buffer)
      (insert (format "Jay Doctor Report - %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Emacs: %s\nOrg: %s\norg-link-preview: %s\n\n"
                      emacs-version
                      org-version
                      (if (fboundp 'org-link-preview) "yes" "no")))

      (insert "== External Binaries ==\n")
      (dolist (row bins)
        (insert (format "- %s: %s\n" (car row) (cdr row))))
      (insert "\n== Org-roam DB ==\n")
      (dolist (row roam)
        (insert (format "- %s: %s\n" (car row) (cdr row))))
      (insert "\n== Dropbox Path Sanity ==\n")
      (dolist (row dropbox)
        (insert (format "- %s: %s\n" (car row) (cdr row))))

      (insert (format "\n== Keybinding Collisions (key-minor vs global) ==\n- Count: %d\n"
                      (length collisions)))
      (dolist (row (if (> (length collisions) 20)
                       (cl-subseq collisions 0 20)
                     collisions))
        (insert (format "- %s :: minor=%s | global=%s\n"
                        (nth 0 row) (nth 1 row) (nth 2 row))))

      (insert "\n== Recent Error-like Messages ==\n")
      (if errors
          (dolist (line errors)
            (insert (format "- %s\n" line)))
        (insert "- none detected in recent *Messages* scan\n"))

      (when fix-hints
        (insert "\n== Fix Hints ==\n")
        (dolist (hint hints)
          (insert (format "- %s\n" hint))))

      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message (if fix-hints
                 "Doctor report with fix hints written to *Jay Doctor*"
               "Doctor report written to *Jay Doctor*"))))

(defun jay/doctor-fix-hints ()
  "Run `jay/doctor' and include actionable remediation hints."
  (interactive)
  (jay/doctor t))






;; debugging steps. Commenting these out now that it seems to be working.

;; (message "DEBUG: About to load helpers...")
;; (defvar te nil)

;; (advice-add 'load :before (lambda (f &rest _) (message ">>> LOADING %s" f)))


;; (defun trace-load (file &rest _)
;;   (message ">>> LOADING %s" file))
;; (advice-add 'load :before #'trace-load)


;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Scratch-12.org

;; --------------------
;; Performance optimizations
;; --------------------




;; 2) --- use GCMH via use-package -----------------------
(use-package gcmh
  :ensure t
  :demand t
  :init (gcmh-mode 1))

(setq message-log-max t)
;; (use-package benchmark-init
;;   :ensure t

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; --------------------
;; Don't give org-assert-version error
;; --------------------
;; (require 'org-macs)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)

                                        ; Source: https://ambrevar.xyz/emacs2/



(setq find-file-visit-truename nil)
;; speed optimizations from
;; https://www.reddit.com/r/emacs/comments/f3ed3r/how_is_doom_emacs_so_damn_fast/

(setq frame-inhibit-implied-resize t)
;; (setq initial-major-mode 'fundamental-mode)

(let ((directory "~/emacs/emacs-settings/elpa-supplement/"))
  (dolist (file (directory-files directory t "\\.el$"))
    (jay/load-with-timing file)))



;; (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))




(setq vc-follow-symlinks t)



(setq default-frame-alist
      '(
        (width . 160) ; character
        (height . 42) ; lines
        ))

(setq yas-snippet-dirs '("/Users/jay/emacs/interesting-snippets/" "~/emacs/snippets"))


(use-package wc-goal-mode
  :defer
  :ensure t
  :defer
  :load-path "/Users/jay/emacs/emacs-settings/")

(use-package counsel
  :defer)


(autoload 'whittle "whittle" nil t)







(jay/load-with-timing "/Users/jay/emacs/emacs-settings/jay-osx.el")
(jay/load-with-timing "/Users/jay/emacs/emacs-settings/keys.el")
(jay/org-babel-load-file-with-timing "~/emacs/emacs-settings/gnu-emacs-startup.org")
(jay/org-babel-load-file-with-timing "~/emacs/emacs-settings/shared-functions.org")
(jay/org-babel-load-file-with-timing "~/emacs/emacs-settings/spacecraft-mode.org")
(jay/load-with-timing "/Users/jay/emacs/emacs-settings/pasteboard-copy-and-paste-functions.el")
(jay/org-babel-load-file-with-timing "/Users/jay/emacs/emacs-settings/search-commands.org")
(jay/org-babel-load-file-with-timing "/Users/jay/emacs/emacs-settings/fonts-and-themes.org")
(jay/org-babel-load-file-with-timing "/Users/jay/emacs/emacs-settings/goals-agenda.org")

;; (org-babel-load-file "/Users/jay/emacs/external-packages/org-mime-stuff/org-mime-stuff.org")
(jay/load-with-timing "/Users/jay/emacs/external-packages/prelude/core/prelude-core.el")
(jay/load-with-timing "/Users/jay/emacs/emacs-settings/skeletons.el")
(jay/load-with-timing "/Users/jay/emacs/emacs-settings/prelude-key-chord.el")
;; (load "/Users/jay/gnulisp/book-functions.el")
(jay/load-with-timing "/Users/jay/emacs/emacs-settings/poetry_JD.el")
;; (load "/Users/jay/emacs/emacs-settings/define-word.el")
;; (load "/Users/jay/emacs/emacs-settings/searchlink/searchlink-new.el")
;; (load "/Users/jay/emacs/emacs-settings/emacs_friends.el")
;; (load "/Users/jay/gnulisp/org-image.el")

;; Load modular org-roam suite
(add-to-list 'load-path "/Users/jay/emacs/emacs-settings/jay-org-roam-suite")
(jay/require-with-timing 'jay-org-roam-core)       ;; lazy Org-roam core (idle timers, keybindings, profiles, templates)
(jay/require-with-timing 'jay-editor-extras)       ;; environment: PATH, ispell, XeLaTeX, captain-predicate
(jay/load-with-timing "/Users/jay/emacs/emacs-settings/jay-goals-system.el")  ;; rep counter, morning ritual, anti-stall
(run-with-idle-timer 3 nil #'jay-goals-setup)  ;; initialize goals system after startup

;;(load "/Users/jay/emacs/emacs-settings/org-roam-review.el")

;; (monaco-font)

;; (load "/Users/jay/emacs/org-mime.el")

;; (load "/Users/jay/tramp-settings.el")

;; automatically display any prefix
(setq guide-key/recursive-key-sequence-flag t)

;; use OSX standard keybindings for navigating word-by-word and selecting
;; whole words at a time
;; I've been wanting to do this for so long. :-)
;; this works correctly!!
;; Keybindings moved to keys.el


;;  (setq helm-echo-input-in-header-line nil)
;; (add-hook 'helm-after-initialize-hook
;;           #'(lambda () (setq helm-echo-input-in-header-line nil)))



(setq org-bullets-bullet-list '("• "))

;; (use-package transient)
;; (use-package rg)

(use-package reveal-in-finder
  :defer
  )

;; (recenter-top-bottom)
(setq case-fold-search t)

(setq company-global-modes '(not org-mode))




(setq org-hide-leading-stars t)


(add-hook 'emacs-startup-hook
          (lambda ()
            ;;            (toggle-menu-bar-mode-from-frame)
            ;;(scrollbar-init)
            (menu-bar-mode -1)
            (smartparens-mode 1)
            (smartparens-global-mode 1)
            ))

;; (jay/toggle-fullscreen)




;; (electric-pair-mode 1)



;; (defadvice load-theme (before theme-dont-propagate activate)
;;   (mapcar #'disable-theme custom-enabled-themes))

;; if Emacs is running in terminal
(if (is-in-terminal)
    (iterm-mode)
    ;; (load-theme 'zenburn)
    (org-mode)
    )

;; (iterm-mode)

(setq org-emphasis-alist
      (quote
       (("*" bold)
        ("/" italic)
        ("_" underline)
        ("~" org-code verbatim)
        ("=" flyspell-incorrect)
        ("+"
         (:strike-through t)))))

;; Org emphasis regex - must survive org reloads and space-doc resets
;; Define the desired components once
(defvar jay/org-emphasis-regexp-components
  '("-—–[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
  "Jay's custom org-emphasis-regexp-components.")

(defun jay/apply-org-emphasis-settings ()
  "Apply custom org emphasis settings. Survives org reloads."
  (setq org-emphasis-regexp-components jay/org-emphasis-regexp-components)
  (when (fboundp 'org-set-emph-re)
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)))

;; Apply on org load AND on every org-mode buffer (survives resets)
(with-eval-after-load 'org
  (jay/apply-org-emphasis-settings))
(add-hook 'org-mode-hook #'jay/apply-org-emphasis-settings)

;; Reapply after Spacemacs/space-doc helpers override emphasis settings.
(with-eval-after-load 'core-funcs
  (advice-add 'spacemacs/prettify-org-buffer :after #'jay/apply-org-emphasis-settings))
(with-eval-after-load 'space-doc
  (advice-add 'spacemacs//space-doc-alternative-emphasis :after #'jay/apply-org-emphasis-settings))

(setq org-adapt-indentation nil)

;; disable smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;;



(setq package-archive-priorities nil)

(setq global-auto-complete-mode -1)

(setq ido-save-directory-list-file "/Users/jay/emacs/emacs-settings/spacemacs.d/.savefile/ido.hist")

(display-time)

;; (setq evil-emacs-state-cursor '("red" (hbar . 2))) ; for horizontal cursor
(setq evil-emacs-state-cursor '("red")) ; ;; for box cursor


(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;;(horizontal-cursor)
(box-cursor)
(incarnadine-cursor)
(scroll-bar-mode 1)
(defun package--save-selected-packages (&rest opt) nil)

;; (load "/Users/jay/emacs/emacs-settings/mu4e-send-delay.el")



;; To permanently enable mode line display of org clock, add this snippet to your dotspacemacs/user-config function:
;; (setq spaceline-org-clock-p t)

(setq user-init-file "/Users/jay/emacs/emacs-settings/spacemacs.d/init.el")

(setq case-fold-search t)

(setq global-hl-line-mode nil)
(setq hl-line-mode nil)

(setq org-twbs-link-home "http://jaydixit.com")
(setq org-twbs-postamble nil)
(setq org-twbs-postamble-format nil)
(setq org-twbs-preamble nil)

(load "/Users/jay/emacs/emacs-settings/jay-org-in-item-p.el")

;; (load "/Users/jay/emacs/emacs-settings/pdf-continuous-scroll.el")

;; (server-reflash)
;;  (triplicate-code)
;; (embiggen-text)
;;(scrollbar-init)

;; (setq max-lisp-eval-depth 10000)
;;; When opening a file that is a symbolic link, don't ask whether I
;;; want to follow the link. Just do it
;; (setq find-file-visit-truename t)
;; this seems to break in Emacs 28, so I commented it



;; (define-key org-ai-mode-map (kbd "C-c r") 'eval-region)

(global-fasd-mode 1)





(redbold)
