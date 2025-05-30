;; --------------------
;; Performance optimizations
;; --------------------


(setq load-prefer-newer t)              ; do this once, near top of init

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
    (load file)))



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
  :defer t
  :ensure t
  :load-path "/Users/jay/emacs/emacs-settings/")

(use-package counsel
  :defer t
  )


(autoload 'whittle "whittle" nil t)

;; suppress auto evilifaction errors
(with-eval-after-load 'evil-evilified-state
  (add-to-list 'evil-evilified-state-modes 'org-agenda-mode))


;; Automatically tangle config files




(defun my/auto-tangle-org-file ()
  "Automatically tangle and byte-compile org file on save"
  (when (and (eq major-mode 'org-mode)
             (string-match-p "/emacs-settings/" (buffer-file-name)))
    (message ">>> AUTO-TANGLE running for %s" (buffer-name))
    (require 'ob-tangle)
    (let* ((org-file (buffer-file-name))
           (el-file (concat (file-name-sans-extension org-file) ".el"))
           (elc-file (concat el-file "c")))
      (message "DEBUG: Tangling %s to %s" org-file el-file)

      ;; Use the same tangle method as your load function
      (org-babel-tangle-file org-file el-file "emacs-lisp")

      (message "DEBUG: After tangling, .el file exists: %s" (file-exists-p el-file))

      (when (file-exists-p el-file)
        (message "DEBUG: Compiling %s" el-file)
        (byte-compile-file el-file)
        (when (get-buffer "*Compile-Log*")
          (kill-buffer "*Compile-Log*"))
        (message "Tangled and compiled: %s" (file-name-nondirectory el-file))))))

(add-hook 'after-save-hook #'my/auto-tangle-org-file)


;;; -----------------------------------------------------------
;;; helpers ----------------------------------------------------
(defun my/native-comp-available-p ()
  "Return non-nil only when the native compiler *really* works."
  (and (fboundp 'native-compile)          ; symbol exists
       (require 'comp nil t)             ; library is loaded (≥ 28)
       (functionp 'native-comp-available-p)
       (native-comp-available-p)))       ; linked with libgccjit

(defun my/compiled-path (el-file)
  "Best existing artefact for EL-FILE: .eln ▸ .elc ▸ EL-FILE."
  (let ((eln (and (fboundp 'native-compiled-file)
                  (native-compiled-file el-file)))
        (elc (byte-compile-dest-file el-file)))
    (cond ((and eln (file-exists-p eln)) eln)
          ((file-exists-p elc)           elc)
          (t                             el-file))))



(defun my/load-org-config (org-file)
  "Tangle ORG-FILE → *.el, compile if needed, then load it."
  (require 'ob-tangle) ; Ensure tangling functions are available
  (let* ((org-file   (expand-file-name org-file))
         (el-file    (concat (file-name-sans-extension org-file) ".el"))
         ;; Get the path to the best *existing* compiled artifact or the .el file
         (existing-bin (my/compiled-path el-file)))

    ;; Fast path: if a compiled version exists and it's newer than the .org source, just load it.
    ;; `existing-bin` could be .eln, .elc, or .el itself if nothing compiled yet.
    ;; We only take the fast path if `existing-bin` is NOT the .el file (i.e., it's a compiled version)
    ;; AND this compiled version is up-to-date with respect to the .org file.
    (if (and existing-bin
             (not (string= el-file existing-bin)) ; Ensure 'existing-bin' is a compiled file, not the .el itself
             (file-exists-p existing-bin)
             (file-newer-than-file-p existing-bin org-file))
        (progn
          ;; (message "Fast path: Loading pre-compiled %s (for %s)" existing-bin org-file)
          (load (file-name-sans-extension el-file) nil 'nomessage))

        ;; Slow path: (Re)tangle and/or (re)compile, then load.
        (progn
          ;; (message "Slow path: Processing %s" org-file)
          ;; 1 ─ Tangle if .el is missing or .org is newer
          (when (or (not (file-exists-p el-file))
                    (file-newer-than-file-p org-file el-file))
            ;; (message "Tangling %s -> %s" org-file el-file)
            (org-babel-tangle-file org-file el-file "emacs-lisp"))

          ;; Proceed only if tangling produced the .el file
          (when (file-exists-p el-file)
            ;; Update `bin` to reflect the best compiled version *after* potential re-tangle/re-compile
            ;; This check is for the state *before* we potentially compile *now*.
            (let ((current-compiled-artifact (my/compiled-path el-file)))
              ;; 2 ─ (Re)compile if .el is newer than its current compiled artifact,
              ;;     or if the compiled artifact doesn't exist (e.g. current-compiled-artifact is el-file itself).
              (when (or (not (file-exists-p current-compiled-artifact))
                        (string= el-file current-compiled-artifact) ; .el file is the "bin", so it needs compilation
                        (file-newer-than-file-p el-file current-compiled-artifact))
                ;; (message "Compiling %s..." el-file)
                (if (my/native-comp-available-p)
                    (native-compile el-file)
                    (byte-compile-file el-file))
                ;; 3 ─ Hide *Compile-Log*
                (when-let ((buf (get-buffer "*Compile-Log*"))) (kill-buffer buf))))

            ;; 4 ─ Load the code
            ;; (message "Loading (after potential tangle/compile) %s" (file-name-sans-extension el-file))
            (load (file-name-sans-extension el-file) nil 'nomessage))))))

;;; -----------------------------------------------------------

(dolist (f '("~/emacs/emacs-settings/gnu-emacs-startup.org"
             "~/emacs/emacs-settings/shared-functions.org"
             "~/emacs/emacs-settings/spacecraft-mode.org"
             "~/emacs/emacs-settings/pasteboard-copy-and-paste-functions.org"
             "~/emacs/emacs-settings/search-commands.org"
             "~/emacs/emacs-settings/fonts-and-themes.org"))
  (my/load-org-config f))




(load "/Users/jay/emacs/emacs-settings/jay-osx.el")
(load "/Users/jay/emacs/external-packages/prelude/core/prelude-core.el")

(autoload 'prelude-core "prelude-core" nil t)

(load "/Users/jay/emacs/emacs-settings/skeletons.el")

(autoload 'prelude-key-chord "prelude-key-chord" nil t)


(load "/Users/jay/emacs/emacs-settings/org-roam-config.el")


;; (load "/Users/jay/emacs/emacs-settings/poetry_JD.el")

;; (load "/Users/jay/gnulisp/book-functions.el")
;; (load "/Users/jay/emacs/archive/email.el")
;; (org-babel-load-file "/Users/jay/emacs/external-packages/org-mime-stuff/org-mime-stuff.org")

;; (load "/Users/jay/emacs/emacs-settings/define-word.el")
;; (load "/Users/jay/emacs/emacs-settings/searchlink/searchlink-new.el")
;; (load "/Users/jay/emacs/emacs-settings/ivy-smex.el")
;; (load "/Users/jay/emacs/emacs-settings/emacs_friends.el")
;; (load "/Users/jay/gnulisp/org-image.el")
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
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<M-S-left>") nil)
     (define-key org-mode-map (kbd "<M-S-right>") nil)
     (define-key org-mode-map (kbd "<M-S-up>") nil)
     (define-key org-mode-map (kbd "<M-S-down>") nil)
     (define-key org-mode-map (kbd "<M-left>") nil)
     (define-key org-mode-map (kbd "<M-right>") nil)
     (define-key org-mode-map (kbd "<M-right>") nil)
     (define-key org-mode-map [C-S-right] 'org-shiftmetaright)
     (define-key org-mode-map [C-S-left] 'org-shiftmetaleft)
     (define-key org-mode-map [C-right] 'org-metaright)
     (define-key org-mode-map [C-left] 'org-metaleft)
     (define-key org-mode-map [C-up] 'org-metaup)
     (define-key org-mode-map [C-down] 'org-metadown)
     (define-key org-mode-map (kbd "<C-return>") 'return-insert-blank-line-before)
     (define-key org-mode-map (kbd "<C-S-return>") 'smart-org-insert-todo-heading-dwim)
     ;; (define-key key-minor-mode-map (kbd "<C-M-right>") 'org-shiftright)
     ;; (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-shiftleft)
     ;;     (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-backward-sentence)
     (define-key key-minor-mode-map (kbd "<C-M-left>") 'org-outdent-or-promote)
     ;;     (define-key key-minor-mode-map (kbd "<C-M-right>") 'smart-forward-sentence)
     (define-key key-minor-mode-map (kbd "<C-M-right>") 'org-indent-or-demote)


     (define-key org-mode-map [C-S-return] 'org-insert-todo-heading)
     (define-key org-mode-map (kbd "<C-return>") 'return-insert-blank-line-before)
     (define-key org-mode-map (kbd "<C-S-return>") 'smart-org-insert-todo-heading-dwim)

     (define-key key-minor-mode-map (kbd "<M-S-up>") 'org-shiftup)
     (define-key key-minor-mode-map (kbd "<M-S-down>") 'org-shiftdown)
     (define-key org-mode-map (kbd "<M-up>") 'up-by-degrees)
     (define-key org-mode-map (kbd "<M-down>") 'down-by-degrees)
     (define-key key-minor-mode-map (kbd "<M-down>") 'down-by-degrees)
     (define-key key-minor-mode-map (kbd "<M-up>") 'up-by-degrees)

     ;; (define-key org-mode-map (kbd "needs a binding") 'org-insert-heading-respect-content)
     ;; formerly bound to C-return
     ))


;;  (setq helm-echo-input-in-header-line nil)
;; (add-hook 'helm-after-initialize-hook
;;           #'(lambda () (setq helm-echo-input-in-header-line nil)))



(setq org-bullets-bullet-list '("• "))

;; (use-package transient)
;; (use-package rg)

(use-package reveal-in-finder
  :defer t
  )

;; (recenter-top-bottom)
(setq case-fold-search t)

(setq company-global-modes '(not org-mode))




(setq org-hide-leading-stars t)


(add-hook 'emacs-startup-hook
          (lambda ()
            (toggle-menu-bar-mode-from-frame)
            (scrollbar-init)
            (menu-bar-mode -1)
            (smartparens-mode 1)
            (smartparens-global-mode 1)
            ))


;;; --- Full-screen at start-up ---------------------------------------------
(defun jd/enter-fullscreen-once ()
  "Make the initial GUI frame full-screen, then remove the hook."
  (when (and (display-graphic-p)      ; skip if running in a tty
             (fboundp 'toggle-frame-fullscreen))  ; built-in since Emacs 24
    (toggle-frame-fullscreen))
  ;; Only need to run once:
  (remove-hook 'window-setup-hook #'jd/enter-fullscreen-once))

(add-hook 'window-setup-hook #'jd/enter-fullscreen-once)


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


;; (setq max-lisp-eval-depth 10000)
;;; When opening a file that is a symbolic link, don't ask whether I
;;; want to follow the link. Just do it
;; (setq find-file-visit-truename t)
;; this seems to break in Emacs 28, so I commented it



;; (define-key org-ai-mode-map (kbd "C-c r") 'eval-region)

(global-fasd-mode 1)




(use-package server
  )
(when (server-running-p)
  (server-force-delete))
(server-start)

(redbold)
