;;; org-visual-style.el --- Tufte-style visual enhancements for Org Mode -*- lexical-binding: t; -*-
;;
;; Author: Your Name
;; URL: https://github.com/your-repo/org-visual-style
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-pretty-table "0.1") (org-bullets "0.2"))
;;
;;; Commentary:
;;
;; This file provides a Tufte-inspired set of visual enhancements for Org Mode:
;; - Sets the variable-pitch face to EtBembo (if available) or a fallback.
;; - Tweaks Org’s bullets, ellipses, and emphasis markers for a clean look.
;; - Disables hl-line-mode in Org buffers for a simpler, less distracting UI.
;; - Uses org-pretty-table for nicer table rendering.
;;
;; *Dependencies*:
;; - org-bullets (optional) for bullet styling
;; - org-pretty-table for table styling
;;
;; Usage:
;; 1. Ensure this file is in your `load-path`.
;; 2. (require 'org-visual-style)
;; 3. M-x jay/org-visual-mode   to toggle all enhancements at once.
;; 4. Alternatively, use M-x jay/preview-org-visual-preset to choose a preset from a list.
;;
;;; Code:

;;;; 1) Define a custom group
(defgroup jay/org-visual nil
  "Tufte-style visual enhancements for Org Mode."
  :prefix "jay/org-visual-"
  :group 'faces
  :group 'org)

;;;; 2) Keep track of enabled features
(defvar jay/org-visual-enabled-features '()
  "List of currently enabled Org visual features.")

;;;; 3) Utility to toggle features
(defun jay/org-visual-toggle-feature (feature-name enable-fn disable-fn)
  "Toggle FEATURE-NAME using ENABLE-FN and DISABLE-FN.
If FEATURE-NAME is currently enabled, call DISABLE-FN and remove
it from `jay/org-visual-enabled-features`. Otherwise, call
ENABLE-FN and add it to that list."
  (if (member feature-name jay/org-visual-enabled-features)
      (progn
        (when (functionp disable-fn) (funcall disable-fn))
        (setq jay/org-visual-enabled-features
              (remove feature-name jay/org-visual-enabled-features))
        (message "Disabled: %s" feature-name))
    (when (functionp enable-fn) (funcall enable-fn))
    (add-to-list 'jay/org-visual-enabled-features feature-name)
    (message "Enabled: %s" feature-name)))

;;;; 4) Font Setup: ET Book (EtBembo), fallback, and org-indent fix
(defun jay/org-visual-enable-etbook-font ()
  "Enable ET Book font if available; fallback to Sans Serif. Also enable `variable-pitch-mode'."
  (when (find-font (font-spec :family "EtBembo"))
    (set-face-attribute 'variable-pitch nil :family "EtBembo" :height 180))
  (unless (find-font (font-spec :family "EtBembo"))
    (message "EtBembo not found; using Sans Serif instead.")
    (set-face-attribute 'variable-pitch nil :family "Sans Serif" :height 120))
  ;; Turn on variable-pitch mode in org
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  ;; Example tweak: Don’t hide the leading stars in indentation
  (setq org-indent-mode-turns-on-hiding-stars nil))

(defun jay/org-visual-disable-etbook-font ()
  "Disable ET Book enhancements; revert to a basic variable-pitch face. Also disable variable-pitch-mode in org."
  (set-face-attribute 'variable-pitch nil :family "Sans Serif" :height 100)
  (remove-hook 'org-mode-hook #'variable-pitch-mode)
  (setq org-indent-mode-turns-on-hiding-stars t))

;;;; 5) UI Tweaks Inspired by Lepisma
(defun jay/org-visual-enable-lepisma-ui ()
  "Enable Lepisma's recommended Org UI tweaks: bullets, ellipses, etc."
  (setq org-startup-indented t
        ;; Requires org-bullets or similar package
        org-bullets-bullet-list '(" ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  (when (featurep 'org-bullets)
    (add-hook 'org-mode-hook #'org-bullets-mode))
  (message "Enabled Lepisma UI tweaks."))

(defun jay/org-visual-disable-lepisma-ui ()
  "Disable Lepisma's recommended Org UI tweaks: revert bullets, ellipses, etc."
  (setq org-startup-indented nil
        org-bullets-bullet-list '("*")
        org-ellipsis "..."
        org-pretty-entities nil
        org-hide-emphasis-markers nil
        org-agenda-block-separator "-"
        org-fontify-whole-heading-line nil
        org-fontify-done-headline nil
        org-fontify-quote-and-verse-blocks nil)
  (when (featurep 'org-bullets)
    (remove-hook 'org-mode-hook #'org-bullets-mode))
  (message "Disabled Lepisma UI tweaks."))

;;;; 6) Enable/Disable hl-line in Org
(defun jay/org-visual-enable-hl-line ()
  "Enable hl-line-mode in Org buffers."
  (add-hook 'org-mode-hook #'hl-line-mode))

(defun jay/org-visual-disable-hl-line ()
  "Disable hl-line-mode in Org buffers."
  (remove-hook 'org-mode-hook #'hl-line-mode))

;;;; 7) Org Pretty Table
(defun jay/org-visual-enable-pretty-table ()
  "Enable `org-pretty-table-mode` in Org buffers."
  (when (require 'org-pretty-table nil 'noerror)
    (add-hook 'org-mode-hook #'org-pretty-table-mode)))

(defun jay/org-visual-disable-pretty-table ()
  "Disable `org-pretty-table-mode` in Org buffers."
  (when (featurep 'org-pretty-table)
    (remove-hook 'org-mode-hook #'org-pretty-table-mode)))

;;;; 8) Define a unified set of presets
(defcustom jay/org-visual-style-presets
  '(("Minimalist"
     . (jay/org-visual-enable-etbook-font
        jay/org-visual-enable-lepisma-ui))
    ("Tufte Inspired"
     . (jay/org-visual-enable-etbook-font
        jay/org-visual-enable-lepisma-ui
        jay/org-visual-disable-hl-line))
    ("Pretty Tables Only"
     . (jay/org-visual-enable-pretty-table))
    ("All Enhancements"
     . (jay/org-visual-enable-etbook-font
        jay/org-visual-enable-lepisma-ui
        jay/org-visual-disable-hl-line
        jay/org-visual-enable-pretty-table)))
  "Named visual style presets for Org Mode.
Each entry is a cons cell (NAME . (FN1 FN2 ...)), where each FN is an enable function."
  :type '(repeat (cons (string :tag "Preset name")
                       (repeat (function :tag "Functions to call"))))
  :group 'jay/org-visual)

(defun jay/preview-org-visual-preset ()
  "Interactively select and apply a named Org visual style preset from `jay/org-visual-style-presets`."
  (interactive)
  (let* ((names (mapcar #'car jay/org-visual-style-presets))
         (choice (completing-read "Preview Org preset: " names))
         (fns (cdr (assoc choice jay/org-visual-style-presets))))
    (when fns
      (mapc (lambda (fn) (when (functionp fn) (funcall fn))) fns)
      (message "Applied Org visual preset: %s" choice))))

;;;; 9) Provide a minor mode to toggle everything at once
;;;###autoload
(define-minor-mode jay/org-visual-mode
  "Toggle a Tufte-inspired set of Org visual enhancements as a single minor mode."
  :init-value nil
  :lighter " JayOrgVis"
  :group 'jay/org-visual
  (if jay/org-visual-mode
      ;; Enabling the minor mode => run your default preset
      (progn
        ;; You could pick which preset you want to apply by default:
        (mapc (lambda (fn)
                (when (functionp fn)
                  (funcall fn)
                  (add-to-list 'jay/org-visual-enabled-features (symbol-name fn))))
              (cdr (assoc "All Enhancements" jay/org-visual-style-presets)))
        (message "jay/org-visual-mode enabled."))
    ;; Disabling the minor mode => revert everything
    (jay/org-visual-revert-all)
    (message "jay/org-visual-mode disabled.")))

;;;; 10) Revert everything
(defun jay/org-visual-revert-all ()
  "Disable all Org visual enhancements that are currently in `jay/org-visual-enabled-features`."
  (interactive)
  (dolist (feature-sym (reverse jay/org-visual-enabled-features))
    (let* ((feature-fn (intern feature-sym))
           ;; We guess the matching disable fn by name, e.g.:
           ;;  'jay/org-visual-enable-foo => 'jay/org-visual-disable-foo
           ;; This only works if your naming is consistent.
           (disable-fn (intern (replace-regexp-in-string
                                "enable" "disable" (symbol-name feature-fn)))))
      (when (functionp disable-fn)
        (funcall disable-fn))))
  (setq jay/org-visual-enabled-features '())
  (message "All Org visual enhancements have been reverted."))

(provide 'org-visual-style)

;;; org-visual-style.el ends here
