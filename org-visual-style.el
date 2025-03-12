;;; org-visual-style.el --- Tufte-style visual enhancements for Org Mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Modular, opt-in enhancements to Emacs Org Mode for a minimal, elegant,
;; and distraction-free writing environment inspired by Edward Tufte and Lepisma's Unixporn ricing post.

;;; Code:

;;;; 📚 Font Setup: ET Book (EtBembo) and Org Indentation Alignment
(defun jay/set-etbook-font-and-indent-fix ()
  "Set ET Book font and fix org-indent alignment for proportional fonts."
  (set-face-attribute 'variable-pitch nil :family "EtBembo" :height 180)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (setq org-indent-mode-turns-on-hiding-stars nil))

;;;; 🖼️ Org Mode UI Settings From Lepisma's Unixporn Setup
(defun jay/lepisma-org-ui-tweaks ()
  "Apply Lepisma-inspired Org UI visual tweaks."
  (setq org-startup-indented t
        org-bullets-bullet-list '(" ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t))

;;;; 🧼 Optional: Disable hl-line-mode in Org
(defun jay/lepisma-disable-hl-line ()
  "Disable hl-line-mode in Org buffers to avoid highlighting issues."
  (add-hook 'org-mode-hook (lambda () (hl-line-mode -1))))

;;;; 📊 Org Pretty Table (Fixes missing function error)
(defun jay/setup-org-pretty-table ()
  "Enable org-pretty-table-mode for Unicode border tables."
  (use-package org-pretty-table
    :ensure t
    :hook (org-mode . org-pretty-table-mode)))

(defun jay/lepisma-pretty-tables ()
  "Alias for pretty table setup."
  (jay/setup-org-pretty-table))

;;;; 🚀 Optional: Enable All Lepisma Visual Enhancements
(defun jay/enable-all-lepisma-visuals ()
  "Enable all Lepisma-inspired Org visuals."
  (interactive)
  (jay/set-etbook-font-and-indent-fix)
  (jay/lepisma-org-ui-tweaks)
  (jay/lepisma-disable-hl-line)
  (jay/lepisma-pretty-tables))

;;;; 📋 Unified Org Visual Feature Selector
(defun jay/select-any-org-visual-feature ()
  "Interactively choose from all visual Org mode enhancements to enable."
  (interactive)
  (let* ((features
          '(
            ;; Tufte / core enhancements
            ("[Typography] Variable pitch font (ET Book)" . jay/set-variable-pitch-etbook)
            ("[Typography] Enable variable-pitch-mode in Org buffers" . jay/enable-variable-pitch-in-org)
            ("[Spacing] Line spacing for readability" . jay/set-line-spacing)
            ("[Spacing] Top padding using header line" . jay/add-top-padding)
            ("[Spacing] Side margins in Org buffers" . jay/add-side-margins)
            ("[Spacing] Padding around entire frame" . jay/add-frame-padding)
            ("[Layout] Distraction-free writing (writeroom-mode)" . jay/setup-writeroom)
            ("[Org UI] Riced Org UI tweaks (bullets, ellipsis, markers)" . jay/ricing-org-ui)
            ("[Org UI] Disable hl-line-mode in Org buffers" . jay/disable-hl-line-in-org)
            ("[Org UI] Pretty Unicode Org tables" . jay/setup-org-pretty-table)
            ("[Org UI] Inline image and LaTeX preview" . jay/enable-inline-previews)
            ("[Org UI] Enable org-bullets (clean list symbols)" . jay/setup-org-bullets)
            ;; Lepisma extras
            ("[Lepisma] Set ET Book font and fix indentation" . jay/set-etbook-font-and-indent-fix)
            ("[Lepisma] Org UI tweaks (bullets, ellipsis, italics)" . jay/lepisma-org-ui-tweaks)
            ("[Lepisma] Disable hl-line-mode in Org buffers" . jay/lepisma-disable-hl-line)
            ("[Lepisma] Enable org-pretty-table-mode (Unicode tables)" . jay/lepisma-pretty-tables)))
         (choice (completing-read "Choose Org visual enhancement to enable: " (mapcar #'car features)))
         (fn (cdr (assoc choice features))))
    (when fn
      (funcall fn)
      (message "Enabled: %s" choice))))

;;;; 🎨 Preview Mode: Cycle through presets
(defvar jay/org-visual-style-presets
  '(("Minimalist" . (jay/set-variable-pitch-etbook jay/enable-variable-pitch-in-org jay/ricing-org-ui))
    ("Tufte Inspired" . (jay/set-etbook-font-and-indent-fix jay/lepisma-org-ui-tweaks jay/lepisma-disable-hl-line))
    ("Distraction Free" . (jay/setup-writeroom jay/set-line-spacing jay/add-side-margins))
    ("All Enhancements" . (jay/enable-all-lepisma-visuals jay/enable-all-org-visual-style)))
  "Named visual style presets for Org Mode.")

(defun jay/preview-org-visual-preset ()
  "Interactively select and apply a visual style preset."
  (interactive)
  (let* ((names (mapcar #'car jay/org-visual-style-presets))
         (choice (completing-read "Preview Org preset: " names))
         (fns (cdr (assoc choice jay/org-visual-style-presets))))
    (when fns
      (mapc (lambda (f)
              (when (functionp f)
                (funcall f)))
            fns)
      (message "Applied Org visual preset: %s" choice))))

;;;; 🌟 Define a Minor Mode
(define-minor-mode org-visual-style-mode
  "Toggle Jay’s Org visual ricing mode. Applies all visual enhancements."
  :global t
  :init-value nil
  :lighter " ✨OrgVisual"
  (if org-visual-style-mode
      (progn
        (jay/set-variable-pitch-etbook)
        (jay/enable-variable-pitch-in-org)
        (jay/set-line-spacing)
        (jay/add-top-padding)
        (jay/add-side-margins)
        (jay/add-frame-padding)
        (jay/setup-writeroom)
        (jay/ricing-org-ui)
        (jay/disable-hl-line-in-org)
        (jay/setup-org-pretty-table)
        (jay/enable-inline-previews)
        (jay/setup-org-bullets)
        (jay/set-etbook-font-and-indent-fix)
        (jay/lepisma-org-ui-tweaks)
        (jay/lepisma-disable-hl-line)
        (jay/lepisma-pretty-tables)
        (message "Org visual style enabled."))
    (message "Org visual style disabled. (Note: This does not undo changes.)")))

(provide 'org-visual-style)

;;; org-visual-style.el ends here
