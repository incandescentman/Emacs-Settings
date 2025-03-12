;;; org-visual-style.el --- Tufte-style visual enhancements for Org Mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides modular, opt-in enhancements to Emacs Org Mode for a
;; minimal, elegant, and distraction-free writing environment inspired by
;; Edward Tufte’s book design style.

;;; Code:
;;; (use-package spacious-padding :custom (line-spacing 3) (spacious-padding-mode 1))


;;;; 📚 ET Book / EtBembo Font for Prose
(with-eval-after-load 'ox
  (require 'ox-extra)
  (ox-extra-activate '(ignore-headlines)))

(defun jay/set-variable-pitch-etbook ()
  "Use the ET Book (EtBembo) font for variable-pitch text."
  (set-face-attribute 'variable-pitch nil :family "EtBembo" :height 180))

(defun jay/enable-variable-pitch-in-org ()
  "Enable variable-pitch mode in Org buffers (for proportional font)."
  (add-hook 'org-mode-hook #'variable-pitch-mode))

;;;; 🎨 Spacing: Line height, margins, padding

(defun jay/set-line-spacing ()
  "Set subtle line spacing between lines for readability."
  (setq-default line-spacing 0.1))

(defun jay/add-top-padding ()
  "Add top padding by displaying an empty header line."
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :height 200))

(defun jay/add-side-margins ()
  "Add left and right margins in Org mode buffers."
  (defun jay/org-side-margins ()
    (setq left-margin-width 2
          right-margin-width 2)
    (set-window-buffer nil (current-buffer)))
  (add-hook 'org-mode-hook #'jay/org-side-margins))

(defun jay/add-frame-padding ()
  "Add internal padding around the entire Emacs frame."
  (add-to-list 'default-frame-alist '(internal-border-width . 20)))

;;;; 🧘 Centered Writing: Writeroom Mode

(defun jay/setup-writeroom ()
  "Enable writeroom mode for distraction-free centered text."
  (use-package writeroom-mode
    :hook (org-mode . writeroom-mode)
    :custom
    (writeroom-width 100)
    (writeroom-mode-line t)))

;;;; 🎭 Org Mode Face & UI Enhancements

(defun jay/ricing-org-ui ()
  "Customize Org mode faces and visuals for minimal, elegant writing."
  (setq org-startup-indented t                          ; Indent sections
        org-bullets-bullet-list '(" ")                  ; Blank bullets
        org-ellipsis "  "                              ; Folding symbol
        org-pretty-entities t                           ; Show symbols (π → π)
        org-hide-emphasis-markers t                     ; Hide /italic/, *bold*
        org-agenda-block-separator ""                   ; Remove agenda divider
        org-fontify-whole-heading-line t                ; Style full heading
        org-fontify-done-headline t                     ; Done headings styled
        org-fontify-quote-and-verse-blocks t))          ; Style quote blocks

;;;; 🧼 Hide Line Highlighting (hl-line-mode)

(defun jay/disable-hl-line-in-org ()
  "Disable line highlighting in Org mode for a cleaner look."
  (add-hook 'org-mode-hook (lambda () (hl-line-mode -1))))

;;;; 📊 Pretty Tables

(defun jay/setup-org-pretty-table ()
  "Use unicode lines for prettier Org mode tables."
  (use-package org-pretty-table
    :hook (org-mode . org-pretty-table-mode)))

;;;; 🖼️ Inline Images and LaTeX Preview (built-in)

(defun jay/enable-inline-previews ()
  "Enable image and LaTeX previews in Org mode."
  (setq org-startup-with-inline-images t)
  (add-hook 'org-mode-hook #'org-display-inline-images)
  (add-hook 'org-mode-hook #'org-latex-preview))

;;;; 🔘 Org Bullets

(defun jay/setup-org-bullets ()
  "Enable org-bullets for custom list symbols."
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)))

;;; 🚀 Optional: Enable All at Once

(defun jay/enable-all-org-visual-style ()
  "Enable all visual enhancements for Org mode."
  (interactive)
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
  (jay/setup-org-bullets))


(defun jay/select-org-visual-feature ()
  "Interactively choose a visual Org mode feature to enable."
  (interactive)
  (let* ((features '(("Variable pitch font (ET Book)" . jay/set-variable-pitch-etbook)
                     ("Enable variable-pitch-mode in Org buffers" . jay/enable-variable-pitch-in-org)
                     ("Line spacing for readability" . jay/set-line-spacing)
                     ("Top padding using header line" . jay/add-top-padding)
                     ("Side margins in Org buffers" . jay/add-side-margins)
                     ("Padding around entire frame" . jay/add-frame-padding)
                     ("Distraction-free writing (writeroom-mode)" . jay/setup-writeroom)
                     ("Riced Org UI tweaks (bullets, ellipsis, markers)" . jay/ricing-org-ui)
                     ("Disable hl-line-mode in Org buffers" . jay/disable-hl-line-in-org)
                     ("Pretty Unicode Org tables" . jay/setup-org-pretty-table)
                     ("Inline image and LaTeX preview" . jay/enable-inline-previews)
                     ("Enable org-bullets (clean list symbols)" . jay/setup-org-bullets)))
         (choice (completing-read "Choose visual enhancement to enable: " (mapcar #'car features)))
         (fn (cdr (assoc choice features))))
    (when fn
      (funcall fn)
      (message "Enabled: %s" choice))))



(provide 'org-visual-style)

;;; org-visual-style.el ends here
