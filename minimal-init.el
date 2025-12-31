;;; minimal-init.el --- Emergency minimal Emacs config -*- lexical-binding: t -*-
;;
;; Usage: emacs -Q -l ~/emacs/emacs-settings/minimal-init.el
;;
;; This provides a working Emacs with basic org-mode when Spacemacs is broken.
;; No packages, no complexity - just enough to edit files.

;;; Basic Settings
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Minimal Emacs - Spacemacs bypassed\n;; Use C-x C-f to open files\n\n")

;; Appearance
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-face-attribute 'default nil :height 160)

;; Behavior
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; macOS keybindings
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-w") 'delete-frame)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal))

;; Org-mode (built-in)
(require 'org)
(setq org-startup-indented t)
(setq org-startup-folded 'content)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Theme (built-in)
(load-theme 'wombat t)

;; Show what config is loaded
(message "Minimal init loaded. Spacemacs bypassed.")

;;; minimal-init.el ends here
