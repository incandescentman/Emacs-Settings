;;; org-roam-config.el --- Legacy compatibility shim for Org-roam setup -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is deprecated as an implementation file.
;; The active Org-roam implementation now lives in:
;;   - jay-org-roam-suite/jay-org-roam-core.el
;;   - jay-org-roam-suite/jay-org-roam-profiles.el
;;   - jay-org-roam-suite/jay-editor-extras.el
;;
;; The legacy full implementation has been archived at:
;;   archive/org-roam-config.el_legacy_2026-02-24
;;
;; Loading this file is still supported for backward compatibility.

;;; Code:

(defvar org-roam-config--deprecation-warning-shown nil
  "Non-nil after deprecation warning has been shown once per session.")

(let* ((root (file-name-directory (or load-file-name buffer-file-name)))
       (suite (expand-file-name "jay-org-roam-suite" root)))
  (when (file-directory-p suite)
    (add-to-list 'load-path suite))
  (unless org-roam-config--deprecation-warning-shown
    (setq org-roam-config--deprecation-warning-shown t)
    (message "org-roam-config.el is deprecated; loading jay-org-roam-suite instead.")))

;; Keep behavior backward compatible when older entry points still load this file.
(require 'xdg nil t)

(condition-case err
    (progn
      (require 'jay-org-roam-core)
      (require 'jay-editor-extras nil t))
  (error
   (message "org-roam-config.el shim could not load suite fully: %s"
            (error-message-string err))))

(provide 'org-roam-config)
;;; org-roam-config.el ends here
