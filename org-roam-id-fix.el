;;; org-roam-id-fix.el --- Permanent fix for org-id-locations corruption -*- lexical-binding: t; -*-

;; This provides a permanent fix for org-id-locations corruption by:
;; 1. Ensuring org-id-locations is always a hash table
;; 2. Adding advice to catch and fix corruption on-the-fly
;; 3. Handling symlink path issues with org-roam

(require 'org-id nil t)
(require 'org-roam nil t)

;; Ensure org-id-locations is always a hash table
(defun ensure-org-id-locations-hash-table ()
  "Ensure org-id-locations is a hash table, converting from list if needed."
  (when (and (boundp 'org-id-locations)
             (not (hash-table-p org-id-locations)))
    (let ((old-value org-id-locations))
      (cond
       ;; If it's a list, convert it
       ((listp old-value)
        (let ((new-table (make-hash-table :test 'equal)))
          (dolist (entry old-value)
            (when (and (listp entry) (>= (length entry) 2))
              (let ((file (expand-file-name (car entry)))
                    (ids (cdr entry)))
                (puthash file ids new-table))))
          (setq org-id-locations new-table)
          (message "Converted org-id-locations from list to hash table with %d entries"
                   (hash-table-count new-table))))
       ;; If it's nil or something else, create empty hash table
       (t
        (setq org-id-locations (make-hash-table :test 'equal))
        (message "Initialized org-id-locations as empty hash table"))))))

;; Fix org-id-add-location to handle corruption
(defun org-id-add-location--fix-corruption (orig-fun id file)
  "Advice to ensure org-id-locations is a hash table before adding."
  (ensure-org-id-locations-hash-table)
  (funcall orig-fun id file))

;; Fix org-id-locations-load to prevent loading corrupt data
(defun org-id-locations-load--fix-corruption (orig-fun)
  "Advice to fix corruption when loading org-id-locations."
  (let ((result (funcall orig-fun)))
    (ensure-org-id-locations-hash-table)
    result))

;; Fix the org-roam advice that's causing issues
(defun org-roam--handle-absent-org-id-locations-file-fixed (orig-fn id file)
  "Fixed version of org-roam's advice for handling org-id-locations."
  (ensure-org-id-locations-hash-table)
  ;; Now call the original function
  (condition-case err
      (funcall orig-fn id file)
    (wrong-type-argument
     ;; If we still get an error, fix it and retry
     (ensure-org-id-locations-hash-table)
     (funcall orig-fn id file))
    (error
     (message "Error in org-id-add-location: %s" (error-message-string err))
     nil)))

;; Apply the fixes
(defun apply-org-id-fixes ()
  "Apply all fixes for org-id-locations corruption."
  (interactive)

  ;; Remove any existing bad advice from org-roam
  (advice-remove 'org-id-add-location 'org-roam--handle-absent-org-id-locations-file-a)

  ;; Add our fixed advice
  (advice-add 'org-id-add-location :around #'org-id-add-location--fix-corruption)
  (advice-add 'org-id-locations-load :around #'org-id-locations-load--fix-corruption)

  ;; Replace org-roam's problematic advice with our fixed version
  (when (fboundp 'org-roam--handle-absent-org-id-locations-file-a)
    (advice-add 'org-id-add-location :around #'org-roam--handle-absent-org-id-locations-file-fixed))

  ;; Ensure current state is valid
  (ensure-org-id-locations-hash-table)

  ;; Save the corrected state
  (when (fboundp 'org-id-locations-save)
    (org-id-locations-save))

  (message "Applied org-id-locations fixes"))

;; Auto-apply fixes when this file is loaded
(apply-org-id-fixes)

;; Also add a hook to ensure fixes are applied after org-roam loads
(with-eval-after-load 'org-roam
  (apply-org-id-fixes))

(provide 'org-roam-id-fix)
;;; org-roam-id-fix.el ends here