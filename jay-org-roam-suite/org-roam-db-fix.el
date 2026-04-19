;;; org-roam-db-fix.el --- Fix org-roam database connection issues -*- lexical-binding: t; -*-

;; This provides fixes for org-roam database connection errors

(defun org-roam-db-clear-and-rebuild ()
  "Clear org-roam database and rebuild from scratch."
  (interactive)
  (when (and (boundp 'org-roam-db-location)
             (file-exists-p org-roam-db-location))
    (message "Backing up existing database...")
    (copy-file org-roam-db-location
               (concat org-roam-db-location ".backup-"
                       (format-time-string "%Y%m%d-%H%M%S"))
               t))

  ;; Close existing connection if any
  (when (and (boundp 'org-roam-db)
             (emacsql-live-p org-roam-db))
    (emacsql-close org-roam-db))

  ;; Delete the database file
  (when (and (boundp 'org-roam-db-location)
             (file-exists-p org-roam-db-location))
    (delete-file org-roam-db-location))

  ;; Reinitialize
  (when (fboundp 'org-roam-db-sync)
    (org-roam-db-sync 'force)
    (message "Database rebuilt successfully")))

(defun org-roam-db-diagnose ()
  "Diagnose org-roam database issues."
  (interactive)
  (let ((msgs '()))
    ;; Check if database location is set
    (if (boundp 'org-roam-db-location)
        (push (format "DB location: %s" org-roam-db-location) msgs)
      (push "ERROR: org-roam-db-location not set" msgs))

    ;; Check if database file exists
    (if (and (boundp 'org-roam-db-location)
             (file-exists-p org-roam-db-location))
        (push (format "DB file exists: %s bytes"
                      (file-attribute-size
                       (file-attributes org-roam-db-location))) msgs)
      (push "ERROR: Database file does not exist" msgs))

    ;; Check connection
    (if (and (boundp 'org-roam-db)
             org-roam-db
             (emacsql-live-p org-roam-db))
        (push "DB connection: LIVE" msgs)
      (push "ERROR: Database connection not live" msgs))

    ;; Check org-roam directory
    (if (and (boundp 'org-roam-directory)
             (file-exists-p org-roam-directory))
        (push (format "Roam directory: %s" org-roam-directory) msgs)
      (push "ERROR: org-roam-directory not accessible" msgs))

    (message (mapconcat 'identity (nreverse msgs) "\n"))))

(defun org-roam-db-safe-sync ()
  "Safely sync org-roam database with error handling."
  (interactive)
  (condition-case err
      (progn
        ;; Ensure database connection is alive
        (when (and (boundp 'org-roam-db)
                   (not (emacsql-live-p org-roam-db)))
          (message "Reconnecting to database...")
          (setq org-roam-db nil)
          (org-roam-db))

        ;; Now sync
        (org-roam-db-sync)
        (message "Database sync completed successfully"))
    (error
     (message "Database sync failed: %s" (error-message-string err))
     (when (yes-or-no-p "Database sync failed. Rebuild database? ")
       (org-roam-db-clear-and-rebuild)))))

;; Advice to make db-sync more robust
(defun org-roam-db-sync--robust (orig-fun &rest args)
  "Make org-roam-db-sync more robust against connection errors."
  (condition-case err
      (apply orig-fun args)
    (emacsql-error
     (message "Database error during sync: %s" (error-message-string err))
     ;; Try to recover
     (when (and (boundp 'org-roam-db)
                (not (emacsql-live-p org-roam-db)))
       (message "Attempting to reconnect...")
       (setq org-roam-db nil)
       (org-roam-db)
       ;; Retry once
       (condition-case err2
           (apply orig-fun args)
         (error
          (message "Retry failed: %s" (error-message-string err2))
          nil))))))

;; Apply the robust sync advice
(with-eval-after-load 'org-roam-db
  (advice-add 'org-roam-db-sync :around #'org-roam-db-sync--robust))

(provide 'org-roam-db-fix)
;;; org-roam-db-fix.el ends here