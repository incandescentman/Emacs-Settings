;;; timeline-cleanup.el --- Cleanup and cancel helpers for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatic cleanup of empty diary stubs and interactive cancel commands.

;;; Code:

(require 'timeline-core)
(require 'timeline-calendar)
(require 'timeline-diary)

(defun my-timeline--cleanup-empty-entries ()
  "Remove empty date stubs (date lines with only a blank bullet) from the diary file before saving."
  (when (and (not my-timeline--suspend-cleanup)
             buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name diary-file)))
    (save-excursion
      (goto-char (point-min))
      (let ((heading-regex "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$\\|^## \\|^# "))
        (while (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" nil t)
          (let ((date-start (match-beginning 0))
                (current-date (match-string 0))
                (has-content nil)
                (block-end nil))
            (save-excursion
              (goto-char (match-end 0))
              (forward-line 1)
              (while (and (not (eobp)) (not (looking-at heading-regex)))
                (when (looking-at "^  -\\(.*\\)$")
                  (let ((content (string-trim (match-string 1))))
                    (unless (string-empty-p content)
                      (setq has-content t))))
                (forward-line 1))
              (setq block-end (point)))
            (unless (or has-content
                        (and my-timeline--cleanup-skip-date
                             (string= current-date my-timeline--cleanup-skip-date)))
              (delete-region date-start block-end)
              (goto-char date-start))))))))

(add-hook 'before-save-hook #'my-timeline--cleanup-empty-entries)

(defun my-calendar-cancel-current-entry ()
  "Delete the current date block if empty (or ask if non-empty), then return to Calendar.
Briefly highlights the candidate block before deletion."
  (interactive)
  (unless (and buffer-file-name
               (string= (expand-file-name buffer-file-name)
                        (expand-file-name diary-file)))
    (user-error "Not in the diary file"))
  (save-excursion
    (let* ((date-line-regex "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$")
           (heading-regex "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$\\|^## \\|^# ")
           (date-pos (progn
                       (beginning-of-line)
                       (if (looking-at date-line-regex)
                           (point)
                         (re-search-backward date-line-regex nil t)))))
      (unless date-pos
        (user-error "Not on a date entry"))
      (let* ((block-start date-pos)
             (block-end nil)
             (has-nonblank-bullet nil))
        (goto-char (line-end-position))
        (forward-line 1)
        (while (and (not (eobp)) (not (looking-at heading-regex)))
          (when (looking-at "^  -\\(.*\\)$")
            (let ((content (string-trim (match-string 1))))
              (unless (string-empty-p content)
                (setq has-nonblank-bullet t))))
          (forward-line 1))
        (setq block-end (point))
        (let ((ov (make-overlay block-start block-end)))
          (overlay-put ov 'face '(:background "lemonchiffon"))
          (run-at-time 0.5 nil (lambda (o) (when (overlayp o) (delete-overlay o))) ov))
        (if has-nonblank-bullet
            (when (yes-or-no-p "Date entry is not empty. Delete anyway? ")
              (delete-region block-start block-end)
              (message "Deleted non-empty date block.")
              (when (fboundp 'my-diary-return-to-calendar)
                (run-at-time 0.1 nil #'my-diary-return-to-calendar)))
          (delete-region block-start block-end)
          (message "Deleted empty date block.")
          (when (fboundp 'my-diary-return-to-calendar)
            (run-at-time 0.1 nil #'my-diary-return-to-calendar)))))))

(defun my-calendar--setup-cancel-shortcut ()
  "Bind C-c C-k to `my-calendar-cancel-current-entry` in the diary file."
  (when (and buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name diary-file)))
    (local-set-key (kbd "C-c C-k") #'my-calendar-cancel-current-entry)))

(add-hook 'markdown-mode-hook #'my-calendar--setup-cancel-shortcut)

(provide 'timeline-cleanup)

;;; timeline-cleanup.el ends here
