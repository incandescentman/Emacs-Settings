;;; timeline-diary.el --- Diary commands for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Diary editing, insertion helpers, and minor mode for the timeline workflow.

;;; Code:

(require 'timeline-core)
(require 'timeline-calendar)

(defvar my-calendar-diary-history nil
  "Minibuffer history for `my-calendar-insert-diary-entry'.")

(defvar-local my-diary-mode--lighter nil)

(define-minor-mode my-diary-mode
  "Minor mode for the diary file, with context in the mode line."
  :lighter (:eval (or my-diary-mode--lighter " 📅"))
  (if my-diary-mode
      (let ((date (or my-diary--origin-date
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" nil t)
                          (list (string-to-number (match-string 1))
                                (string-to-number (match-string 2))
                                (string-to-number (match-string 3))))))))
        (setq my-diary-mode--lighter
              (if date
                  (format " 📅 %s" (my-calendar--describe-date date))
                " 📅")))
    (setq my-diary-mode--lighter nil)))

(defun my-diary--maybe-enable-mode ()
  "Enable `my-diary-mode` automatically for the diary file."
  (when (and (not my-timeline--suspend-cleanup)
             buffer-file-name
             (string= (expand-file-name buffer-file-name)
                      (expand-file-name diary-file)))
    (my-diary-mode 1)))

(add-hook 'markdown-mode-hook #'my-diary--maybe-enable-mode)

(defun my-calendar--diary-insert-entry (date lines)
  "Insert diary entry for DATE (MONTH DAY YEAR) using LINES."
  (let* ((month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (date-line (my-calendar--diary-format-date month day year))
         (buffer (find-file-noselect diary-file)))
    (with-current-buffer buffer
      (save-excursion
        (pcase-let ((`(,month-start ,month-end) (my-calendar--diary-month-region month year)))
          (save-restriction
            (narrow-to-region month-start month-end)
            (goto-char (point-min))
            (let ((insert-point nil)
                  (duplicate nil))
              (while (and (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" nil t)
                          (not insert-point))
                (let ((existing-day (string-to-number (match-string 2))))
                  (cond
                   ((= existing-day day)
                    (setq duplicate t)
                    (forward-line 1)
                    (while (and (not (eobp))
                                (looking-at "  - "))
                      (forward-line 1))
                    (unless (or (eobp) (looking-at "^\\s-*$"))
                      (insert "\n"))
                    (setq insert-point (point)))
                   ((> existing-day day)
                    (setq insert-point (match-beginning 0))))))
              (unless insert-point
                (setq insert-point (point-max)))
              (goto-char insert-point)
              (unless duplicate
                (my-calendar--ensure-blank-line-before)
                (insert date-line "\n"))
              (dolist (line lines)
                (insert "  - " line "\n"))
              (my-calendar--ensure-blank-line-after)))
          (widen)))
      (save-buffer))))

(defun my-calendar--insert-diary-entry (date stay-in-diary initial)
  "Insert a diary entry for DATE, seeding the minibuffer with INITIAL.
DATE defaults to the calendar date at point; STAY-IN-DIARY matches
the interactive prefix argument behaviour from the public commands."
  (let* ((stay-in-diary (not (null stay-in-diary)))
         (date (or date (calendar-cursor-to-date t)))
         (month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (prompt (format "Entry for %s: "
                         (my-calendar--describe-date date)))
         (text (read-from-minibuffer prompt initial nil nil 'my-calendar-diary-history)))
    (let ((lines (my-calendar--diary-normalize-lines text)))
      (when (null lines)
        (user-error "Diary entry cannot be empty"))
      (let ((my-timeline--suspend-cleanup t)
            (my-timeline--cleanup-skip-date (my-calendar--diary-format-date month day year)))
        (my-calendar--diary-insert-entry date lines))
      (my-calendar-jump-to-diary-entry date stay-in-diary)
      (message "Added diary entry for %s"
               (format "%d/%d/%d" month day year)))))

(defun my-calendar-insert-diary-entry (&optional date stay-in-diary)
  "Prompt for a diary entry and insert it for DATE with an empty minibuffer."
  (interactive (list nil current-prefix-arg))
  (my-calendar--insert-diary-entry date stay-in-diary ""))

(defun my-calendar-insert-diary-entry-and-autopopulate (&optional date stay-in-diary)
  "Prompt for a diary entry using the most recent history entry as default text."
  (interactive (list nil current-prefix-arg))
  (my-calendar--insert-diary-entry
   date stay-in-diary (or (car my-calendar-diary-history) "")))

(defun my-calendar-edit-diary-entry ()
  "Open the diary entry for the date at point and leave focus in the diary buffer.
After jumping, move point to the end of the last bullet item for that date."
  (interactive)
  (let* ((date (calendar-cursor-to-date t))
         (date-string (my-calendar--diary-format-date (nth 0 date)
                                                      (nth 1 date)
                                                      (nth 2 date))))
    (unless (my-calendar--diary-entry-exists-p date)
      (let ((my-timeline--suspend-cleanup t)
            (my-timeline--cleanup-skip-date date-string))
        (my-calendar--diary-insert-entry date '(""))))
    (let ((entry-pos (my-calendar-jump-to-diary-entry date t)))
      (unless entry-pos
        (user-error "Could not locate diary entry for %s"
                    (my-calendar--describe-date date)))
      (goto-char entry-pos)
      (let ((date-line-end (line-end-position))
            (last-bullet-pos nil))
        (forward-line 1)
        ;; Skip blank lines immediately following the heading.
        (while (and (not (eobp)) (looking-at "^\\s-*$"))
          (forward-line 1))
        ;; Track the end of each bullet line.
        (while (looking-at "^  - ")
          (setq last-bullet-pos (line-end-position))
          (forward-line 1)
          ;; Allow optional blank separators inside the block without losing the last bullet.
          (while (and (not (eobp)) (looking-at "^\\s-*$"))
            (forward-line 1)))
        (goto-char (or last-bullet-pos date-line-end))))))

(provide 'timeline-diary)

;;; timeline-diary.el ends here
