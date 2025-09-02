;;; org-srs-schedule-offset.el --- Due date offsetting mechanism -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Bohong Huang

;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (org "9.7") (fsrs "6.0"))
;; URL: https://github.com/bohonghuang/org-srs
;; Keywords: outlines

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adjusts scheduled timestamps when reviews occur outside
;; planned timeframes. It ensures learning-ahead items are scheduled
;; based on their original planned timestamps, preventing them from being
;; prioritized over due items that should be reviewed first.

;;; Code:

(require 'cl-lib)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-table)
(require 'org-srs-time)
(require 'org-srs-schedule)

(defgroup org-srs-schedule-offset nil
  "Offsetting due timestamps to handle undue reviews, such as learning ahead or overdue reviews."
  :group 'org-srs-schedule
  :prefix "org-srs-schedule-offset-")

(org-srs-property-defcustom org-srs-schedule-offset-learn-ahead-time-p #'org-srs-time-today-p
  "Whether to offset the scheduled time by the time difference of learning ahead."
  :group 'org-srs-schedule-offset
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "If scheduled for today" org-srs-time-today-p)))

(defun org-srs-schedule-offset-learn-ahead-due-timestamp (timestamp-due)
  "Postpone the review item by the learning ahead time based on TIMESTAMP-DUE."
  (save-excursion
    (let ((timestamp-scheduled (org-srs-table-field 'timestamp))
          (timestamp-review (progn (forward-line -1) (org-srs-table-field 'timestamp))))
      (or (let* ((difference (max (org-srs-timestamp-difference timestamp-due timestamp-review) 0))
                 (timestamp-scheduled (org-srs-timestamp+ timestamp-scheduled difference :sec)))
            (when (let ((offset-time-p (org-srs-schedule-offset-learn-ahead-time-p)))
                    (cl-etypecase offset-time-p
                      (function (funcall offset-time-p (org-srs-timestamp-time timestamp-scheduled)))
                      (boolean offset-time-p)))
              timestamp-scheduled))
          timestamp-scheduled))))

(defvar org-srs-schedule-offset-due-timestamp)

(cl-defun org-srs-schedule-offset-due-timestamp (&optional (buffer (current-buffer)))
  "Get the value of `org-srs-schedule-offset-due-timestamp' in BUFFER."
  (cl-assert (buffer-local-boundp 'org-srs-schedule-offset-due-timestamp buffer))
  (buffer-local-value 'org-srs-schedule-offset-due-timestamp buffer))

(cl-defmethod (setf org-srs-schedule-offset-due-timestamp) (timestamp &optional (buffer (current-buffer)))
  "Set the value of `org-srs-schedule-offset-due-timestamp' in BUFFER to TIMESTAMP."
  (cl-assert (xor (buffer-local-boundp 'org-srs-schedule-offset-due-timestamp buffer) timestamp))
  (if timestamp
      (setf (buffer-local-value 'org-srs-schedule-offset-due-timestamp buffer) timestamp)
    (kill-local-variable 'org-srs-schedule-offset-due-timestamp)))

(defun org-srs-schedule-offset-save-due-timestamp ()
  "Save the current due timestamp before it gets updated by rating."
  (when (bound-and-true-p org-srs-review-rating)
    (org-srs-item-with-current org-srs-review-item
      (org-srs-table-goto-starred-line)
      (setf (org-srs-schedule-offset-due-timestamp) (org-srs-table-field 'timestamp)))))

(add-hook 'org-srs-review-before-rate-hook #'org-srs-schedule-offset-save-due-timestamp)

(defun org-srs-schedule-offset-update-due-timestamp (&optional timestamp-due)
  "Offset the due timestamp after reviewing an item optionally using TIMESTAMP-DUE."
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (let ((timestamp-due (or timestamp-due (cl-shiftf (org-srs-schedule-offset-due-timestamp) nil))))
          (org-srs-item-with-current org-srs-review-item
            (org-srs-table-goto-starred-line)
            (org-srs-property-let (org-srs-schedule-offset-learn-ahead-time-p)
              (org-srs-table-with-temp-buffer
                (setf (org-srs-table-field 'timestamp) (org-srs-schedule-offset-learn-ahead-due-timestamp timestamp-due)))))))
    (setf (org-srs-table-field 'timestamp) (org-srs-schedule-offset-learn-ahead-due-timestamp timestamp-due))))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-schedule-offset-update-due-timestamp 70)

(provide 'org-srs-schedule-offset)
;;; org-srs-schedule-offset.el ends here
