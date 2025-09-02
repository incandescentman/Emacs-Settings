;;; org-srs-schedule-bury.el --- Burying mechanism for review items -*- lexical-binding: t -*-

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

;; This package is used for burying due items, allowing some items that
;; should be due, such as sibling items, to be postponed for review,
;; thereby avoiding an impact on the accuracy of ratings.

;;; Code:

(require 'cl-lib)
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-item)
(require 'org-srs-time)
(require 'org-srs-schedule)

(defgroup org-srs-schedule-bury nil
  "Burying mechanism to prevent relevant items from being shown on the same day."
  :group 'org-srs-schedule
  :prefix "org-srs-schedule-bury-")

(org-srs-property-defcustom org-srs-schedule-bury-sibling-items-p nil
  "Non-nil means to bury sibling items to postpone their reviews."
  :group 'org-srs-schedule-bury
  :type 'boolean)

(org-srs-property-defcustom org-srs-schedule-bury-interval '(1 :day)
  "Interval after which reviews of buried items are postponed."
  :group 'org-srs-schedule-bury
  :type 'sexp)

(cl-defun org-srs-schedule-bury-due-timestamp (&optional (interval (org-srs-schedule-bury-interval)))
  "Add INTERVAL to now if the current timestamp is due today."
  (let ((timestamp (org-srs-table-field 'timestamp)))
    (when (org-srs-timestamp< timestamp (org-srs-timestamp (org-srs-time-tomorrow)))
      (apply #'org-srs-timestamp+ (org-srs-timestamp-now) interval))))

(cl-defun org-srs-schedule-bury-update-due-timestamp (&optional (interval (org-srs-schedule-bury-interval)))
  "Postpone sibling review items by INTERVAL."
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (org-srs-item-with-current org-srs-review-item
          (when (org-srs-schedule-bury-sibling-items-p)
            (cl-loop with current-item = (cl-multiple-value-list (org-srs-item-at-point))
                     for item in (org-srs-query-region #'always (org-entry-beginning-position) (org-entry-end-position))
                     unless (equal item current-item)
                     do (org-srs-item-with-current item
                          (org-srs-table-goto-starred-line)
                          (when-let ((timestamp (org-srs-schedule-bury-due-timestamp interval)))
                            (setf (org-srs-item-due-timestamp) timestamp)))))))
    (when-let ((timestamp (org-srs-schedule-bury-due-timestamp interval)))
      (setf (org-srs-table-field 'timestamp) timestamp))))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-schedule-bury-update-due-timestamp 40)

(provide 'org-srs-schedule-bury)
;;; org-srs-schedule-bury.el ends here
