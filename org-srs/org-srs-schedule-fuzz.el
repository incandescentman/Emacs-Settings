;;; org-srs-schedule-fuzz.el --- Fuzzing mechanism for due dates -*- lexical-binding: t -*-

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

;; This package is used to fuzz the due dates of review items to prevent
;; multiple items from expiring on the same day, thereby reducing
;; cognitive load and enhancing long-term retention of information.

;;; Code:

(require 'cl-lib)
(cl-eval-when (:compile-toplevel :load-toplevel :execute) (cl-float-limits))
(require 'custom)

(require 'org-srs-property)
(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-time)
(require 'org-srs-schedule)

(defgroup org-srs-schedule-fuzz nil
  "Fuzzing review intervals to balance due date distribution."
  :group 'org-srs-schedule
  :prefix "org-srs-schedule-fuzz-")

(org-srs-property-defcustom org-srs-schedule-fuzz-ranges '(((2.5 :day) . 0.15)
                                                           ((7.0 :day) . 0.1)
                                                           ((20.0 :day) . 0.05))
  "Fuzzing factors corresponding to different interval ranges."
  :group 'org-srs-schedule-fuzz
  :type 'sexp)

(org-srs-property-defcustom org-srs-schedule-fuzz-unit '(1 :day)
  "Smallest unit of time change when fuzzing due timestamps."
  :group 'org-srs-schedule-fuzz
  :type 'sexp)

(cl-defun org-srs-schedule-fuzz-interval-default (interval &optional (unit (org-srs-schedule-fuzz-unit)))
  "Return INTERVAL increased by UNIT when positive, otherwise return zero."
  (let ((interval-seconds (org-srs-time-desc-seconds interval))
        (unit-seconds (org-srs-time-desc-seconds unit)))
    (list (if (cl-plusp interval-seconds) (+ interval-seconds unit-seconds) 0.0) :sec)))

(org-srs-property-defcustom org-srs-schedule-fuzz-interval #'org-srs-schedule-fuzz-interval-default
  "Function used to calculate the interval for fuzzing from the base interval."
  :group 'org-srs-schedule-fuzz
  :type 'function)

(cl-defun org-srs-schedule-fuzz-interval-round (interval &optional (unit (org-srs-schedule-fuzz-unit)))
  "Round INTERVAL to the nearest multiple of UNIT."
  (let ((interval-seconds (org-srs-time-desc-seconds interval))
        (unit-seconds (org-srs-time-desc-seconds unit)))
    (list (* (round interval-seconds unit-seconds) unit-seconds) :sec)))

(cl-defun org-srs-schedule-fuzz-calculate-interval (time-scheduled time-review)
  "Calculate a random fuzzing interval based on TIME-SCHEDULED and TIME-REVIEW."
  (cl-flet ((cl-clamp (number min max) (if (< number min) min (if (> number max) max number))))
    (let ((interval (org-srs-time-desc-seconds
                     (funcall (org-srs-schedule-fuzz-interval)
                              (list (cl-loop with scheduled-interval-seconds = (org-srs-time-difference time-scheduled time-review)
                                             for ((interval . factor) (next-interval . nil)) on (org-srs-schedule-fuzz-ranges)
                                             for interval-seconds = (org-srs-time-desc-seconds interval)
                                             for next-interval-seconds = (if next-interval (org-srs-time-desc-seconds next-interval) cl-most-positive-float)
                                             sum (* factor (- (cl-clamp scheduled-interval-seconds interval-seconds next-interval-seconds) interval-seconds)))
                                    :sec)))))
      (list (if (cl-plusp interval) (- (cl-random (* 2.0 interval)) interval) 0.0) :sec))))

(defun org-srs-schedule-fuzz-due-timestamp ()
  "Return a fuzzed due timestamp for the current review item."
  (save-excursion
    (let ((timestamp-scheduled (org-srs-table-field 'timestamp))
          (timestamp-review (progn (forward-line -1) (org-srs-table-field 'timestamp))))
      (let ((time-scheduled (org-srs-timestamp-time timestamp-scheduled))
            (time-review (org-srs-timestamp-time timestamp-review)))
        (apply #'org-srs-timestamp+ timestamp-scheduled
               (org-srs-schedule-fuzz-interval-round (org-srs-schedule-fuzz-calculate-interval time-scheduled time-review)))))))

(defun org-srs-schedule-fuzz-update-due-timestamp ()
  "Update the due timestamp of the current review item with fuzzing applied."
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (org-srs-item-with-current org-srs-review-item
          (org-srs-table-goto-starred-line)
          (org-srs-property-let (org-srs-schedule-fuzz-ranges org-srs-schedule-fuzz-unit)
            (org-srs-table-with-temp-buffer
              (setf (org-srs-table-field 'timestamp) (org-srs-schedule-fuzz-due-timestamp))))))
    (setf (org-srs-table-field 'timestamp) (org-srs-schedule-fuzz-due-timestamp))))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-schedule-fuzz-update-due-timestamp 60)

(provide 'org-srs-schedule-fuzz)
;;; org-srs-schedule-fuzz.el ends here
