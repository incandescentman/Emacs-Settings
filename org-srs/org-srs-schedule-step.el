;;; org-srs-schedule-step.el --- Stepped (re)learning mechanism -*- lexical-binding: t -*-

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

;; This package replicates the stepped (re)learning feature in Anki,
;; allowing the (re)learning process to be divided into specified
;; intervals to enhance learning effectiveness.

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

(defgroup org-srs-schedule-step nil
  "Stepped learning mechanism to ensure retention before reviews."
  :group 'org-srs-schedule
  :prefix "org-srs-schedule-step-")

(org-srs-property-defcustom org-srs-schedule-step-learning-steps '((1 :minute) (10 :minute))
  "Number of learning repetitions, and the delay between them."
  :group 'org-srs-schedule-step
  :type 'sexp)

(org-srs-property-defcustom org-srs-schedule-step-relearning-steps '((10 :minute))
  "Number of relearning repetitions, and the delay between them."
  :group 'org-srs-schedule-step
  :type 'sexp)

(defun org-srs-schedule-step-list ()
  "Collect learning steps from the review item's history."
  (save-excursion
    (cl-loop with step-max = (1- most-positive-fixnum) and steps = nil
             initially (setf step 1)
             for field = (org-srs-table-field 'rating)
             for rating = (if (string-empty-p field) :again (read field))
             for step = (cl-ecase rating
                          (:easy step-max)
                          (:good (min (truncate (1+ step)) step-max))
                          (:hard (if (= (truncate step) 1) 1.5 step))
                          (:again (setf steps (list step)) 1))
             nconc (cl-shiftf steps nil)
             until (cl-minusp (forward-line -1))
             until (org-at-table-hline-p))))

(cl-defun org-srs-schedule-step-learned-p (&optional (learning-steps (org-srs-schedule-step-learning-steps)) (step-list (org-srs-schedule-step-list)))
  "Return non-nil if STEP-LIST of the item has completed all LEARNING-STEPS."
  (cl-some (apply-partially #'< (length learning-steps)) (cl-rest step-list)))

(defun org-srs-schedule-step-steps ()
  "Determine the current learning step and the corresponding step list."
  (let ((step-list (org-srs-schedule-step-list))
        (learning-steps (org-srs-schedule-step-learning-steps)))
    (cl-assert (cl-plusp (length step-list)))
    (cl-values
     (cl-first step-list)
     (if (cl-some (apply-partially #'< (length learning-steps)) (cl-rest step-list))
         (org-srs-schedule-step-relearning-steps)
       learning-steps))))

(defun org-srs-schedule-step-state ()
  "Determine the current learning or relearning state for the review item at point."
  (org-srs-property-let (org-srs-schedule-step-learning-steps org-srs-schedule-step-relearning-steps)
    (let ((learning-steps (org-srs-schedule-step-learning-steps))
          (relearning-steps (org-srs-schedule-step-relearning-steps)))
      (save-excursion
        (cl-loop while (org-at-table-p)
                 while (string-empty-p (org-srs-table-field 'rating))
                 until (cl-minusp (forward-line -1))
                 until (org-at-table-hline-p))
        (cl-multiple-value-bind (step steps) (org-srs-schedule-step-steps)
          (unless (> step (length steps))
            (if (eq steps learning-steps) :learning
              (cl-assert (eq steps relearning-steps))
              :relearning)))))))

(cl-defun org-srs-schedule-step-due-timestamp ()
  "Calculate the review item's new due timestamp based on its (re)learning step."
  (save-excursion
    (let ((timestamp-scheduled (org-srs-table-field 'timestamp))
          (timestamp-review (progn (forward-line -1) (org-srs-table-field 'timestamp))))
      (cl-multiple-value-bind (step steps) (org-srs-schedule-step-steps)
        (cl-assert (cl-plusp step))
        (cl-multiple-value-bind (step frac) (cl-truncate step)
          (let* ((index (1- step))
                 (index-next (if (< (abs frac) 1e-3) index (1+ index))))
            (unless (< index (length steps))
              (cl-return-from org-srs-schedule-step-due-timestamp timestamp-scheduled))
            (if (< index-next (length steps))
                (when-let ((step (nth index steps))
                           (step-next (nth index-next steps)))
                  (cl-assert (= (length step) (length step-next) 2))
                  (let ((step (cons (* (car step) frac) (cdr step)))
                        (step-next (cons (* (car step-next) (- 1.0 frac)) (cdr step))))
                    (apply #'org-srs-timestamp+ (apply #'org-srs-timestamp+ timestamp-review step) step-next)))
              (let* ((step-last (car (last steps)))
                     (step-next (cons (* 1.5 (car step-last)) (cdr step-last))))
                (org-srs-timestamp-min
                 (apply #'org-srs-timestamp+ timestamp-review step-next)
                 (org-srs-timestamp+ (apply #'org-srs-timestamp+ timestamp-review step-last) 1 :day))))))))))

(defun org-srs-schedule-step-update-due-timestamp ()
  "Update the due timestamp of the current review item based on its learning steps."
  (if (boundp 'org-srs-review-rating)
      (when (symbol-value 'org-srs-review-rating)
        (org-srs-item-with-current org-srs-review-item
          (org-srs-table-goto-starred-line)
          (org-srs-property-let (org-srs-schedule-step-learning-steps org-srs-schedule-step-relearning-steps)
            (org-srs-table-with-temp-buffer
              (setf (org-srs-table-field 'timestamp) (org-srs-schedule-step-due-timestamp))))))
    (setf (org-srs-table-field 'timestamp) (org-srs-schedule-step-due-timestamp))))


(add-hook 'org-srs-review-after-rate-hook #'org-srs-schedule-step-update-due-timestamp 50)

(provide 'org-srs-schedule-step)
;;; org-srs-schedule-step.el ends here
