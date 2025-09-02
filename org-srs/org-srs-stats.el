;;; org-srs-stats.el --- Interface and common utilities for status statistics -*- lexical-binding: t -*-

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

;; This package serves as the interface and provides common utilities for
;; statistics to monitor learning status.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)
(require 'custom)

(require 'org)

(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-review-cache)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-item)
(require 'org-srs-time)

(defgroup org-srs-stats nil
  "Interface for status statistics."
  :group 'org-srs
  :prefix "org-srs-stats-")

(cl-defgeneric org-srs-stats-deep-copy (object)
  (:method
   (object)
   "Default method to return OBJECT as is."
   object)
  (:documentation "Make a deep copy of OBJECT."))

(cl-defmethod org-srs-stats-deep-copy ((null null))
  "Method to return nil (NULL) as is."
  null)

(cl-defmethod org-srs-stats-deep-copy ((cons cons))
  "Method to return a newly allocated CONS with its car and cdr deep copied."
  (cons (org-srs-stats-deep-copy (car cons)) (org-srs-stats-deep-copy (cdr cons))))

(cl-defmethod org-srs-stats-deep-copy ((vector vector))
  "Method to return a newly allocated VECTOR with all elements deep copied."
  (cl-map (cl-type-of vector) #'org-srs-stats-deep-copy vector))

(cl-defmethod org-srs-stats-deep-copy ((object cl-structure-object))
  "Method to return a newly allocated structure OBJECT with all slots deep copied."
  (cl-loop with new-object = (copy-sequence object)
           for slot in (mapcar #'cl--slot-descriptor-name (cl--class-slots (cl-find-class (cl-type-of object))))
           do (setf (eieio-oref new-object slot) (org-srs-stats-deep-copy (eieio-oref object slot)))
           finally (cl-return new-object)))

(cl-defun org-srs-stats-call-with-rating-simulator (thunk)
  "Call THUNK with a rating function in a simulated environment."
  (org-srs-property-let org-srs-algorithm
    (org-srs-property-let org-srs-schedule
      (org-srs-property-let ((org-srs-review-cache-p nil)
                             (org-srs-time-now (cl-constantly (org-srs-time-now))))
        (let ((org-srs-review-item-args (cl-multiple-value-list (org-srs-item-at-point)))
              (org-srs-table-with-temp-buffer-function #'funcall)
              (org-table-automatic-realign nil))
          (save-excursion
            (org-srs-table-goto-starred-line)
            (org-srs-table-with-temp-buffer-1
              (make-local-variable 'org-srs-review-item)
              (save-excursion
                (goto-char (point-min))
                (open-line 1)
                (insert "* HEADLINE")
                (newline)
                (insert "#+NAME: " (apply #'org-srs-item-link org-srs-review-item-args)))
              (org-srs-table-duplicate-line)
              (let ((buffer-undo-list nil))
                (defvar org-srs-review-rating)
                (defvar org-srs-review-item)
                (defvar cl--random-state)
                (cl-flet ((rate (rating)
                            (save-excursion
                              (prog2 (let ((org-srs-review-rating rating)
                                           (org-srs-review-item org-srs-review-item-args)
                                           (cl--random-state (org-srs-stats-deep-copy cl--random-state)))
                                       (undo-boundary)
                                       (cl-assert (not (local-variable-p 'org-srs-review-before-rate-hook)))
                                       (cl-assert (not (local-variable-p 'org-srs-review-after-rate-hook)))
                                       (run-hooks 'org-srs-review-before-rate-hook)
                                       (org-srs-log-repeat :rating rating)
                                       (run-hooks 'org-srs-review-after-rate-hook))
                                  (org-srs-timestamp-difference
                                   (org-srs-item-due-timestamp)
                                   (org-srs-timestamp-now))
                                (primitive-undo 1 buffer-undo-list)))))
                  (cl-return-from org-srs-stats-call-with-rating-simulator (funcall thunk #'rate)))))))))))

(cl-defmacro org-srs-stats-with-rating-simulator (args &rest body)
  "Execute BODY with a rating simulator function bound to ARGS."
  (declare (indent 1))
  `(org-srs-stats-call-with-rating-simulator
    (lambda ,args
      (cl-flet ,(cl-loop for arg in args collect (cl-with-gensyms (args) `(,arg (&rest ,args) (apply ,arg ,args))))
        ,@body))))

(provide 'org-srs-stats)
;;; org-srs-stats.el ends here
