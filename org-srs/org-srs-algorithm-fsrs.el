;;; org-srs-algorithm-fsrs.el --- FSRS algorithm integration for Org-srs -*- lexical-binding: t -*-

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

;; This package serves as the algorithm interface for Org-srs, allowing
;; the core to be independent of specific algorithms.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)

(require 'fsrs)

(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-schedule-step)

(cl-defmethod org-srs-algorithm-ensure ((_type (eql 'fsrs)) &rest args)
  "Return an FSRS scheduler instance initialized with ARGS."
  (setf args (cl-nsubstitute :parameters :weights args)
        args (cl-nsubstitute :desired-retention :request-retention args))
  (when-let ((maximum-interval (cl-getf args :maximum-interval)))
    (cl-etypecase maximum-interval
      (integer (setf (cl-getf args :maximum-interval) (list maximum-interval :day)))
      (cons)))
  (when-let ((parameters (cl-getf args :parameters)))
    (cl-loop with standard-parameters = (copy-sequence fsrs-default-parameters)
             for i from 0
             for parameter across parameters
             do (setf (aref standard-parameters i) parameter)
             finally (setf (cl-getf args :parameters) standard-parameters)))
  (apply #'fsrs-make-scheduler :enable-fuzzing-p nil :learning-steps nil :relearning-steps nil args))

(cl-defmethod org-srs-algorithm-repeat ((_fsrs fsrs-scheduler) (_args null))
  "Return the initial algorithm state for the FSRS algorithm."
  '((rating . nil) (stability . 0.0) (difficulty . 0.0) (state . :new)))

(defconst org-srs-algorithm-fsrs-card-slots
  (mapcar #'cl--slot-descriptor-name (cl--class-slots (cl-find-class 'fsrs-card)))
  "List of slot names for class `fsrs-card'.")

(defun org-srs-algorithm-fsrs-ensure-card (object)
  "Ensure OBJECT is a valid `fsrs-card'.

If OBJECT is an `fsrs-card', return it as is.
If OBJECT is an alist, convert it to an `fsrs-card'."
  (cl-etypecase object
    (fsrs-card object)
    (list
     (when (eq (alist-get 'state object) :new)
       (setf (alist-get 'stability object) nil
             (alist-get 'difficulty object) nil
             (alist-get 'state object) :learning))
     (cl-loop with card = (fsrs-make-card) and args = (copy-alist object)
              initially (setf (car (cl-find 'timestamp args :key #'car :from-end t)) 'last-review)
              for slot in org-srs-algorithm-fsrs-card-slots
              for cons = (assoc slot args)
              for (key . value) = cons
              when cons
              do (setf (eieio-oref card key) value)
              finally (cl-return card)))))

(cl-defun org-srs-algorithm-fsrs-card-round-last-review (card &optional (now (org-srs-time-now)))
  "Ensure CARD's last review time is properly rounded when reviewing at time NOW.

This guarantees that reviewing a card the day before counts as at least one-day
interval."
  (setf (fsrs-card-last-review card)
        (org-srs-timestamp+
         (org-srs-timestamp now)
         (- (max (org-srs-time-difference
                  (org-srs-time-today now)
                  (org-srs-time-today (org-srs-timestamp-time (fsrs-card-last-review card))))
                 (org-srs-time-difference
                  now
                  (org-srs-timestamp-time (fsrs-card-last-review card)))))
         :sec))
  card)

(cl-defmethod org-srs-algorithm-repeat ((fsrs fsrs-scheduler) (args list))
  "Pass input ARGS to FSRS scheduler and return its output."
  (cl-loop with card-old = (org-srs-algorithm-fsrs-card-round-last-review (org-srs-algorithm-fsrs-ensure-card args))
           and rating = (alist-get 'rating args)
           and timestamp = (alist-get 'timestamp args (org-srs-timestamp-now))
           with card-new = (cl-nth-value 0 (fsrs-scheduler-review-card fsrs card-old rating timestamp))
           for slot in org-srs-algorithm-fsrs-card-slots
           collect (cons
                    (cl-case slot
                      (due 'timestamp)
                      (t slot))
                    (cl-case slot
                      (state (or (org-srs-schedule-step-state) (fsrs-card-state card-new)))
                      (t (eieio-oref card-new slot))))
           into slots
           finally (cl-return (nconc slots args))))

(provide 'org-srs-algorithm-fsrs)
;;; org-srs-algorithm-fsrs.el ends here
