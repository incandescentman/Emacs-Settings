;;; org-srs-review-cache.el --- Cache query results in a review session -*- lexical-binding: t -*-

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

;; This package provides caching mechanisms for query results in a
;; review session, speeding up retrieving the next review item.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'pcase)
(require 'custom)

(require 'org)
(require 'org-element)

(require 'org-srs-time)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-property)
(require 'org-srs-query)

(defgroup org-srs-review-cache nil
  "Caching mechanisms to improve scheduling performance in a review session."
  :group 'org-srs-review
  :prefix "org-srs-review-cache-")

(cl-defstruct (org-srs-review-cache (:predicate org-srs-review-cache--p))
  "Cache structure for review session items.

SOURCE is the review session source.
TIME is the last update time of the cache.
QUERIES is an alist mapping queries to sets of review items.
PENDING is an alist mapping potential due times to review items.
DUE-TIMES is a hash table caching review items to their due times.
MARKERS is a hash table caching review items to their markers."
  (source nil :type t)
  (time (floor (time-to-seconds)) :type fixnum)
  (queries nil :type list)
  (pending nil :type list)
  (due-times (make-hash-table :test #'equal) :type hash-table)
  (markers (make-hash-table :test #'equal) :type hash-table))

(defvar org-srs-review-cache nil
  "Review cache used for the current review session.")

(defun org-srs-review-cache ()
  "Return the current review cache."
  org-srs-review-cache)

(cl-defmethod (setf org-srs-review-cache) (value)
  "Set the current review cache to VALUE."
  (setf org-srs-review-cache value))

(cl-pushnew #'org-srs-review-cache org-srs-reviewing-predicates)

(defun org-srs-review-cache-clear (&rest _args)
  "Clear the current review cache."
  (setf (org-srs-review-cache) nil))

(cl-defun org-srs-review-cache-query-predicate-due-time (predicate)
  "Extract the due time from PREDICATE if it contains a due clause."
  (pcase predicate
    (`(and ,(or 'due `(due . ,args)) . ,_)
     (cl-destructuring-bind (&optional (time (org-srs-time-now))) args time))))

(defconst org-srs-review-cache-null (make-symbol (symbol-name 'nil))
  "Special nil indicating a cache miss for query results.")

(cl-defun org-srs-review-cache-update-pending-1 (&optional (cache (org-srs-review-cache)))
  "Update the pending items in CACHE."
  (setf (org-srs-review-cache-pending cache)
        (cl-loop with queries = (org-srs-review-cache-queries cache)
                 for time-predicate-item in (org-srs-review-cache-pending cache)
                 for (time . (predicate . item)) = time-predicate-item
                 for due-time = (org-srs-review-cache-query-predicate-due-time predicate)
                 if (org-srs-time> time due-time)
                 collect time-predicate-item
                 else
                 do (setf (gethash item (alist-get predicate queries nil nil #'equal)) t))))

(cl-defun org-srs-review-cache-update-pending (&optional (cache (org-srs-review-cache)))
  "Update the pending items in CACHE if necessary."
  (let ((time (floor (time-to-seconds (org-srs-time-now)))))
    (cl-assert (>= time (org-srs-review-cache-time cache)))
    (when (> time (org-srs-review-cache-time cache))
      (org-srs-review-cache-update-pending-1 cache)
      (setf (org-srs-review-cache-time cache) time))))

(defun org-srs-review-cache-query (predicate &optional source)
  "Query the cache for items matching PREDICATE in SOURCE."
  (if-let ((cache (org-srs-review-cache)))
      (let ((queries (org-srs-review-cache-queries cache)))
        (cl-assert (org-srs-review-cache-source cache))
        (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
        (org-srs-review-cache-update-pending cache)
        (if-let ((items (alist-get predicate queries nil t #'equal)))
            (hash-table-keys items)
          org-srs-review-cache-null))
    org-srs-review-cache-null))

(cl-defmethod (setf org-srs-review-cache-query) (value predicate &optional source)
  "Set the cached query result for PREDICATE in SOURCE to VALUE."
  (let ((cache (org-srs-review-cache)))
    (cl-assert (org-srs-review-cache-source cache))
    (when source (cl-assert (equal (org-srs-review-cache-source cache) source)))
    (setf (alist-get predicate (org-srs-review-cache-queries cache) nil t #'equal)
          (cl-loop with items = (make-hash-table :test #'equal)
                   for item-cons on value
                   do (setf (gethash (setf (car item-cons) (apply #'org-srs-review-cache-item (car item-cons))) items) t)
                   finally (cl-return items)))
    value))

(org-srs-property-defcustom org-srs-review-cache-p nil
  "Non-nil means to enable caching mechanisms in a review session.

This can increase the speed of retrieving the next review item
from a large set of review items."
  :group 'org-srs-review-cache
  :type 'boolean)

(define-advice org-srs-review-cache-p (:around (fun &rest args) org-srs-review-cache)
  (if (and args (null (cdr args)))
      (cl-typecase (car args)
        (function (apply fun args))
        (t (apply #'org-srs-review-cache--p args)))
    (apply fun args)))

(defun org-srs-review-cache-active-p ()
  "Return non-nil if review caching is active in the current review session."
  (and (org-srs-reviewing-p) (org-srs-review-cache-p)))

(defmacro org-srs-review-cache-ensure-gethash (key hash-table &optional default)
  "Get the value from HASH-TABLE for KEY, setting it to DEFAULT if not found."
  (cl-once-only (key hash-table)
    (cl-with-gensyms (value null)
      `(let ((,value (gethash ,key ,hash-table ',null)))
         (if (eq ,value ',null)
             (setf (gethash ,key ,hash-table ',null) ,default)
           ,value)))))

(defun org-srs-review-cache-item (&rest args)
  "Convert a review item specified by ARGS into a full specification for it."
  (cl-destructuring-bind (item &optional (id (org-id-get)) (buffer (current-buffer) bufferp)) args
    (if bufferp args (list item id buffer))))

(define-advice org-srs-item-due-time (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) args)
      (let ((args (apply #'org-srs-review-cache-item args))
            (due-times (org-srs-review-cache-due-times (org-srs-review-cache))))
        (org-srs-review-cache-ensure-gethash args due-times (apply fun args)))
    (apply fun args)))

(define-advice org-srs-item-marker (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) args)
      (let ((args (apply #'org-srs-review-cache-item args))
            (markers (org-srs-review-cache-markers (org-srs-review-cache))))
        (org-srs-review-cache-ensure-gethash args markers (apply fun args)))
    (apply fun args)))

(define-advice org-srs-item-goto (:around (fun &rest args) org-srs-review-cache)
  (cl-assert args)
  (defvar org-srs-item-marker@org-srs-review-cache)
  (if (and (not (bound-and-true-p org-srs-item-marker@org-srs-review-cache)) (org-srs-review-cache-active-p))
      (let ((marker (let ((org-srs-item-marker@org-srs-review-cache t)) (apply #'org-srs-item-marker args))))
        (switch-to-buffer (marker-buffer marker) nil t)
        (goto-char marker))
    (apply fun args)))

(define-advice org-srs-query (:around (fun &rest args) org-srs-review-cache)
  (cl-destructuring-bind (predicate &optional (source (or (buffer-file-name) default-directory))) args
    (if (org-srs-review-cache-active-p)
        (let ((result (org-srs-review-cache-query predicate source)))
          (if (eq result org-srs-review-cache-null)
              (let* ((cache (or (org-srs-review-cache) (setf (org-srs-review-cache) (make-org-srs-review-cache :source source))))
                     (query (if-let ((due-time (org-srs-review-cache-query-predicate-due-time predicate)))
                                (pcase-exhaustive predicate
                                  (`(and ,(and (or 'due `(due . ,_)) first-predicate) . ,rest-predicates)
                                   (let ((tomorrow-time (org-srs-time-tomorrow)))
                                     (cl-flet* ((cache-item ()
                                                  (nconc (cl-multiple-value-list (org-srs-item-at-point)) (list (current-buffer))))
                                                (cache-marker (&optional (item (cache-item)))
                                                  (setf (gethash item (org-srs-review-cache-markers cache)) (point-marker)))
                                                (cache-due-time ()
                                                  (let ((due-time (org-srs-item-due-time)))
                                                    (when (org-srs-time> tomorrow-time due-time)
                                                      (let ((item (cache-item)))
                                                        (setf (org-srs-review-cache-pending cache)
                                                              (cons (cons due-time (cons predicate item))
                                                                    (cl-delete (cons predicate item) (org-srs-review-cache-pending cache)
                                                                               :key #'cdr :test #'equal)))
                                                        (cache-marker item))))
                                                  nil))
                                       `(and ,@rest-predicates (or (and ,first-predicate ,#'cache-marker) ,#'cache-due-time))))))
                              predicate)))
                (setf (org-srs-review-cache-query predicate source) (funcall fun query source)))
            result))
      (funcall fun predicate source))))

(define-advice org-srs-review-start (:around (fun &rest args) org-srs-review-cache)
  (org-srs-property-let (org-srs-review-cache-p)
    (apply fun args)))

(define-advice org-srs-review-rate (:around (fun &rest args) org-srs-review-cache)
  (org-srs-property-let (org-srs-review-cache-p)
    (apply fun args)))

(cl-defun org-srs-review-cache-updated-item (&rest args)
  "Update the review cache for the updated review item specified by ARGS."
  (when-let ((cache (org-srs-review-cache))
             (item (apply #'org-srs-review-cache-item (or args (cl-multiple-value-list (org-srs-item-at-point))))))
    (setf (org-srs-review-cache-pending cache) (cl-delete item (org-srs-review-cache-pending cache) :key #'cddr :test #'equal))
    (org-srs-item-with-current item
      (cl-loop with tomorrow-time = (org-srs-time-tomorrow)
               and due-time = (setf (gethash item (org-srs-review-cache-due-times cache)) (org-srs-item-due-time))
               for (predicate . items) in (org-srs-review-cache-queries cache)
               for (all-satisfied-p . rest-satisfied-p)
               = (pcase predicate
                   (`(and ,(and (or 'due `(due . ,_)) first-predicate) . ,rest-predicates)
                    (when (funcall (org-srs-query-predicate `(and . ,rest-predicates)))
                      (cons (funcall (org-srs-query-predicate first-predicate)) t)))
                   (_ (cons (funcall (org-srs-query-predicate predicate)) nil)))
               if all-satisfied-p do (setf (gethash item items) t)
               else do (remhash item items)
               when (org-srs-time> tomorrow-time due-time)
               when (and rest-satisfied-p (not all-satisfied-p))
               do (push (cons due-time (cons predicate item)) (org-srs-review-cache-pending cache))))))

(cl-defun org-srs-review-cache-after-rate (&optional (item org-srs-review-item))
  "Update the review cache after rating ITEM."
  (when (org-srs-review-cache-p)
    (apply #'org-srs-review-cache-updated-item item)))

(add-hook 'org-srs-review-after-rate-hook #'org-srs-review-cache-after-rate 95)

(cl-defun org-srs-review-cache-cleanup-on-quit ()
  "Clear the review cache when quitting a review session."
  (when (and (org-srs-review-cache-p) (not (org-srs-reviewing-p)))
    (org-srs-review-cache-clear)))

(add-hook 'org-srs-review-continue-hook #'org-srs-review-cache-cleanup-on-quit)
(add-hook 'org-srs-review-finish-hook #'org-srs-review-cache-cleanup-on-quit)

(define-advice \(setf\ org-srs-item-due-timestamp\) (:after (_ &rest args) org-srs-review-cache)
  (when (org-srs-review-cache-p)
    (apply #'org-srs-review-cache-updated-item args)))

(define-advice org-toggle-comment (:after (&rest _args) org-srs-review-cache)
  (when (org-srs-review-cache-active-p)
    (mapc
     (apply-partially #'apply #'org-srs-review-cache-updated-item)
     (org-srs-property-let ((org-srs-review-cache-p nil))
       (org-srs-query '(and) (cons (org-entry-beginning-position) (org-entry-end-position)))))))

(define-advice org-srs-query-predicate (:around (fun &rest args) org-srs-review-cache)
  (if (and (org-srs-review-cache-active-p) (not (bound-and-true-p org-srs-query-predicate@org-srs-review-cache)))
      (cl-destructuring-bind (desc) args
        (cl-etypecase desc
          ((or list symbol)
           (cl-labels ((predicate (&rest args &aux (cache (org-srs-review-cache)))
                         (if-let ((items (cdr (cl-assoc desc (org-srs-review-cache-queries cache) :test #'equal)))
                                  (item (or args (cl-multiple-value-call #'org-srs-review-cache-item (org-srs-item-at-point)))))
                             (gethash item items)
                           (cl-assert (org-srs-review-cache-active-p))
                           (org-srs-query desc (org-srs-review-cache-source cache))
                           (apply #'predicate args))))
             #'predicate))
          (function desc)))
    (apply fun args)))

(defmacro org-srs-review-cache-without-query-predicate (&rest body)
  "Execute BODY without using predicates that utilize the review cache."
  (declare (indent 0))
  `(progn
     (defvar org-srs-query-predicate@org-srs-review-cache)
     (let ((org-srs-query-predicate@org-srs-review-cache t)) . ,body)))

(define-advice org-srs-review-cache-updated-item (:around (fun &rest args) org-srs-query-predicate@org-srs-review-cache)
  (org-srs-review-cache-without-query-predicate (apply fun args)))

(define-advice org-srs-query (:around (fun &rest args) org-srs-query-predicate@org-srs-review-cache)
  (org-srs-review-cache-without-query-predicate (apply fun args)))

(define-advice org-srs-query-item-p (:around (fun . (predicate &rest item)) org-srs-query-predicate@org-srs-review-cache)
  (if (org-srs-review-cache-active-p)
      (progn
        (cl-assert (not (bound-and-true-p org-srs-query-predicate@org-srs-review-cache)))
        (if item (apply predicate item) (apply fun predicate item)))
    (apply fun predicate item)))

(provide 'org-srs-review-cache)
;;; org-srs-review-cache.el ends here
