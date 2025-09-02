;;; org-srs-time.el --- Time(stamp) utilities -*- lexical-binding: t -*-

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

;; This package defines the timestamp representation for Org-srs and
;; provides various utility functions for time(stamp) operations.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'parse-time)
(require 'custom)

(require 'org-srs-property)

(defgroup org-srs-time nil
  "Time(stamp) facilities."
  :group 'org-srs
  :prefix "org-srs-time-")

(defconst org-srs-time-units '((:sec . 1) (:minute . 60) (:hour . 3600) (:day . 86400)))

(defun org-srs-time-desc-seconds (desc)
  "Return the total seconds from time description DESC."
  (cl-loop for (amount unit) on desc by #'cddr
           sum (* amount (alist-get unit org-srs-time-units))))

(defun org-srs-time-seconds-desc (seconds)
  "Convert SECONDS to a time description."
  (cl-loop for (unit . amount) in (reverse org-srs-time-units)
           for (part remainder) = (cl-multiple-value-list (cl-truncate (or remainder seconds) amount))
           unless (zerop part)
           nconc (list part unit)
           until (zerop remainder)))

(defun org-srs-time+ (time &rest desc)
  "Add the duration specified by DESC to TIME and return the resulting time value."
  (time-add time (seconds-to-time (org-srs-time-desc-seconds desc))))

(defun org-srs-time-difference (time-a time-b)
  "Calculate the difference in seconds between TIME-A and TIME-B."
  (- (time-to-seconds time-a) (time-to-seconds time-b)))

(cl-defmacro org-srs-time-define-comparators (&optional (prefix 'org-srs-time))
  "Define comparison functions for time differences.

PREFIX is used for generating function names."
  (cl-with-gensyms (a b)
    `(progn . ,(cl-loop for comparator in '(< <= > >= = /=)
                        collect `(defun ,(intern (format "%s%s" prefix comparator)) (,a ,b)
                                   (,comparator (,(intern (format "%s-%s" prefix 'difference)) ,a ,b) 0))))))

(org-srs-time-define-comparators)

(defun org-srs-time-truncate-hms (time)
  "Truncate the hours, minutes and seconds from TIME."
  (let* ((time (decode-time time))
         (hms (cl-subseq time 0 3)))
    (cl-values (encode-time (cl-fill time 0 :start 0 :end 3)) (cl-mapcan #'list hms '(:sec :minute :hour)))))

(org-srs-property-defcustom org-srs-time-start-of-next-day '(4 :hour)
  "Offset used to calculate the start time of the next day."
  :group 'org-srs-time
  :type 'sexp)

(org-srs-property-defcustom org-srs-time-now #'current-time
  "Function to get the current time."
  :group 'org-srs-time
  :type 'function
  :transform #'funcall)

(cl-defun org-srs-time-today (&optional (now (org-srs-time-now)))
  "Return the beginning of today relative to NOW.

The start of day is determined by customizable option
`org-srs-time-start-of-next-day'."
  (cl-multiple-value-bind (time hms) (org-srs-time-truncate-hms now)
    (let ((start-of-day (org-srs-time-start-of-next-day)))
      (if (< (org-srs-time-desc-seconds hms) (org-srs-time-desc-seconds start-of-day))
          (apply #'org-srs-time+ time -1 :day start-of-day)
        (apply #'org-srs-time+ time start-of-day)))))

(defun org-srs-time-today+1 ()
  "Return the beginning of tomorrow relative to `org-srs-time-today'."
  (org-srs-time+ (org-srs-time-today) 1 :day))

(org-srs-property-defcustom org-srs-time-tomorrow #'org-srs-time-today+1
  "Function to get the tomorrow time."
  :group 'org-srs-time
  :type 'function
  :transform #'funcall)

(cl-defun org-srs-time-today-p (time)
  "Return non-nil if TIME is between today and tomorrow."
  (let ((seconds (time-to-seconds time)))
    (and (<= (time-to-seconds (org-srs-time-today)) seconds)
         (< seconds (time-to-seconds (org-srs-time-tomorrow))))))

(cl-deftype org-srs-timestamp ()
  "Timestamp type represented as an ISO 8601 formatted string."
  'string)

(defun org-srs-timestamp-time (timestamp)
  "Convert TIMESTAMP to a time value."
  (cl-assert (string-match
              (rx (group (= 4 digit)) ?- (group (= 2 digit)) ?- (group (= 2 digit)) ?T
                  (group (= 2 digit)) ?: (group (= 2 digit)) ?: (group (= 2 digit)) ?Z)
              timestamp))
  (encode-time
   (string-to-number (match-string-no-properties 6 timestamp))
   (string-to-number (match-string-no-properties 5 timestamp))
   (string-to-number (match-string-no-properties 4 timestamp))
   (string-to-number (match-string-no-properties 3 timestamp))
   (string-to-number (match-string-no-properties 2 timestamp))
   (string-to-number (match-string-no-properties 1 timestamp))
   t))

(cl-defun org-srs-timestamp-now (&optional (time (org-srs-time-now)))
  "Return the current time as a timestamp.

TIME specifies the time to convert and defaults to now."
  (format-time-string "%FT%TZ" time t))

(defalias 'org-srs-timestamp 'org-srs-timestamp-now)

(defun org-srs-timestamp-difference (time-a time-b)
  "Return the difference in seconds between TIME-A and TIME-B."
  (- (time-to-seconds (org-srs-timestamp-time time-a))
     (time-to-seconds (org-srs-timestamp-time time-b))))

(org-srs-time-define-comparators org-srs-timestamp)

(defun org-srs-timestamp+ (timestamp &rest desc)
  "Add the duration specified by DESC to the TIMESTAMP and return a new timestamp."
  (org-srs-timestamp-now
   (+ (time-to-seconds (org-srs-timestamp-time timestamp))
      (org-srs-time-desc-seconds desc))))

(defun org-srs-timestamp-min (&rest args)
  "Return the earliest timestamp among ARGS."
  (cl-reduce (lambda (time-a time-b) (if (org-srs-timestamp> time-a time-b) time-b time-a)) args))

(defun org-srs-timestamp-max (&rest args)
  "Return the latest timestamp among ARGS."
  (cl-reduce (lambda (time-a time-b) (if (org-srs-timestamp> time-a time-b) time-a time-b)) args))

(defconst org-srs-timestamp-date-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
  "Regular expression matching the date part of an ISO8601 timestamp.")

(defconst org-srs-timestamp-time-regexp (rx (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
  "Regular expression matching the time part of an ISO8601 timestamp.")

(defconst org-srs-timestamp-regexp (rx (regexp org-srs-timestamp-date-regexp) "T" (regexp org-srs-timestamp-time-regexp) "Z")
  "Regular expression matching a complete ISO8601 timestamp string.")

(defun org-srs-timestamp-date (timestamp)
  "Extract the date part from TIMESTAMP."
  (string-match org-srs-timestamp-date-regexp timestamp)
  (match-string-no-properties 0 timestamp))

(provide 'org-srs-time)
;;; org-srs-time.el ends here
