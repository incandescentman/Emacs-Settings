;;; org-srs-log.el --- Operations related to review logs -*- lexical-binding: t -*-

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

;; This package provides operations related to the review log for
;; Org-srs.

;;; Code:

(require 'cl-lib)
(require 'rx)

(require 'org)
(require 'org-element)

(require 'org-srs-algorithm)
(require 'org-srs-time)
(require 'org-srs-table)

(defun org-srs-log-insert ()
  "Insert a new log element with current timestamp and algorithm parameters."
  (let ((initargs (org-srs-algorithm-repeat (org-srs-algorithm-current) nil)))
    (org-srs-table-from-alist `((timestamp . ,(org-srs-timestamp-now)) . ,initargs))
    (org-srs-table-forward-star)
    (setf (org-srs-table-field 'timestamp) (org-srs-timestamp-now))
    (org-table-align)))

(defun org-srs-log-plist-alist (plist)
  "Convert PLIST to an alist destructively."
  (cl-loop for last = nil then list
           for list = plist then next
           for (key value) = list
           for cons = (cdr list)
           for next = (cddr list)
           when last
           do (setf (cdr last) list)
           while list
           do (setf (car cons) key (cdr cons) value (car list) cons)
           finally (cl-return plist)))

(cl-defun org-srs-log-repeat (&rest args &key (timestamp nil timestampp) &allow-other-keys)
  "Repeat the current log entry with parameters ARGS passed to the algorithm.

TIMESTAMP specifies the review time.

Return the updated parameters if successful."
  (org-srs-table-goto-starred-line)
  (org-srs-property-let t
    (org-srs-table-with-temp-buffer
      (setf args (cond
                  (timestamp args)
                  (timestampp (cl-remf args :timestamp) args)
                  (t (cl-list* :timestamp (org-srs-timestamp-now) args)))
            args (mapc (lambda (cons) (setf (car cons) (intern (string-trim-left (symbol-name (car cons)) ":"))))
                       (org-srs-log-plist-alist args))
            args (nconc args '(t))
            args (cl-loop for arg in args
                          if (consp arg)
                          collect arg
                          and nconc (when-let ((field (org-srs-table-ensure-read-field (org-srs-table-field (car arg)))))
                                      (list (cons (car arg) field)))
                          and do (setf (org-srs-table-field (car arg)) (prin1-to-string (cdr arg) t))
                          else if (eq arg t)
                          nconc (save-excursion
                                  (forward-line -1)
                                  (cl-assert (not (org-at-table-hline-p)))
                                  (org-srs-table-current-line))))
      (unless (setf args (org-srs-algorithm-repeat (org-srs-algorithm-current) args))
        (cl-return-from org-srs-log-repeat))
      (cl-loop for (name . nil) in (org-srs-table-column-name-number-alist)
               for field = (alist-get name args)
               unless (eq name 'timestamp)
               do (setf (org-srs-table-field name) (if field (prin1-to-string field t) ""))
               finally
               (org-srs-table-forward-star)
               (setf (org-srs-table-field 'timestamp) (alist-get 'timestamp args)))))
  args)

(defconst org-srs-log-latest-timestamp-regexp
  (rx (regexp org-srs-table-starred-line-regexp) (* blank) (group (regexp org-srs-timestamp-regexp)))
  "Regular expression matching the timestamp in the latest starred line.")

(defconst org-srs-log-drawer-name "SRSITEMS"
  "Name of the drawer used for storing Org-srs items.")

(defun org-srs-log-end-of-drawer ()
  "Move point to the end of the Org-srs drawer, creating it if necessary."
  (save-restriction
    (org-back-to-heading)
    (let ((entry-start (point))
          (entry-end (org-entry-end-position))
          (drawer-start-regexp (rx bol (* blank) ":" (literal org-srs-log-drawer-name) ":" (* blank) eol))
          (drawer-end-regexp (rx bol (* blank) ":END:" (* blank) eol)))
      (if (re-search-forward drawer-start-regexp entry-end t)
          (progn
            (goto-char (org-element-end (org-element-at-point)))
            (re-search-backward drawer-end-regexp entry-start))
        (org-end-of-meta-data t)
        (unless (re-search-backward drawer-end-regexp entry-start t)
          (org-back-to-heading))
        (end-of-line)
        (newline-and-indent)
        (insert ":" org-srs-log-drawer-name ":")
        (newline-and-indent)
        (insert ":END:")
        (beginning-of-line)))))

(defun org-srs-log-beginning-of-drawer ()
  "Move point to the beginning of the Org-srs drawer, creating it if necessary."
  (save-restriction
    (org-narrow-to-subtree)
    (org-back-to-heading)
    (let ((heading-start (point)))
      (org-srs-log-end-of-drawer)
      (re-search-backward (rx bol (* blank) ":" (literal org-srs-log-drawer-name) ":" (* blank) eol) heading-start))))

(cl-defun org-srs-log-hide-drawer (&optional (position (point)))
  "Toggle the visibility of the Org-srs drawer at POSITION."
  (save-excursion
    (goto-char position)
    (org-srs-log-beginning-of-drawer)
    (org-fold-hide-drawer-toggle t)))

(defalias 'org-srs-log-begin 'org-srs-table-element-begin)
(defalias 'org-srs-log-end 'org-srs-table-element-end)

(provide 'org-srs-log)
;;; org-srs-log.el ends here
