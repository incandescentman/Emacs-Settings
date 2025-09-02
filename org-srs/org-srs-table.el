;;; org-srs-table.el --- Table operation utilities -*- lexical-binding: t -*-

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

;; This package provides various functions to manipulate the tables of
;; review logs in Org-srs.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'nadvice)

(require 'org)
(require 'org-element)

(defun org-srs-table-column-name-number-alist ()
  "Return an alist mapping the current table column names to their numeric indices."
  (cl-loop for name in (when (save-excursion
                               (goto-char (org-srs-table-begin))
                               (re-search-forward "^[ \t]*| *! *\\(|.*\\)" (org-srs-table-end) t))
                         (org-split-string (match-string-no-properties 1) " *| *"))
           for column from 2
           when (string-match-p "\\`[a-zA-Z][_a-zA-Z0-9]*\\'" name)
           collect (cons (intern (cl-substitute ?- ?_ name)) column)))

(defconst org-srs-table-readable-field-regexp (rx bos (or (and ":" (+ anychar)) (and (? "-") (+ digit) (? "." (* digit)) (? "e" (? "-") (+ digit)))) eos)
  "Regular expression matching fields in tables that can be read as Lisp values.")

(defun org-srs-table-ensure-read-field (field)
  "Convert string FIELD to the appropriate value based on its content.

Returns nil for empty strings. For strings matching readable patterns, returns
the Lisp value read from the string. Otherwise returns the string as is."
  (cond
   ((string-empty-p field) nil)
   ((string-match-p org-srs-table-readable-field-regexp field) (car (read-from-string field)))
   (t (org-no-properties field))))

(defun org-srs-table-lines ()
  "Parse the current Org table data into a list of alists, one per row."
  (cl-loop with names = (mapcar #'car (org-srs-table-column-name-number-alist))
           for line in (cdr (org-table-to-lisp))
           when (listp line)
           collect (cl-loop for name in names
                            for field in (cdr line)
                            unless (string-empty-p field)
                            collect (cons name (org-srs-table-ensure-read-field field)))))

(cl-defun org-srs-table-element-begin (&optional (element (org-element-at-point-no-context)))
  "Return the beginning position of table ELEMENT."
  (cl-ecase (car element)
    (table (org-element-begin element))
    (table-row (org-srs-table-element-begin (org-element-parent element)))))

(cl-defun org-srs-table-element-end (&optional (element (org-element-at-point-no-context)))
  "Return the end position of table ELEMENT."
  (cl-ecase (car element)
    (table (org-element-end element))
    (table-row (org-srs-table-element-end (org-element-parent element)))))

(defun org-srs-table-begin ()
  "Return the beginning position of the current table."
  (if (looking-at-p org-table-line-regexp)
      (org-table-begin)
    (save-excursion
      (unless (and (looking-at-p org-keyword-regexp) (not (looking-at-p org-TBLFM-regexp)))
        (goto-char (org-srs-table-element-begin)))
      (re-search-forward org-table-line-regexp)
      (line-beginning-position))))

(defun org-srs-table-end ()
  "Return the end position of the current table."
  (if (looking-at-p org-table-line-regexp)
      (org-table-end)
    (save-excursion
      (goto-char (org-srs-table-begin))
      (org-table-end))))

(defconst org-srs-table-starred-line-regexp (rx bol (* blank) "|" (* blank) (group "*") (* blank) "|")
  "Regular expression matching a starred line in an Org table.")

(defun org-srs-table-goto-starred-line ()
  "Move point to the star in the first starred line of the current table."
  (prog2 (goto-char (org-srs-table-begin))
      (re-search-forward org-srs-table-starred-line-regexp (org-srs-table-end))
    (goto-char (match-beginning 1))))

(defun org-srs-table-string-trim (string)
  "Return STRING with its leading and trailing whitespace removed."
  (string-match (rx bos (* (char " \t\n\r")) (group (*? anychar)) (* (char " \t\n\r")) eos) string)
  (match-string-no-properties 1 string))

(cl-defun org-srs-table-current-line (&optional (columns (org-srs-table-column-name-number-alist)))
  "Return an alist of column names to values for the current table row.

COLUMNS specifies the column name to number mapping to use and defaults to that
of the current table."
  (cl-loop for (name . number) in columns
           for field = (org-srs-table-string-trim (org-table-get-field number))
           unless (string-empty-p field)
           collect (cons name (org-srs-table-ensure-read-field field))))

(cl-defun org-srs-table-starred-line (&optional (offset 0))
  "Return the starred line in the current table as an alist of fields.

OFFSET specifies how many rows to move from the starred line and defaults to 0."
  (save-excursion
    (when (org-srs-table-goto-starred-line)
      (cond
       ((cl-plusp offset)
        (cl-loop repeat offset do (org-table-next-row)))
       ((cl-minusp offset)
        (forward-line offset)))
      (org-srs-table-current-line))))

(defun org-srs-table-forward-star ()
  "Move the starred marker to the next row in the current table."
  (when (org-srs-table-goto-starred-line)
    (goto-char (match-beginning 1))
    (save-mark-and-excursion
      (deactivate-mark)
      (org-table-rotate-recalc-marks " "))
    (org-table-next-row)
    (save-mark-and-excursion
      (deactivate-mark)
      (org-table-rotate-recalc-marks "*"))))

(defun org-srs-table-from-alist (alist)
  "Create a new Org table from ALIST of column names to values."
  (cl-loop initially (insert "| ! |")
           for (name . nil) in alist
           do (insert (cl-substitute ?_ ?- (prin1-to-string name t)) " | "))
  (newline)
  (insert "|-")
  (newline)
  (cl-loop initially (insert "| * |")
           for (nil . field) in alist
           when field do (insert (prin1-to-string field t))
           do (insert " | "))
  (org-table-align))

(defun org-srs-table-goto-column (name)
  "Move point to the column with NAME in the current table row."
  (when-let ((column (alist-get name (org-srs-table-column-name-number-alist))))
    (org-table-goto-column column) t))

(defun org-srs-table-field (&optional column)
  "Return the field contents at point in the current table.

When COLUMN is non-nil, move to COLUMN first before getting the field."
  (when column (cl-assert (org-srs-table-goto-column column)))
  (org-no-properties (org-table-get nil nil)))

(cl-defmethod (setf org-srs-table-field) (value &optional column)
  "Set the field contents at point in the current table to VALUE.

When COLUMN is non-nil, move to COLUMN first before setting the field."
  (if column
      (progn
        (cl-assert (org-srs-table-goto-column column))
        (setf (org-srs-table-field) value))
    (org-table-blank-field)
    (cl-assert (looking-at (rx (+ blank))))
    (let* ((value-length (length value))
           (field-length (- (match-end 0) (match-beginning 0))))
      (cl-assert (= (point) (match-beginning 0)))
      (when (> field-length value-length)
        (delete-region (point) (+ (point) value-length))))
    (insert value)))

(cl-defun org-srs-table-duplicate-line (&optional (arg 1))
  "Duplicate the current line in the table ARG times."
  (save-excursion
    (cl-loop with string = (buffer-substring-no-properties (line-beginning-position) (line-end-position))
             repeat arg
             do (end-of-line) (newline) (insert string))))

(cl-defun org-srs-table-call-with-temp-buffer-1 (thunk
                                                 &optional
                                                 (table (buffer-substring-no-properties
                                                         (org-srs-table-begin)
                                                         (org-srs-table-end)))
                                                 (point (- (point) (org-srs-table-begin))))
  "Call THUNK in a temporary buffer with a copy of the current table.
Return the modified table and the point position as multiple values.

THUNK is a function to execute in the temporary buffer.
TABLE is a string containing the table contents to use.
POINT is the starting position within the table when executing THUNK.

This function is internally used in `org-srs-table-call-with-temp-buffer'."
  (cl-assert (not (cl-minusp point)))
  (with-temp-buffer
    (insert table)
    (let ((org-mode-hook nil)) (org-mode))
    (goto-char (+ (org-srs-table-begin) point))
    (funcall thunk)
    (let* ((begin (org-srs-table-begin)) (end (org-table-end)) (point (- (point) begin)))
      (cl-values (buffer-substring-no-properties begin end) point))))

(cl-defmacro org-srs-table-with-temp-buffer-1 (&rest body)
  "Execute BODY in a temporary buffer with a copy of the current table.

Return the modified table and the point position as multiple values."
  (declare (indent 0))
  `(org-srs-table-call-with-temp-buffer-1 (lambda () . ,body)))

(defun org-srs-table-call-with-temp-buffer (thunk)
  "Call THUNK in a temporary buffer with a copy of the current table.

Update the buffer with any modifications made by THUNK and restore point."
  (let* ((begin (org-srs-table-begin)) (end (1- (org-srs-table-end))) (point (- (point) begin))
         (table (buffer-substring-no-properties begin end)))
    (cl-assert (>= (point) begin))
    (cl-multiple-value-bind (table point) (org-srs-table-call-with-temp-buffer-1 thunk table point)
      (delete-region begin end)
      (insert table)
      (cl-loop while (= (line-beginning-position) (point) (line-end-position)) do (delete-char 1))
      (goto-char (+ begin point)))))

(defvar org-srs-table-with-temp-buffer-function #'org-srs-table-call-with-temp-buffer
  "Function called by `org-srs-table-with-temp-buffer'.")

(cl-defmacro org-srs-table-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with a copy of the current table.

The actual table is updated with any modifications after BODY's execution."
  (declare (indent 0))
  `(funcall org-srs-table-with-temp-buffer-function (lambda () . ,body)))

(provide 'org-srs-table)
;;; org-srs-table.el ends here
