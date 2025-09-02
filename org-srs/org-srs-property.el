;;; org-srs-property.el --- Property-based customization facilities -*- lexical-binding: t -*-

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

;; This package provides support of defining, accessing, and modifying
;; global, per-buffer, per-heading, per-item, and even per-control
;; flow customizable variables for Org-srs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'custom)
(require 'cus-edit)

(require 'org)
(require 'org-element)

(require 'org-srs-table)

(defgroup org-srs nil
  "Spaced repetition system inside Org-mode."
  :group 'org
  :prefix "org-srs-"
  :link '(url-link "https://github.com/bohonghuang/org-srs"))

(cl-defun org-srs-property-plist (&optional (position (point)))
  "Get the property list specified by #+ATTR_SRS at POSITION."
  (ignore-error error
    (save-excursion
      (goto-char position)
      (goto-char (org-srs-table-element-begin))
      (when-let ((property (org-element-property :attr_srs (org-element-at-point-no-context))))
        (read (concat "(" (cl-reduce (lambda (acc it) (concat acc " " it)) property) ")"))))))

(cl-defmethod (setf org-srs-property-plist) (value &optional (position (point)))
  "Set the property list specified by #+ATTR_SRS to VALUE at POSITION."
  (save-excursion
    (let ((header "#+ATTR_SRS: "))
      (goto-char position)
      (goto-char (org-srs-table-element-begin))
      (if (re-search-forward (rx bol (* blank) (literal header)) (org-srs-table-element-end) t)
          (delete-region (line-beginning-position) (line-end-position))
        (goto-char (org-srs-table-begin))
        (open-line 1))
      (insert header)
      (insert (string-remove-suffix ")" (string-remove-prefix "(" (prin1-to-string value)))))))

(defvar org-srs-property-use-inheritance 'unspecified
  "Control whether Org-srs properties should inherit from parent headings.

When set to `unspecified', the default inheritance behavior is used.
When set to t, Org-srs properties inherit from parent headings.
When set to nil, Org-srs properties do not inherit from parent headings.")

(defmacro org-srs-property-defcustom (name &rest defcustom-args)
  "Define an Org-srs property (customizable option) named NAME with DEFCUSTOM-ARGS.

The property expects to be accessed via function call rather than direct
variable reference and can be customized globally, per directory, per buffer,
per entry, or per item."
  (declare (doc-string 3) (indent defun))
  (cl-assert (string-prefix-p (symbol-name 'org-srs) (symbol-name name)))
  (let* ((property (string-remove-prefix (symbol-name 'org-) (symbol-name name)))
         (property-name (string-replace "-" "_" (upcase property)))
         (property (string-remove-prefix (symbol-name 'srs-) property))
         (propname (intern (concat ":" property)))
         (transform (prog1 (cl-getf defcustom-args :transform '#'identity) (cl-remf defcustom-args :transform)))
         (inherit (prog1 (cl-getf defcustom-args :inherit 't) (cl-remf defcustom-args :inherit))))
    (cl-with-gensyms (value anonymous-variable thunk block use-inheritance)
      `(progn
         (defcustom ,name . ,defcustom-args)
         (put ',name 'safe-local-variable #'always)
         (cl-defun ,name (&optional (,value ,transform) ,thunk)
           (if ,thunk
               (progn
                 (defvar ,anonymous-variable)
                 (let ((,anonymous-variable ,value))
                   (funcall ,thunk)))
             (funcall
              ,value
              (cl-block ,block
                (when (boundp ',anonymous-variable)
                  (cl-return-from ,block (symbol-value ',anonymous-variable)))
                (when (eq major-mode 'org-mode)
                  (let* ((,value (cl-getf (org-srs-property-plist) ,propname ',anonymous-variable)))
                    (unless (eq ,value ',anonymous-variable)
                      (cl-return-from ,block ,value)))
                  (when-let ((,value (org-entry-get
                                      nil ,property-name
                                      (let ((,use-inheritance org-srs-property-use-inheritance))
                                        (cl-etypecase ,use-inheritance
                                          ((eql unspecified) ,inherit)
                                          (boolean ,use-inheritance)))
                                      t)))
                    (cl-return-from ,block (read ,value))))
                ,name))))))))

(cl-defun org-srs-property-group-members (&optional (group 'org-srs))
  "Return a list of all custom variables in GROUP and its subgroups."
  (cl-loop for (member type) in (custom-group-members group nil)
           if (eq type 'custom-variable)
           collect member
           else if (eq type 'custom-group)
           nconc (org-srs-property-group-members member)
           else
           do (cl-assert nil)))

(cl-defun org-srs-property-thunk-with-saved-properties (thunk &optional (properties (org-srs-property-group-members)))
  "Return a thunk that calls THUNK with saved PROPERTIES."
  (defvar org-srs-property-thunk-args)
  (let ((thunk (cl-reduce
                (lambda (thunk property)
                  (apply-partially property (funcall property #'identity) thunk))
                (cl-etypecase properties
                  (list properties)
                  (symbol (org-srs-property-group-members properties)))
                :initial-value (lambda () (apply thunk org-srs-property-thunk-args)))))
    (lambda (&rest args) (let ((org-srs-property-thunk-args args)) (funcall thunk)))))

(defmacro org-srs-property-let (bindings &rest body)
  "Temporarily bind Org-srs properties while executing BODY.

BINDINGS is a list of property bindings or a symbol naming a property group.

When BINDINGS is a list, each element can be either VAR (binding to its saved
value), or (VAR VAL) binding VAR to VAL.
When BINDINGS is a symbol naming a property group, all properties in that group
are temporarily bound to their current values."
  (declare (indent 1))
  (pcase-exhaustive bindings
    (`((,(and (pred symbolp) var) ,val) . ,rest)
     `(,var ,val (lambda () (org-srs-property-let ,rest . ,body))))
    (`() `(progn . ,body))
    (`(,(and (pred symbolp) var) . ,rest)
     `(funcall (org-srs-property-thunk-with-saved-properties (lambda () (org-srs-property-let ,rest . ,body)) '(,var))))
    ((and (pred symbolp) (or (and 't (let group 'org-srs)) group))
     `(funcall (org-srs-property-thunk-with-saved-properties (lambda () . ,body) (org-srs-property-group-members ',group))))))

(provide 'org-srs-property)
;;; org-srs-property.el ends here
