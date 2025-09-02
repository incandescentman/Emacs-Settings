;;; org-srs-item.el --- Interface for review items -*- lexical-binding: t -*-

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

;; This package serves as the review item interface for Org-srs and
;; provides common functions, allowing the core to be independent of
;; specific item types.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)
(require 'custom)

(require 'org)
(require 'org-element)

(require 'org-srs-property)
(require 'org-srs-log)

(defgroup org-srs-item nil
  "Interface for various types of review items."
  :group 'org-srs
  :prefix "org-srs-item-")

(defalias 'org-srs-item-begin 'org-srs-log-begin)
(defalias 'org-srs-item-end 'org-srs-log-end)

(defconst org-srs-item-regexp (rx "srsitem:" (group (+ (not (any ?: blank control)))) (? "::" (group (+ (not (any blank control))))))
  "Regular expression matching Org-srs item links.")

(defun org-srs-item-name ()
  "Return the ID of the current Org entry."
  (let ((id (org-id-get)))
    (cl-assert id)
    id))

(defun org-srs-item-princ-to-string (object)
  "Convert OBJECT to the string used for review item links."
  (if (and (symbolp object) (not (eq (read (symbol-name object)) object)))
      (format "\\%s" object)
    (format "%s" object)))

(cl-defun org-srs-item-link (item &optional (id (org-id-get)))
  "Construct a link string for review ITEM with the entry ID."
  (cl-reduce
   (lambda (acc it) (format "%s::%s" acc (org-srs-item-princ-to-string it)))
   (cl-delete nil (ensure-list item) :end 1)
   :initial-value (format "srsitem:%s" id)))

(defun org-srs-item-insert (item &rest args)
  "Insert a review ITEM with ARGS at point."
  (insert "#+NAME: " (apply #'org-srs-item-link item args))
  (newline-and-indent)
  (org-srs-log-insert))

(defun org-srs-item-link-search (s)
  "Search for review item link string S from the beginning of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx bol "#+NAME:" (+ blank) (literal s) eol)))
  (goto-char (match-beginning 0)))

(cl-defun org-srs-item-marker (item &optional (id (org-id-get)) (buffer (current-buffer)))
  "Return the marker pointing to review ITEM in the entry with ID in BUFFER."
  (with-current-buffer buffer
    (org-srs-item-link-search (org-srs-item-link item id))
    (point-marker)))

(defun org-srs-item-goto (&rest args)
  "Switch buffer and move point to ITEM specified by ARGS.

ARGS are passed to `org-srs-item-marker' to locate the review item."
  (let* ((marker (apply #'org-srs-item-marker args))
         (buffer (marker-buffer marker)))
    (cl-assert (eq (window-buffer) (current-buffer)))
    (unless (eq buffer (current-buffer))
      (switch-to-buffer buffer nil t)
      (cl-assert (eq (window-buffer) buffer)))
    (cl-assert (eq (current-buffer) buffer))
    (goto-char marker)))

(defmacro org-srs-item-save-selected-window-excursion (&rest body)
  "Execute BODY while preserving selected window and buffer."
  (declare (indent 0))
  (cl-with-gensyms (buffer)
    `(let ((,buffer (window-buffer)))
       (unwind-protect (progn . ,body)
         (unless (eq ,buffer (window-buffer))
           (switch-to-buffer ,buffer nil t)
           (cl-assert (eq (window-buffer) ,buffer))
           (cl-assert (eq (window-buffer) (current-buffer))))))))

(defun org-srs-item-call-with-current (thunk &rest args)
  "Call THUNK with point on the review item specified by ARGS."
  (let* ((marker (apply #'org-srs-item-marker args))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (goto-char marker)
      (funcall thunk))))

(defmacro org-srs-item-with-current (args &rest body)
  "Execute BODY with point on the review item specified by ARGS."
  (declare (indent 1))
  `(apply
    #'org-srs-item-call-with-current
    (lambda () . ,body)
    ,@(cl-etypecase args
        (symbol (list args))
        ((satisfies proper-list-p) (append args '(nil)))
        (list (cl-loop for (arg . rest) on args
                       collect arg
                       unless (listp rest) collect rest
                       while (listp rest))))))

(defun org-srs-item-due-timestamp-1 ()
  "Return the timestamp string of the latest review from the current review item."
  (save-excursion
    (goto-char (org-srs-table-begin))
    (re-search-forward org-srs-log-latest-timestamp-regexp (org-srs-table-end) t)
    (match-string-no-properties 2)))

(defun org-srs-item-due-timestamp (&rest args)
  "Return the due timestamp string for the review item specified by ARGS."
  (if args
      (org-srs-item-with-current args
        (org-srs-item-due-timestamp))
    (save-excursion
      (org-srs-item-due-timestamp-1))))

(cl-defmethod (setf org-srs-item-due-timestamp) (value &rest args)
  "Set the due timestamp of the review item specified by ARGS to VALUE."
  (if args
      (org-srs-item-with-current args
        (setf (org-srs-item-due-timestamp) value))
    (if (org-srs-item-due-timestamp-1)
        (replace-match value t t nil 2)
      (setf (org-srs-table-field 'timestamp) value))))

(defun org-srs-item-due-time (&rest args)
  "Return the due time value for the review item specified by ARGS."
  (org-srs-timestamp-time (apply #'org-srs-item-due-timestamp args)))

(defun org-srs-item-priority (&rest args)
  "Return the priority of the review item specified by ARGS as a number."
  (org-srs-item-with-current args
    (org-back-to-heading)
    (cl-assert (looking-at org-heading-regexp))
    (org-get-priority (match-string-no-properties 0))))

(defun org-srs-item-repeat (item &rest args)
  "Repeat the review ITEM with ARGS and update the review log."
  (org-srs-item-goto item)
  (apply #'org-srs-log-repeat args))

(defconst org-srs-item-header-regexp
  (rx bol (* blank) "#+NAME: " (* blank) (regexp org-srs-item-regexp) (* blank) eol)
  "Regular expression matching the header line of an Org-srs item.")

(defun org-srs-item-from-match-data ()
  "Extract the review item and its entry ID from the current link's match data."
  (let ((id (match-string-no-properties 1)))
    (cl-values (when-let ((string (match-string-no-properties 2))) (mapcar #'read (split-string string "::"))) id)))

(defun org-srs-item-at-point ()
  "Return the review item and its entry ID at point."
  (save-excursion
    (goto-char (org-srs-item-begin))
    (when (re-search-forward org-srs-item-header-regexp (org-srs-item-end) t)
      (org-srs-item-from-match-data))))

(cl-defun org-srs-item-bounds (&optional (item (cl-nth-value 0 (org-srs-item-at-point))) &rest args)
  "Return the start and end positions of review ITEM with ARGS as a cons cell."
  (org-srs-item-with-current (item . args)
    (let ((element (org-element-at-point)))
      (cons (org-element-begin element) (org-element-end element)))))

(cl-defun org-srs-item-delete (&rest args)
  "Delete the review item specified by ARGS."
  (cl-destructuring-bind (start . end) (apply #'org-srs-item-bounds args)
    (delete-region start end)))

(cl-defgeneric org-srs-item-review (type &rest args)
  (:documentation "Review the item specified by TYPE and ARGS."))

(defun org-srs-item-exists-p (&rest args)
  "Return non-nil if a review item specified by ARGS exists."
  (ignore-error error (org-srs-item-with-current args t)))

(cl-defgeneric org-srs-item-new (type &rest args)
  (:method
   (type &rest args)
   "Default method to create a new item of TYPE with ARGS."
   (let ((item (cons type args)))
     (cl-assert (not (apply #'org-srs-item-exists-p item)) nil "Item %s already exists" item)
     (cl-assert (org-id-get))
     (org-srs-log-end-of-drawer)
     (org-open-line 1)
     (apply #'org-srs-item-insert type args)))
  (:documentation "Create a new item of TYPE with ARGS."))

(defun org-srs-item-types ()
  "Return a list of all defined review item types."
  (cl-delete-duplicates
   (cl-loop for gf in '(org-srs-item-review org-srs-item-new)
            nconc (cl-loop for method in (cl--generic-method-table (cl-generic-ensure-function gf))
                           for (eql symbol) = (ensure-list (cl-first (cl--generic-method-specializers method)))
                           when (eq eql 'eql)
                           do (cl-assert (eq (cl-first symbol) 'quote))
                           and collect (cl-second symbol)))))

(cl-defgeneric org-srs-item-new-interactively (type &rest args)
  (:method
   (type &rest args)
   "Default method for interactively creating a new review item of TYPE with ARGS.

TYPE and ARGS are passed to `org-srs-item-new' as is."
   (apply #'org-srs-item-new type args))
  (:method
   :around (_type &rest _args)
   "Method to restore point and hide the drawer after an interactive item creation."
   (save-excursion (cl-call-next-method) (org-srs-log-hide-drawer)))
  (:documentation "Interactively create a new review item of TYPE with ARGS."))

;;;###autoload
(defun org-srs-item-create ()
  "Create a review item in the current entry."
  (interactive)
  (require 'org-srs)
  (org-srs-item-new-interactively
   (prog1 (read (completing-read "Item type: " (org-srs-item-types) nil t))
     (org-id-get-create))))

(cl-defun org-srs-item-add-hook-once (hook function &optional depth (local t))
  "Add FUNCTION to HOOK and remove it after its first execution.

HOOK is the hook to which FUNCTION will be added.
FUNCTION is the function to run once in the hook.
DEPTH controls where FUNCTION is placed in HOOK and defaults to 0.
LOCAL controls whether to add HOOK buffer-locally and defaults to t.

The returned function can be used to call `remove-hook' if needed."
  (letrec ((hook-function (lambda () (remove-hook hook hook-function local) (funcall function))))
    (add-hook hook hook-function depth local)
    hook-function))

(defun org-srs-item-narrow ()
  "Narrow to the current subtree.

Automatically widen after reviewing the current item."
  (org-back-to-heading)
  (org-narrow-to-subtree)
  (org-srs-item-add-hook-once 'org-srs-review-continue-hook #'widen))

(defvar org-srs-item-before-confirm-hook nil
  "Hook run before confirming a review item.")

(defvar org-srs-item-after-confirm-hook nil
  "Hook run after confirming a review item.")

(defun org-srs-item-confirm-read-key (&rest _args)
  "Wait for a key press to confirm the current item review."
  (run-hooks 'org-srs-item-before-confirm-hook)
  (read-key "Continue with any key")
  (run-hooks 'org-srs-item-after-confirm-hook))

(defun org-srs-item-confirm-command (&rest _args)
  "Continue the item being reviewed in the current review session.

This command is intended to be used only when customizable option
`org-srs-item-confirm' is set to `org-srs-item-confirm-command' for
the current review item."
  (interactive)
  (let ((flag-hook (eval-when-compile (letrec ((hook (lambda () (remove-hook 'org-srs-item-after-confirm-hook hook t)))) hook))))
    (if (member flag-hook org-srs-item-after-confirm-hook)
        (run-hooks 'org-srs-item-after-confirm-hook)
      (cl-assert (not (called-interactively-p 'any)))
      (run-hooks 'org-srs-item-before-confirm-hook)
      (message (substitute-command-keys "Continue with \\[org-srs-item-confirm-command]"))
      (add-hook 'org-srs-item-after-confirm-hook flag-hook nil t))))

(cl-defun org-srs-item-confirm-pending-p (&optional (command #'org-srs-item-confirm-command))
  "Check if there is a pending review item confirmation.

COMMAND specifies the confirmation command to check and defaults to
`org-srs-item-confirm-command'."
  (when (local-variable-p 'org-srs-item-after-confirm-hook)
    (let ((org-srs-item-before-confirm-hook
           (cons
            (lambda ()
              (setf org-srs-item-before-confirm-hook (cl-copy-list org-srs-item-before-confirm-hook))
              (cl-return-from org-srs-item-confirm-pending-p nil))
            org-srs-item-before-confirm-hook))
          (org-srs-item-after-confirm-hook
           (cons
            (lambda ()
              (setf org-srs-item-after-confirm-hook (cl-copy-list org-srs-item-after-confirm-hook))
              (cl-return-from org-srs-item-confirm-pending-p command))
            org-srs-item-after-confirm-hook)))
      (ignore-errors (funcall command)))))

(defun org-srs-item-confirm-cleanup ()
  "Clean up any pending review item confirmations."
  (cl-loop (funcall (or (org-srs-item-confirm-pending-p) (cl-return)))))

(add-hook 'org-srs-review-continue-hook #'org-srs-item-confirm-cleanup)

(org-srs-property-defcustom org-srs-item-confirm #'org-srs-item-confirm-read-key
  "Method to confirm the current review item and reveal its answer."
  :group 'org-srs-item
  :type 'function)

(provide 'org-srs-item)
;;; org-srs-item.el ends here
