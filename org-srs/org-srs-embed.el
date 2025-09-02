;;; org-srs-embed.el --- Embed Org-srs entries into ordinary Org files -*- lexical-binding: t -*-

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

;; This package allows you to create and update Org-srs entries inside
;; your ordinary Org files, sparing you the maintenance of outline-based
;; Org files dedicated to review entries. All markers used to identify
;; embedded entries and cloze deletions can be prettified using
;; `org-srs-embed-overlay-mode' and are non-intrusive, which means they
;; won't pollute your Org file exports.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)
(require 'custom)

(require 'org)
(require 'org-element)
(require 'org-habit)

(require 'org-srs-property)
(require 'org-srs-item)
(require 'org-srs-item-cloze)

(defgroup org-srs-embed nil
  "Embedding and exporting review items to reduce their maintenance cost."
  :group 'org-srs
  :prefix "org-srs-embed-")

(defvar org-srs-embed-overlay-mode)

(defvar org-srs-embed-cloze-tag "srs"
  "Tag used for identifying embedded cloze deletions.")

(defvar org-srs-embed-cloze-brackets '(?{ ?})
  "Brackets used to enclose content within embedded cloze deletions.")

(defvar org-srs-embed-cloze-overlay-category 'org-srs-embed-cloze
  "Category symbol for overlays to prettify embedded cloze deletions.")

(cl-defun org-srs-embed-put-cloze-overlays (&optional (start (point-min)) (end (point-max)))
  "Prettify embedded cloze deletions between START and END using overlays."
  (save-excursion
    (cl-loop with ({ }) = (mapcar #'char-to-string org-srs-embed-cloze-brackets)
             initially (goto-char start)
             for () = (or (re-search-forward
                           (rx "@@" (literal org-srs-embed-cloze-tag) ":"
                               (group (+? anychar)) "@@")
                           end t)
                          (cl-return))
             for overlay = (make-overlay (match-beginning 0) (match-end 0) nil 'front-advance)
             for string = (replace-regexp-in-string
                           (rx (literal {) (group (literal {) (+? anychar) (literal })) (literal {))
                           "" (match-string 1) t t 1)
             for padding = (- (string-width (match-string 0)) (string-width (match-string 1)))
             for openp = (cl-plusp (cl-loop for char across string
                                            sum (cl-loop for bracket in org-srs-embed-cloze-brackets
                                                         for number downfrom 1 by 2
                                                         when (= bracket char)
                                                         return number
                                                         finally (cl-return 0))))
             do
             (overlay-put overlay 'category org-srs-embed-cloze-overlay-category)
             (overlay-put overlay 'invisible nil)
             (overlay-put overlay 'display (if (org-at-table-p) (string-pad string (+ (length string) padding) nil openp) string)))))

(cl-defun org-srs-embed-remove-cloze-overlays (&optional (start (point-min)) (end (point-max)))
  "Remove all cloze deletion overlays between START and END."
  (remove-overlays start end 'category org-srs-embed-cloze-overlay-category))

(cl-defun org-srs-embed-put-meta-overlays (&optional (start (point-min)) (end (point-max)))
  "Prettify embedded entry metadata between START and END using overlays."
  (cl-assert org-srs-embed-overlay-mode)
  (save-excursion
    (goto-char start)
    (ignore-error search-failed
      (cl-loop
       (let ((case-fold-search t))
         (re-search-forward
          (rx (or (and (group-n 1 "@@comment:+srs_embedded:" (group (+? anychar)) "@@"))
                  (and bol (* blank) (group-n 1 "#+srs_embedded:" (+? anychar)) eol)))
          end))
       (let ((overlay (make-overlay
                       (match-beginning 1)
                       (match-end 1)
                       nil 'front-advance)))
         (overlay-put overlay 'category 'org-srs-embed-meta)
         (overlay-put overlay 'invisible nil)
         (overlay-put overlay 'display (propertize "#+SRS" 'face 'org-habit-ready-face)))))))

(cl-defun org-srs-embed-remove-meta-overlays (&optional (start (point-min)) (end (point-max)))
  "Remove all metadata overlays between START and END."
  (remove-overlays start end 'category 'org-srs-embed-meta))

(cl-defun org-srs-embed-put-overlays (&optional (start (point-min)) (end (point-max)))
  "Apply both cloze and metadata overlays between START and END."
  (org-srs-embed-put-cloze-overlays start end)
  (org-srs-embed-put-meta-overlays start end))

(cl-defun org-srs-embed-remove-overlays (&optional (start (point-min)) (end (point-max)))
  "Remove both cloze and metadata overlays between START and END."
  (org-srs-embed-remove-cloze-overlays start end)
  (org-srs-embed-remove-meta-overlays start end))

(cl-defun org-srs-embed-update-overlays (&optional (start (point-min)) (end (point-max)))
  "Refresh both cloze and metadata overlays between START and END."
  (org-srs-embed-remove-overlays start end)
  (when org-srs-embed-overlay-mode
    (org-srs-embed-put-overlays start end)))

;;;###autoload
(define-minor-mode org-srs-embed-overlay-mode
  "Minor mode for prettifying the embedded Org-srs entries using overlays."
  :group 'org-srs-embed
  (cl-assert (eq major-mode 'org-mode))
  (if org-srs-embed-overlay-mode (org-srs-embed-put-overlays) (org-srs-embed-remove-overlays)))

(defun org-srs-embed-cloze (start end &optional hint id)
  "Create a cloze deletion between START and END with optional HINT and ID."
  (cl-destructuring-bind ({ } &aux (tag org-srs-embed-cloze-tag))
      (mapcar #'char-to-string org-srs-embed-cloze-brackets)
    (save-excursion
      (goto-char end)
      (if hint (insert "@@" tag ":" } { hint } } "@@")
        (insert "@@" tag ":" } } "@@"))
      (goto-char start)
      (if id (insert "@@" tag ":" { { id } { "@@") (insert "@@" tag ":" { { "@@")))))

;;;###autoload
(cl-defun org-srs-embed-cloze-dwim ()
  "Like `org-srs-item-cloze', but use the non-intrusive cloze markers."
  (interactive)
  (let ((org-srs-item-cloze-function #'org-srs-embed-cloze))
    (call-interactively #'org-srs-item-cloze-dwim)))

(cl-defmethod org-srs-item-cloze-interactively :around
  (type &context (org-srs-item-cloze-function (eql #'org-srs-embed-cloze)) &optional props)
  "Method for handling interactively creating embedded cloze deletions.

TYPE and PROPS specify the element for which the cloze deletion is created.
ORG-SRS-ITEM-CLOZE-FUNCTION must be `org-srs-embed-cloze', ensuring this method
only applies when creating embedded cloze deletions."
  (let* ((element (list type props))
         (start (copy-marker (org-element-begin element)))
         (end (copy-marker (org-element-end element))))
    (cl-call-next-method)
    (org-srs-embed-update-overlays start end)))

(cl-defun org-srs-embed-process-clozes (&optional (start (point-min)) (end (point-max)) (process-function #'cl-values))
  "Process embedded cloze deletions between START and END using PROCESS-FUNCTION."
  (save-excursion
    (cl-loop with start = (copy-marker start) and end = (copy-marker end)
             with ({ }) = org-srs-embed-cloze-brackets
             with regexp = (rx "@@srs:" (group (+? anychar)) "@@")
             initially (goto-char start)
             for cloze-start = (if (not (re-search-forward regexp end t))
                                   (cl-return count)
                                 (cl-assert (string-equal (match-string 1) (string { {)))
                                 (prog1 (save-excursion
                                          (goto-char (match-end 0))
                                          (point-marker))
                                   (replace-match "\\1")))
             for cloze-end = (if (not (re-search-forward regexp end t))
                                 (cl-assert nil)
                               (cl-assert (string-suffix-p (string } }) (match-string 1)))
                               (prog1 (save-excursion
                                        (goto-char (match-beginning 0))
                                        (point-marker))
                                 (replace-match "\\1")))
             for count from 0
             for cloze = (string-trim (buffer-substring cloze-start cloze-end))
             do (funcall process-function cloze))))

(cl-defun org-srs-embed-cloze-bounds (&optional (position (point)) start end)
  "Return the embedded cloze deletion's bounds at POSITION between START and END."
  (cl-destructuring-bind ({ } &aux (tag org-srs-embed-cloze-tag))
      (mapcar #'char-to-string org-srs-embed-cloze-brackets)
    (save-excursion
      (goto-char position)
      (let ((regexp-left (rx "@@" (literal tag) ":" (literal {) (literal {)))
            (regexp-right (rx (literal }) (literal }) "@@"))
            (start (or start (line-beginning-position))) (end (or end (line-end-position))))
        (let ((bl (or (and (save-excursion (re-search-backward regexp-left start t)) (match-beginning 0)) start))
              (br (or (and (save-excursion (re-search-backward regexp-right start t)) (match-end 0)) start))
              (fl (or (and (save-excursion (re-search-forward regexp-left end t)) (match-beginning 0)) end))
              (fr (or (and (save-excursion (re-search-forward regexp-right end t)) (match-end 0)) end)))
          (if (< (1- br) bl (point) fr (1+ fl)) (cons bl fr)
            (when (< (1- (point)) fl fr) (cons fl fr))))))))

(defun org-srs-embed-uncloze (start end)
  "Remove all embedded cloze deletions between START and END."
  (let ((start (copy-marker start)) (end (copy-marker end)))
    (cl-destructuring-bind ({ }) (mapcar #'char-to-string org-srs-embed-cloze-brackets)
      (prog1 (org-srs-embed-process-clozes
              start end
              (lambda (cloze)
                (cl-assert
                 (looking-back
                  (rx (literal {) (literal {) (literal cloze) (literal }) (*? anychar) (literal }))
                  (line-beginning-position)))
                (replace-match cloze)))
        (org-srs-embed-update-overlays start end)))))

(cl-defmethod org-srs-item-uncloze-interactively
  (type &context ((region-active-p) (eql nil)) (org-srs-item-uncloze-function (eql #'org-srs-embed-uncloze)) &optional props)
  "Method for handling interactively removing embedded cloze deletions.

TYPE and PROPS specify the element for which the cloze deletion is removed.
ORG-SRS-ITEM-UNCLOZE-FUNCTION must be `org-srs-embed-uncloze' and
REGION-ACTIVE-P must be nil, ensuring this method only applies when removing
embedded cloze deletions when no active region is present."
  (if-let ((bounds (org-srs-embed-cloze-bounds)))
      (cl-destructuring-bind (start . end) bounds
        (cl-call-next-method 'paragraph (cl-second (org-srs-item-cloze-region-element start end (list type props)))))
    (cl-call-next-method)))

;;;###autoload
(cl-defun org-srs-embed-uncloze-dwim ()
  "Like `org-srs-item-uncloze', but remove the non-intrusive cloze markers."
  (interactive)
  (let ((org-srs-item-uncloze-function #'org-srs-embed-uncloze))
    (call-interactively #'org-srs-item-uncloze-dwim)))

(cl-defun org-srs-embed-export-file-relative (&optional (root (file-name-concat org-directory "org-srs")))
  "Return the export file path for the current buffer relative to ROOT."
  (let ((file (buffer-file-name (current-buffer))))
    (cl-assert file)
    (let ((relative (file-relative-name file org-directory)))
      (cl-assert (not (string-prefix-p ".." relative)))
      (let ((absolute (expand-file-name relative root)))
        (make-directory (file-name-directory absolute) t)
        absolute))))

(org-srs-property-defcustom org-srs-embed-export-file #'org-srs-embed-export-file-relative
  "A variable that determines the file to which Org-srs entries are exported."
  :group 'org-srs-embed
  :type 'function)

(defvar org-srs-embed-export-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-srs-embed-export-finalize)
    (define-key map "\C-c\C-k" #'org-srs-embed-export-kill)
    map)
  "Keymap for `org-srs-embed-export-mode', a minor mode.")

;;;###autoload
(define-minor-mode org-srs-embed-export-mode
  "Minor mode for special key bindings in an Org-srs entry export buffer."
  :group 'org-srs-embed
  (cl-assert (eq major-mode 'org-mode))
  (if org-srs-embed-export-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-srs-embed-export-mode-map>Org-srs entry export buffer.  Finish `\\[org-srs-embed-export-finalize]', abort `\\[org-srs-embed-export-kill]'."))
    (setq-local header-line-format nil)))

(cl-defun org-srs-embed-export-clozes (&optional (start (point-min)) (end (point-max)))
  "Convert embedded cloze deletions to Org-srs cloze syntax from START to END."
  (let ((cloze-identifier (org-srs-item-cloze-identifier)))
    (org-srs-embed-process-clozes
     start end
     (let ((identifiers (make-hash-table)))
       (lambda (cloze)
         (let ((identifier (funcall cloze-identifier cloze)))
           (cl-assert (looking-back (rx "{{" (group (*? not-newline)) (or "}}" (and "}{" (group (*? not-newline)) "}}"))) (line-beginning-position)))
           (goto-char (match-beginning 0))
           (forward-char 1)
           (cl-assert (null (gethash identifier identifiers)))
           (setf (gethash identifier identifiers) cloze)
           (insert "{" (org-srs-item-princ-to-string identifier) "}")))))))

(defvar-local org-srs-embed-export-window-configuration nil
  "Saved window configuration during the export process.")

(defvar org-srs-embed-prepare-finalize-hook nil
  "Hook that is run before the finalization starts.")

(defun org-srs-embed-export-finish ()
  "Finalize and clean up the export process."
  (cl-assert org-srs-embed-export-mode)
  (kill-local-variable 'org-srs-embed-prepare-finalize-hook)
  (widen)
  (org-srs-embed-export-mode -1)
  (set-window-configuration (cl-shiftf org-srs-embed-export-window-configuration nil)))

(defun org-srs-embed-export-finalize ()
  "Finalize the Org-srs entry export process."
  (interactive)
  (cl-assert org-srs-embed-export-mode)
  (run-hooks 'org-srs-embed-prepare-finalize-hook)
  (org-srs-embed-export-finish))

(defun org-srs-embed-export-kill ()
  "Abort the current Org-srs entry export process."
  (interactive)
  (cl-assert org-srs-embed-export-mode)
  (widen)
  (org-back-to-heading)
  (org-cut-subtree)
  (org-srs-embed-export-finish))

(org-srs-property-defcustom org-srs-embed-export-item-type nil
  "Default item type used when exporting an Org-srs entry."
  :group 'org-srs-embed
  :type (cons 'choice (mapcar (apply-partially #'list 'const) (org-srs-item-types))))

(org-srs-property-defcustom org-srs-embed-export-cloze-type 'one-by-one
  "Whether the exported cloze deletions will be reviewed all at once or one by one."
  :group 'org-srs-embed
  :type '(choice
          (const :tag "One-by-one" one-by-one)
          (const :tag "All-at-once" all-at-once)))

(defun org-srs-embed-item-cloze-updater ()
  "Return the function to update cloze items."
  (cl-ecase (org-srs-embed-export-cloze-type)
    (one-by-one
     (apply-partially #'call-interactively #'org-srs-item-cloze-update))
    (all-at-once
     (lambda ()
       (goto-char (org-entry-beginning-position))
       (when (re-search-forward org-srs-item-cloze-regexp (org-entry-end-position) t)
         (ignore-errors (org-srs-item-new 'cloze)))))))

(cl-defgeneric org-srs-embed-export-entry (type props)
  (:method
   (type props)
   "Default method to export the element specified by TYPE and PROPS."
   (let* ((element (list type props))
          (buffer (current-buffer))
          (content (buffer-substring (org-element-begin element) (org-element-end element)))
          (window-configuration (current-window-configuration))
          (type (org-srs-embed-export-item-type))
          (cloze-updater (org-srs-embed-item-cloze-updater)))
     (with-current-buffer (switch-to-buffer (find-file-noselect (funcall (org-srs-embed-export-file))))
       (setf org-srs-embed-export-window-configuration window-configuration)
       (goto-char (point-max))
       (insert "* ")
       (org-return-and-maybe-indent)
       (org-narrow-to-subtree)
       (save-excursion
         (insert content)
         (delete-blank-lines))
       (indent-region (point) (point-max))
       (if (cl-plusp (org-srs-embed-export-clozes (point)))
           (setf type 'cloze)
         (cl-assert (not (eq type 'cloze))))
       (org-id-get-create)
       (org-back-to-heading)
       (org-end-of-line)
       (cl-assert (null org-srs-embed-prepare-finalize-hook))
       (cl-assert (not (local-variable-p 'org-srs-embed-prepare-finalize-hook)))
       (org-srs-item-add-hook-once
        'org-srs-embed-prepare-finalize-hook
        (lambda ()
          (org-back-to-heading)
          (if (eq type 'cloze)
              (funcall cloze-updater)
            (org-srs-item-new-interactively type))
          (let ((id (org-id-get))
                (front (cl-fifth (org-heading-components))))
            (cl-assert id) (cl-assert front)
            (with-current-buffer buffer
              (save-excursion
                (goto-char (org-element-begin element))
                (let ((start (point))
                      (indentation (rx bol (* blank))))
                  (if (not (and (looking-back indentation (line-beginning-position)) (looking-at indentation)))
                      (insert (format "@@comment:+SRS_EMBEDDED: [[id:%s][%s]]@@ " id front))
                    (let ((indentation (match-string 0)))
                      (open-line 1)
                      (insert indentation)
                      (insert (format "#+SRS_EMBEDDED: [[id:%s][%s]]" id front))))
                  (org-srs-embed-update-overlays start (point))))))))
       (org-srs-embed-export-mode +1))))
  (:documentation "Export the element specified by TYPE and PROPS as a new review item."))

(defun org-srs-embed-link-file-position ()
  "Retrieve the file and position from the link at point."
  (cl-assert (looking-at (rx "[" "[" (group (*? anychar)) "]" (*? anychar) "]")))
  (let ((link (match-string 1)))
    (cl-assert (string-prefix-p "id:" link))
    (let ((id (string-trim-left link (rx "id:"))))
      (cl-destructuring-bind (file . position) (org-id-find id)
        (cl-values file position)))))

(cl-defun org-srs-embed-remove-comments (&optional (start (point-min)) (end (point-max)))
  "Remove all inline comments from the region between START and END.

Return the number of comments removed."
  (save-excursion
    (cl-loop initially (goto-char start)
             while (re-search-forward (rx "@@comment:" (*? anychar) "@@" (* blank)) end t)
             do (replace-match "")
             sum 1)))

(defconst org-srs-embed-entry-header-regexp (rx (or (and "@@comment:+srs_embedded:" (* blank) (group-n 1 (*? anychar)) "@@")
                                                    (and "#+srs_embedded:" (* blank) (group-n 1 (*? anychar)) eol)))
  "Regular expression matching embedded entry headers.")

(cl-defgeneric org-srs-embed-update-entry (type props)
  (:method
   (type props)
   "Default method to update the entry with the element specified by TYPE and PROPS."
   (cl-assert (looking-at org-link-bracket-re))
   (let* ((front (match-string 2))
          (element (list type props))
          (content (buffer-substring (org-element-begin element) (org-element-end element)))
          (updater (org-srs-embed-item-cloze-updater)))
     (cl-multiple-value-bind (file position) (org-srs-embed-link-file-position)
       (with-current-buffer (find-file-noselect file)
         (save-excursion
           (save-restriction
             (widen)
             (goto-char position)
             (org-narrow-to-subtree)
             (when front (org-edit-headline front))
             (org-end-of-meta-data t)
             (delete-region (point) (point-max))
             (save-excursion (insert content))
             (indent-region (point) (point-max))
             (org-srs-embed-remove-comments (point))
             (org-srs-embed-export-clozes)
             (goto-char (point-max))
             (widen)
             (delete-blank-lines)
             (goto-char position)
             (funcall updater)))))))
  (:documentation "Update the exported entry with the element specified by TYPE and PROPS."))

(defun org-srs-embed-element-at-point ()
  "Like `org-element-at-point', but handle plain list elements specially."
  (let ((element (org-element-at-point)))
    (cl-case (org-element-type element)
      (plain-list (org-element-at-point (1+ (point))))
      (t element))))

(defun org-srs-embed-goto-link-to-entry ()
  "Move point to the link to the exported entry of the element at point."
  (cl-flet ((org-element-at-point () (org-srs-embed-element-at-point)))
    (let ((marker (point-marker))
          (current-start (org-element-begin (org-element-at-point)))
          (current-end (org-element-end (org-element-at-point)))
          (child-start (or (ignore-errors
                             (org-down-element)
                             (org-element-begin (org-element-at-point)))
                           (point-max))))
      (cl-loop for element = (if (ignore-errors (org-backward-element) t)
                                 (or (org-element-at-point) (cl-return))
                               (when fallback-start
                                 (goto-char fallback-start))
                               (cl-return))
               for previous-element-start = current-start then element-start
               for (element-start . element-end) = (cons (org-element-begin element) (org-element-end element))
               when (< element-end current-end)
               if (not (org-at-keyword-p)) return (goto-char (max element-end previous-element-start))
               else unless (save-excursion (goto-char (1- element-start)) (org-at-keyword-p))
               return (goto-char element-start)
               minimize element-start into fallback-start)
      (or (let ((case-fold-search t))
            (when (re-search-forward org-srs-embed-entry-header-regexp (min current-end child-start) t)
              (goto-char (match-beginning 1))
              (cons (match-beginning 0) (match-end 0))))
          (null (goto-char marker))))))

(defun org-srs-embed-open-entry ()
  "Open the link to an exported entry at point."
  (cl-multiple-value-bind (file position)
      (save-excursion
        (cl-assert (org-srs-embed-goto-link-to-entry))
        (org-srs-embed-link-file-position))
    (find-file file)
    (goto-char position)))

(org-srs-property-defcustom org-srs-embed-export-headline (rx (* blank) (? "- ") (group (*? anychar)) (char "：:"))
  "Default regexp used to extract the headlines of entries in batch export."
  :group 'org-srs-embed
  :type 'regexp)

;;;###autoload
(cl-defun org-srs-embed-dwim (arg)
  "Perform context-aware operations on the current element or embedded entry.

If point is on the header of an already exported entry, jump to the exported
entry.
If called interactively with a `\\[universal-argument]` or ARG greater than 1
when point is on the header, edit the link to the exported entry.
If point is within the content of an already exported entry, update the exported
entry (note that this will overwrite the previously exported content).
If point is on content that has not yet been exported, export the current Org
element as the entry content.
If called interactively with a `\\[universal-argument]` prefix or ARG greater
than 1 and there is an active region, perform a batch export on the region."
  (interactive "p")
  (require 'org-srs)
  (cl-flet* ((org-forward-element ()
               (cl-case (org-element-type (org-element-at-point))
                 (plain-list (forward-char) (org-forward-element))
                 (t (org-forward-element))))
             (org-element-at-point ()
               (org-srs-embed-element-at-point))
             (read-export-item-type ()
               (if (> arg 1) (intern (completing-read "Item type: " (org-srs-item-types) nil t))
                 (org-srs-embed-export-item-type)))
             (ensure-entry (&optional (element (org-element-at-point)))
               (cl-assert (not (cl-find (org-element-type element) '(comment keyword))))
               (save-excursion
                 (if-let ((bounds (org-srs-embed-goto-link-to-entry)))
                     (let ((element (org-element-copy element)))
                       (setf (org-element-begin element) (max (org-element-begin element) (cdr bounds)))
                       (apply #'org-srs-embed-update-entry element))
                   (org-srs-property-let ((org-srs-embed-export-item-type (read-export-item-type)))
                     (apply #'org-srs-embed-export-entry element))))))
    (cond
     ((and (> arg 1) (region-active-p))
      (let* ((front "")
             (export-hook (lambda ()
                            (when org-srs-embed-export-mode
                              (insert front)
                              (org-srs-embed-export-finalize)))))
        (org-srs-property-let ((org-srs-embed-export-headline (read-string "Headline regexp: " (org-srs-embed-export-headline))))
          (unwind-protect
              (cl-loop with start = (copy-marker (region-beginning)) and end = (copy-marker (region-end))
                       initially (add-hook 'org-srs-embed-export-mode-hook export-hook) (deactivate-mark) (goto-char start)
                       do (save-excursion
                            (re-search-forward (org-srs-embed-export-headline) (line-end-position))
                            (setf front (match-string 1))
                            (let ((element (org-element-copy (org-element-at-point))))
                              (setf (org-element-begin element) (point))
                              (ensure-entry element)))
                       while (progn (org-forward-element) (<= (point) (marker-position end))))
            (remove-hook 'org-srs-embed-export-mode-hook export-hook)))))
     ((looking-at-p org-srs-embed-entry-header-regexp)
      (if (<= arg 1)
          (org-srs-embed-open-entry)
        (save-excursion
          (org-srs-embed-goto-link-to-entry)
          (call-interactively #'org-insert-link))))
     ((region-active-p) (error "Exporting a region as an Org-srs entry is not yet implemented"))
     (t (ensure-entry)))))

(provide 'org-srs-embed)
;;; org-srs-embed.el ends here
