;;; org-srs-item-card.el --- Flashcard item type -*- lexical-binding: t -*-

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

;; This package implements flashcard review items in Org-srs, supporting
;; multiple representations for the front and back.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'custom)

(require 'org)

(require 'org-srs-property)
(require 'org-srs-item)
(require 'org-srs-review)

(defgroup org-srs-item-card nil
  "Flashcard type for review items."
  :group 'org-srs-item
  :prefix "org-srs-item-card-")

(cl-defmethod org-srs-item-review ((_type null) &rest args)
  "Method to set the default item type to `card' with ARGS passed as is."
  (apply #'org-srs-item-review 'card args))

(defun org-srs-item-card-regions ()
  "Return the front and back regions of the current flashcard as cons cells."
  (cl-flet ((org-entry-end-position (&aux (position (org-entry-end-position)))
              (if (= position (point-max)) (1+ position) position)))
    (let ((initalp t) (front nil) (back nil))
      (org-map-entries
       (lambda ()
         (unless (cl-shiftf initalp nil)
           (let ((heading (cl-fifth (org-heading-components))))
             (cond
              ((string-equal-ignore-case heading "Front")
               (setf front (cons (point) (1- (org-entry-end-position)))))
              ((string-equal-ignore-case heading "Back")
               (setf back (cons (point) (1- (org-entry-end-position)))))))))
       nil 'tree)
      (let ((heading (save-excursion
                       (org-back-to-heading)
                       (cons (point) (line-end-position))))
            (content (cons
                      (save-excursion
                        (org-end-of-meta-data t)
                        (point))
                      (1- (org-entry-end-position)))))
        (if front
            (if back
                (cl-values front back)
              (error "Unable to determine the back of the card"))
          (if back
              (cl-values content back)
            (cl-values heading content)))))))

(defun org-srs-item-card-put-ellipsis-overlay (start end)
  "Create an overlay from START to END that displays as ellipsis."
  (let ((overlay (make-overlay start end nil 'front-advance)))
    (overlay-put overlay 'category 'org-srs-item-card)
    (overlay-put overlay 'display "...")))

(cl-defun org-srs-item-card-remove-ellipsis-overlays (&optional (start (point-min)) (end (point-max)))
  "Remove all ellipsis overlays in the current buffer or between START and END."
  (remove-overlays start (1+ end) 'category 'org-srs-item-card))

(defun org-srs-item-card-show ()
  "Show the current flashcard entirely by unfolding the text and removing ellipses."
  (save-excursion
    (org-fold-show-subtree)
    (org-end-of-subtree)
    (org-srs-item-card-remove-ellipsis-overlays
     (org-entry-beginning-position) (point))))

(cl-defun org-srs-item-card-hide (&optional (side :back))
  "Hide either the front or back SIDE of the current flashcard."
  (org-srs-item-card-show)
  (cl-ecase side
    (:front
     (cl-destructuring-bind (beg . end) (cl-nth-value 0 (org-srs-item-card-regions))
       (cond
        ((= (save-excursion (org-back-to-heading) (point)) beg)
         (save-excursion
           (goto-char beg)
           (re-search-forward org-outline-regexp-bol)
           (org-srs-item-card-put-ellipsis-overlay (point) end)))
        ((save-excursion (goto-char beg) (org-at-heading-p))
         (save-excursion (goto-char beg) (org-fold-hide-entry)))
        (t (org-srs-item-card-put-ellipsis-overlay beg end)))))
    (:back
     (cl-destructuring-bind (beg . end) (cl-nth-value 1 (org-srs-item-card-regions))
       (if (save-excursion (goto-char beg) (org-at-heading-p))
           (save-excursion (goto-char beg) (org-fold-hide-entry))
         (org-srs-item-card-put-ellipsis-overlay beg end))))))

(cl-defmethod org-srs-item-review ((type (eql 'card)) &rest args)
  "Method to review an item of TYPE `card' with ARGS."
  (cl-destructuring-bind (&optional (side 'front)) args
    (org-srs-item-narrow)
    (org-srs-item-card-hide (cl-ecase side (front :back) (back :front)))
    (org-srs-item-add-hook-once 'org-srs-item-after-confirm-hook #'org-srs-item-card-show)
    (apply (org-srs-item-confirm) type args)))

(cl-defmethod org-srs-item-new ((_type (eql 'card)) &rest args)
  "Method for creating a new flashcard with ARGS."
  (apply #'org-srs-item-new nil args))

(cl-defmethod org-srs-item-new ((_type (eql 'card-reversible)) &rest args)
  "Method for creating a reversible flashcard with ARGS."
  (cl-assert (null args))
  (org-srs-item-new '(card front))
  (org-srs-item-new '(card back)))

(cl-defmethod org-srs-item-new ((_type (eql 'card-reversed)) &rest args)
  "Method for creating a reversed flashcard with ARGS."
  (cl-assert (null args))
  (org-srs-item-new '(card back)))

(provide 'org-srs-item-card)
;;; org-srs-item-card.el ends here
