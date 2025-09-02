;;; org-srs-child-frame.el --- Child frame utilities -*- lexical-binding: t -*-

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

;; This package offers convenient interfaces to create, display and
;; manage child frames.

;;; Code:

(require 'cl-lib)

(defvar org-srs-child-frames nil
  "Alist mapping parent frames and child frame names to child frames.")

(cl-defun org-srs-child-frame-p (&optional (frame (selected-frame)))
  "Return the parent frame and name if FRAME is a child frame."
  (cl-values-list (car (cl-rassoc frame org-srs-child-frames :test #'eq))))

(cl-defun org-srs-child-frame-root (&optional (frame (selected-frame)))
  "Return the root ancestor frame of FRAME by traversing parent frames."
  (if-let ((parent (cl-nth-value 0 (org-srs-child-frame-p frame))))
      (progn
        (cl-assert (eq parent (frame-parent frame)))
        (cl-assert (null (frame-parent parent)))
        (org-srs-child-frame-root parent))
    frame))

(cl-defun org-srs-child-frames-1 (&optional (name nil namep))
  "Return a filtered alist of child frames from variable `org-srs-child-frames'.

If NAME is provided, only return child frames whose name matches NAME.
Otherwise, return the value of variable `org-srs-child-frames' as is."
  (if namep (cl-remove name org-srs-child-frames :key #'cadar :test-not #'eq) org-srs-child-frames))

(defun org-srs-child-frames (&rest args)
  "Return a filtered list of child frames matching ARGS.

ARGS is passed to `org-srs-child-frames-1' which determines the filtering
behavior."
  (mapcar #'cdr (apply #'org-srs-child-frames-1 args)))

(cl-defmethod (setf org-srs-child-frames) (value &rest args)
  "Set the filtered child frame list matching ARGS to VALUE.

ARGS is passed to `org-srs-child-frames-1' which determines the filtering
behavior.

This function can only be used to delete child frames, meaning any existing
child frames not present in VALUE will be removed."
  (cl-assert (null value))
  (mapc #'delete-frame (apply #'org-srs-child-frames args))
  (setf org-srs-child-frames (cl-nset-difference org-srs-child-frames (apply #'org-srs-child-frames-1 args))))

(cl-defun org-srs-child-frame (name
                               &key
                               (parent (org-srs-child-frame-root))
                               (window (frame-selected-window parent))
                               (size (/ 16.0))
                               (position :bottom)
                               (buffer (get-buffer-create (format " *org-srs-child-frame %x/%s*" (sxhash-eq parent) name))))
  "Create or retrieve a child frame with NAME relative to PARENT frame.

PARENT specifies the parent frame, which defaults to the root frame.
WINDOW specifies the window in PARENT used for positioning.
SIZE determines the child frame dimensions as a fraction or cons of pixels.
POSITION specifies the child frame position relative to WINDOW.
BUFFER specifies the buffer to display in the child frame.

This function ensures the child frame has no decorations and automatically
manages frame size and position while displaying BUFFER."
  (cl-assert (null (frame-parent parent)))
  (cl-destructuring-bind (parent-left parent-top parent-right parent-bottom)
      (window-inside-absolute-pixel-edges window)
    (let ((parent-width (- parent-right parent-left))
          (parent-height (- parent-bottom parent-top)))
      (cl-multiple-value-bind (child-width child-height)
          (cl-etypecase size
            (float (cl-values parent-width (truncate (* parent-height size))))
            (cons (cl-values (car size) (cdr size))))
        (cl-multiple-value-bind (child-left child-top)
            (cl-etypecase position
              ((eql :bottom) (cl-values parent-left (- parent-bottom child-height)))
              (cons (cl-values (car position) (cdr position))))
          (cl-flet ((make-child-frame ()
                      (make-frame
                       `((name . "org-srs-child-frame")
                         (parent-frame . ,parent)
                         (no-accept-focus . t)
                         (visibility . nil)
                         (border-width . 0)
                         (minibuffer . nil)
                         (menu-bar-lines . 0)
                         (tool-bar-lines . 0)
                         (tab-bar-lines . 0)
                         (unsplittable . t)
                         (min-height . 0)
                         (min-width . 0)
                         (cursor-type . nil)
                         (no-other-frame . t)
                         (undecorated . t)
                         (no-special-glyphs . t)
                         (skip-taskbar . t)
                         (desktop-dont-save . t)))))
            (let ((frame (let ((key (list parent name)))
                           (or #1=(alist-get key org-srs-child-frames nil t #'equal)
                               (let ((frame (make-child-frame)))
                                 (if-let ((new-frame #1#))
                                     (progn (delete-frame frame) new-frame)
                                   (setf #1# frame)))))))
              (set-frame-size frame child-width child-height t)
              (set-frame-position frame child-left child-top)
              (set-window-buffer (frame-selected-window frame) buffer)
              (cl-assert (eq (window-buffer (frame-selected-window frame)) buffer))
              (setf (buffer-local-value 'mode-line-format buffer) nil
                    (buffer-local-value 'truncate-lines buffer) t
                    (buffer-local-value 'buffer-read-only buffer) t
                    (buffer-local-value 'cursor-type buffer) nil)
              frame)))))))

(provide 'org-srs-child-frame)
;;; org-srs-child-frame.el ends here
