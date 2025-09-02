;;; org-srs-mouse.el --- Mouse/touchscreen support for review interactions -*- lexical-binding: t -*-

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

;; This package provides mouse and touchscreen interaction support for
;; reviewing, allowing confirming and rating items through mouse events
;; (clicks/touches).

;;; Code:

(require 'cl-lib)

(require 'org-srs-child-frame)
(require 'org-srs-review)
(require 'org-srs-item)
(require 'org-srs-stats-interval)

(cl-defun org-srs-mouse-bottom-panel-hide (&optional (frame (org-srs-child-frame 'org-srs-mouse-bottom-panel)))
  "Hide bottom panel child FRAME if it is visible."
  (when (frame-visible-p frame)
    (make-frame-invisible frame)))

(cl-defun org-srs-mouse-string-pad-pixel (string &optional (width (string-pixel-width string)) (height (line-pixel-height)))
  "Return STRING padded to WIDTH and HEIGHT in pixels."
  (let ((space-width (/ (- width (string-pixel-width string)) 2)))
    (concat
     (propertize " " 'display `(space :width (,space-width) :height (,height)))
     string
     (propertize " " 'display `(space :width (,space-width) :height (,height))))))

(defconst org-srs-mouse-bottom-panel-button-faces
  (cl-loop for rating in org-srs-review-ratings
           collect (cons rating (cl-ecase rating (:easy 'homoglyph) (:good 'success) (:hard 'warning) (:again 'error))))
  "Alist mapping review ratings to their corresponding button faces.")

(cl-defun org-srs-mouse-bottom-panel-show (labels
                                           &key
                                           (faces (cl-subst-if 'default (lambda (elem) (and elem (symbolp elem))) labels))
                                           (callback #'ignore))
  "Show a bottom panel with interactive buttons.

LABELS is a list of button labels to display.
FACES is a list of faces for each button.
CALLBACK is a function called with the pressed button's label."
  (cl-assert labels) (cl-assert faces)
  (let* ((labels-list (if (cl-every #'symbolp labels) (list labels) labels))
         (faces-list (if (cl-every #'symbolp faces) (list faces) faces))
         (child-frame (org-srs-child-frame 'org-srs-mouse-bottom-panel :size (/ (length labels-list) 16.0)))
         (current-buffer (current-buffer)))
    (with-selected-frame (make-frame-visible child-frame)
      (cl-assert (not (eq current-buffer (current-buffer))))
      (cl-loop with inhibit-read-only = t
               initially (erase-buffer)
               for labels in labels-list
               for faces in faces-list
               for button-width = (- (/ (frame-pixel-width child-frame) (float (length labels))) (* (string-pixel-width "​") 2))
               for button-height = (/ (frame-pixel-height child-frame) (length labels-list))
               do (cl-loop for label in labels
                           for face in faces
                           do
                           (insert "​")
                           (insert-text-button
                            (org-srs-mouse-string-pad-pixel (capitalize (string-trim-left (format "%s" label) ":")) button-width button-height)
                            'face `((:foreground ,(face-foreground face))
                                    (:inherit custom-button))
                            'action (let ((label label)) (lambda (&optional _) (select-frame (frame-parent child-frame)) (funcall callback label))))
                           (insert "​")
                           finally (insert "\n"))
               finally (goto-char (point-min))))))

;;;###autoload
(define-minor-mode org-srs-mouse-mode
  "Minor mode to enable mouse/touchscreen input support for Org-srs review actions."
  :group 'org-srs-mouse
  :global t
  (if org-srs-mouse-mode
      (progn
        (add-hook 'window-selection-change-functions #'org-srs-mouse-mode-update-panels)
        (add-hook 'window-buffer-change-functions #'org-srs-mouse-mode-update-panels)
        (add-hook 'window-size-change-functions #'org-srs-mouse-mode-update-panels))
    (remove-hook 'window-selection-change-functions #'org-srs-mouse-mode-update-panels)
    (remove-hook 'window-buffer-change-functions #'org-srs-mouse-mode-update-panels)
    (remove-hook 'window-size-change-functions #'org-srs-mouse-mode-update-panels)
    (setf (org-srs-child-frames 'org-srs-mouse-bottom-panel) nil)))

(defun org-srs-mouse-show-intervals-in-minibuffer ()
  "Display review intervals for the current review item in the minibuffer."
  (when (and org-srs-mouse-mode (org-srs-reviewing-p) (not (org-srs-item-confirm-pending-p)))
    (let ((item org-srs-review-item))
      (cl-loop with message-log-max = nil
               with width = (window-pixel-width (minibuffer-window))
               and height = (with-minibuffer-selected-window (line-pixel-height))
               with label-width = (/ width 4)
               for (rating interval) on (org-srs-item-with-current item
                                          (if (org-srs-table-goto-column 'rating)
                                              (org-srs-stats-intervals)
                                            (cl-return)))
               by #'cddr
               concat (propertize
                       (org-srs-mouse-string-pad-pixel
                        (cl-loop for (amount unit has-next-p) on (org-srs-time-seconds-desc interval) by #'cddr
                                 for i from 1
                                 concat (format "%d%.1s" amount (string-trim-left (symbol-name unit) ":"))
                                 while (< i 2)
                                 when has-next-p
                                 concat " ")
                        label-width height)
                       'face (alist-get rating org-srs-mouse-bottom-panel-button-faces))
               into message
               finally (message "%s" message)))))

(defun org-srs-mouse-mode-update-panels-1 ()
  "Update panels based on the current review state."
  (if (and org-srs-mouse-mode (eq major-mode 'org-mode) org-srs-review-item)
      (if-let ((confirm-command (org-srs-item-confirm-pending-p)))
          (org-srs-mouse-bottom-panel-show
           '(continue)
           :callback (lambda (continue)
                       (cl-assert (eq continue 'continue))
                       (call-interactively confirm-command)))
        (org-srs-mouse-bottom-panel-show
         org-srs-review-ratings
         :faces (mapcar (lambda (rating) (alist-get rating org-srs-mouse-bottom-panel-button-faces)) org-srs-review-ratings)
         :callback (lambda (rating)
                     (cl-assert (org-srs-reviewing-p))
                     (org-srs-review-rate rating))))
    (org-srs-mouse-bottom-panel-hide)))

(defun org-srs-mouse-mode-update-panels (&rest _)
  "Handle panel updates when the window configuration changes."
  (if (org-srs-child-frame-p)
      (with-selected-frame (org-srs-child-frame-root)
        (org-srs-mouse-mode-update-panels-1))
    (org-srs-mouse-mode-update-panels-1)))

(add-hook 'org-srs-item-before-confirm-hook #'org-srs-mouse-mode-update-panels)
(add-hook 'org-srs-item-after-confirm-hook #'org-srs-mouse-mode-update-panels)
(add-hook 'org-srs-item-after-confirm-hook #'org-srs-mouse-show-intervals-in-minibuffer)

(provide 'org-srs-mouse)
;;; org-srs-mouse.el ends here
