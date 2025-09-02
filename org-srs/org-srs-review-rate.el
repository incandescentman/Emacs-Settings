;;; org-srs-review-rate.el --- Rating facilities -*- lexical-binding: t -*-

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

;; This package provides rating facilities for Org-srs review items,
;; including rating commands and hooks for customizing rating behavior.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'custom)

(require 'org-srs-time)
(require 'org-srs-item)
(require 'org-srs-review)

(defgroup org-srs-review-rate nil
  "Rating facilities for Org-srs review items."
  :group 'org-srs-review
  :prefix "org-srs-review-rat")

(defvar org-srs-reviewing-p)

(defvar org-srs-review-item)

(defvar org-srs-review-rating)

(cl-eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst org-srs-review-ratings '(:easy :good :hard :again)
    "List of rating keywords available for reviewing items."))

(defvar org-srs-review-before-rate-hook nil
  "Hook run before a review item is rated.")

(defvar org-srs-review-after-rate-hook nil
  "Hook run after a review item has been rated.")

(cl-defun org-srs-review-rate (rating &rest args)
  "Rate the current review item specified by ARGS with RATING."
  (let ((item (or args org-srs-review-item)))
    (if org-srs-review-item
        (let ((org-srs-reviewing-p (org-srs-reviewing-p)))
          (prog2 (let ((org-srs-review-rating rating))
                   (run-hooks 'org-srs-review-before-rate-hook))
              (org-srs-item-with-current item
                (org-srs-table-goto-starred-line)
                (unless args
                  (cl-assert
                   (time-less-p
                    (org-srs-timestamp-time (org-srs-table-field 'timestamp))
                    (org-srs-time-tomorrow))))
                (apply #'org-srs-item-repeat (cl-nth-value 0 (org-srs-item-at-point)) (when rating (list :rating rating))))
            (let ((org-srs-review-rating rating))
              (run-hooks 'org-srs-review-after-rate-hook)
              (run-hooks 'org-srs-review-continue-hook))))
      (cl-assert args)
      (let ((org-srs-review-item args))
        (apply #'org-srs-review-rate rating args)))))

(defmacro org-srs-review-define-rating-commands ()
  "Define commands for each rating in `org-srs-review-ratings'."
  `(progn . ,(cl-loop for rating in org-srs-review-ratings
                      for rating-name = (string-trim (symbol-name rating) ":")
                      collect `(defun ,(intern (format "%s%s" 'org-srs-review-rate- rating-name)) ()
                                 ,(format "Rate the item being reviewed as %s." rating-name)
                                 (interactive)
                                 (require 'org-srs)
                                 (cl-assert (org-srs-reviewing-p))
                                 (org-srs-review-rate ,rating)))))

;;;###autoload (autoload 'org-srs-review-rate-easy "org-srs-review" "Rate the item being reviewed as easy." t)
;;;###autoload (autoload 'org-srs-review-rate-good "org-srs-review" "Rate the item being reviewed as good." t)
;;;###autoload (autoload 'org-srs-review-rate-hard "org-srs-review" "Rate the item being reviewed as hard." t)
;;;###autoload (autoload 'org-srs-review-rate-again "org-srs-review" "Rate the item being reviewed as again." t)
(org-srs-review-define-rating-commands)

(defun org-srs-review-rate-cleanup-hooks ()
  "Clean up review rating hooks."
  (kill-local-variable 'org-srs-review-before-rate-hook)
  (kill-local-variable 'org-srs-review-after-rate-hook))

(add-hook 'org-srs-review-continue-hook #'org-srs-review-rate-cleanup-hooks)

(provide 'org-srs-review-rate)
;;; org-srs-review-rate.el ends here
