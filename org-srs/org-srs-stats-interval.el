;;; org-srs-stats-interval.el --- Repetition interval calculation -*- lexical-binding: t -*-

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

;; This package is used to calculate the time interval until the next
;; review for a review item under different ratings.

;;; Code:

(require 'cl-lib)
(require 'custom)

(require 'org-srs-stats)

(defgroup org-srs-stats-interval nil
  "Calculate review intervals ahead of rating."
  :group 'org-srs-stats
  :prefix "org-srs-stats-interval-")

(cl-defun org-srs-stats-intervals (&optional (ratings org-srs-review-ratings))
  "Calculate the time intervals until the next review for RATINGS."
  (org-srs-stats-with-rating-simulator (rate)
    (cl-loop for rating in ratings nconc (list rating (rate rating)))))

(defun org-srs-stats-interval (rating)
  "Return the time interval until the next review for RATING."
  (cl-getf (org-srs-stats-intervals (list rating)) rating))

(provide 'org-srs-stats-interval)
;;; org-srs-stats-interval.el ends here
