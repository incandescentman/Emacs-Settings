;;; org-srs-algorithm.el --- Interface for algorithms -*- lexical-binding: t -*-

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

;; This package serves as the algorithm interface for Org-srs, allowing
;; the core to be independent of specific algorithms.

;;; Code:

(require 'cl-generic)
(require 'custom)

(require 'org-srs-property)

(defgroup org-srs-algorithm nil
  "Interface for spaced repetition algorithms."
  :group 'org-srs
  :prefix "org-srs-algorithm")

(org-srs-property-defcustom org-srs-algorithm nil
  "Spaced repetition algorithm used for scheduling."
  :group 'org-srs-algorithm
  :type 'sexp)

(cl-defgeneric org-srs-algorithm-ensure (object &rest args)
  (:method
   (object &rest _args)
   "Default method that returns OBJECT as is."
   object)
  (:documentation "Convert OBJECT with ARGS into an object usable for `org-srs-algorithm-repeat'."))

(defun org-srs-algorithm-current ()
  "Return the current algorithm suitable for `org-srs-algorithm-repeat'."
  (apply #'org-srs-algorithm-ensure (ensure-list (org-srs-algorithm))))

(cl-defgeneric org-srs-algorithm-repeat (algorithm args)
  (:documentation "Pass the algorithm state to ALGORITHM through alist ARGS.

Return the new algorithm state alist to be used for the next spaced repetition.
When ARGS is nil, return the initial algorithm state."))

(provide 'org-srs-algorithm)
;;; org-srs-algorithm.el ends here
