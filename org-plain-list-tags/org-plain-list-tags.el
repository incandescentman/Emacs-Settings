;;; org-inline-tags.el --- Insert and search for inline tags in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Your Name

;; Author: Jay Dixit <jaydixit.work@gmail.com>
;; URL: https://github.com/incandescentman/org-inline-tags
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.1"))
;; Keywords: org, inline, tags, plain lists

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a simple way to insert and search for inline tags in org-mode.

;;; Code:

(require 'org)

(defface org-plain-list-tags-face
  '((t (:foreground "orange" :weight bold)))
  "Face for custom inline tags in plain list items.")

(font-lock-add-keywords 'org-mode
  '(("#\\(\\w+\\)" 1 'org-plain-list-tags-face)))

(defun org-plain-list-tags-search (tag)
  (org-search-view nil (concat "#" tag)))

(defun org-plain-list-tags-search-buffer (tag)
  (consult-line (concat "#" tag)))

(defun org-plain-list-tags-search-project-wide (tag)
  "Search for inline TAG project-wide using consult-git-grep."
  (interactive "sEnter tag to search for: ")
  (consult-git-grep (concat "\\#" tag)))

(defun org-plain-list-tags-insert ()
  (interactive)
  (let* ((tag-alist '((?r . "review")
                      (?b . "book")
                      (?t . "todo")
                      (?u . "urgent")
                      (?p . "tweet")
                      (?i . "insight")
                      (?c . "cook-ideas-over-time")))
         (selected-key (read-char "Choose a tag:\n
r: review
b: book
t: todo
u: urgent
p: tweet
i: insight
c: cook-ideas-over-time\n")))
    (setq selected-tag (cdr (assoc selected-key tag-alist)))
    (if selected-tag
        (insert (format " #%s" selected-tag))
      (message "Invalid tag selection"))))

(provide 'org-inline-tags)

;;; org-inline-tags.el ends here

