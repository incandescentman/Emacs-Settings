;;; org-srs.el --- A flexible spaced repetition system for Org-mode -*- lexical-binding: t -*-

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

;; Org-srs is a feature-rich and flexible spaced repetition system,
;; integrated into your learning and knowledge management workflow in
;; Org-mode.

;;; Code:

(require 'org-srs-property)
(require 'org-srs-time)
(require 'org-srs-table)
(require 'org-srs-log)
(require 'org-srs-query)

(require 'org-srs-item)
(require 'org-srs-item-card)
(require 'org-srs-item-cloze)

(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-review-strategy)
(require 'org-srs-review-cache)

(require 'org-srs-schedule)
(require 'org-srs-schedule-offset)
(require 'org-srs-schedule-step)
(require 'org-srs-schedule-fuzz)
(require 'org-srs-schedule-bury)

(require 'org-srs-algorithm)
(require 'org-srs-algorithm-fsrs)
(setf (default-value 'org-srs-algorithm) (or (default-value 'org-srs-algorithm) 'fsrs))

(require 'org-srs-stats)
(require 'org-srs-stats-interval)
(require 'org-srs-stats-history)

(require 'org-srs-child-frame)
(require 'org-srs-mouse)

(provide 'org-srs)
;;; org-srs.el ends here
