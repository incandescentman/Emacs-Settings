;;; notes-list.el --- Notes list -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/notes-list
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Notes list collects notes in user-defined directories and populate a buffer with a two-lines summary for each note. To do so, notes are parsed such as to extract title, icon, date, summary and tags. A typical org note header is thus
;;
;;  #+TITLE:    Emacs hacking
;;  #+DATE:     2023-03-17
;;  #+FILETAGS: HACK EMACS CODE
;;  #+SUMMARY:  Notes about emacs hacking ideas
;;  #+ICON:     bootstrap/journal-code
;;
;; Icon are built using the svg-lib library and syntax is
;; "collection/name" where collection is one one "simple",
;; "bootstrap", "material" or "octicons". For available icons, please
;; refere to the svg-lib documentation.

;;; News
;;
;;  Version 0.1.0
;;  Initial version
;;
;;; Code:
(require 'stripes)
(require 'svg-lib)
(require 'svg-tag-mode)

(use-package nano-theme
  :ensure nil
  :defer t
  :quelpa (nano-theme
           :fetcher github
           :repo "rougier/nano-theme"))

(require 'cl-lib)

(defgroup notes-list nil
  "Note list"
  :group 'convenience)

(defcustom notes-list-directories '("/Users/jay/nd")
  "List of directories where to search notes"
  :type '(repeat directory)
  :group 'notes-list)

(defcustom notes-list-sort-function #'notes-list-compare-modification-time
  "Criterion for sorting notes"
  :type '(choice (const :tag "Title"             notes-list-compare-title)
                 (const :tag "Access time"       notes-list-compare-access-time)
                 (const :tag "Creation time"     notes-list-compare-creation-time)
                 (const :tag "Modification time" notes-list-compare-modification-time))
  :group 'notes-list)

(defcustom notes-list-display-icons t
  "Display icon on (left)"
  :type 'boolean
  :group 'notes-list)

(defcustom notes-list-display-tags t
  "Display tags (top right)"
  :type 'boolean
  :group 'notes-list)

(defcustom notes-list-display-date t
  "Display date (bottom right)"
  :type 'boolean
  :group 'notes-list)

(defface notes-list-face-title
  '((t (:inherit (nano-salient nano-strong))))
  "Face for notes title"
  :group 'notes-list)

(defface notes-list-face-tags
  '((t (:inherit nano-faded)))
  "Face for notes tags"
  :group 'notes-list)

(defface notes-list-face-summary
  '((t (:inherit nano-default)))
  "Face for notes summary"
  :group 'notes-list)

(defface notes-list-face-time
  '((t (:inherit nano-faded)))
  "Face for notes time"
  :group 'notes-list)



(defvar notes-list--icons nil
  "Icons cache")

(defun notes-list--make-icon (icon)
  "Make an icon from ICON name"

  (unless (assoc icon notes-list--icons)
    (let* ((image (if (file-regular-p icon)
                      (create-image icon)
                    (let ((collection (nth 0 (split-string icon "/")))
                          (name (nth 1 (split-string icon "/"))))
                       (svg-lib-icon name nil :collection collection
                                              :stroke 0
                                              :scale .75
                                              :padding 0))))
            (dy (frame-char-height))
            (dx (frame-char-width))
            (height (frame-char-height))
            (img-height (cdr (image-size image t)))
            (img-height (* (+ (/ height dy) 1) dy))
            (char-width (+ (/ img-height dx) 1))
            (img-width (* char-width dx))
            (thumbnail (cons (car image) (cl-copy-list (cdr image)))))
      (plist-put (cdr thumbnail) :height img-height)
      (plist-put (cdr thumbnail) :width nil)
      (plist-put (cdr thumbnail) :ascent 80)
      (let ((icon-1 (propertize (make-string char-width ? )
                                'display (list (list 'slice 0  0 img-width dy) thumbnail)
                                'line-height t))
            (icon-2 (propertize (make-string char-width ? )
                                'display (list (list 'slice 0  dy img-width dy) thumbnail)
                                'line-height t)))
        (setq notes-list--icons
              (add-to-list 'notes-list--icons
                           (cons icon (cons icon-1 icon-2)))))))
  (cdr (assoc icon notes-list--icons)))


(defvar notes-list--svg-tags nil
  "SVG tags cache")

(defun notes-list--make-tag (tag)

  (let ((svg-tag (if (string-equal tag "INBOX")
                     (svg-tag-make tag :face 'nano-salient :inverse t)
                   (svg-tag-make tag :face 'default))))
  (propertize (concat tag " ") 'display svg-tag)))

(defun notes-list-format-tags (tags)
  "Transform a list of tags into a SVG tags string"

  (mapconcat (lambda (tag)
               (unless (assoc tag notes-list--svg-tags)
                 (setq notes-list--svg-tags
                       (add-to-list 'notes-list--svg-tags
                                    (cons tag (notes-list--make-tag tag)))))
               (cdr (assoc tag notes-list--svg-tags)))
             tags " "))

(defun notes-list-format-title (title)
  (propertize title 'face 'notes-list-face-title))

(defun notes-list-format-time (time)
  (let ((time (format-time-string "%B %d, %Y" time)))
    (propertize time 'face 'notes-list-face-time)))

(defun notes-list-format-summary (summary)
  (propertize summary 'face 'notes-list-face-summary))


(defun notes-list-format (note)
  "This function format a note. Result is a two-lines string with
title at top left, time at top-right, summary at bottom left and
tags at bottom right. If TITLE or SUMMARY is too long, it is
truncated."

  (let* ((window (get-buffer-window (notes-list-buffer)))
         (width (- (window-width window) 1))
         (icon (or (cdr (assoc "ICON" note)) "note-outline"))
         (icon (notes-list--make-icon icon))
         (filename (cdr (assoc "FILENAME" note)))

         (tags (or (cdr (assoc "TAGS" note)) ""))
         (tags (notes-list-format-tags tags))
         (tags (if notes-list-display-tags
                   tags
                 ""))

         (time (or (cdr (assoc "TIME-MODIFICATION" note)) ""))
         (time (notes-list-format-time time))
         (time (if notes-list-display-date
                   time
                 ""))

         (title (or (cdr (assoc "TITLE" note)) ""))
         (title (notes-list-format-title title))
         (title (if notes-list-display-icons
                    (concat (car icon) " " title)
                  title))
         (title (concat (propertize " " 'display '(raise 0.5)) title))
         (title (truncate-string-to-width
                    title
                    (- width (length time) 1) nil nil "…"))

         (summary (or (cdr (assoc "SUMMARY" note)) ""))
         (summary (notes-list-format-summary summary))
         (summary (if notes-list-display-icons
                      (concat (cdr icon) " " summary)
                    summary))

         (top-filler (propertize " " 'display
                                 `(space :align-to (- right ,(length time) 1))))
         (summary (concat (propertize " " 'display '(raise -0.5))
                              summary))
         (summary (truncate-string-to-width summary
                             (- width (length tags) 1) nil nil "…"))
         (bottom-filler (propertize " " 'display
                                    `(space :align-to (- right ,(length tags) 1)))))
    (propertize (concat title top-filler time
                        (propertize " " 'display "\n")
                        summary bottom-filler tags)
                'filename filename)))

(defun notes-list-parse-org-note (filename)
  "Parse an org file and extract title, date, summary and tags that
need to be defined at top level as keywords."

  (let ((keep (find-buffer-visiting filename)))
    (with-current-buffer (find-file-noselect filename)
      (let* ((attributes (file-attributes filename))
             (size (file-attribute-size attributes))
             (access-time (file-attribute-access-time attributes))
             (modification-time (file-attribute-modification-time attributes))
             (info (org-collect-keywords '("TITLE"
                                           "ICON"
                                           "DATE"
                                           "SUMMARY"
                                           "FILETAGS")))
           (title (cadr (assoc "TITLE" info)))
           (icon (cadr (assoc "ICON" info)))
           (date (cadr (assoc "DATE" info)))
           (time (parse-time-string date))
           (time (let ((n 0))
                   (mapcar (lambda (x)
                             (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))
           (time (encode-time time))
           (summary (cadr (assoc "SUMMARY" info)))
           (tags (cadr (assoc "FILETAGS" info)))
           (tags (split-string tags)))
        (unless keep (kill-buffer))
        (list (cons "FILENAME" filename)
              (cons "TITLE" title)
              (cons "ICON" icon)
              (cons "TIME-CREATION" time)
              (cons "TIME-MODIFICATION" modification-time)
              (cons "TIME-ACCESS" access-time)
              (cons "SUMMARY" summary)
              (cons "TAGS" tags))))))


(defun notes-list-compare-creation-time (note-1 note-2)
  (time-less-p (cdr (assoc "TIME-CREATION" note-1))
               (cdr (assoc "TIME-CREATION" note-2))))

(defun notes-list-compare-access-time (note-1 note-2)
  (time-less-p (cdr (assoc "TIME-ACCESS" note-1))
               (cdr (assoc "TIME-ACCESS" note-2))))

(defun notes-list-compare-modification-time (note-1 note-2)
  (time-less-p (cdr (assoc "TIME-MODIFICATION" note-1))
               (cdr (assoc "TIME-MODIFICATION" note-2))))

(defun notes-list-compare-title (note-1 note-2)
  (string-lessp (cdr (assoc "TITLE" note-1))
                (cdr (assoc "TITLE" note-2))))

(defun notes-list-note-p (filename)
  "Return t if FILENAME names a note file."

  (file-regular-p filename))

(defvar notes-list--notes nil
  "List of collected notes")

(defun notes-list-collect-notes ()
  "Collect notes from note directories"

  (let ((notes nil)
        (recentf-list-saved recentf-list))
    (dolist (directory notes-list-directories)
      (dolist (filename (directory-files directory t ".*\\.org"))
        (when (notes-list-note-p filename)
          (let ((note (notes-list-parse-org-note filename)))
            (setq notes (add-to-list 'notes note))))))
    (setq notes-list--notes notes)
    (setq recentf-list recentf-list-saved))
  notes-list--notes)

(defun notes-list-quit ()
  (interactive)
  (kill-buffer))

(defun notes-list-next-note ()
  (interactive)
  (forward-line 1)
  (when (eq (point) (point-max))
    (goto-char (point-min))))

(defun notes-list-prev-note ()
  (interactive)
  (if (eq (point) (point-min))
      (goto-char (- (point-max) 1))
    (forward-line -1)))

(defun notes-list-open-note ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename)))
    (find-file filename)))

(defun notes-list-open-note-other-window ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename)))
    (other-window 1)
    (find-file filename)))

(defun notes-list-show-note-other-window ()
  (interactive)
  (let ((filename (get-text-property (point) 'filename)))
    (with-selected-window (next-window)
    (find-file filename))))

(defvar notes-list--buffer-width nil
  "Notes list buffer width")

(defun notes-list--resize-hook (frame)
  "Refresh notes list if necessary"

  (when-let* ((window (get-buffer-window (notes-list-buffer))))
    (let ((window-width (window-width window)))
      (unless (eq window-width notes-list--buffer-width)
        (notes-list-refresh))
      (setq notes-list--buffer-width window-width))))

(defun notes-list-reload ()
  "Rebuild the note list"

  (interactive)
  (notes-list-collect-notes)
  (notes-list-refresh))

(defun notes-list-refresh ()
  "Rebuild the note list if necessary (no reload)"

  (interactive)
  (setq notes-list--notes
        (reverse (sort notes-list--notes
                       #'notes-list-compare-modification-time)))
  (with-current-buffer (notes-list-buffer)
    (let ((filename (get-text-property (point) 'filename)))
      (beginning-of-line)
      (let ((line (count-lines 1 (point)))
            (inhibit-read-only t))
        (erase-buffer)
        (insert (mapconcat #'notes-list-format
                           notes-list--notes
                           "\n"))
        (insert "\n")
        (goto-char (point-min))
        (let ((match (text-property-search-forward 'filename filename t)))
          (if match
              (goto-char (prop-match-beginning match))
            (forward-line line)))
        (beginning-of-line)))))

(defun notes-list-toggle-icons ()
  "Toggle icons display"

  (interactive)
  (if notes-list-display-icons
      (setq notes-list-display-icons nil)
    (setq notes-list-display-icons t))
  (notes-list-refresh))

(defun notes-list-toggle-date ()
  "Toggle date display"

  (interactive)
  (if notes-list-display-date
      (setq notes-list-display-date nil)
    (setq notes-list-display-date t))
  (notes-list-refresh))

(defun notes-list-toggle-tags ()
  "Toggle tags display"

  (interactive)
  (if notes-list-display-tags
      (setq notes-list-display-tags nil)
    (setq notes-list-display-tags t))
  (notes-list-refresh))

(defun notes-list-buffer ()
  "Return the notes list buffer"

  (get-buffer-create "*notes-list*"))


(define-minor-mode notes-list-mode
  "A minor mode for browsing note list"

  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "d") #'notes-list-toggle-date)
            (define-key map (kbd "i") #'notes-list-toggle-icons)
            (define-key map (kbd "t") #'notes-list-toggle-tags)
            (define-key map (kbd "r") #'notes-list-reload)
            (define-key map (kbd "g") #'notes-list-refresh)
            (define-key map (kbd "q") #'notes-list-quit)
            (define-key map (kbd "SPC") #'notes-list-show-note-other-window)
            (define-key map (kbd "<tab>") #'notes-list-open-note-other-window)
            (define-key map (kbd "<RET>") #'notes-list-open-note)
            (define-key map (kbd "<left") nil)
            (define-key map (kbd "<right") nil)
            (define-key map (kbd "<up>") #'notes-list-prev-note)
            (define-key map (kbd "<down>") #'notes-list-next-note)
            map)
  (when notes-list-mode
    (setq stripes-unit 1)
    (stripes-mode t)
    (setq hl-line-overlay-priority 100)
    (hl-line-mode t)
    (face-remap-set-base 'stripes :inherit 'highlight)
    (face-remap-add-relative 'hl-line :inherit 'nano-subtle)
    (setq-local cursor-type nil)
    (read-only-mode t)
    (add-hook 'window-size-change-functions #'notes-list--resize-hook)))

;;;###autoload
(defun notes-list ()
  "Display note list in current buffer"

  (interactive)
  (switch-to-buffer (notes-list-buffer))
  (notes-list-reload)
  (notes-list-mode 1))

;;;###autoload
(defun notes ()
  "Display note list in current buffer"

  (interactive)
  (notes-list))

(provide 'notes-list)
;;; notes-list.el ends here
