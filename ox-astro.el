;;; ox-astro.el --- Astro MDX Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Author: Gemini
;; Version: 0.4.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords: Org, markdown, docs, astro
;; URL: https://github.com/your-repo/ox-astro

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ox-astro implements a Markdown back-end for the Org export engine.
;; The exported Markdown is compatible with the Astro framework
;; (https://astro.build/) and is saved in the .mdx format.
;; This exporter generates the post front-matter in YAML format and allows
;; for component imports, which are essential for MDX.

;; To start using this exporter, add the below to your Emacs config:
;;
;;   (with-eval-after-load 'ox
;;     (require 'ox-astro))
;;
;; # Workflow
;;
;; This exporter is designed for a one-post-per-file workflow.
;; A single Org file exports to a single .mdx file. If #+POSTS_FOLDER is not
;; set, files are exported to a subdirectory named "astro-posts". This can
;; be customized via `org-astro-default-posts-folder`.
;;
;; # Supported Org Keywords
;;
;; The exporter recognizes the following keywords:
;;
;;   - #+title: The title of the post.
;;   - #+author: The author's name.
;;   - #+date / #+publish_date: The publication date.
;;   - #+excerpt: A short summary of the post.
;;   - #+cover_image: Path to the post's hero image.
;;   - #+cover_image_alt: Alt text for the cover image.
;;   - #+tags: Comma or space-separated list of tags.
;;   - #+posts_folder: The absolute or relative path to the export directory.
;;   - #+astro_imports: Raw import statements for MDX components.

;;; Code:

(require 'ox-md)
(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User-Configurable Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup org-export-astro nil
  "Options for exporting Org mode files to Astro-compatible MDX."
  :tag "Org Export Astro"
  :group 'org-export
  :version "26.3")

(defcustom org-astro-date-format "%Y-%m-%dT%T:00:00Z"
  "Date format used for exporting 'publishDate' in front-matter.
The format should be compatible with `format-time-string' and ideally
produce an ISO 8601 compliant date string, as is common in Astro."
  :group 'org-export-astro
  :type 'string)

(defcustom org-astro-default-posts-folder "astro-posts"
  "Default subdirectory for exported posts if #+POSTS_FOLDER is not set."
  :group 'org-export-astro
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--slugify (s)
  "Convert string S to a slug."
  (when (stringp s)
    (let ((s (downcase s)))
      (replace-regexp-in-string "[^a-z0-9]+" "-" (org-trim s) nil))))

(defun org-astro--format-date (date-raw info)
  "Format DATE-RAW into a string suitable for Astro front matter.
Handles both Org timestamps and plain 'YYYY-MM-DD' strings.
INFO is the export options plist."
  (let ((date-fmt (plist-get info :astro-date-format)))
    (format-time-string date-fmt
                        (if (string-match-p org-ts-regexp-both date-raw)
                            (org-time-string-to-time date-raw)
                            (apply 'encode-time (parse-time-string date-raw))))))

(defun org-astro--gen-yaml-front-matter (data)
  "Generate a YAML front-matter string from an alist DATA."
  (if (null data)
      ""
      (let ((yaml-str "---\n"))
        (dolist (pair data)
          (let ((key (car pair))
                (val (cdr pair)))
            (when val
              (setq yaml-str
                    (concat yaml-str
                            (format "%s: " (symbol-name key))
                            (cond
                             ((listp val) ; For tags
                              (concat "\n" (mapconcat (lambda (item) (format " - %s" item)) val "\n")))
                             ((stringp val)
                              ;; Don't quote image path if it starts with ~ or /
                              (if (and (eq key 'image) (string-match-p "^[~/]" val))
                                  (format "%s\n" val)
                                  (format "\"%s\"\n" (replace-regexp-in-string "\"" "\\\\\"" val))))
                             (t (format "%s\n" val)))))))
          (concat yaml-str "---\n"))))

  (defun org-astro--get-front-matter (info)
    "Return the Astro front-matter string.
INFO is a plist used as a communication channel."
    (let* ((title (plist-get info :title))
           (author (plist-get info :author))
           (excerpt (or (plist-get info :astro-excerpt) (plist-get info :excerpt)))
           (image (or (plist-get info :astro-image) (plist-get info :cover-image)))
           (image-alt (or (plist-get info :astro-image-alt) (plist-get info :cover-image-alt)))
           (tags-raw (or (plist-get info :astro-tags) (plist-get info :tags)))
           (tags (when tags-raw (org-split-string tags-raw "[, ]+")))
           (publish-date
            (or (plist-get info :astro-publish-date)
                (let ((date-raw (or (plist-get info :publish-date) (plist-get info :date))))
                  (when date-raw (org-astro--format-date date-raw info)))))
           (data `((title . ,title)
                   (author . ,author)
                   (publishDate . ,publish-date)
                   (excerpt . ,excerpt)
                   (image . ,image)
                   (imageAlt . ,image-alt)
                   (tags . ,tags))))
      (org-astro--gen-yaml-front-matter data)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun org-astro-link (link desc info)
    "Transcode a LINK object for Astro MDX.
Handles internal links to headings by creating anchor links."
    (let ((type (org-element-property :type link))
          (path (org-element-property :path link)))
      (if (and (string= type "fuzzy") (not (string-match-p "://" path)))
          ;; This is likely an internal link like [[Introduction]]
          (let* ((target (org-export-resolve-fuzzy-link link info))
                 (title (org-element-property :raw-value target))
                 (slug (org-astro--slugify title)))
            (format "[%s](#%s)" (or desc title) slug))
          ;; Fallback to the default markdown link handler for other types
          (org-md-link link desc info))))

  (defun org-astro-heading (heading contents info)
    "Transcode a HEADING element, adding an ID for anchor links."
    (let* ((title (org-export-data (org-element-property :title heading) info))
           (slug (org-astro--slugify title))
           (level (+ (org-element-property :level heading)
                     (or (plist-get info :headline-offset) 0)))
           (headline (format "%s %s" (make-string level ?#) title)))
      (format "%s {#%s}\n\n%s" headline slug (or contents ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun org-astro-body-filter (body _backend info)
    "Add front-matter and imports to the BODY of the document."
    (let ((front-matter (org-astro--get-front-matter info))
          (imports (or (plist-get info :astro-imports) "")))
      (concat front-matter
              (if (string-blank-p imports) "" (concat imports "\n\n"))
              body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Export Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
  (defun org-astro-export-as-mdx (&optional async)
    "Export current buffer to an Astro-compatible MDX buffer."
    (interactive)
    (org-export-to-buffer 'astro "*Astro MDX Export*" async))

;;;###autoload
  (defun org-astro-export-to-mdx (&optional async)
    "Export current buffer to an Astro-compatible MDX file.
If a POSTS_FOLDER keyword is present, export to that directory.
Otherwise, export to the directory specified by
`org-astro-default-posts-folder'."
    (interactive)
    (let* ((info (org-export-get-environment 'astro))
           (posts-folder (or (plist-get info :astro-posts-folder)
                             (plist-get info :posts-folder)))
           (pub-dir
            (if posts-folder
                (file-name-as-directory (expand-file-name (org-trim posts-folder)))
                (let ((default-astro-dir
                       (file-name-as-directory
                        (expand-file-name org-astro-default-posts-folder default-directory))))
                  (make-directory default-astro-dir :parents)
                  default-astro-dir)))
           (outfile (org-export-output-file-name ".mdx" nil pub-dir)))
      (org-export-to-file 'astro outfile async)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (org-export-define-derived-backend 'astro 'md
    :menu-entry
    '(?a "Export to Astro"
         ((?X "As MDX buffer" org-astro-export-as-mdx)
          (?x "To MDX file" org-astro-export-to-mdx)
          (?o "To MDX file and open"
              (lambda (_a _s _v _b) (org-open-file (org-astro-export-to-mdx))))))
    :translate-alist '((link . org-astro-link)
                       (headline . org-astro-heading))
    :filters-alist '((:filter-body . org-astro-body-filter))
    ;; Define the keywords this backend will recognize from the Org file.
    :options-alist '(
                     (:title "TITLE" nil)
                     (:author "AUTHOR" nil)
                     (:date "DATE" nil)
                     (:publish-date "PUBLISH_DATE" nil)
                     (:excerpt "EXCERPT" nil)
                     (:tags "TAGS" nil newline)
                     (:cover-image "COVER_IMAGE" nil)
                     (:cover-image-alt "COVER_IMAGE_ALT" nil)
                     (:posts-folder "POSTS_FOLDER" nil)
                     (:astro-publish-date "ASTRO_PUBLISH_DATE" nil)
                     (:astro-excerpt "ASTRO_EXCERPT" nil)
                     (:astro-image "ASTRO_IMAGE" nil)
                     (:astro-image-alt "ASTRO_IMAGE_ALT" nil)
                     (:astro-tags "ASTRO_TAGS" nil newline)
                     (:astro-imports "ASTRO_IMPORTS" nil newline)
                     (:astro-posts-folder "ASTRO_POSTS_FOLDER" nil)
                     (:astro-date-format nil "date-format" org-astro-date-format)
                     ))

  (provide 'ox-astro)

;;; ox-astro.el ends here
