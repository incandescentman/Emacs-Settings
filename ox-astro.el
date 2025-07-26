;;; ox-astro.el --- Astro MDX Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Author: Gemini & Jay Dixit
;; Version: 0.5.2
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

(defcustom org-astro-date-format "%Y-%m-%dT%H:%M:%SZ"
  "Date format used for exporting 'publishDate' in front-matter."
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
    (format-time-string
     date-fmt
     (if (string-match-p org-ts-regexp-both date-raw)
         (org-time-string-to-time date-raw)
         (let* ((parsed (parse-time-string date-raw))
                (sec    (or (nth 0 parsed) 0))
                (min    (or (nth 1 parsed) 0))
                (hour   (or (nth 2 parsed) 0))
                (day    (nth 3 parsed))
                (mon    (nth 4 parsed))
                (year   (nth 5 parsed))
                (zone   (nth 8 parsed)))
           (encode-time sec min hour day mon year zone))))))

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
                              (concat "\n"
                                      (mapconcat
                                       (lambda (item) (format "- %s" item))  ; Remove space before -
                                       val "\n")
                                      "\n"))  ; Add newline after tags
                             ((stringp val)
                              ;; Remove quotes for all values
                              (format "%s\n" val))
                             (t
                              (format "%s\n" val))))))))
        ;; close front-matter block
        (concat yaml-str "---\n"))))


(defun org-astro--get-front-matter (info)
  "Return the Astro front-matter string.
INFO is a plist used as a communication channel."
  (let* ((title (plist-get info :title))
         (author (plist-get info :author))
         (excerpt (or (plist-get info :astro-excerpt)
                      (plist-get info :excerpt)))
         (image (or (plist-get info :astro-image)
                    (plist-get info :cover-image)))
         (image-alt (or (plist-get info :astro-image-alt)
                        (plist-get info :cover-image-alt)))
         (tags-raw (or (plist-get info :astro-tags)
                       (plist-get info :tags)))
         (tags (when tags-raw
                 (org-split-string tags-raw "[, \n]+")))
         (publish-date (or (plist-get info :astro-publish-date)
                           (let ((date-raw (or (plist-get info :publish-date)
                                               (plist-get info :date))))
                             (when date-raw
                               (org-astro--format-date date-raw info))))))
    (org-astro--gen-yaml-front-matter
     `((title . ,title)
       (author . ,author)
       (publishDate . ,publish-date)
       (excerpt . ,excerpt)
       (image . ,image)
       (imageAlt . ,image-alt)
       (tags . ,tags)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX.
Handles internal links to headings by creating anchor links."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (if (and (string= type "fuzzy")
             (not (string-match-p "://" path)))
        ;; Internal fuzzy link
        (let* ((target (org-export-resolve-fuzzy-link link info))
               (title (org-element-property :raw-value target))
               (slug (org-astro--slugify title)))
          (format "[%s](#%s)" (or desc title) slug))
        ;; fallback to default
        (org-md-link link desc info))))

(defun org-astro-heading (heading contents info)
  "Transcode a HEADING element."
  (let* ((title (org-export-data (org-element-property :title heading) info))
         (level (+ (org-element-property :level heading)
                   (or (plist-get info :headline-offset) 0)))
         (header (format "%s %s" (make-string level ?#) title)))
    (format "%s\n\n%s" header (or contents ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-body-filter (body _backend info)
  "Add front-matter and imports to the BODY of the document."
  (let* ((tree (org-element-parse-buffer))
         (front-matter (org-astro--get-front-matter info))
         (auto-imports (org-astro--generate-imports info tree))
         (manual-imports (plist-get info :astro-imports))
         (all-imports (if (and auto-imports manual-imports)
                          (concat auto-imports "\n" manual-imports)
                          (or auto-imports manual-imports))))
    (concat front-matter
            (if (and all-imports (not (string-blank-p all-imports)))
                (concat all-imports "\n\n")
                "")
            body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Export Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-astro-export-as-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX buffer."
  (interactive)
  (org-export-to-buffer 'astro "*Astro MDX Export*"
    async subtreep visible-only body-only))

;;;###autoload
(defun org-astro-export-to-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX file.
If a POSTS_FOLDER keyword is present, export to that directory.
Otherwise, export to the directory specified by
`org-astro-default-posts-folder'."
  (interactive)
  (let* ((info (org-export-get-environment 'astro))
         (posts-folder (or (plist-get info :astro-posts-folder)
                           (plist-get info :posts-folder)))
         (pub-dir (if posts-folder
                      (file-name-as-directory
                       (expand-file-name (org-trim posts-folder)))
                      (let ((d (file-name-as-directory
                                (expand-file-name org-astro-default-posts-folder
                                                  default-directory))))
                        (make-directory d :parents)
                        d)))
         (outfile (org-export-output-file-name ".mdx" subtreep pub-dir)))
    (org-export-to-file 'astro outfile
      async subtreep visible-only body-only)))



(defun org-astro--extract-image-filename (path)
  "Extract just the filename without extension from PATH."
  (file-name-sans-extension (file-name-nondirectory path)))

(defun org-astro--collect-images-from-tree (tree info)
  "Collect all image paths from the parse TREE."
  (let (images)
    (org-element-map tree 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link)))
          (when (and (string= type "file")
                     (string-match-p "\\(?:png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)$" path))
            (push path images))))
      info)
    images))

(defun org-astro--generate-imports (info tree)
  "Generate import statements for all images in the document."
  (let* ((front-image (or (plist-get info :astro-image)
                          (plist-get info :cover-image)))
         (content-images (org-astro--collect-images-from-tree tree info))
         (all-images (delete-dups
                      (delq nil
                            (append (when front-image (list front-image))
                                    content-images))))
         imports)
    (dolist (img all-images)
      (let ((var-name (org-astro--extract-image-filename img)))
        ;; Make variable names valid JS identifiers
        (setq var-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" var-name))
        (push (format "import %s from '%s';" var-name img) imports)))
    (when imports
      (mapconcat 'identity (nreverse imports) "\n"))))


(defun org-astro-debug-info ()
  "Debug function to inspect export info."
  (interactive)
  (let ((info (org-export-get-environment 'astro)))
    (pp info)))


(defun org-astro--generate-imports (info tree)
  "Generate import statements for all images in the document."
  (let* ((front-image (or (plist-get info :astro-image)
                          (plist-get info :cover-image)))
         (content-images (org-astro--collect-images-from-tree tree info))
         imports)

    ;; Handle the front matter image specially - always import as 'hero'
    (when front-image
      (push (format "import hero from '%s';" front-image) imports))

    ;; Handle other images in the content
    (dolist (img content-images)
      ;; Skip if it's the same as the front image (already imported as hero)
      (unless (equal img front-image)
        (let ((var-name (org-astro--extract-image-filename img)))
          (setq var-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" var-name))
          (push (format "import %s from '%s';" var-name img) imports))))

    (when imports
      (mapconcat 'identity (nreverse imports) "\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-export-define-derived-backend 'astro 'md
  :menu-entry
  '(?a "Export to Astro"
       ((?X "As MDX buffer" org-astro-export-as-mdx)
        (?x "To MDX file" org-astro-export-to-mdx)
        (?o "To MDX file and open"
            (lambda (_a _s _v _b)
              (org-open-file
               (org-astro-export-to-mdx))))))

  :translate-alist
  '((link . org-astro-link)
    (headline . org-astro-heading))

  :filters-alist
  '((:filter-body . org-astro-body-filter))

  ;; The format for each entry is:
  ;; (PLIST-KEYWORD ORG-KEYWORD OPTION-KEY DEFAULT-VAR BEHAVIOR-SYMBOL)
  ;; Most are nil, but the structure must be correct.
  :options-alist
  '((:title              "TITLE"                nil nil nil)
    (:author             "AUTHOR"               nil nil nil)
    (:date               "DATE"                 nil nil nil)
    (:publish-date       "PUBLISH_DATE"         nil nil nil)
    (:excerpt            "EXCERPT"              nil nil nil)
    (:tags               "TAGS"                 nil nil 'newline)
    (:cover-image        "COVER_IMAGE"          nil nil nil)
    (:cover-image-alt    "COVER_IMAGE_ALT"      nil nil nil)
    (:posts-folder       "POSTS_FOLDER"         nil nil nil)
    (:astro-publish-date "ASTRO_PUBLISH_DATE"   nil nil nil)
    (:astro-excerpt      "ASTRO_EXCERPT"        nil nil nil)
    (:astro-image        "ASTRO_IMAGE"          nil nil nil)
    (:astro-image-alt    "ASTRO_IMAGE_ALT"      nil nil nil)
    (:astro-tags         "ASTRO_TAGS"           nil nil 'newline)
    (:astro-imports      "ASTRO_IMPORTS"        nil nil 'newline)
    (:astro-posts-folder "ASTRO_POSTS_FOLDER"   nil nil nil)
    (:astro-date-format  nil "date-format" org-astro-date-format nil)))

(provide 'ox-astro)

;;; ox-astro.el ends here
