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
     (cond
      ;; Handle Org timestamps
      ((string-match-p org-ts-regexp-both date-raw)
       (org-time-string-to-time date-raw))
      ;; Handle plain dates like "2025-07-26"
      ((string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" date-raw)
       (encode-time 0 0 0
                    (string-to-number (match-string 3 date-raw))  ; day
                    (string-to-number (match-string 2 date-raw))  ; month
                    (string-to-number (match-string 1 date-raw))  ; year
                    nil nil nil))
      ;; Fallback to parse-time-string
      (t
       (let* ((parsed (parse-time-string date-raw))
              (sec    (or (nth 0 parsed) 0))
              (min    (or (nth 1 parsed) 0))
              (hour   (or (nth 2 parsed) 0))
              (day    (nth 3 parsed))
              (mon    (nth 4 parsed))
              (year   (nth 5 parsed))
              (zone   (nth 8 parsed)))
         (encode-time sec min hour day mon year zone)))))))

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
                                       (lambda (item) (format "  - %s" item))
                                       val "\n")
                                      "\n"))
                             ((stringp val)
                              ;; Use format with ~S for values that might contain special characters
                              ;; but for file paths we want them as-is.
                              ;; Let's assume the path is clean now.
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
         (author-image (or (plist-get info :astro-author-image)
                           (plist-get info :author-image)))  ; Add this
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
       (authorImage . ,author-image)  ; Add this - note camelCase
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
  (let* ((front-image-raw (or (plist-get info :astro-image)
                              (plist-get info :cover-image)))
         ;; Clean the front-image path
         (front-image (when front-image-raw (replace-regexp-in-string "^'\\|'$" "" (org-trim front-image-raw))))
         (content-images (org-astro--collect-images-from-tree tree info))
         imports)

    ;; Handle the front matter image
    (when front-image
      (push (format "import hero from '%s';" front-image) imports))

    ;; Handle other images in the content
    (dolist (img content-images)
      (let ((clean-img (replace-regexp-in-string "^'\\|'$" "" (org-trim img))))
        (unless (equal clean-img front-image)
          (let ((var-name (org-astro--extract-image-filename clean-img)))
            (setq var-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" var-name))
            (push (format "import %s from '%s';" var-name clean-img) imports)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGE HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--get-assets-folder (posts-folder)
  "Get the assets/images folder based on POSTS-FOLDER.
Assumes standard Astro project structure where posts are in src/content/blog
and images are in src/assets/images."
  (when posts-folder
    (let* ((posts-dir (file-name-as-directory (expand-file-name posts-folder)))
           ;; Go up from content/blog to src
           (src-dir (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (directory-file-name posts-dir))))))
      (expand-file-name "assets/images/posts/" src-dir))))



(defun org-astro--process-image-path (image-path posts-folder)
  "Process IMAGE-PATH, copying if needed and returning relative path.
POSTS-FOLDER is the destination folder for blog posts."
  (when (and image-path posts-folder)
    ;; Strip leading/trailing whitespace and then any surrounding quotes.
    (let* ((clean-path (replace-regexp-in-string
                        "^'\\|'$" ""
                        (replace-regexp-in-string
                         "^\"\\|\"$" "" (org-trim image-path))))
           (expanded-path (expand-file-name clean-path))
           (assets-folder (org-astro--get-assets-folder posts-folder)))

      (if (and (file-exists-p expanded-path)
               (file-name-absolute-p clean-path)
               assets-folder)
          (let* ((filename (file-name-nondirectory expanded-path))
                 (dest-path (expand-file-name filename assets-folder)))
            (make-directory assets-folder t)

            (unless (file-exists-p dest-path)
              (message "Copying %s to %s" expanded-path dest-path)
              (copy-file expanded-path dest-path t)) ; Overwrite existing file

            ;; Return the path without quotes for use in imports
            (format "~/assets/images/posts/%s" filename))
          ;; Return original path if not absolute or file doesn't exist
          image-path))))




(defun org-astro--update-image-keywords ()
  "Update image keywords in the current buffer with processed paths."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((info (org-export-get-environment 'astro))
           (posts-folder (or (plist-get info :astro-posts-folder)
                             (plist-get info :posts-folder))))
      (when posts-folder
        ;; Process #+COVER_IMAGE
        (goto-char (point-min))
        (when (re-search-forward "^#\\+COVER_IMAGE:\\s-*\\(.+\\)$" nil t)
          (let* ((current-path (match-string 1))
                 (new-path (org-astro--process-image-path current-path posts-folder)))
            (when (and new-path (not (string= current-path new-path)))
              (replace-match (format "#+COVER_IMAGE: %s" new-path)))))

        ;; Process #+ASTRO_IMAGE
        (goto-char (point-min))
        (when (re-search-forward "^#\\+ASTRO_IMAGE:\\s-*\\(.+\\)$" nil t)
          (let* ((current-path (match-string 1))
                 (new-path (org-astro--process-image-path current-path posts-folder)))
            (when (and new-path (not (string= current-path new-path)))
              (replace-match (format "#+ASTRO_IMAGE: %s" new-path)))))))))

;; Update the export functions to process images before export
;;;###autoload
(defun org-astro-export-to-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX file.
If a POSTS_FOLDER keyword is present, export to that directory.
Otherwise, export to the directory specified by
`org-astro-default-posts-folder'."
  (interactive)
  ;; Process images first
  (org-astro--update-image-keywords)
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

;; Add a convenience function to just process images without exporting
;;;###autoload
(defun org-astro-process-images ()
  "Process and copy images in the current Org buffer.
Updates image paths to use relative ~/assets/images/posts/ format."
  (interactive)
  (org-astro--update-image-keywords)
  (message "Images processed and paths updated."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-export-define-derived-backend 'astro 'md
  :menu-entry
  '(?a "Export to Astro"
       ((?a "As MDX buffer" org-astro-export-as-mdx)
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
    (:author-image       "AUTHOR_IMAGE"         nil nil nil)  ; Add this line
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
    (:astro-author-image "ASTRO_AUTHOR_IMAGE"   nil nil nil)  ; Add this too
    (:astro-tags         "ASTRO_TAGS"           nil nil 'newline)
    (:astro-imports      "ASTRO_IMPORTS"        nil nil 'newline)
    (:astro-posts-folder "ASTRO_POSTS_FOLDER"   nil nil nil)
    (:astro-date-format  nil "date-format" org-astro-date-format nil)))

(provide 'ox-astro)

;;; ox-astro.el ends here
