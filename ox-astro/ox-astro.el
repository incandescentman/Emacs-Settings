;;; ox-astro.el --- Astro MDX Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Author: Gemini & Jay Dixit
;; Version: 0.7.0
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
(require 'cl-lib)

;; In the main ox-astro.el, you will need to require the new files like this:
;; (require 'ox-astro-config)
;; (require 'ox-astro-functions)


;;; ------------------------------------------------------------------
;;; CUT HERE FOR ox-astro-config.el
;;; ------------------------------------------------------------------
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

(defcustom org-astro-default-author-image
  "~/assets/images/authors/jay-dixit-512.png"   ;; <─ only this line changed
  "Default author image path if not specified in the Org file.
Uses Astro's alias, which maps to the project's src/ directory."
  :group 'org-export-astro
  :type 'string)   ;; treat it as raw front-matter text, not a local file

(defcustom org-astro-known-posts-folders
  '(("actions" . "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/actions/src/content/blog")
    ("jaydocs" . "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/jaydocs/src/content/blog")
    ("socratic" . "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/socraticai/src/content/blog"))
  "An alist of known directories for exporting Astro posts.
Each element is a cons cell of the form (NICKNAME . PATH)."
  :group 'org-export-astro
  :type '(alist :key-type (string :tag "Nickname")
                :value-type (directory :tag "Path")))
;;; ------------------------------------------------------------------
;;; END CUT FOR ox-astro-config.el
;;; ------------------------------------------------------------------


;;; ------------------------------------------------------------------
;;; CUT HERE FOR ox-astro-functions.el
;;; ------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--slugify (s)
  "Convert string S to a slug."
  (when (stringp s)
    (let ((s (downcase s))
      (replace-regexp-in-string "[^a-z0-9]+" "-" (org-trim s) nil)))))

(defun org-astro--format-date (date-raw info)
  "Format DATE-RAW into a string suitable for Astro front matter."
  (let ((date-fmt (plist-get info :astro-date-format)))
    (format-time-string
     date-fmt
     (org-time-string-to-time date-raw))))

(defun org-astro--filename-to-alt-text (path)
  "Generate a human-readable alt text from an image PATH."
  (when (stringp path)
    (let* ((filename (file-name-sans-extension (file-name-nondirectory path)))
           ;; Replace hyphens and underscores with spaces
           (human-readable (replace-regexp-in-string "[-_]" " " filename))
           ;; Remove excessive numbers/dates and clean up
           (cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9] [0-9][0-9] [0-9][0-9] " " " human-readable))
           (cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9][0-9]+ " " " cleaned))
           (cleaned (org-trim cleaned)))
      (capitalize cleaned))))

(defun org-astro--path-to-var-name (path)
  "Convert a file PATH to a camelCase JS variable name."
  (when (stringp path)
    (let* ((filename (file-name-sans-extension (file-name-nondirectory path)))
           (parts (split-string filename "[-_]")))
      (if (null parts)
          ""
          (concat (car parts)
                  (mapconcat #'capitalize (cdr parts) ""))))))

(defun org-astro--get-task-nesting-level (heading)
  "Calculate nesting level for a TODO task by counting TODO ancestors."
  (let ((level 0)
        (current heading))
    (while (setq current (org-element-parent current))
      (when (and (eq (org-element-type current) 'headline)
                 (org-element-property :todo-keyword current))
        (setq level (1+ level))))
    level))

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
                             ((listp val)
                              (concat "\n"
                                      (mapconcat
                                       (lambda (item) (format "- %s" item))
                                       val "\n")
                                      "\n"))
                             (t (format "%s\n"
                                        (if (and (stringp val)
                                                 (string-match-p ":" val)
                                                 (not (eq key 'publishDate)))
                                            (format "\"%s\"" (replace-regexp-in-string "\"" "\\\"" val))
                                            val)))))))))
        (concat yaml-str "---\n"))))

(defun org-astro--get-front-matter-data (tree info)
  "Build an alist of final front-matter data, applying defaults."
  (let* (;; Get the posts-folder, needed for processing image paths.
         (posts-folder (or (plist-get info :posts-folder)
                           (plist-get info :astro-posts-folder)))
         ;; --- Metadata with defaults (respecting narrowing) ---
         (title
          (or (let ((kw (org-element-map tree 'keyword
                          (lambda (k) (when (string-equal "TITLE" (org-element-property :key k)) k))
                          nil 'first-match)))
                (when kw (org-element-property :value kw)))
              (let ((headline (org-element-map tree 'headline 'identity nil 'first-match)))
                (when headline
                  (org-export-data (org-element-property :title headline) info)))
              "Untitled Post"))
         (author (or (plist-get info :author) "Jay Dixit"))
         (excerpt
          (or (let ((kw (org-element-map tree 'keyword
                          (lambda (k)
                            (when (or (string-equal "ASTRO_EXCERPT" (org-element-property :key k))
                                      (string-equal "EXCERPT" (org-element-property :key k)))
                              k)))
                          nil 'first-match)))
                (when kw (replace-regexp-in-in-string "[*]" "" (org-element-property :value kw))))
              (let ((paragraph (org-element-map tree 'paragraph
                                 'org-element-contents
                                 nil 'first-match)))
                (when paragraph
                  (replace-regexp-in-string "[*]" "" (org-export-data paragraph info))))
              ""))
         (tags-raw (or (plist-get info :astro-tags) (plist-get info :tags)))
         (tags (when tags-raw (org-split-string tags-raw "[, \n]+")))
         ;; --- Publish Date (with fallback to current time) ---
         (publish-date
          (let ((date-raw (or (plist-get info :astro-publish-date)
                              (plist-get info :publish-date)
                              (plist-get info :date))))
            (if date-raw
                (org-astro--format-date date-raw info)
                (format-time-string (plist-get info :astro-date-format) (current-time)))))
         ;; --- Author Image (with default and specific path) ---
         (author-image-raw (or (plist-get info :astro-author-image)
                               (plist-get info :author-image)
                               org-astro-default-author-image))
         (author-image (and author-image-raw posts-folder
                            (org-astro--process-image-path
                             author-image-raw posts-folder "authors/")))
         ;; --- Cover Image & Alt Text (with generated alt text) ---
         (image-raw (or (plist-get info :astro-image)
                        (plist-get info :cover-image)))
         (image (and image-raw posts-folder
                     (org-astro--process-image-path
                      image-raw posts-folder "posts/")))
         (image-alt (or (plist-get info :astro-image-alt)
                        (plist-get info :cover-image-alt)
                        (and image (org-astro--filename-to-alt-text image)))))
    ;; Return the alist of final data
    `((title . ,title)
      (author . ,author)
      (authorImage . ,author-image)
      (publishDate . ,publish-date)
      (excerpt . ,excerpt)
      (image . ,image)
      (imageAlt . ,image-alt)
      (tags . ,tags))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX.
This handles raw URLs specially to format them as [url](url)
instead of <url>."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link))
        (raw-link (org-element-property :raw-link link)))
    (cond
     ;; Fuzzy links for internal headings
     ((and (string= type "fuzzy") (not (string-match-p "://" path)))
      (let* ((target (org-export-resolve-fuzzy-link link info))
             (title (org-element-property :raw-value target))
             (slug (org-astro--slugify title)))
        (concat "[" (or desc title) "](" (string ?#) slug ")")))
     ;; Raw URLs (no description)
     ((and (not desc) (member type '("http" "https" "ftp" "mailto")))
      (format "[%s](%s)" raw-link raw-link))
     ;; Defer to default markdown for all other links
     (t
      (org-md-link link desc info)))))

(defun org-astro-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element into fenced Markdown format."
  (if (not (org-export-read-attribute :attr_md src-block :textarea))
      (let* ((lang (org-element-property :language src-block))
             ;; Use :value to get raw content, preserving internal newlines.
             (code (org-element-property :value src-block)))
        (when (and (member lang '("user" "prompt" "quote")) (string-match-p "---" code))
          (setq code (replace-regexp-in-string "---" "—" code)))
        ;; Remove any trailing newlines to prevent extra space at the end.
        (setq code (replace-regexp-in-string "\\`\n+\\|\\s-+\\'" "" code))
        (format "```%s\n%s\n```" (or lang "") code))
      (org-html-textarea-block src-block contents info)))

(defun org-astro-heading (heading contents info)

  "Transcode a HEADING element.
If it has a TODO keyword, convert it to a Markdown task list item."
  (let ((todo-keyword (org-element-property :todo-keyword heading)))
    (if todo-keyword
        ;; It's a TODO item, format as a task list.
        (let* ((title (org-export-data (org-element-property :title heading)
                                       (cons :with-smart-quotes (cons nil info))))
               (nesting-level (org-astro--get-task-nesting-level heading))
               (indent (make-string (* 2 nesting-level) ? ))
               (donep (member todo-keyword org-done-keywords))
               (checkbox (if donep "[x]" "[ ]"))
               (trimmed-contents (if contents (org-trim contents) ""))
               (indented-contents
                (if (> (length trimmed-contents) 0)
                    (let ((content-indent (make-string (+ 2 (* 2 nesting-level)) ? )))
                      (concat "\n" content-indent
                              (replace-regexp-in-string
                               "\n"
                               (concat "\n" content-indent)
                               trimmed-contents)))
                    "")))
          (format "%s- %s %s%s\n" indent checkbox title indented-contents))
        ;; It's a regular heading.
        (let* ((title (org-export-data (org-element-property :title heading)
                                       (cons :with-smart-quotes (cons nil info))))
               (level (+ (org-element-property :level heading)
                         (or (plist-get info :headline-offset) 0))))
               (header (format "%s %s" (make-string level ?#) title)))
          (format "%s\n\n%s" header (or contents ""))))))

(defun org-astro-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element.
If the paragraph is a raw image path, convert it to an <img> tag.
Otherwise, use the default Markdown paragraph transcoding."
  (let* ((children (org-element-contents paragraph))
         (child (and (= 1 (length children)) (car children)))
         (is-image-path nil)
         path)
    (when (and child (eq 'plain-text (org-element-type child)))
      (let* ((raw-text (org-element-property :value child))
             (text (when (stringp raw-text) (org-trim raw-text))))
        (when (and text
                   (string-match-p "^/.*\\.\\(png\\|jpe?g\\)$" text)
                   (file-exists-p text))
          (setq is-image-path t)
          (setq path text))))

    (if is-image-path
        (let* ((image-imports (plist-get info :astro-body-images-imports))
               (image-data (cl-find path image-imports
                                    :key (lambda (item) (plist-get item :path))
                                    :test #'string-equal)))
          (if image-data
              (let ((var-name (plist-get image-data :var-name))
                    (alt-text (or (org-astro--filename-to-alt-text path) "Image")))
                (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
              ;; Fallback: if image wasn't processed by the filter, just output the path as text.
              contents))
        ;; Not an image path, use default paragraph handling
        (org-md-paragraph paragraph contents info))))

(defun org-astro-plain-text (text info)
  "Transcode a plain-text element.
If the text contains raw image paths on their own lines, convert them to <img> tags.
If the text contains raw URLs on their own lines, convert them to LinkPeek components."
  (let* ((lines (split-string text "\n"))
         (image-imports (plist-get info :astro-body-images-imports))
         (has-linkpeek nil)
         (processed-lines
          (mapcar
           (lambda (line)
             (let ((trimmed-line (org-trim line)))
               (cond
                ;; Raw image path
                ((and trimmed-line
                      (string-match-p "^/.*\\.\\(png\\|jpe?g\\)$" trimmed-line)
                      (file-exists-p trimmed-line))
                 (let ((image-data (cl-find trimmed-line image-imports
                                            :key (lambda (item) (plist-get item :path))
                                            :test #'string-equal)))
                   (if image-data
                       (let ((var-name (plist-get image-data :var-name))
                             (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                         (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                       ;; Fallback: if image wasn't processed, return empty string to remove the raw path
                       "")))
                ;; Raw URL
                ((and trimmed-line
                      (string-match-p "^https?://[^[:space:]]+$" trimmed-line))
                 (setq has-linkpeek t)
                 (format "<LinkPeek href=\"%s\"></LinkPeek>" trimmed-line))
                ;; Regular line
                (t line))))
           lines)))
    ;; Store LinkPeek usage in info for import generation
    (when has-linkpeek
      (plist-put info :astro-uses-linkpeek t))
    (mapconcat 'identity processed-lines "\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-prepare-images-filter (tree _backend info)
  "Find all local images, process them, and store import data in INFO.
This filter runs on the parse TREE before transcoding. It collects
all local image links, copies them to the Astro assets
directory, and prepares a list of import statements to be added
to the final MDX file. The data is stored in the INFO plist
under the key `:astro-body-images-imports`."
  (let* ((posts-folder (or (plist-get info :posts-folder)
                           (plist-get info :astro-posts-folder)))
         ;; Collect all image links from the document body.
         (image-paths (org-astro--collect-images-from-tree tree))
         image-imports-data)
    (when posts-folder
      (dolist (path image-paths)
        ;; For each image, copy it to assets and get its new path.
        (let* ((astro-path (org-astro--process-image-path path posts-folder "posts/"))
               (var-name (org-astro--path-to-var-name path)))
          (when (and astro-path var-name)
            (push `(:path ,path :var-name ,var-name :astro-path ,astro-path)
                  image-imports-data)))))
    ;; Store the collected data in the info plist for other functions to use.
    (when image-imports-data
      (let ((final-data (nreverse image-imports-data)))
        (plist-put info :astro-body-images-imports final-data))))
  ;; Return the tree, as required for a parse-tree filter.
  tree)

(defun org-astro-body-filter (body _backend info)
  "Add front-matter and imports to the BODY of the document."
  (let* ((tree (plist-get info :parse-tree))  ; Use the already-parsed tree from export
         (front-matter-data (org-astro--get-front-matter-data tree info))
         (front-matter-string (org-astro--gen-yaml-front-matter front-matter-data))
         ;; --- Handle All Imports ---
         ;; 1. Body image imports (collected by our filter)
         (body-images-imports (plist-get info :astro-body-images-imports))
         (body-imports-string
          (when body-images-imports
            (mapconcat
             (lambda (item)
               (format "import %s from '%s';"
                       (plist-get item :var-name)
                       (plist-get item :astro-path)))
             body-images-imports
             "\n")))
         ;; 2. Manual imports from #+ASTRO_IMPORTS
         (manual-imports (plist-get info :astro-imports))
         ;; 3. Astro Image component import (always include if we have any body images)
         (astro-image-import (when body-images-imports
                               "import { Image } from 'astro:assets';"))
         ;; 4. LinkPeek component import (if raw URLs are used - check body for raw URL patterns)
         (linkpeek-import (when (or (plist-get info :astro-uses-linkpeek)
                                    (string-match-p "\\[\\(https?://[^]]+\\\\)\\](\\1)" body))
                            "import LinkPeek from '../../components/ui/LinkPeek.astro';"))
         ;; 5. Combine all imports, filtering out nil/empty values
         (all-imports (mapconcat #'identity
                                 (delq nil (list astro-image-import linkpeek-import body-imports-string manual-imports))
                                 "\n")))
    (concat front-matter-string
            (if (and all-imports (not (string-blank-p all-imports)))
                (concat all-imports "\n\n")
                "")
            body)))

(defun org-astro-final-output-filter (output _backend info)
  "Final filter for Astro export.
- Replaces HTML entities with literal characters.
- Converts indented example blocks to Markdown blockquotes.
- Converts markdown image syntax with absolute paths to Image components."
  (let* ((s output)
         ;; HTML entities
         (s (replace-regexp-in-string "&#x2013;" "–" s t t))
         (s (replace-regexp-in-string "&rsquo;" "'" s t t))
         (s (replace-regexp-in-string "&lsquo;" "'" s t t))
         (s (replace-regexp-in-string "&rdquo;" "\"" s t t))
         (s (replace-regexp-in-string "&ldquo;" "\"" s t t))
         ;; Convert markdown image syntax with absolute paths to Image components
         (image-imports (plist-get info :astro-body-images-imports))
         (s (if image-imports
                (replace-regexp-in-string
                 "!\\[\\([^]]*\\)\\](\\(/[^)]+\\.\\(?:png\\|jpe?g\\)\\\\))"
                 (lambda (match)
                   (let* ((alt (match-string 1 match))
                          (path (match-string 2 match))
                          (image-data (cl-find path image-imports
                                               :key (lambda (item) (plist-get item :path))
                                               :test #'string-equal)))
                     (if image-data
                         (let ((var-name (plist-get image-data :var-name))
                               (alt-text (or (org-astro--filename-to-alt-text path) alt "Image")))
                           (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                         match)))
                 s)
                s))
         ;; Convert markdown links that are raw URLs to LinkPeek components
         (s (replace-regexp-in-string
             "\\[\\(https?://[^]]+\\\\)\\](\\(\\1\\))"
             (lambda (match)
               (let ((url (match-string 1 match)))
                 ;; Mark that we're using LinkPeek (for import)
                 (plist-put info :astro-uses-linkpeek t)
                 (format "<LinkPeek href=\"%s\"></LinkPeek>" url)))
             s))
         ;; Indented blocks to blockquotes
         (lines (split-string s "\n"))
         (processed-lines
          (mapcar (lambda (line)
                    (if (string-prefix-p "    " line)
                        (concat "> " (substring line 4))
                        line))
                  lines))
         (s (mapconcat 'identity processed-lines "\n")))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Export Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--collect-images-from-tree (tree)
  "Collect all image paths from the parse TREE.
This includes both `[[file:...]]` links and raw image paths on their own line."
  (let (images)
    ;; 1. Collect from `link` elements
    (org-element-map tree 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link)))
          (when (and (string= type "file")
                     (string-match-p "\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)$" path))
            (push path images)))))
    ;; 2. Collect from raw paths in all plain-text elements
    (org-element-map tree 'plain-text
      (lambda (text-element)
        (let* ((raw-text (org-element-property :value text-element))
               (lines (when (stringp raw-text) (split-string raw-text "\n"))))
          (dolist (line lines)
            (let ((text (org-trim line)))
              (when (and text
                         (string-match-p "^/.*\\.\\(png\\|jpe?g\\)$" text)
                         (file-exists-p text))
                (push text images)))))))
    ;; Return a list with no duplicates
    (delete-dups (nreverse images))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGE HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--get-assets-folder (posts-folder sub-dir)
  "Get the assets folder based on POSTS-FOLDER and SUB-DIR."
  (when posts-folder
    (let* ((posts-dir (file-name-as-directory (expand-file-name posts-folder)))
           ;; Go up from content/blog to src
           (src-dir (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (directory-file-name posts-dir))))))
      (expand-file-name (concat "assets/images/" sub-dir) src-dir))))

(defun org-astro--process-image-path (image-path posts-folder sub-dir)
  "Process IMAGE-PATH, copying to SUB-DIR and returning relative path."
  (when (and image-path posts-folder)
    (let* ((clean-path (replace-regexp-in-string
                        "['\"]" "" (org-trim image-path)))
           (expanded-path (expand-file-name clean-path))
           (assets-folder (org-astro--get-assets-folder posts-folder sub-dir)))
      (if (and (file-exists-p expanded-path) assets-folder)
          (let* ((filename (file-name-nondirectory expanded-path))
                 (dest-path (expand-file-name filename assets-folder)))
            (make-directory assets-folder t)
            (unless (file-exists-p dest-path)
              (message "Copying %s to %s" expanded-path dest-path)
              (copy-file expanded-path dest-path t)))
            ;; Return the path for Astro's import syntax
            (format "~/assets/images/%s%s" sub-dir filename))
          image-path))))

(defun org-astro--insert-keyword-at-end-of-block (key value)
  "Insert #+KEY: VALUE at the end of the Org keyword block."
  (save-excursion
    (goto-char (point-min))
    (let ((insert-point (point-min))
          (found-keywords nil))
      ;; Find the last line that is a keyword
      (while (re-search-forward "^#\\+[A-Z_]+:" nil t)
        (setq found-keywords t)
        (setq insert-point (point-at-eol)))
      (goto-char insert-point)
      ;; If we found keywords, insert after them. Otherwise, at the top.
      (if found-keywords (end-of-line))
      (insert (format "\n#+%s: %s" (upcase key) value)))))
;;; ------------------------------------------------------------------
;;; END CUT FOR ox-astro-functions.el
;;; ------------------------------------------------------------------


;;;###autoload
(defun org-astro-export-as-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX buffer."
  (interactive)
  (org-export-to-buffer 'astro "*Astro MDX Export*"
    async subtreep visible-only body-only))

;;;###autoload
(defun org-astro-export-to-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX file.
If title, excerpt, or publish date are missing, they will be
generated and added to the Org source file."
  (interactive)
  (if (string-equal ".mdx" (file-name-extension (buffer-file-name)))
      (message "Cannot export from an .mdx file. Run this from the source .org file.")
      (let ((info (org-export-get-environment 'astro))
            (buffer-modified-p nil))
        ;; --- Ensure essential front-matter exists, writing back if not ---
        (save-excursion
          (let* ((tree (org-element-parse-buffer))
                 (title-present (plist-get info :title))
                 (excerpt-present (or (plist-get info :astro-excerpt) (plist-get info :excerpt)))
                 (date-present (or (plist-get info :astro-publish-date) (plist-get info :publish-date) (plist-get info :date))))

            ;; 1. Handle Title
            (unless title-present
              (let* ((headline (org-element-map tree 'headline 'identity nil 'first-match))
                     (title (when headline (org-export-data (org-element-property :title headline) info))))
                (when (and title (not (string-blank-p title)))
                  (org-astro--insert-keyword-at-end-of-block "TITLE" title)
                  (setq buffer-modified-p t))))

            ;; 2. Handle Excerpt
            (unless excerpt-present
              (let* ((paragraph (org-element-map tree 'paragraph 'identity nil 'first-match))
                     (excerpt-text
                      (when paragraph
                        (let* ((raw-text (org-export-data (org-element-contents paragraph) info))
                               ;; Remove markdown formatting and newlines
                               (clean-text (replace-regexp-in-string "[*_/]" "" raw-text))
                               (single-line-text (replace-regexp-in-string "\n" " " clean-text))
                               ;; Find the first sentence
                               (first-sentence
                                (if (string-match "\\(.+?[.?!]\\)" single-line-text)
                                    (match-string 1 single-line-text)
                                    ;; Fallback for short texts without punctuation
                                    single-line-text)))
                          (org-trim first-sentence)))))
                (when (and excerpt-text (not (string-blank-p excerpt-text)))
                  (org-astro--insert-keyword-at-end-of-block "EXCERPT" excerpt-text)
                  (setq buffer-modified-p t))))

            ;; 3. Handle Date
            (unless date-present
              (let ((date-str (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
                (org-astro--insert-keyword-at-end-of-block "PUBLISH_DATE" date-str)
                (setq buffer-modified-p t)))))

        ;; If we modified the buffer, save it and refresh the export environment
        (when buffer-modified-p
          (save-buffer)
          (setq info (org-export-get-environment 'astro)))

        ;; --- Original export logic continues below ---
        (let* ((posts-folder-from-file (or (plist-get info :astro-posts-folder)
                                           (plist-get info :posts-folder)))
               (resolved-posts-folder (and posts-folder-from-file
                                           (cdr (assoc posts-folder-from-file org-astro-known-posts-folders))))
               (posts-folder
                (or resolved-posts-folder
                    (let* ((selection (completing-read "Select a posts folder: "
                                                       org-astro-known-posts-folders
                                                       nil t posts-folder-from-file))
                           (selected-path (when selection
                                            (cdr (assoc selection org-astro-known-posts-folders)))))
                      (when selected-path
                        ;; Add the POSTS_FOLDER keyword to the org file
                        (save-excursion
                          (goto-char (point-min))
                          (if (re-search-forward "^#\\+POSTS_FOLDER:" nil t)
                              ;; Update existing POSTS_FOLDER keyword
                              (progn
                                (beginning-of-line)
                                (kill-line)
                                (insert (format "#+POSTS_FOLDER: %s" selection)))
                              ;; Add new POSTS_FOLDER keyword
                              (org-astro--insert-keyword-at-end-of-block "POSTS_FOLDER" selection)))
                        (save-buffer))
                      selected-path)))
               (pub-dir (when posts-folder
                          (file-name-as-directory
                           (expand-file-name (org-trim posts-folder)))))
               (default-outfile (org-export-output-file-name ".mdx" subtreep pub-dir))
               (out-dir (file-name-directory default-outfile))
               (out-filename (file-name-nondirectory default-outfile))
               (final-filename
                (replace-regexp-in-string
                 "_" "-"
                 (replace-regexp-in-string "^[0-9]+-" "" out-filename)))
               (outfile (expand-file-name final-filename out-dir)))

          (if pub-dir
              (progn
                (make-directory pub-dir :parents)
                (org-export-to-file 'astro outfile async subtreep visible-only body-only)
                outfile)  ; Return the output file path
              (progn
                (message "Astro export cancelled: No posts folder selected.")
                nil))))))
;;;;;;;;;;;;;;;;;;I have added comments to the file to indicate where you should cut the code.
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
  '((src-block . org-astro-src-block)
    (link . org-astro-link)
    (headline . org-astro-heading)
    (paragraph . org-astro-paragraph)
    (plain-text . org-astro-plain-text))

  :filters-alist
  '((:filter-parse-tree . org-astro-prepare-images-filter)
    (:filter-body . org-astro-body-filter)
    (:filter-final-output . org-astro-final-output-filter))

  :options-alist
  '((:smart-quotes       nil                   org-md-use-smart-quotes nil)
    (:title              "TITLE"               nil nil nil)
    (:author             "AUTHOR"              nil nil nil)
    (:author-image       "AUTHOR_IMAGE"        nil nil nil)
    (:date               "DATE"                nil nil nil)
    (:publish-date       "PUBLISH_DATE"        nil nil nil)
    (:excerpt            "EXCERPT"             nil nil nil)
    (:tags               "TAGS"                nil nil 'newline)
    (:cover-image        "COVER_IMAGE"         nil nil nil)
    (:cover-image-alt    "COVER_IMAGE_ALT"     nil nil nil)
    (:posts-folder       "POSTS_FOLDER"        nil nil nil)
    (:astro-publish-date "ASTRO_PUBLISH_DATE"  nil nil nil)
    (:astro-excerpt      "ASTRO_EXCERPT"       nil nil nil)
    (:astro-image        "ASTRO_IMAGE"         nil nil nil)
    (:astro-image-alt    "ASTRO_IMAGE_ALT"     nil nil nil)
    (:astro-author-image "ASTRO_AUTHOR_IMAGE"  nil nil nil)
    (:astro-tags         "ASTRO_TAGS"          nil nil 'newline)
    (:astro-imports      "ASTRO_IMPORTS"       nil nil 'newline)
    (:astro-posts-folder "ASTRO_POSTS_FOLDER"  nil nil nil)
    (:astro-date-format  nil "date-format" org-astro-date-format nil)))

(provide 'ox-astro)

;;; ox-astro.el ends here