;;; ox-tufte-latex.el --- Tufte LaTeX Back-End for Org Export Engine

;; Copyright (C) 2016 Thomas S. Dye

;; Author: Thomas S. Dye
;; Keywords: org, wp, Tufte, LaTeX

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Tufte LaTeX back-end for Org
;; exporter, based on the `ox-latex' back-end.

;;; Code:

(require 'ox-latex)


;;; Define Back-End

(org-export-define-derived-backend 'tufte-latex 'latex
  :menu-entry
  '(?T "Export to Tufte LaTeX"
    ((?T "As LaTeX buffer"
      (lambda (a s v b) (org-tufte-latex-export-as-latex a s v)))
     (?t "As LaTeX file" (lambda (a s v b) (org-tufte-latex-export-to-latex a s v)))
     (?l "As LaTeX file and open"
      (lambda (a s v b)
        (if a (org-tufte-latex-export-to-latex t s v)
            (org-open-file (org-tufte-latex-export-to-latex nil s v)))))
     (?P "As PDF file" org-tufte-latex-export-to-pdf)
     (?p "As PDF file and open"
      (lambda (a s v b)
        (if a (org-tufte-latex-export-to-pdf t s v b)
            (org-open-file (org-tufte-latex-export-to-pdf nil s v b)))))))
  :translate-alist '((footnote-reference . org-tufte-latex-footnote-reference)
                     (table . org-tufte-latex-table)
                     (link . org-tufte-latex-link)))



;;; Transcode Functions

;;;; Link

(defun org-tufte-latex--inline-image (link info)
  "Return Tufte LaTeX code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-element link))
         (path (let ((raw-path (org-element-property :path link)))
                 (if (not (file-name-absolute-p raw-path)) raw-path
                     (expand-file-name raw-path))))
         (filetype (file-name-extension path))
         (caption (org-latex--caption/label-string parent info))
         (caption-above-p (org-latex--caption-above-p link info))
         ;; Retrieve latex attributes from the element around.
         (attr (org-export-read-attribute :attr_latex parent))
         (float (let ((float (plist-get attr :float)))
                  (cond ((string= float "wrap") 'wrap)
                        ((string= float "sideways") 'sideways)
                        ((string= float "multicolumn") 'multicolumn)
                        ;; td - start
                        ((string= float "margin") 'margin)
                        ;; td - end
                        ((or float
                             (org-element-property :caption parent)
                             (org-string-nw-p (plist-get attr :caption)))
                         (if (and (plist-member attr :float) (not float))
                             'nonfloat
                             'figure))
                        ((and (not float) (plist-member attr :float)) nil))))
         (placement
          (let ((place (plist-get attr :placement)))
            (cond
              (place (format "%s" place))
              ((eq float 'wrap) "{l}{0.5\\textwidth}")
              ((eq float 'figure)
               (format "[%s]" (plist-get info :latex-default-figure-position)))
              (t ""))))
         (comment-include (if (plist-get attr :comment-include) "%" ""))
         ;; It is possible to specify width and height in the
         ;; ATTR_LATEX line, and also via default variables.
         (width (cond ((plist-get attr :width))
                      ((plist-get attr :height) "")
                      ((eq float 'wrap) "0.48\\textwidth")
                      (t (plist-get info :latex-image-default-width))))
         (height (cond ((plist-get attr :height))
                       ((or (plist-get attr :width)
                            (memq float '(figure wrap))) "")
                       (t (plist-get info :latex-image-default-height))))
         (options (let ((opt (or (plist-get attr :options)
                                 (plist-get info :latex-image-default-option))))
                    (if (not (string-match "\\`\\[\\(.*\\)\\]\\'" opt)) opt
                        (match-string 1 opt))))
         ;; td - start
         (offset
          (let ((offs (plist-get attr :offset)))
            (cond
              (offs (format "[%s]" offs))
              ((eq float 'margin)
               (format "[%s]" "0pt"))
              (t ""))))
         ;; td - end
         ;; td - start
         (vertical-alignment
          (let ((v-align (plist-get attr :vertical-alignment)))
            (cond
              ((string= v-align "t")
               (format "\\setfloatalignment{%s}" v-align))
              ((string= v-align "b")
               (format "\\setfloatalignment{%s}" v-align))
              (t ""))))
         ;; td - end
         ;; td - start
         (horizontal-alignment
          (let ((h-align (plist-get attr :horizontal-alignment)))
            (cond
              ((string= h-align "l")
               (format "%s" "\\forceversofloat"))
              ((string= h-align "r")
               (format "%s" "\\forcerectofloat"))
              (t ""))))
         ;; td - end
         image-code)
    (if (member filetype '("tikz" "pgf"))
        ;; For tikz images:
        ;; - use \input to read in image file.
        ;; - if options are present, wrap in a tikzpicture environment.
        ;; - if width or height are present, use \resizebox to change
        ;;   the image size.
        (progn
          (setq image-code (format "\\input{%s}" path))
          (when (org-string-nw-p options)
            (setq image-code
                  (format "\\begin{tikzpicture}[%s]\n%s\n\\end{tikzpicture}"
                          options
                          image-code)))
          (when (or (org-string-nw-p width) (org-string-nw-p height))
            (setq image-code (format "\\resizebox{%s}{%s}{%s}"
                                     (if (org-string-nw-p width) width "!")
                                     (if (org-string-nw-p height) height "!")
                                     image-code))))
        ;; For other images:
        ;; - add width and height to options.
        ;; - include the image with \includegraphics.
        (when (org-string-nw-p width)
          (setq options (concat options ",width=" width)))
        (when (org-string-nw-p height)
          (setq options (concat options ",height=" height)))
        (let ((search-option (org-element-property :search-option link)))
          (when (and search-option
                     (equal filetype "pdf")
                     (org-string-match-p "\\`[0-9]+\\'" search-option)
                     (not (org-string-match-p "page=" options)))
            (setq options (concat options ",page=" search-option))))
        (setq image-code
              (format "\\includegraphics%s{%s}"
                      (cond ((not (org-string-nw-p options)) "")
                            ((= (aref options 0) ?,)
                             (format "[%s]"(substring options 1)))
                            (t (format "[%s]" options)))
                      path))
        (when (equal filetype "svg")
          (setq image-code (replace-regexp-in-string "^\\\\includegraphics"
                                                     "\\includesvg"
                                                     image-code
                                                     nil t))
          (setq image-code (replace-regexp-in-string "\\.svg}"
                                                     "}"
                                                     image-code
                                                     nil t))))
    ;; Return proper string, depending on FLOAT.
    (case float
      (wrap (format "\\begin{wrapfigure}%s
%s\\centering
%s%s
%s\\end{wrapfigure}"
                    placement
                    (if caption-above-p caption "")
                    comment-include image-code
                    (if caption-above-p "" caption)))
      (sideways (format "\\begin{sidewaysfigure}
%s\\centering
%s%s
%s\\end{sidewaysfigure}"
                        (if caption-above-p caption "")
                        comment-include image-code
                        (if caption-above-p "" caption)))
      (multicolumn (format "\\begin{figure*}%s
%s\\centering
%s%s
%s\\end{figure*}"
                           placement
                           (if caption-above-p caption "")
                           comment-include image-code
                           (if caption-above-p "" caption)))
      ;; td - start
      (margin (format "\\begin{marginfigure}%s
%s
%s%s
%s\\end{marginfigure}"
                      offset
                      (if caption-above-p caption "")
                      comment-include image-code
                      (if caption-above-p "" caption)))
      ;; td - end
      (figure (format "\\begin{figure}%s
%s\\centering
%s%s
%s%s%s\\end{figure}"
                      placement
                      (if caption-above-p caption "")
                      comment-include image-code
                      (if caption-above-p "" caption)
                      vertical-alignment horizontal-alignment))
      ;;       (figure (format "\\begin{figure}%s
      ;; %s\\centering
      ;; %s%s
      ;; %s\\end{figure}"
      ;;                      placement
      ;;                      (if caption-above-p caption "")
      ;;                      comment-include image-code
      ;;                      (if caption-above-p "" caption)))
      (nonfloat
       (format "\\begin{center}
%s%s
%s\\end{center}"
               (if caption-above-p caption "")
               image-code
               (if caption-above-p "" caption)))
      (otherwise image-code))))


(defun org-tufte-latex-link (link desc info)
  "Transcode a LINK object from Org to LaTeX.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
         (raw-path (replace-regexp-in-string
                    "%" "\\%" (org-element-property :path link) nil t))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))
         (imagep (org-export-inline-image-p
                  link (plist-get info :latex-inline-image-rules)))
         (path (cond
                 ((member type '("http" "https" "ftp" "mailto" "doi"))
                  (concat type ":" raw-path))
                 ((string= type "file") (org-export-file-uri raw-path))
                 (t raw-path))))
    (cond
      ;; Link type is handled by a special function.
      ((org-export-custom-protocol-maybe link desc 'latex))
      ;; Image file.
      (imagep (org-tufte-latex--inline-image link info))
      ;; Radio link: Transcode target's contents and use them as link's
      ;; description.
      ((string= type "radio")
       (let ((destination (org-export-resolve-radio-link link info)))
         (if (not destination) desc
             (format "\\hyperref[%s]{%s}"
                     (org-export-get-reference destination info)
                     desc))))
      ;; Links pointing to a headline: Find destination and build
      ;; appropriate referencing command.
      ((member type '("custom-id" "fuzzy" "id"))
       (let ((destination (if (string= type "fuzzy")
                              (org-export-resolve-fuzzy-link link info)
                              (org-export-resolve-id-link link info))))
         (case (org-element-type destination)
           ;; Id link points to an external file.
           (plain-text
            (if desc (format "\\href{%s}{%s}" destination desc)
                (format "\\url{%s}" destination)))
           ;; Fuzzy link points nowhere.
           ((nil)
            (format (plist-get info :latex-link-with-unknown-path-format)
                    (or desc
                        (org-export-data
                         (org-element-property :raw-link link) info))))
           ;; LINK points to a headline.  If headlines are numbered
           ;; and the link has no description, display headline's
           ;; number.  Otherwise, display description or headline's
           ;; title.
           (headline
            (let ((label (org-latex--label destination info t)))
              (if (and (not desc)
                       (org-export-numbered-headline-p destination info))
                  (format "\\ref{%s}" label)
                  (format "\\hyperref[%s]{%s}" label
                          (or desc
                              (org-export-data
                               (org-element-property :title destination) info))))))
           ;; Fuzzy link points to a target.  Do as above.
           (otherwise
            (let ((ref (org-latex--label destination info t)))
              (if (not desc) (format "\\ref{%s}" ref)
                  (format "\\hyperref[%s]{%s}" ref desc)))))))
      ;; Coderef: replace link with the reference name or the
      ;; equivalent line number.
      ((string= type "coderef")
       (format (org-export-get-coderef-format path desc)
               (org-export-resolve-coderef path info)))
      ;; External link with a description part.
      ((and path desc) (format "\\href{%s}{%s}" path desc))
      ;; External link without a description part.
      (path (format "\\url{%s}" path))
      ;; No path, only description.  Try to do something useful.
      (t (format (plist-get info :latex-link-with-unknown-path-format) desc)))))


;;;; Table
;;
;; `org-tufte-latex-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" mode.  Otherwise, it
;; delegates the job to either `org-latex--table.el-table',
;; `org-tufte-latex--org-table' or `org-latex--math-table' functions,
;; depending of the type of the table and the mode requested.
;;
;; `org-latex--align-string' is a subroutine used to build alignment
;; string for Org tables.

(defun org-tufte-latex-table (table contents info)
  "Transcode a TABLE element from Org to LaTeX.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-latex--table.el-table table info)
      (let ((type (or (org-export-read-attribute :attr_latex table :mode)
                      (plist-get info :latex-default-table-mode))))
        (cond
          ;; Case 1: Verbatim table.
          ((string= type "verbatim")
           (format "\\begin{verbatim}\n%s\n\\end{verbatim}"
                   ;; Re-create table, without affiliated keywords.
                   (org-trim (org-element-interpret-data
                              `(table nil ,@(org-element-contents table))))))
          ;; Case 2: Matrix.
          ((or (string= type "math") (string= type "inline-math"))
           (org-latex--math-table table info))
          ;; Case 3: Standard table.
          (t (concat (org-tufte-latex--org-table table contents info)
                     ;; When there are footnote references within the
                     ;; table, insert their definition just after it.
                     (org-latex--delayed-footnotes-definitions table info)))))))

(defun org-tufte-latex--org-table (table contents info)
  "Return appropriate Tufte LaTeX code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."
  (let* ((caption (org-latex--caption/label-string table info))
         (attr (org-export-read-attribute :attr_latex table))
         ;; Determine alignment string.
         (alignment (org-latex--align-string table info))
         ;; Determine environment for the table: longtable, tabular...
         (table-env (or (plist-get attr :environment)
                        (plist-get info :latex-default-table-environment)))
         ;; If table is a float, determine environment: table, table*
         ;; or sidewaystable.
         (float-env (unless (member table-env '("longtable" "longtabu"))
                      (let ((float (plist-get attr :float)))
                        (cond
                          ((and (not float) (plist-member attr :float)) nil)
                          ((or (string= float "sidewaystable")
                               (string= float "sideways")) "sidewaystable")
                          ((string= float "multicolumn") "table*")
                          ;; td - start
                          ((string= float "margin") "margintable")
                          ;; td - end
                          ((or float
                               (org-element-property :caption table)
                               (org-string-nw-p (plist-get attr :caption)))
                           "table")))))
         ;; Extract others display options.
         (fontsize (let ((font (plist-get attr :font)))
                     (and font (concat font "\n"))))
         ;; "tabular" environment doesn't allow to define a width.
         (width (and (not (equal table-env "tabular")) (plist-get attr :width)))
         (spreadp (plist-get attr :spread))
	 (offset (and (equal "margintable" float-env) (plist-get attr :offset)))
         (placement
          (or (plist-get attr :placement)
              (format "[%s]" (plist-get info :latex-default-figure-position))))
         (centerp (if (plist-member attr :center) (plist-get attr :center)
                      (plist-get info :latex-tables-centered)))
         (caption-above-p (org-latex--caption-above-p table info)))
    ;; Prepare the final format string for the table.
    (cond
      ;; Longtable.
      ((equal "longtable" table-env)
       (concat (and fontsize (concat "{" fontsize))
               (format "\\begin{longtable}{%s}\n" alignment)
               (and caption-above-p
                    (org-string-nw-p caption)
                    (concat caption "\\\\\n"))
               contents
               (and (not caption-above-p)
                    (org-string-nw-p caption)
                    (concat caption "\\\\\n"))
               "\\end{longtable}\n"
               (and fontsize "}")))
      ;; Longtabu
      ((equal "longtabu" table-env)
       (concat (and fontsize (concat "{" fontsize))
               (format "\\begin{longtabu}%s{%s}\n"
                       (if width
                           (format " %s %s "
                                   (if spreadp "spread" "to") width) "")
                       alignment)
               (and caption-above-p
                    (org-string-nw-p caption)
                    (concat caption "\\\\\n"))
               contents
               (and (not caption-above-p)
                    (org-string-nw-p caption)
                    (concat caption "\\\\\n"))
               "\\end{longtabu}\n"
               (and fontsize "}")))
      ;; Others.
      (t (concat (cond
                   (float-env
                    (concat (format "\\begin{%s}%s\n" float-env
				    (if (string= "margintable" float-env)
					(if offset (format "[%s]" offset) "")
					placement))
                            (if caption-above-p caption "")
                            (when centerp "\\centering\n")
                            fontsize))
                   ((and (not float-env) caption)
                    (concat
                     (and centerp "\\begin{center}\n" )
                     (if caption-above-p caption "")
                     (cond ((and fontsize centerp) fontsize)
                           (fontsize (concat "{" fontsize)))))
                   (centerp (concat "\\begin{center}\n" fontsize))
                   (fontsize (concat "{" fontsize)))
                 (cond ((equal "tabu" table-env)
                        (format "\\begin{tabu}%s{%s}\n%s\\end{tabu}"
                                (if width (format
                                           (if spreadp " spread %s " " to %s ")
                                           width) "")
                                alignment
                                contents))
                       (t (format "\\begin{%s}%s{%s}\n%s\\end{%s}"
                                  table-env
                                  (if width (format "{%s}" width) "")
                                  alignment
                                  contents
                                  table-env)))
                 (cond
                   (float-env
                    (concat (if caption-above-p "" (concat "\n" caption))
                            (format "\n\\end{%s}" float-env)))
                   ((and (not float-env) caption)
                    (concat
                     (if caption-above-p "" (concat "\n" caption))
                     (and centerp "\n\\end{center}")
                     (and fontsize (not centerp) "}")))
                   (centerp "\n\\end{center}")
                   (fontsize "}")))))))

;;;; Footnote Reference

(defun org-export-get-footnote-definition--latex-attributes
    (footnote-reference info)
  "Return latex attributes of FOOTNOTE-REFERENCE, or nil if there
are none.  INFO is the plist used as a communication channel."
  (let ((label (org-element-property :label footnote-reference)))
    (when label
      (org-element-map (plist-get info :parse-tree)
          '(footnote-definition footnote-reference)
        (lambda (f)
          (and (equal (org-element-property :label f) label)
               (org-export-read-attribute :attr_latex f)))
        info t))))

(defun org-tufte-latex-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Tufte LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :latex-footnote-separator)))
   (cond
     ;; Use \footnotemark if the footnote has already been defined.
     ((not (org-export-footnote-first-reference-p footnote-reference info))
      (format "\\footnotemark[%s]{}"
              (org-export-get-footnote-number footnote-reference info)))
     ;; Use \footnotemark if reference is within another footnote
     ;; reference, footnote definition or table cell.
     ((org-element-lineage footnote-reference
                           '(footnote-reference footnote-definition table-cell))
      "\\footnotemark")
     ;; Otherwise, define it with \sidenote command.
     (t
      (let* ((def (org-export-get-footnote-definition footnote-reference info))
             (attr (org-export-get-footnote-definition--latex-attributes
                    footnote-reference info))
             (offs (plist-get attr :offset))
             (offset (if offs (format "[][%s]" offs) "[]")))
        (concat
         (format "\\sidenote%s{%s}" offset (org-trim (org-export-data def info)))
         ;; Retrieve all footnote references within the footnote and
         ;; add their definition after it, since LaTeX doesn't support
         ;; them inside.
         (org-latex--delayed-footnotes-definitions def info)))))))

;;; Interactive function

;;;###autoload
(defun org-tufte-latex-export-as-latex (&optional async subtreep visible-only)
  "Export current buffer to a Tufte LaTeX buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Tufte LaTeX Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'tufte-latex "*Org Tufte LaTeX Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


;;;###autoload
(defun org-tufte-latex-convert-region-to-latex ()
  "Assume the current region has org-mode syntax, and convert it
to Tufte LaTeX.  This can be used in any buffer.
For example, you can write an itemized list in org-mode syntax in
a Tufte LaTeX buffer and use this command to convert it."
  (interactive)
  (org-export-replace-region-by 'tufte-latex))


;;;###autoload
(defun org-tufte-latex-export-to-latex (&optional async subtreep visible-only)
  "Export current buffer to a Tufte LaTeX file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'tufte-latex outfile async subtreep visible-only)))

(defun org-tufte-latex-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'tufte-latex outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(provide 'ox-tufte-latex)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-tufte-latex.el ends here
