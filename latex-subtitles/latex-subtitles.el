(defun my-org-latex-subtitle-filter (contents backend info)
  "Convert lines starting with ~ to subtitle environment in LaTeX export.
   Also fixes subtitle environments to use correct levels:
   - subtitle for sections
   - subsubtitle for subsections
   - subsubsubtitle for subsubsections"
  (when (eq backend 'latex)
    ;; Process the content line by line for more control
    (let ((lines (split-string contents "\n"))
          (result '())
          (last-heading-type nil))
      ;; Track what type of heading we're under
      (dolist (line lines)
        (cond
         ;; Track heading levels (both numbered and unnumbered with *)
         ((string-match "^\\\\section\\*?{" line)
          (setq last-heading-type 'section)
          (push line result))
         ((string-match "^\\\\subsection\\*?{" line)
          (setq last-heading-type 'subsection)
          (push line result))
         ((string-match "^\\\\subsubsection\\*?{" line)
          (setq last-heading-type 'subsubsection)
          (push line result))
         ;; Convert tilde lines to appropriate subtitle environment
         ((string-match "^\\\\textasciitilde{}[[:space:]]*\\(.+\\)$" line)
          (let ((subtitle-text (match-string 1 line))
                (env-name (cond
                          ((eq last-heading-type 'subsection) "subsubtitle")
                          ((eq last-heading-type 'subsubsection) "subsubsubtitle")
                          (t "subtitle"))))
            (push (format "\\begin{%s}" env-name) result)
            (push subtitle-text result)
            (push (format "\\end{%s}" env-name) result)))
         ;; Fix existing subtitle environments that are all "subtitle"
         ((string-match "^\\\\begin{subtitle}" line)
          (let ((env-name (cond
                          ((eq last-heading-type 'subsection) "subsubtitle")
                          ((eq last-heading-type 'subsubsection) "subsubsubtitle")
                          (t "subtitle"))))
            (push (format "\\begin{%s}" env-name) result)))
         ((string-match "^\\\\end{subtitle}" line)
          (let ((env-name (cond
                          ((eq last-heading-type 'subsection) "subsubtitle")
                          ((eq last-heading-type 'subsubsection) "subsubsubtitle")
                          (t "subtitle"))))
            (push (format "\\end{%s}" env-name) result)))
         ;; Pass through other lines
         (t (push line result))))
      ;; Join the lines back together
      (mapconcat 'identity (nreverse result) "\n"))))

;; Add the filter to final output
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-final-output-functions
               'my-org-latex-subtitle-filter))