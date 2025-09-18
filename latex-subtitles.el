(defun my-org-latex-subtitle-filter (contents backend info)
  "Convert lines starting with ~ to subtitle environment in LaTeX export.
   Uses different subtitle environments based on the heading level:
   - subtitle for sections
   - subsubtitle for subsections
   - subsubsubtitle for subsubsections"
  (when (eq backend 'latex)
    ;; Handle subtitles after sections
    (setq contents
          (replace-regexp-in-string
           "\\(\\\\section{[^}]+}\\)\n\\(\\\\label{[^}]+}\\)\n\\\\textasciitilde{}[[:space:]]*\\(.+\\)"
           "\\1\n\\2\n\\\\begin{subtitle}\n\\3\n\\\\end{subtitle}"
           contents))
    ;; Handle subtitles after subsections
    (setq contents
          (replace-regexp-in-string
           "\\(\\\\subsection{[^}]+}\\)\n\\(\\\\label{[^}]+}\\)\n\\\\textasciitilde{}[[:space:]]*\\(.+\\)"
           "\\1\n\\2\n\\\\begin{subsubtitle}\n\\3\n\\\\end{subsubtitle}"
           contents))
    ;; Handle subtitles after subsubsections
    (setq contents
          (replace-regexp-in-string
           "\\(\\\\subsubsection{[^}]+}\\)\n\\(\\\\label{[^}]+}\\)\n\\\\textasciitilde{}[[:space:]]*\\(.+\\)"
           "\\1\n\\2\n\\\\begin{subsubsubtitle}\n\\3\n\\\\end{subsubsubtitle}"
           contents)))
  contents)

;; Add the filter to final output
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-final-output-functions
               'my-org-latex-subtitle-filter))