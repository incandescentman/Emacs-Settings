(defun my-org-latex-subtitle-filter (contents backend info)
  "Convert lines starting with ~ to subtitle environment in LaTeX export."
  (when (eq backend 'latex)
    ;; Replace the escaped tilde pattern with subtitle environment
    (setq contents
          (replace-regexp-in-string
           "\\\\textasciitilde{}[[:space:]]*\\(.+\\)"
           "\\\\begin{subtitle}\n\\1\n\\\\end{subtitle}"
           contents)))
  contents)

;; Add the filter to final output
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-final-output-functions
               'my-org-latex-subtitle-filter))