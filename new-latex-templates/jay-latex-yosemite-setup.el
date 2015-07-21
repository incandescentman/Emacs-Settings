(require 'ox-latex)

;; Find LaTeX on my system
(if (eq window-system 'mac)
    (add-to-list 'exec-path "/usr/local/texlive/2014/bin/universal-darwin")
  )

;; XeLaTeX customisations

(setq  ; org-export-dispatch-use-expert-ui t non-intrusive export dispatch
 org-latex-pdf-process               ; for regular export

 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; TOC and section numbers
(setq org-export-with-section-numbers t)
(setq org-export-with-toc t)

