

(provide 'width)

(add-to-list 'org-latex-classes
  '("width"
"

\\documentclass[12pt]{article}
\\usepackage[includeheadfoot,margin=1.25in,hmargin=1.25in,vmargin=0.5in]{geometry} % for normal margins



\\usepackage[div9]{typearea}
\\usepackage{lipsum}

\\usepackage{letltxmacro}
\\makeatletter
\\LetLtxMacro{\\ltx@subsection}{\\subsection}
\\renewcommand{\\subsection}[2][]{\\ltx@subsection[#1]{\\protect\\parbox[t]{4in}{#2}}}
\\makeatother



\\renewcommand\\maketitle{}


      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(setq org-latex-to-pdf-process
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes
