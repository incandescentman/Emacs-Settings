(provide 'hipster-cv)

(add-to-list 'org-latex-classes
             '("hipster-cv"
               "
\\documentclass[lighthipster]{simplehipstercv}
% available options: darkhipster, lighthipster, pastel, allblack, grey, verylight, withoutsidebar
\\usepackage[utf8]{inputenc}
\\usepackage[default]{raleway}
\\usepackage[margin=1cm, a4paper]{geometry}

%------------------------------------------------------------------ Variables

\\newlength{\\rightcolwidth}
\\newlength{\\leftcolwidth}
\\setlength{\\leftcolwidth}{0.23\\textwidth}
\\setlength{\\rightcolwidth}{0.75\\textwidth}

%------------------------------------------------------------------

\\pagestyle{empty}

[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section*{%s}" . "\\section*{%s}")
               ("\\subsection*{%s}" . "\\subsection*{%s}")
               ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph{%s}")))

(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f")) ;; for multiple passes
