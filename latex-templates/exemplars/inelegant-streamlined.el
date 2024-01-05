(provide 'inelegant-streamlined)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
    '("inelegant-streamlined"
      "
\\documentclass[12pt]{article}
\\usepackage[margin=1.5in]{geometry}
\\linespread{1.2}

\\usepackage{fontspec}
\\defaultfontfeatures{Ligatures=TeX,Scale=MatchLowercase}
\\setmainfont[
  Path = /Users/jay/Library/Fonts/,
  UprightFont = HelveticaNowText-Light,
  BoldFont = HelveticaNowText-Bold,
  ItalicFont = HelveticaNowText-LightItalic,
  Extension = .ttf
]{Helvetica Display}

\\setsansfont{TeX Gyre Pagella}

\\usepackage{enumitem}
\\setlist{noitemsep, topsep=-8pt, after=\\vspace{12pt}}

\\usepackage{titlesec}
\\titlespacing*{\\section}{1.5ex}{12pt}{0pt}
\\titleformat*{\\section}{\\sffamily\\fontsize{24}{36}\\selectfont}
\\titleformat*{\\subsection}{\\normalfont\\fontsize{18}{15}\\selectfont}

\\usepackage[breaklinks=true,linktocpage,xetex]{hyperref}
\\hypersetup{colorlinks, citecolor=blue, filecolor=blue, linkcolor=blue, urlcolor=blue}

\\renewcommand\\maketitle{}

      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-to-pdf-process
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f"))
