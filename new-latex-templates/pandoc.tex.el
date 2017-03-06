(provide 'jay-pandoc)

(add-to-list 'org-latex-classes
  '("jay-pandoc"
"


\\documentclass[12pt]{article}
\\usepackage[hyperref,x11names]{xcolor}
\\usepackage{float}
% \\usepackage{amsmath}
% \\usepackage{algorithm}
\\usepackage{fontspec,xltxtra,xunicode}
 \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
 \\setromanfont{Adobe Garamond Pro}
 \\setsansfont{Helvetica Neue}
 \\setmonofont{Courier}
% \\usepackage{microtype}
% \\usepackage{listings}
% \\lstnewenvironment{code}{\\lstset{language=Haskell,basicstyle=\\small\\ttfamily}}{}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{12pt plus 2pt minus 1pt}
% \\usepackage{fancyvrb}
\\usepackage{enumerate}
\\usepackage{ctable}
\\usepackage{color}
\\usepackage{titlesec}
\\usepackage{fix-cm}

 \\titleformat{\\section}[hang]{\\sffamily\\bfseries}{ % bold heading
 % \\titleformat{\\section}[hang]{\\sffamily}{ % nonbold heading
\\fontsize{100}{100}\\color{SteelBlue4}\\thesection}{0pt}{\\linebreak\\huge\\raggedright}[{\\titlerule[0.5pt]}]



\\setlength{\\paperwidth}{8.5in}
\\setlength{\\paperheight}{11in}
\\usepackage{tocloft}
% Add dots to chapter entries in the TOC
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}

\\usepackage[includeheadfoot,margin=2.3in,hmargin=1in,vmargin=1.5in]{geometry}
 \\tolerance=1000

\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\pagenumbering{arabic}
\\lhead{\\href{http://jaydixit.com}{Jay Dixit}}
\\chead{\\itshape}
\\rhead{\\itshape{\\nouppercase{$title$}: {\\nouppercase\\leftmark}}}
\\lfoot{$version$}
\\cfoot{\\thepage}
\\rfoot{}



% This is needed because raggedright in table elements redefines \\\\:
\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}
\\let\\PBS=\\PreserveBackslash
% \\usepackage[normalem]{ulem}
% \\newcommand{\\textsubscr}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}
\\usepackage[breaklinks=true,linktocpage,pdftitle={$title$},pdfauthor={$for(author)$$author$$sep$; $endfor$},xetex]{hyperref}
\\usepackage{url}
% \\usepackage{graphicx}

\\usepackage{hyperref}
\\hypersetup{ colorlinks, citecolor=SteelBlue4,filecolor=SteelBlue4,linkcolor=SteelBlue4,urlcolor=SteelBlue4}

\\setcounter{secnumdepth}{-1}

% \\VerbatimFootnotes % allows verbatim text in footnotes

\\makeatletter
\\def\\maketitle{%
 \\thispagestyle{empty}%
 \\vfill
 \\begin{raggedright}
 \\leavevmode
 \\vskip 1cm
 {\\fontsize{50}{60}\\selectfont{\\textbf{\\@title}}\\par}%

 \\normalfont
  {\\fontsize{30}{40}\\selectfont{\\textit{\\@author}}\\par}%
   \\vskip 1cm
 \\vfill
  {\\Large Jay Dixit}%
 \\newline
   {\\Large \\href{mailto:dixit@aya.yale.edu}{dixit@aya.yale.edu}}%
 \\newline
   {\\Large \\href{http://jaydixit.com/}{jaydixit.com} }%
 \\newline
 {\\Large \\@date\\par}%
 \\null
  \\end{raggedright}%
 \\cleardoublepage
 }

\\usepackage{titlesec}

\\titleformat*{\\section}{\\sffamily\\Large\\bfseries\\color{SteelBlue4}}
\\titleformat*{\\subsection}{\\sffamily\\large\\bfseries\\color{SteelBlue4}}
\\titleformat*{\\subsubsection}{\\sffamily\\normalsize\\bfseries\\color{SteelBlue4}}
\\titleformat*{\\paragraph}{\\sffamily\\normalsize\\bfseries\\color{SteelBlue4}}
\\titleformat*{\\subparagraph}{\\sffamily\\normalsize\\bfseries\\color{SteelBlue4}}



\\title{$title$}

\\author{$for(author)$$author$$sep$\\\\$endfor$}
\\begin{document}

\\maketitle



\\newpage
\\thispagestyle{empty}
\\tableofcontents
\\newpage


$body$





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
