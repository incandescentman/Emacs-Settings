(provide 'blue-ruin-no-cover)

(add-to-list 'org-latex-classes
  '("blue-ruin-no-cover"
"

\\documentclass[12pt]{article}
\\usepackage[includeheadfoot,margin=1.0in,hmargin=1.0in,vmargin=0.5in]{geometry}
\\usepackage{float}


\\usepackage{algorithm}
\\usepackage{amsmath}
\\usepackage{ifxetex}
\\ifxetex
  \\usepackage{fontspec,xltxtra,xunicode}
  \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
  \\setromanfont{Garamond Premier Pro}
 \\setsansfont{ChunkFive}
  \\setmonofont{Myriad Pro}
\\else
  \\usepackage[mathletters]{ucs}
  \\usepackage[utf8x]{inputenc}
\\fi
\\usepackage{url}
\\usepackage{paralist}
\\usepackage{graphicx}
\\usepackage{tikz}
\\usepackage{calc}
\\usepackage{eso-pic}
\\usepackage{etoolbox}
\\usepackage{xcolor}
\\PassOptionsToPackage{hyperref,x11names}{xcolor}
\\definecolor{pinterestred}{HTML}{C92228}
\\definecolor{ulyssesbutterflyblue}{HTML}{1464F4}
\\definecolor{signalflare}{HTML}{FB782C}
\\definecolor{niceorange}{HTML}{77CC6D}
\\definecolor{ghostlygrey}{HTML}{000000}
\\definecolor{firstcolor}{HTML}{00ADEF}
\\definecolor{secondcolor}{HTML}{DD3E74}
\\definecolor{periodblue}{HTML}{12239e}
\\definecolor{denimblue}{HTML}{3A5F90}
\\definecolor{electricblue}{HTML}{05ADF3}


\\newtoks\\leftheader 
\\newtoks\\leftheaderurl
\\newtoks\\coverimage


\\hyphenpenalty=5000 
\\tolerance=1000

%This macro is to make cleaner the specification of the titling font
\\newfontfamily\\mytitlefont[Color={FB782C}]{ChunkFive}
\\newfontfamily\\myauthorfont[Color={FB782C}]{Gill Sans Display MT Pro}
\\newfontfamily\\mybluefont[Color=electricblue]{Gill Sans Display MT Pro}
\\DeclareTextFontCommand{\\textbf}{\\rmfamily\\color{electricblue}}
\\DeclareTextFontCommand{\\textit}{\\\itshape\\color{electricblue}}



\\usepackage{textcase}

\\pagenumbering{arabic}
\\makeatletter


\\setcounter{secnumdepth}{0}




\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\lhead{\\href{\\the\\leftheaderurl}{\\the\\leftheader}}
\\chead{}
\\rhead{\\@title: {\\nouppercase{\\leftmark}}}
\\lfoot{}
\\cfoot{}
\\rfoot{}
\\usepackage{listings}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{12pt plus 2pt minus 1pt} %Space between paragraphs
\\usepackage{fancyvrb}
\\usepackage{enumerate}
\\usepackage{ctable}
\\setlength{\\paperwidth}{8.5in}
\\setlength{\\paperheight}{11in}
  \\tolerance=1000
\\usepackage{tocloft}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}
\\usepackage[normalem]{ulem}


\\makeatletter
\\newcommand{\\globalcolor}[1]{%
  \\color{#1}\\global\\let\\default@color\\current@color
}
\\makeatother

\\newcommand{\\textsubscr}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}

\\usepackage{enumitem}
%\\setlist{nolistsep}
\\setlist{topsep=0pt}
\\renewcommand{\\labelitemi}{$\\bullet$}
\\renewcommand{\\labelitemii}{$\\bullet$}
\\renewcommand{\\labelitemiii}{$\\bullet$}
\\renewcommand{\\labelitemiv}{$\\bullet$}

\\usepackage[sc]{titlesec}
\\titlespacing*{\\section}{0pt}{6pt}{-7pt}
\\titlespacing*{\\subsection}{0pt}{0pt}{-7pt}
\\titlespacing*{\\subsubsection}{0pt}{6pt}{-5pt}

\\titleformat*{\\section}{\\normalfont\\fontsize{36}{36}\\raggedright\\sffamily\\color{pinterestred}}
\\titleformat*{\\subsection}{\\normalfont\\fontsize{20}{20}\\scshape\\color{electricblue}}
\\titleformat*{\\subsubsection}{\\normalfont\\fontsize{12}{8}\\raggedright\\bfseries\\rmfamily\\color{pinterestred}}
\\titleformat*{\\paragraph}{\\normalfont\\normalsize\\raggedright\\bfseries\\rmfamily\\color{pinterestred}}
\\titleformat*{\\subparagraph}{\\normalfont\\fontsize{14}{14}\\raggedright\\bfseries\\ttfamily\\color{electricblue}}
\\usepackage[breaklinks=true,linktocpage,xetex]{hyperref} 
\\hypersetup{colorlinks, citecolor=electricblue,filecolor=electricblue,linkcolor=electricblue,urlcolor=electricblue}


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
