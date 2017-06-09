(provide 'blue-ruin)

(add-to-list 'org-latex-classes
  '("blue-ruin"
"

\\documentclass[12pt]{article}
% \\usepackage[includeheadfoot,margin=1.0in,hmargin=1.0in,vmargin=0.5in]{geometry} % for normal margins
\\usepackage[includeheadfoot,margin=1.5in,hmargin=1.5in,vmargin=0.5in]{geometry} % for insanely wide margins
% \\usepackage[includeheadfoot,margin=2.0in,hmargin=2.0in,vmargin=0.5in]{geometry} % for insanely wide margins
\\usepackage{float}


\\usepackage{algorithm}
\\usepackage{amsmath}
\\usepackage{ifxetex}
\\ifxetex
  \\usepackage{fontspec,xltxtra,xunicode}
  \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
  \\setromanfont{Garamond Premier Pro}
%  \\setromanfont{Adobe Caslon Pro}
 \\setsansfont{Gotham Narrow Bold}
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
\\newfontfamily\\mytitlefont[Color={FB782C}]{Gotham Narrow Bold}
\\newfontfamily\\myauthorfont[Color={FB782C}]{Gotham Narrow Bold}
\\newfontfamily\\mybluefont[Color=electricblue]{Gotham Narrow Bold}
\\DeclareTextFontCommand{\\textbf}{\\bfseries\\color{electricblue}}
\\DeclareTextFontCommand{\\textit}{\\itshape}


\\usepackage{textcase}

\\pagenumbering{arabic}
\\makeatletter

%This macro now controls the position of the background pic
%Please do not change from here
\\newcommand\\BackgroundPic{%
\\put(0,0){%
\\parbox[b][\\paperheight]{\\paperwidth}{%
\\vfill
\\centering
%inside the tikzpicture environment, you can do anything you want with the image
\\begin{tikzpicture}

\\node [inner sep=0pt,outer sep=0pt] at (0,0) {\\includegraphics[width=\\paperwidth,height=\\paperheight]{\\the\\coverimage}};

\\node at  (0,5) [opacity=1.0] {\\parbox[b][0.5\\textheight]{\\textwidth}{%
  \\begin{raggedright}
  \\leavevmode
    \\vskip 1cm
  {\\mytitlefont\\fontsize{75}{85}\\bfseries{\\@title}\\par}
    \\vskip 1cm
    
    %{\\myauthorfont\\fontsize{30}{40}{{\\bfseries{\\@degree}\\par}}}

\\vfill
\\end{raggedright}}};
\\node at (0,-8) [opacity=1] {\\parbox[b][0.3\\textheight]{\\textwidth}{%
\\begin{raggedright}
\\vfill
{\\myauthorfont\\Large {\\@author}}
    \\newline
          {\\myauthorfont\\Large \\href{mailto:jay@storytelling.nyc}{jay@storytelling.nyc}}
        \\newline
{\\myauthorfont\\Large {Storytelling NYC}}
        \\newline
    {\\myauthorfont\\Large \\@date\\par}
    \\newline
{\\myauthorfont\\Large © 2017 \\@author}
    \\newline
{\\myauthorfont\\Large Private and Confidential}
    \\newline
%{\\myauthorfont\\Large \\href{http://jaydixit.com}{\\@degree }}
\\end{raggedright}
}};
\\end{tikzpicture}
%Don't change
\\vfill
}}}
%This macro executes a hook at the beginning of the document that  puts the background correctly. 
\\AtBeginDocument{\\AddToShipoutPicture*{\\BackgroundPic}}
\\AtBeginDocument{\\globalcolor{ghostlygrey}}



%The maketitle macro now only includes the titling and not the background. 
\\def\\maketitle{ \\newgeometry{margin=1in} \\thispagestyle{empty} \\vfill \\null \\cleardoublepage\\restoregeometry}



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
\\newlist{mylist}{enumerate}{10} 
%\\setlist{nolistsep}
\\setlist{topsep=0pt}
\\renewcommand{\\labelitemi}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemiii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemiv}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemv}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemvi}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemvii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemviii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemix}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemx}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}

\\setlistdepth{10}
\\setlist[itemize,1]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,2]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,3]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,4]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,5]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,6]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,7]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,8]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,9]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,10]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\renewlist{itemize}{itemize}{10}





\\definecolor{azure}{HTML}{f2feff}

\\usepackage{lipsum}
\\usepackage{tikz}
\\usetikzlibrary{backgrounds}
\\makeatletter

\\tikzset{%
  fancy quotes/.style={
    text width=\\fq@width pt,
    align=justify,
    inner sep=1em,
    anchor=north west,
    minimum width=\\linewidth,
  },
  fancy quotes width/.initial={.8\\linewidth},
  fancy quotes marks/.style={
    scale=8,
    text=black,
    inner sep=0pt,
  },
  fancy quotes opening/.style={
    fancy quotes marks,
  },
  fancy quotes closing/.style={
    fancy quotes marks,
  },
  fancy quotes background/.style={
    show background rectangle,
    inner frame xsep=0pt,
    background rectangle/.style={
      fill=azure,
      rounded corners,
    },
  }
}

\\newenvironment{fancyquotes}[1][]{%
\\noindent
\\tikzpicture[fancy quotes background]
\\node[fancy quotes opening,anchor=north west] (fq@ul) at (0,0) {``};
\\tikz@scan@one@point\\pgfutil@firstofone(fq@ul.east)
\\pgfmathsetmacro{\\fq@width}{\\linewidth - 2*\\pgf@x}
\\node[fancy quotes,#1] (fq@txt) at (fq@ul.north west) \\bgroup}
{\\egroup;
\\node[overlay,fancy quotes closing,anchor=east] at (fq@txt.south east) {''};
\\endtikzpicture}
\\makeatother


\\usepackage{setspace}
\\usepackage{lipsum}
\\usepackage{etoolbox}
\\AtBeginEnvironment{quote}{\\singlespace\\vspace{-\\topsep}\\small}
\\AtEndEnvironment{quote}{\\vspace{-\\topsep}\\endsinglespace}


\\usepackage[sc]{titlesec}
\\titlespacing*{\\section}{0pt}{6pt}{7pt}
\\titlespacing*{\\subsection}{0pt}{0pt}{7pt}
\\titlespacing*{\\subsubsection}{0pt}{6pt}{5pt}

\\titleformat*{\\section}{\\normalfont\\fontsize{36}{36}\\raggedright\\bfseries\\sffamily\\color{pinterestred}}
\\titleformat*{\\subsection}{\\normalfont\\fontsize{20}{20}\\scshape\\color{electricblue}}
\\titleformat*{\\subsubsection}{\\normalfont\\fontsize{12}{8}\\raggedright\\bfseries\\rmfamily\\color{pinterestred}}
\\titleformat*{\\paragraph}{\\normalfont\\normalsize\\raggedright\\bfseries\\rmfamily\\color{electricblue}}
\\titleformat*{\\subparagraph}{\\normalfont\\fontsize{14}{14}\\raggedright\\bfseries\\ttfamily\\color{pinterestred}}
\\usepackage[breaklinks=true,linktocpage,xetex]{hyperref} 
\\hypersetup{colorlinks, citecolor=electricblue,filecolor=electricblue,linkcolor=electricblue,urlcolor=electricblue}




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
