(provide 'elegant)

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
  '("elegant"
"

\\documentclass[12pt]{article}
\\usepackage[includeheadfoot,margin=1.5in,hmargin=1.5in,vmargin=0.5in]{geometry} % for normal margins

\\linespread{1.2}


\\usepackage{float}
%\\usepackage{changepage}

%\\usepackage{algorithm}
%\\usepackage{amsmath}


\\usepackage{fontspec}
\\defaultfontfeatures{Ligatures=TeX,Scale=MatchLowercase}

% % define main font
\\setmainfont[
  Path = /Users/jay/Library/Fonts/,
  UprightFont = HelveticaNowText-Light,
  BoldFont = HelveticaNowText-Bold,
  ItalicFont = HelveticaNowText-LightItalic,
  BoldItalicFont = HelveticaNowDisplay-BoldIta,
  Extension = .ttf
]{Helvetica Display}

\\setsansfont{TeX Gyre Pagella}

%\\newfontfamily{\\thindisplayfont}{HelveticaNowDisplay-Light}
\\setmonofont{Consolas}[Scale=0.9]

%\\usepackage[mathletters]{ucs}
%\\usepackage[utf8x]{inputenc}


\\usepackage[obeyspaces]{url}
\\PassOptionsToPackage{obeyspaces}{url}

\\usepackage{paralist}
%\\usepackage{graphicx}
\\usepackage{wrapfig}
\\usepackage{setspace}
\\setkeys{Gin}{resolution=72}
\\usepackage{tikz}

%\\usepackage{calc}
%\\usepackage{eso-pic}
%\\usepackage{etoolbox}
\\usepackage{xcolor}
\\PassOptionsToPackage{hyperref,x11names}{xcolor}
\\definecolor{pinterestred}{HTML}{C92228}
\\definecolor{ulyssesbutterflyblue}{HTML}{1464F4}
\\definecolor{signalflare}{HTML}{FB782C}
\\definecolor{niceorange}{HTML}{77CC6D}
\\definecolor{highlighteryellow}{HTML}{FFFF01}
\\definecolor{ghostlygrey}{HTML}{000000}
\\definecolor{firstcolor}{HTML}{00ADEF}
\\definecolor{secondcolor}{HTML}{DD3E74}
\\definecolor{periodblue}{HTML}{12239e}
\\definecolor{denimblue}{HTML}{3A5F90}
\\definecolor{electricblue}{HTML}{05ADF3}
\\definecolor{resonateblue}{HTML}{005778}
\\definecolor{resonateorange}{HTML}{da7635}
\\definecolor{resonategrey}{HTML}{4d4d4c}
\\definecolor{nliblue}{HTML}{2f9ed3}
%\\definecolor{dullerelegantblue}{HTML}{4380b9}
\\definecolor{elegantblue}{HTML}{1792d1}
\\definecolor{ideablue}{HTML}{55C1E7}
\\definecolor{powderblue}{HTML}{f5f7ff}
\\definecolor{stormybluegrey}{HTML}{898ea4}
\\definecolor{moonrockgrey}{HTML}{212121}
\\definecolor{libertyblue}{HTML}{73b0be}


% \\newtoks\\leftheader
% \\newtoks\\leftheaderurl
% \\newtoks\\coverimage

\\raggedright
\\hyphenpenalty=5000
\\tolerance=1000

\\usepackage{textcase}

\\pagenumbering{arabic}
\\makeatletter


\\renewcommand{\\contentsname}{Table of Contents}

\\setcounter{secnumdepth}{0}


\\usepackage[labelformat=empty]{caption}

\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\lhead{\\href{\\the\\leftheaderurl}{\\the\\leftheader}}
\\chead{}
\\rhead{{\\nouppercase{\\leftmark}}}
% \\rhead{\\@title: {\\nouppercase{\\leftmark}}}

\\lhead{\\bfseries\\@title}}} % title of the document as left header


\\lfoot{}
\\cfoot{\\thepage}
\\rfoot{}
\\usepackage{listings}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{12pt plus 2pt minus 1pt} % space between paragraphs

% spacing: how to read {12pt plus 4pt minus 2pt}
%           12pt is what we would like the spacing to be
%           plus 4pt means that TeX can stretch it by at most 4pt
%           minus 2pt means that TeX can shrink it by at most 2pt
%       This is one example of the concept of, 'glue', in TeX

%\\usepackage{fancyvrb}
\\usepackage{enumerate}
\\usepackage{ctable}
\\setlength{\\paperwidth}{8.5in}
\\setlength{\\paperheight}{11in}
  \\tolerance=1000
\\usepackage{tocloft}
\\renewcommand{\\cftsecfont}{\\normalfont}
\\renewcommand{\\cftsecpagefont}{\\normalfont}
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


% control line spacing in bulleted list
\\setlist{noitemsep, topsep=-8pt, after=\\vspace{12pt}} % for no spacing between list items
% see: https://tex.stackexchange.com/questions/199118/modifying-whitespace-before-and-after-list-separately-using-enumitem-package
%\\setlist{topsep=0pt} % for a line between list items


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

%\\usepackage{lipsum}

%\\setlength{\\intextsep}{10pt plus 1.0pt minus 2.0pt}

\\newenvironment{indentedsection}
  {\\adjustwidth{2em}{0pt}}
  {\\endadjustwidth}



\\usepackage[most]{tcolorbox}
\\usepackage{etoolbox}

\\tcbuselibrary{skins}

\\AtBeginEnvironment{quote}{\\vspace{1em}}
\\AtEndEnvironment{quote}{\\vspace{1em}}



\\renewenvironment{quote}
{%
  \\begin{center}
  \\begin{tcolorbox}[
    colback=powderblue,  % Background color
    colframe=stormybluegrey,  % Frame color
    colupper=moonrockgrey,  % Text color
    boxrule=0.5pt,  % Border thickness
    rounded corners,  % Rounded corners
    fontupper=\\singlespacing\\fontsize{9}{11}\\selectfont\\ttfamily,  % Single-spacing
    width=0.8\\textwidth,  % Width
    halign=flush left  % Left alignment inside the box
  ]
}
{%
  \\end{tcolorbox}
  \\end{center}
}



\\usepackage[sc]{titlesec}


\\newlength\\TitleOverhang
\\setlength\\TitleOverhang{1.5cm}

\\newcommand\\Overhang[1]{%
 \\llap{\\makebox[\\TitleOverhang][l]{#1}}%
}



% \\titlespacing{command}{left spacing}{before spacing}{after spacing}[right]
\\titlespacing*{\\section}{1.5ex}{12pt}{0pt}
\\titlespacing*{\\subsection}{0pt}{0pt}{-6pt}
\\titlespacing*{\\subsubsection}{0pt}{0pt}{-12pt}

%\\titleformat*{\\section}{font}\\fontsize{size}{baseline-skip}\\raggedright\\sffamily}

\\titleformat*{\\section}{\\sffamily\\setstretch{0.1}\\fontsize{24}{40}\\raggedright\\sffamily}

\\titleformat*{\\subsection}{\\fontspec{HelveticaNowDisplay-Bold}\\fontsize{18}{15}\\selectfont\\raggedright\\color{black}}

\\titleformat*{\\subsubsection}{\\fontspec{HelveticaNowDisplay-Bold}\\fontsize{14}{16}\\raggedright\\color{black}}


\\titleformat{\\paragraph}[block]{\\normalfont\\sffamily\\fontsize{13}{12}\\bfseries\\color{black}}{}{0em}{}
\\titlespacing*{\\paragraph}{0pt}{0pt}{-6pt}



\\titleformat{\\subparagraph}{\\normalfont\\sffamily\\fontsize{13}{12}\\bfseries\\color{black}}{}{0em}{}
\\titlespacing*{\\subparagraph}{0pt}{0pt}{-6pt}


\\usepackage[breaklinks=true,linktocpage,xetex]{hyperref}
\\hypersetup{colorlinks, citecolor=libertyblue,filecolor=libertyblue,linkcolor=libertyblue,urlcolor=libertyblue}

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
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes
