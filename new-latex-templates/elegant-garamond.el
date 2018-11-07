(provide 'elegant-garamond)

(add-to-list 'org-latex-classes
  '("elegant-garamond"
"

\\documentclass[12pt]{article}
\\usepackage[includeheadfoot,margin=1.5in,hmargin=1.5in,vmargin=0.5in]{geometry} % for normal margins


\\usepackage{float}
\\usepackage{changepage}
\\usepackage{algorithm}
\\usepackage{pdfpages}
\\usepackage{amsmath}
\\usepackage{ifxetex}
\\ifxetex
  \\usepackage{fontspec,xltxtra,xunicode}
  \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
\\setromanfont{Garamond Premier Pro}
 \\setsansfont{TeX Gyre Pagella}
  \\setmonofont{TeX Gyre Heros}
\\else
  \\usepackage[mathletters]{ucs}
  \\usepackage[utf8x]{inputenc}
\\fi
\\usepackage{url}
\\usepackage{paralist}
\\setkeys{Gin}{resolution=72}
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
\\definecolor{elegantblue}{HTML}{4380b9}

\\newtoks\\leftheader 
\\newtoks\\leftheaderurl
\\newtoks\\coverimage

\\hyphenpenalty=5000 
\\tolerance=1000



\\usepackage{magaz}


%Define Bold face
\\DeclareTextFontCommand{\\textbf}{\\sffamily\\bfseries}
\\DeclareTextFontCommand{\\textit}{\\itshape}

\\usepackage{letterspace}
\\usepackage{microtype}


\\pagenumbering{arabic}
\\makeatletter



\\setcounter{secnumdepth}{0}


\\usepackage[labelformat=empty]{caption}



\\usepackage{fancyhdr}
\\fancyhf{} % sets both header and footer to nothing
\\renewcommand{\\headrulewidth}{0pt}
\\pagestyle{fancy}


% doesn't work
% source: https://tex.stackexchange.com/questions/236705/how-do-i-show-the-subsection-name-and-the-subsubsection-number-and-name-in-a-fan
% https://tex.stackexchange.com/questions/310046/fancyhdr-and-redefinition-of-leftmark-rightmark
%\\usepackage{blindtext}
%\\let\\Sectionmark\\sectionmark
%\\def\\sectionmark#1{\\def\\Sectionname{#1}\\Sectionmark{#1}}
%\\let\\Subsectionmark\\subsectionmark
%\\def\\subsectionmark#1{\\def\\Subsectionname{#1}\\Subsectionmark{#1}}
%\\let\\Subsubsectionmark\\subsubsectionmark
%\\def\\subsubsectionmark#1{\\def\\Subsubsectionname{#1}\\Subsubsectionmark{#1}}


% \\fancyhf{}
% \\fancyhead[L]{\\thesection.\\ \\Sectionname} % displays the section (1. SECTION NAME)
% \\fancyhead[R]{\\thesubsection.\\ \\Subsectionname} % displays the subsection (1.1 SUBSECTION NAME)
% \\fancyfoot[R]{\\thesubsubsection.\\ \\Subsubsectionname}



\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\lhead{\\scshape\\href{\\the\\leftheaderurl}{\\the\\leftheader}}
\\chead{}
\\rhead{{\\scshape{\\leftmark}}}
% \\rhead{\\@title: {{\\leftmark}}}
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


\\usepackage{setspace}
\\onehalfspacing
\\setstretch{1.2} 

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

%\\setlength{\\intextsep}{10pt plus 1.0pt minus 2.0pt}

\\newenvironment{indentedsection}
{  {\\adjustwidth{2em}{0pt}}
  {\\endadjustwidth}
}

\\usepackage{setspace}
\\usepackage{lipsum}
\\usepackage{etoolbox}
\\AtBeginEnvironment{quote}{\\singlespace\\vspace{-\\topsep}\\small}
\\AtEndEnvironment{quote}{\\vspace{-\\topsep}\\endsinglespace}


\\usepackage[sc]{titlesec}


\\newlength\\TitleOverhang
\\setlength\\TitleOverhang{1.5cm}

\\newcommand\\Overhang[1]{%
 \\llap{\\makebox[\\TitleOverhang][l]{#1}}%
}


% \\titlespacing{command}{left spacing}{before spacing}{after spacing}[right]
\\titlespacing*{\\section}{0pt}{150pt}{40pt}
%\\titlespacing*{\\subsection}{0pt}{0pt}{-6pt}
%\\titlespacing*{\\subsubsection}{0pt}{6pt}{-6pt}



\\titleformat*{\\section}{\\ttfamily\\scshape\\fontsize{40}{36}\\raggedleft\\ttfamily}
\\titleformat*{\\subsection}{\\sffamily\\bfseries\\fontsize{24}{36}\\raggedright\\sffamily}
\\titleformat*{\\subsubsection}{\\sffamily\\bfseries\\fontsize{17}{16}\\raggedright\\sffamily}

\\titleformat*{\\paragraph}{\\ttfamily\\fontsize{15}{12}\\raggedright\\bfseries}
\\titleformat*{\\subparagraph}{\\sffamily\\fontsize{12}{12}\\raggedright\\bfseries\\ttfamily}


\\DeclareTextFontCommand{\\nonsection}{\\sffamily\\fontsize{19}{19}\\raggedright\\sffamily\\textlf}

\\DeclareTextFontCommand{\\nonsubsection}{\\sffamily\\itshape\\fontsize{18}\\raggedright\\sffamily}

\\DeclareTextFontCommand{\\nonsubsubsection}{\\sffamily\\fontsize{18}\\raggedright\\sffamily}

\\newenvironment{tagline}% environment name 
{% begin code
\\vspace{-12pt}
\\large
\\begin{itshape}% 
  \\par\\vspace{\\baselineskip}\\noindent\\ignorespaces 
}% 
{% end code 
  \\end{itshape}\\ignorespacesafterend 
}

\\usepackage[breaklinks=true,linktocpage,xetex]{hyperref}
\\hypersetup{colorlinks, citecolor=elegantblue,filecolor=elegantblue,linkcolor=elegantblue,urlcolor=elegantblue}

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
