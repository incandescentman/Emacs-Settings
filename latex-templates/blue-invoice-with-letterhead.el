(provide 'blue-invoice-letterhead)

(add-to-list 'org-latex-classes
             '("blue-invoice-letterhead"
               "

\\documentclass[12pt]{article}
\\usepackage[includeheadfoot,margin=1.0in,hmargin=1.0in,vmargin=0.5in]{geometry}
\\usepackage{float}
\\usepackage{changepage}

\\usepackage{algorithm}
\\usepackage{amsmath}
\\usepackage{ifxetex}
\\ifxetex
  \\usepackage{fontspec,xltxtra,xunicode}
  \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}

\\usepackage{fontspec}

% define Helvetica Now font weights
\\setmainfont{HelveticaNow}[
  Path = /Users/jay/Library/Fonts/,
  UprightFont = HelveticaNowText-Light,
  BoldFont = HelveticaNowDisplay-Bold,
  ItalicFont = HelveticaNowText-LightItalic,
  BoldItalicFont = HelveticaNowDisplay-BoldIta,
  Extension = .ttf
]

\\setromanfont{HelveticaNowText-Light}
\\setsansfont{HelveticaNowDisplay-Regular}

% define sans font
\\setsansfont{Helvetica Neue LT Pro}[
  Path = /Users/jay/Library/Fonts/,
UprightFont = HelveticaNeueLTPro-MdCn,
  BoldFont = HelveticaNeueLTPro-BdCn,
  Extension = .otf
]

\\newfontfamily{\\thindisplayfont}{HelveticaNowDisplay-Light}
\\setmonofont{Myriad Pro}


\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\renewcommand{\\headrulewidth}{0pt} % Removes the default horizontal

\\else
  \\usepackage[mathletters]{ucs}
  \\usepackage[utf8x]{inputenc}
\\fi
\\usepackage{url}
\\usepackage{paralist}
\\usepackage{graphicx}
%\\usepackage{tikz}
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
\\definecolor{resonateblue}{HTML}{005778}
\\definecolor{resonateorange}{HTML}{da7635}
\\definecolor{resonategrey}{HTML}{4d4d4c}
\\definecolor{nliblue}{HTML}{2f9ed3}
\\definecolor{elegantblue}{HTML}{1792d1}
\\definecolor{ideablue}{HTML}{55C1E7}
\\definecolor{libertyblue}{HTML}{73b0be}
\\definecolor{darklibertyblue}{HTML}{19455b}


\\newtoks\\leftheader
\\newtoks\\leftheaderurl
\\newtoks\\coverimage
\\newtoks\\rightheader

\\raggedright
\\hyphenpenalty=5000
\\tolerance=1000


\\renewcommand{\\contentsname}{Invoice}



%This macro is to make cleaner the specification of the titling font
\\newfontfamily\\mytitlefont[Color={libertyblue}]{Arial}
\\newfontfamily\\myauthorfont[Color={libertyblue}]{Arial}
\\newfontfamily\\mybluefont[Color=libertyblue]{Arial}

%Define Bold face
\\DeclareTextFontCommand{\\textbf}{\\sffamily\\bfseries}
\\DeclareTextFontCommand{\\textit}{\\itshape}

\\usepackage{textcase}

\\pagenumbering{arabic}
\\makeatletter


\\setcounter{secnumdepth}{0}


\\usepackage[labelformat=empty]{caption}


\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\renewcommand{\\headrulewidth}{0pt} % Removes the default horizontal line in the header


\\fancyhead[C]{%
  \\begin{minipage}[c][1in][c]{1\\linewidth}
    \\includegraphics[height=1in,keepaspectratio]{/Users/jay/Dropbox/github/incandescentman.github.io/assets/images/2023-10-final-new-logo_high-res-no-text.png}
  \\end{minipage}%
  \\hspace{0in} % Reducing space to 0 inches between the logo and the text
  \\begin{minipage}[c][1in][c]{0.3\\linewidth}
    \\raggedright % Left-align the text in the minipage
    \\raisebox{0.59in}[0pt][0pt]{ % Raise the text by 0.7 inches
      {\\fontsize{73}{82}\\sffamily\\color{darklibertyblue} STORYTELLING.NYC}
    }
  \\end{minipage}%
  \\hspace{2.7in} % Move the text 1 inch to the left
}



\\rhead{}
\\lfoot{}
\\cfoot{}
\\rfoot{}


\\usepackage{listings}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{12pt plus 2pt minus 1pt} % space between paragraphs

% spacing: how to read {12pt plus 4pt minus 2pt}
%           12pt is what we would like the spacing to be
%           plus 4pt means that TeX can stretch it by at most 4pt
%           minus 2pt means that TeX can shrink it by at most 2pt
%       This is one example of the concept of, 'glue', in TeX

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
  {\\adjustwidth{2em}{0pt}}
  {\\endadjustwidth}


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
\\titlespacing*{\\section}{0pt}{24pt}{-6pt}
\\titlespacing*{\\subsection}{0pt}{16pt}{-6pt}
\\titlespacing*{\\subsubsection}{0pt}{6pt}{-6pt}


\\titleformat*{\\section}{\\sffamily\\bfseries\\fontsize{30}{20}\\raggedright\\sffamily\\scshape\\color{libertyblue}}
\\titleformat*{\\subsection}{\\sffamily\\fontsize{18}{15}\\raggedright\\scshape\\color{libertyblue}}
\\titleformat*{\\subsubsection}{\\sffamily\\bfseries\\fontsize{14}{16}\\raggedright\\sffamily\\color{libertyblue}}
\\titleformat*{\\paragraph}{\\sffamily\\fontsize{13}{12}\\raggedright\\bfseries\\color{libertyblue}}
\\titleformat*{\\subparagraph}{\\sffamily\\fontsize{14}{14}\\raggedright\\bfseries\\ttfamily\\color{libertyblue}}


\\DeclareTextFontCommand{\\nonsection}{\\sffamily\\fontsize{19}{19}\\raggedright\\sffamily\\textlf\\color{libertyblue} }

\\DeclareTextFontCommand{\\nonsubsection}{\\sffamily\\fontsize{18}{15}\\raggedright\\scshape\\color{libertyblue}}

\\DeclareTextFontCommand{\\nonsubsubsection}{\\sffamily\\itshape\\fontsize{14}{14}\\raggedright\\sffamily\\color{libertyblue} }


\\usepackage[breaklinks=true,linktocpage,xetex]{hyperref}
\\hypersetup{colorlinks, citecolor=libertyblue,filecolor=libertyblue,linkcolor=libertyblue,urlcolor=libertyblue}

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
