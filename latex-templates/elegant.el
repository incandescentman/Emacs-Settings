(provide 'elegant)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("elegant"
                 "

\\documentclass[12pt]{article}
\\usepackage{geometry}

\\geometry{
  margin=1.5in, % Set all margins to 1.5 inches
  bottom=1.5in, % Bottom margin set to 1.25 inches
  footskip=1.25in % Distance from the bottom of the text area to the baseline of the footer
}

\\usepackage{fontspec}
\\linespread{1.2}

\\usepackage[all]{nowidow}
\\usepackage{float}
%\\usepackage{changepage}

%\\usepackage{algorithm}
%\\usepackage{amsmath}



\\defaultfontfeatures{Ligatures=TeX,Scale=MatchLowercase}



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
\\setmonofont{Myriad Pro} % for nice quotes
% \\setmonofont{Adobe Garamond Pro} % for beautiful LaTeX Formulae


%\\usepackage[mathletters]{ucs}
%\\usepackage[utf8x]{inputenc}


\\usepackage[obeyspaces]{url}
\\PassOptionsToPackage{obeyspaces}{url}

\\usepackage{paralist}
%\\usepackage{graphicx}
\\usepackage{wrapfig}
\\usepackage{setspace}
%\\setkeys{Gin}{resolution=72}
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
\\definecolor{darklibertyblue}{HTML}{19455b}


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







%%%%%%%%%%%%%%%%%

\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhf{}
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
% \\lhead{\\href{\\the\\leftheaderurl}{\\the\\leftheader}}
\\chead{}
\\rhead{{\\nouppercase{\\leftmark}}}
% \\rhead{\\@title: {\\nouppercase{\\leftmark}}}

\\lhead{\\bfseries\\@title} % title of the document as left header

%\\renewcommand{\\footrulewidth}{0.4pt}
\\fancyfoot[C]{%
  % Graphic
  \\raisebox{0.06in}{% Align the bottom of the image with the baseline of the surrounding text
% \\includegraphics[height=0.6in,keepaspectratio]{/Users/jay/Dropbox/writing/prosperous/design/storytelling-nyc-logo/current-2018/_better-storytelling-nyc-canonical-helvetica-condensed-wide.png} % The whole thing

% \\includegraphics[height=0.8in,keepaspectratio]{/Users/jay/Dropbox/writing/prosperous/design/storytelling-nyc-logo/current-2018/_better-storytelling-nyc-period-canonical-helvetica-condensed.png} % The whole thing with a period

%\\includegraphics[height=0.8in,keepaspectratio]{/Users/jay/Dropbox/writing/prosperous/design/storytelling-nyc-logo/current-2018/_better-storytelling-nyc-canonical-helvetica-condensed-wide.png} % The whole thing with NO period

%\\includegraphics[height=0.6in,keepaspectratio]{/Users/jay/Dropbox/github/incandescentman.github.io/assets/images/2023-10-final-new-logo_high-res-no-text.png} % logo of just lady liberty
  }%
  % Space between the graphic and the text (adjust as needed)
  \\hspace{0.05in}%
  % Text
  \\raisebox{0.35in}{% Adjust this value to align the text with the image

 %   \\fontsize{43}{42}\\selectfont\\sffamily\\color{darklibertyblue} STORYTELLING.NYC % large storytelling.NYC footer

% \\fontsize{16}{16}\\sffamily\\color{black}All materials Copyright Jay Dixit. % copyright



  }%
}

% overwrite the logo footer
\\cfoot{\\thepage} % Add page numbers


%%%%%%%%%%%%%%%%

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



\\setlength{\\headheight}{15pt} % Adjusted value based on warning

\\addtolength{\\topmargin}{-3pt} % Adjusted value to maintain layout


\\makeatletter
\\newcommand{\\globalcolor}[1]{%
  \\color{#1}\\global\\let\\default@color\\current@color
}
\\makeatother

\\newcommand{\\textsubscr}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}

\\usepackage{enumitem}
\\setlistdepth{10} % Allows up to 10 levels of nesting

\\newlist{mylist}{enumerate}{10}


% control line spacing in bulleted list
\\setlist{noitemsep, topsep=-8pt, after=\\vspace{12pt}} % for no spacing between list items
% see: https://tex.stackexchange.com/questions/199118/modifying-whitespace-before-and-after-list-separately-using-enumitem-package
%\\setlist{topsep=0pt} % for a line between list items


% Define labels for levels 5 to 10
\\newcommand{\\labelitemv}{$\\bullet$}
\\newcommand{\\labelitemvi}{$\\bullet$}
\\newcommand{\\labelitemvii}{$\\bullet$}
\\newcommand{\\labelitemviii}{$\\bullet$}
\\newcommand{\\labelitemix}{$\\bullet$}
\\newcommand{\\labelitemx}{$\\bullet$}


\\renewcommand{\\labelitemi}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemii}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemiii}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemiv}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemv}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemvi}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemvii}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemviii}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemix}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemx}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}

\\setlistdepth{10}
\\setlist[itemize,1]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,2]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,3]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,4]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,5]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,6]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,7]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,8]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,9]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
\\setlist[itemize,10]{label=\\raisebox{0.25ex}{\\tiny$\\bullet$}}
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





\\usepackage[sc]{titlesec}


\\newlength\\TitleOverhang
\\setlength\\TitleOverhang{1.5cm}

\\newcommand\\Overhang[1]{%
 \\llap{\\makebox[\\TitleOverhang][l]{#1}}%
}


% Ensure the tcolorbox package is included with the 'skins' library
\\usepackage[most]{tcolorbox}

% Define custom colors if not already defined
\\definecolor{powderblue}{HTML}{f5f7ff}
\\definecolor{stormybluegrey}{HTML}{708090}
\\definecolor{moonrockgrey}{HTML}{5D5D5D}

% Define a custom tcolorbox style for quotes
\\tcbset{
  myquote/.style={
    colback=powderblue,
    colframe=stormybluegrey,
    colupper=moonrockgrey,
    boxrule=0.5pt,
    rounded corners,
    fontupper=\\singlespacing\\fontsize{9}{11}\\selectfont\\ttfamily,
    width=0.8\\textwidth,
    left=0pt,
    right=0pt,
    boxalign=center,
    halign=flush left,
    before skip=1em,  % Vertical space before the box
    after skip=1em,   % Vertical space after the box
    parskip=1em       % Space between paragraphs within the box
  }
}

% Redefine the quote environment using the custom style
\\renewenvironment{quote}
{%
  \\begin{tcolorbox}[myquote]
}
{%
  \\end{tcolorbox}
}


% \\titlespacing{command}{left spacing}{before spacing}{after spacing}[right]
\\titlespacing*{\\section}{1.5ex}{12pt}{0pt}
\\titlespacing*{\\subsection}{0pt}{0pt}{-6pt}
\\titlespacing*{\\subsubsection}{0pt}{0pt}{-12pt}

\\titleformat*{\\section}{\\sffamily\\bfseries\\fontsize{30}{20}\\raggedright\\sffamily\\color{libertyblue}}
\\titleformat*{\\subsection}{\\sffamily\\fontsize{18}{16}\\selectfont\\raggedright\\color{libertyblue}}

\\titleformat*{\\subsubsection}{\\sffamily\\bfseries\\fontsize{15}{16}\\raggedright\\sffamily\\color{libertyblue}}
\\titleformat*{\\paragraph}{\\sffamily\\fontsize{13}{12}\\raggedright\\bfseries\\color{libertyblue}}
\\titleformat*{\\subparagraph}{\\sffamily\\fontsize{14}{14}\\raggedright\\bfseries\\ttfamily\\color{libertyblue}}



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
