;;; elegant-garamond.el --- Org LaTeX class -*- lexical-binding: t; -*-

(provide 'elegant-garamond)

(add-to-list 'org-latex-classes
             '("elegant-garamond"
               "
\\providecommand{\\DocumentMetadata}[1]{}
\\DocumentMetadata{lang=en-US}
\\documentclass[12pt]{article}
\\usepackage[includeheadfoot, top=1in, bottom=1in, left=1.375in, right=1.375in, headheight=28pt]{geometry} % Selected reading width; reserve room for a two-line running head
\\setlength{\\footskip}{0.95in}

% Package Inclusions
\\usepackage{wrapfig}
\\usepackage{float} % For precise figure placement
\\usepackage{changepage}
\\usepackage{algorithm}
\\usepackage{pdfpages}
\\usepackage{amsmath}
\\usepackage{amssymb} % Provides Org's \\square checkbox symbol
\\usepackage{ifxetex}
\\usepackage{setspace}
\\usepackage{url}
\\usepackage{xurl}
\\usepackage{paralist}
\\usepackage{tikz}
\\usepackage{calc}
\\usepackage{eso-pic}
\\usepackage{etoolbox}
\\usepackage{xcolor}
\\usepackage{microtype} % Improve typography
\\usepackage{booktabs} % Professional-looking tables
\\usepackage{array} % Column helpers for wrapping tables
\\usepackage{ragged2e} % Ragged alignment with hyphenation
\\usepackage{tabularx} % Auto-resizing/wrapping table columns
\\usepackage{longtable} % Multi-page tables
\\usepackage{fancyhdr} % Custom headers and footers
\\usepackage{lastpage}
\\usepackage{refcount}
\\usepackage{xspace} % Consistent spacing after commands
\\usepackage{listings}
\\usepackage{fancyvrb}
\\usepackage{enumerate}
\\usepackage{ctable}
\\usepackage{tocloft}
\\usepackage[normalem]{ulem}
\\usepackage{enumitem}
\\usepackage{csquotes}
\\usepackage{titlesec}
\\usepackage{needspace}
\\usepackage{lipsum}
\\usepackage[breaklinks=true, linktocpage, xetex]{hyperref}
\\usepackage{bookmark}
\\bookmarksetup{numbered=false,open=true}
\\usepackage{enotez}
\\usepackage[most]{tcolorbox} % For enhanced environments
\\MakeOuterQuote{\"}
\\newif\\ifjayendnotes
\\jayendnotesfalse
\\newcommand{\\EnableEndnotes}{\\jayendnotestrue}
\\newcommand{\\DisableEndnotes}{\\jayendnotesfalse}
\\AtBeginDocument{%
  \\thispagestyle{empty}% Suppress header/footer on first page
  \\ifjayendnotes
    \\let\\footnote\\endnote
  \\fi
}

\\newcounter{level} % Define the custom counter
\\usepackage{forloop}

% Table configuration for automatic text wrapping
\\newcolumntype{Y}{>{\\RaggedRight\\arraybackslash}X}
\\newcolumntype{Z}{>{\\Centering\\arraybackslash}X}
\\newcolumntype{W}{>{\\RaggedLeft\\arraybackslash}X}
\\renewcommand{\\tabularxcolumn}[1]{m{#1}}
\\newcommand{\\jaytableformat}{%
  \\setlength{\\tabcolsep}{6pt}%
  \\renewcommand{\\arraystretch}{1.05}%
  \\fontsize{10}{12}\\selectfont}
\\AtBeginEnvironment{tabularx}{\\jaytableformat}
\\AtBeginEnvironment{tabular}{\\jaytableformat}
\\AtBeginEnvironment{longtable}{\\jaytableformat}
\\setlength{\\LTpre}{0pt}
\\setlength{\\LTpost}{0pt}

% Font Settings
\\ifxetex
  \\usepackage{fontspec}
  \\defaultfontfeatures{Mapping=tex-text, Scale=MatchLowercase}
  \\setsansfont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = GaramondPremrPro-Disp,
    Extension = .otf
  ]{Garamond Premier Pro Display}
  \\newfontfamily\\jayfooterbrandfont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
  \\newfontfamily\\jaylabelfont[
    Scale = 1.0,
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-MdCn,
    BoldFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
  \\newcommand{\\jaycaps}{\\jaylabelfont\\addfontfeatures{LetterSpace=9}}
  \\newcommand{\\jayfooterbrandstyle}[1]{{\\jayfooterbrandfont\\color{jayfooterbrandgrey}\\addfontfeatures{LetterSpace=-5}\\fontsize{50}{50}\\selectfont \\MakeUppercase{#1}}}

  % Set main font to Garamond Premier Pro
  \\setromanfont[
    Path = /Users/jay/Library/Fonts/,
    Numbers = OldStyle,
    Ligatures = {Common},
    Contextuals = Alternate,
    UprightFont = GaramondPremrPro,
    ItalicFont = GaramondPremrPro-It,
    BoldFont = GaramondPremrPro-Bd,
    BoldItalicFont = GaramondPremrPro-BdIt,
    Extension = .otf
  ]{Garamond Premier Pro}

  % Set monospaced font to Helvetica Neue LT Pro
  \\setmonofont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-MdCn,
    BoldFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
\\else
  \\usepackage[mathletters]{ucs}
  \\usepackage[utf8x]{inputenc}
  \\newcommand{\\jaycaps}{\\sffamily}
  \\newcommand{\\jayfooterbrandstyle}[1]{{\\sffamily\\bfseries\\color{jayfooterbrandgrey}\\fontsize{50}{50}\\selectfont \\MakeUppercase{#1}}}
\\fi

% Color Definitions
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
\\definecolor{spacegrey}{HTML}{434346}
\\definecolor{jaysectionbluegrey}{HTML}{24394A}
\\definecolor{azure}{HTML}{f2feff}
\\definecolor{jayfooterbrandgrey}{HTML}{4F4F4F}
\\definecolor{jayinksoft}{HTML}{4A4744}
\\definecolor{jayrule}{HTML}{BDB6B0}
\\definecolor{jayaccent}{HTML}{C0261F}

\\newcommand{\\labelitemv}{\\textbullet}
\\newcommand{\\labelitemvi}{\\textbullet}
\\newcommand{\\labelitemvii}{\\textbullet}
\\newcommand{\\labelitemviii}{\\textbullet}
\\newcommand{\\labelitemix}{\\textbullet}
\\newcommand{\\labelitemx}{\\textbullet}

% Header and Footer Configuration
\\fancyhf{} % Clear all header and footer fields
\\renewcommand{\\headrulewidth}{0.4pt}
\\renewcommand{\\headrule}{{\\color{jayrule}\\hrule height\\headrulewidth width\\headwidth \\vskip-\\headrulewidth}}
\\pagestyle{fancy}
% Give long running heads separate columns so they cannot overlap.
\\fancyheadwidth[L][tl]{0.30\\headwidth}
\\fancyheadwidth[R][tr]{0.67\\headwidth}
\\newtoks\\leftheader
\\newtoks\\leftheaderurl
\\newtoks\\coverimage
\\def\\jayfooterlogo{/Users/jay/Dropbox/writing/prosperous/design/storytelling-nyc-logo/current-2018/_better-storytelling-nyc-period-canonical-helvetica-condensed.png}
\\newcommand{\\footerlogo}[1]{\\def\\jayfooterlogo{#1}\\jayapplyfooter}
\\def\\jayfooterbrand{}
\\newcommand{\\footerbrand}[1]{\\def\\jayfooterbrand{#1}\\jayapplyfooter}
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\renewcommand{\\subsectionmark}[1]{\\markright{#1}}
\\makeatletter
\\def\\jay@leftheader@split#1\\\\#2\\jay@leftheader@end{\\def\\jayresolvedleftheader{#1}}
\\newcommand{\\jayapplyleftheader}{%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheader}
    {\\expandafter\\jay@leftheader@split\\@title\\\\\\jay@leftheader@end}
    {\\def\\jayresolvedleftheader{\\the\\leftheader}}%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheaderurl}
    {\\lhead{\\scshape\\fontsize{10.1}{12}\\selectfont\\jayresolvedleftheader}}
    {\\lhead{\\scshape\\fontsize{10.1}{12}\\selectfont\\href{\\the\\leftheaderurl}{\\jayresolvedleftheader}}}%
}
\\makeatother
\\rhead{\\scshape\\fontsize{10.1}{12}\\selectfont{\\nouppercase{\\rightmark}}}
\\AtBeginDocument{\\jayapplyleftheader}

% Footer configuration (controlled from org file)
\\newif\\ifjaylogofooter
\\jaylogofooterfalse
\\newif\\ifjaynofooter
\\jaynofooterfalse
\\newcommand{\\jayfooterlogoonly}{%
  \\raisebox{-0.045in}{%
    \\includegraphics[height=0.504in,keepaspectratio]{\\jayfooterlogo}%
  }%
}
\\newcommand{\\jayfooterlogowithbrand}{%
  \\raisebox{-0.045in}{%
    \\includegraphics[height=0.504in,keepaspectratio]{\\jayfooterlogo}%
  }%
  \\hspace{0.38em}%
  {\\jayfooterbrandstyle{\\jayfooterbrand}}%
}
\\newcommand{\\jayfooterunlesslast}[1]{%
  \\ifnum\\value{page}=\\getpagerefnumber{LastPage}\\relax
  \\else
    #1%
  \\fi
}
\\newcommand{\\jayapplyfooter}{%
  \\ifjaynofooter
    \\fancyfoot{}%
  \\else\\ifjaylogofooter
    \\ifdefempty{\\jayfooterbrand}
      {\\fancyfoot[C]{\\jayfooterunlesslast{\\jayfooterlogoonly}}}%
      {\\fancyfoot[C]{\\jayfooterunlesslast{\\jayfooterlogowithbrand}}}%
  \\else
    \\cfoot{\\jayfooterunlesslast{\\thepage}}%
  \\fi\\fi
}
\\newcommand{\\EnableLogoFooter}{\\jaynofooterfalse\\jaylogofootertrue\\jayapplyfooter}
\\newcommand{\\DisableFooter}{\\jaylogofooterfalse\\jaynofootertrue\\jayapplyfooter}
\\jayapplyfooter

% Paragraph and Indentation Settings
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{8pt plus 1pt minus 1pt} % Space between paragraphs
\\setstretch{1.18}

% Table of Contents Customization
\\renewcommand{\\contentsname}{Table of Contents}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}
\\renewcommand{\\cftdotsep}{6}
\\renewcommand{\\cftdot}{\\textcolor{jayrule}{.}}
\\cftsetrmarg{2.55em plus 1fil} % Let contents titles wrap between words.

% Description Environment Customization
\\renewcommand{\\descriptionlabel}[1]{%
  {\\hspace{\\labelsep}\\bfseries\\textsc{#1}}}
\\setlist[description]{style=nextline, before=\\vspace{\\baselineskip}}

% List Environment Customization
\\setlist{noitemsep, topsep=-8pt} % Keep nested lists tight; no forced post-list gap
\\setlist[enumerate,1]{after=\\vspace{20pt}} % Net 12pt after the global -8pt topsep; nested lists stay tight
\\setlist[description,1]{after=\\vspace{20pt}} % Net 12pt after the global -8pt topsep; nested lists stay tight
\\setlistdepth{10}
\\newenvironment{resumeenum}{\\begin{enumerate}[resume*]}{\\end{enumerate}}

% Selected smaller bullet, with the original list indents.
\\newcommand{\\raisedtinybullet}{\\raisebox{0.25ex}{\\tiny$\\bullet$}}

% Redefine Itemize Labels Using the Custom Bullet
\\renewcommand{\\labelitemi}{\\raisedtinybullet}
\\renewcommand{\\labelitemii}{\\raisedtinybullet}
\\renewcommand{\\labelitemiii}{\\raisedtinybullet}
\\renewcommand{\\labelitemiv}{\\raisedtinybullet}
\\renewcommand{\\labelitemv}{\\raisedtinybullet}
\\renewcommand{\\labelitemvi}{\\raisedtinybullet}
\\renewcommand{\\labelitemvii}{\\raisedtinybullet}
\\renewcommand{\\labelitemviii}{\\raisedtinybullet}
\\renewcommand{\\labelitemix}{\\raisedtinybullet}
\\renewcommand{\\labelitemx}{\\raisedtinybullet}

% Apply the Custom Bullet to All Itemize Levels
\\forloop{level}{1}{\\value{level} < 11}{%
  \\setlist[itemize,\\arabic{level}]{label=\\raisedtinybullet}
}
\\setlist[itemize,1]{label=\\raisedtinybullet, after=\\vspace{20pt}, itemsep=3pt} % Must follow the loop; otherwise its level-1 reset removes the post-list gap
\\setlist*[itemize,2]{itemsep=1.5pt}
\\setlist*[enumerate,1]{itemsep=3pt}

% Keep outer list introductions attached without changing nested-list breaks.
\\makeatletter
\\edef\\jaydefaultlistbeginpenalty{\\the\\@beginparpenalty}
\\newcommand{\\jaylistbeginpenalty}{%
  \\ifnum\\@listdepth=1 10000\\else\\jaydefaultlistbeginpenalty\\fi
}
\\makeatother
\\setlist*{beginpenalty=\\jaydefaultlistbeginpenalty}
\\setlist*[itemize,enumerate]{beginpenalty=\\jaylistbeginpenalty}

% Define custom colors for quote environment
\\definecolor{powderblue}{HTML}{f5f7ff}
\\definecolor{stormybluegrey}{HTML}{708090}
\\definecolor{moonrockgrey}{HTML}{5D5D5D}

% Quote Environment Customization Using tcolorbox
\\tcbset{
  myquote/.style={
    colback=powderblue,
    colframe=stormybluegrey,
    colupper=moonrockgrey,
    boxrule=0.5pt,
    rounded corners,
    width=0.8\\textwidth,
    left=1em,
    right=1em,
    before skip=1em,
    after skip=1em,
    before upper={\\setlength{\\parskip}{1em}\\footnotesize\\raggedright},
    breakable,
    pad at break*=1em,
    vfill before first
  }
}

\\renewenvironment{quote}
{%
  \\begin{center}%
  \\begin{tcolorbox}[myquote]%
}
{%
  \\end{tcolorbox}%
  \\end{center}%
}

% Subtitle environments for different heading levels
% For section (40pt heading -> 14pt subtitle, compact)
\\newenvironment{subtitle}%
{% begin code
\\vspace{-0.3\\baselineskip}\\fontsize{14}{18}\\selectfont\\itshape
}%
{% end code
\\par\\bigskip
}

% For subsection (24pt heading -> ~17pt subtitle, 70% size)
\\newenvironment{subsubtitle}%
{% begin code
\\par\\nobreak\\vskip3pt\\noindent\\fontsize{17}{20}\\selectfont\\itshape
}%
{% end code
\\par
}

% For subsubsection (18pt heading -> ~13pt subtitle, 70% size)
\\newenvironment{subsubsubtitle}%
{% begin code
\\vspace{-0.7\\baselineskip}\\fontsize{13}{16}\\selectfont\\itshape
}%
{% end code
\\par\\bigskip
}

% Titlesec Configuration
\\titlespacing*{\\section}{0pt}{48pt}{0pt}
\\titlespacing*{\\subsection}{0pt}{30pt plus 6pt minus 4pt}{10pt}
\\titlespacing*{\\subsubsection}{0pt}{26pt plus 5pt minus 3pt}{8pt}
\\titlespacing*{\\paragraph}{0pt}{16pt plus 3pt minus 2pt}{-4pt}
\\titlespacing*{\\subparagraph}{0pt}{12pt plus 2pt}{3pt}

\\newcommand{\\mysectiontitle}[1]{%
  \\markboth{#1}{#1}%
  \\markright{#1}%
  \\raggedright\\fontsize{40}{26}\\selectfont #1
}

\\newcommand{\\mysubsectiontitle}[1]{%
  \\markright{#1}%
  \\sffamily\\setstretch{0.1}\\fontsize{24}{36}\\raggedright\\sffamily #1
}

\\newcommand{\\mysubsubsectiontitle}[1]{%
  \\normalfont\\fontsize{15}{17}\\selectfont\\ifxetex\\addfontfeatures{LetterSpace=5,Numbers=Lining}\\fi\\raggedright\\color{spacegrey}\\MakeUppercase{#1}
}

\\titleformat{\\section}
  {\\normalfont\\ttfamily\\scshape\\color{jaysectionbluegrey}}
  {}
  {0em}
  {\\mysectiontitle}

\\titleformat{\\subsection}
  {}
  {}
  {0em}
  {\\mysubsectiontitle}
\\titleformat{\\subsubsection}
  {}
  {}
  {0em}
  {\\mysubsubsectiontitle}
\\newcommand{\\myparagraphtitle}[1]{\\MakeUppercase{#1}}
\\titleformat{\\paragraph}[block]
  {\\normalfont\\jaycaps\\bfseries\\fontsize{10}{12}\\selectfont\\raggedright\\color{jayaccent}}
  {}{0pt}{\\myparagraphtitle}
\\titleformat{\\subparagraph}[block]
  {\\normalfont\\bfseries\\normalsize\\raggedright}
  {}{0pt}{}

% Reserve space without breaking a chain of consecutive headings.
\\makeatletter
\\newcommand{\\jayheadingneedspace}[1]{%
  \\if@nobreak\\else\\Needspace*{#1}\\fi
}
\\makeatother
\\pretocmd{\\subsubsection}{\\jayheadingneedspace{6\\baselineskip}}{}{}
\\pretocmd{\\paragraph}{\\jayheadingneedspace{4\\baselineskip}}{}{}
\\pretocmd{\\subparagraph}{\\jayheadingneedspace{3\\baselineskip}}{}{}

% Hyperref Configuration
\\hypersetup{
  colorlinks,
  citecolor = elegantblue,
  filecolor = elegantblue,
  linkcolor = elegantblue,
  urlcolor  = elegantblue
}

% Disable Default Title
\\renewcommand\\maketitle{}

[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f")) ;; for multiple passes

(let* ((this-file (or load-file-name buffer-file-name))
       (table-helper (and this-file
                          (expand-file-name "jay-latex-table-wrap.el"
                                            (file-name-directory this-file))))
       (poetry-helper (and this-file
                           (expand-file-name "jay-latex-poetry-blocks.el"
                                             (file-name-directory this-file))))
       (prose-helper (and this-file
                          (expand-file-name "jay-latex-prose-defaults.el"
                                            (file-name-directory this-file)))))
  (when (and table-helper (file-readable-p table-helper))
    (load table-helper nil 'nomessage))
  (when (and poetry-helper (file-readable-p poetry-helper))
    (load poetry-helper nil 'nomessage))
  (when (and prose-helper (file-readable-p prose-helper))
    (load prose-helper nil 'nomessage)))

(when (fboundp 'jay/latex-apply-prose-defaults)
  (jay/latex-apply-prose-defaults "elegant-garamond"))

(when (fboundp 'jay/latex-register-wrap-class)
  (jay/latex-register-wrap-class "elegant-garamond"))
