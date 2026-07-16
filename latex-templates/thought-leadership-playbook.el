;;; thought-leadership-playbook.el --- Org LaTeX class -*- lexical-binding: t; -*-

(provide 'thought-leadership-playbook)

(add-to-list 'org-latex-classes
             '("thought-leadership-playbook"
               "
\\providecommand{\\DocumentMetadata}[1]{}
\\DocumentMetadata{lang=en-US}
\\documentclass[10pt]{article}
\\usepackage[includeheadfoot, top=0.72in, bottom=0.62in, left=0.72in, right=0.72in, headsep=0.18in]{geometry}
\\setlength{\\footskip}{0.32in}

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
\\usepackage[table]{xcolor}
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

\\setlength{\\headheight}{16pt}

% Table configuration for automatic text wrapping
\\newcolumntype{Y}{>{\\RaggedRight\\arraybackslash}X}
\\newcolumntype{Z}{>{\\Centering\\arraybackslash}X}
\\newcolumntype{W}{>{\\RaggedLeft\\arraybackslash}X}
\\renewcommand{\\tabularxcolumn}[1]{m{#1}}
\\newcommand{\\jaytableformat}{%
  \\setlength{\\tabcolsep}{5pt}%
  \\renewcommand{\\arraystretch}{1.32}%
  \\arrayrulecolor{playbookline}%
  \\fontsize{9.2}{11}\\selectfont}
\\AtBeginEnvironment{tabularx}{\\jaytableformat\\rowcolors{1}{playbookwash}{white}}
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
    UprightFont = HelveticaNeueLTPro-MdCn,
    ItalicFont = HelveticaNeueLTPro-MdCnO,
    BoldFont = HelveticaNeueLTPro-BdCn,
    BoldItalicFont = HelveticaNeueLTPro-BdCnO,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
  \\newfontfamily\\playbooksans[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-MdCn,
    ItalicFont = HelveticaNeueLTPro-MdCnO,
    BoldFont = HelveticaNeueLTPro-BdCn,
    BoldItalicFont = HelveticaNeueLTPro-BdCnO,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
  \\newfontfamily\\playbookdisplay[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = GaramondPremrPro-Disp,
    ItalicFont = GaramondPremrPro-ItDisp,
    BoldFont = GaramondPremrPro-BdDisp,
    BoldItalicFont = GaramondPremrPro-BdItDisp,
    Extension = .otf
  ]{Garamond Premier Pro Display}
  \\newfontfamily\\jayfooterbrandfont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
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
\\definecolor{azure}{HTML}{f2feff}
\\definecolor{jayfooterbrandgrey}{HTML}{4F4F4F}
\\definecolor{playbookink}{HTML}{17181B}
\\definecolor{playbookblue}{HTML}{123D87}
\\definecolor{playbookbluebright}{HTML}{1559B5}
\\definecolor{playbookpowder}{HTML}{EAF1FA}
\\definecolor{playbookwash}{HTML}{F5F8FC}
\\definecolor{playbookline}{HTML}{AAB8CC}
\\definecolor{playbookred}{HTML}{D84A3A}
\\definecolor{playbookmuted}{HTML}{596477}

\\newcommand{\\labelitemv}{\\textbullet}
\\newcommand{\\labelitemvi}{\\textbullet}
\\newcommand{\\labelitemvii}{\\textbullet}
\\newcommand{\\labelitemviii}{\\textbullet}
\\newcommand{\\labelitemix}{\\textbullet}
\\newcommand{\\labelitemx}{\\textbullet}

% Header and Footer Configuration
\\fancyhf{} % Clear all header and footer fields
\\renewcommand{\\headrulewidth}{0pt}
\\pagestyle{fancy}
\\newtoks\\leftheader
\\newtoks\\leftheaderurl
\\newtoks\\coverimage
\\def\\jayfooterlogo{/Users/jay/Dropbox/writing/prosperous/design/storytelling-nyc-logo/current-2018/_better-storytelling-nyc-period-canonical-helvetica-condensed.png}
\\newcommand{\\footerlogo}[1]{\\def\\jayfooterlogo{#1}\\jayapplyfooter}
\\def\\jayfooterbrand{}
\\newcommand{\\footerbrand}[1]{\\def\\jayfooterbrand{#1}\\jayapplyfooter}
\\newcommand{\\playbookperiod}{JULY--DECEMBER 2026}
\\newcommand{\\playbookstrap}{COUNTDOWN. EXECUTE. COMPOUND.}
\\newcommand{\\PlaybookPeriod}[1]{\\renewcommand{\\playbookperiod}{#1}}
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\makeatletter
\\def\\jay@leftheader@split#1\\\\#2\\jay@leftheader@end{\\def\\jayresolvedleftheader{#1}}
\\newcommand{\\jayapplyleftheader}{%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheader}
    {\\expandafter\\jay@leftheader@split\\@title\\\\\\jay@leftheader@end}
    {\\def\\jayresolvedleftheader{\\the\\leftheader}}%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheaderurl}
    {\\lhead{{\\playbooksans\\bfseries\\fontsize{8}{9}\\selectfont\\addfontfeatures{LetterSpace=55}\\MakeUppercase{\\jayresolvedleftheader}}}}
    {\\lhead{{\\playbooksans\\bfseries\\fontsize{8}{9}\\selectfont\\addfontfeatures{LetterSpace=55}\\href{\\the\\leftheaderurl}{\\MakeUppercase{\\jayresolvedleftheader}}}}}%
}
\\makeatother
\\rhead{{\\playbooksans\\bfseries\\fontsize{8}{9}\\selectfont\\addfontfeatures{LetterSpace=55}\\playbookperiod}}
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
    \\fancyfoot[L]{{\\playbooksans\\bfseries\\fontsize{7}{8}\\selectfont\\addfontfeatures{LetterSpace=45}PLAYBOOK}}%
    \\fancyfoot[C]{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7}{8}\\selectfont\\addfontfeatures{LetterSpace=50}\\playbookstrap}}%
    \\fancyfoot[R]{{\\playbooksans\\bfseries\\fontsize{7}{8}\\selectfont PAGE \\thepage}}%
  \\fi\\fi
}
\\newcommand{\\EnableLogoFooter}{\\jaynofooterfalse\\jaylogofootertrue\\jayapplyfooter}
\\newcommand{\\DisableFooter}{\\jaylogofooterfalse\\jaynofootertrue\\jayapplyfooter}
\\jayapplyfooter

% Paragraph and Indentation Settings
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{3.5pt plus 1pt minus 1pt}
\\setstretch{1.04}
\\color{playbookink}

% Table of Contents Customization
\\renewcommand{\\contentsname}{Table of Contents}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}

% Description Environment Customization
\\renewcommand{\\descriptionlabel}[1]{%
  {\\hspace{\\labelsep}\\bfseries\\textsc{#1}}}
\\setlist[description]{style=nextline, before=\\vspace{\\baselineskip}}

% List Environment Customization
\\setlist{itemsep=2pt, parsep=0pt, partopsep=0pt, topsep=3pt}
\\setlist[enumerate,1]{leftmargin=3.4em, labelsep=0.7em, itemsep=8pt, topsep=8pt, after=\\vspace{8pt}, label=\\protect\\colorbox{playbookblue}{\\protect\\makebox[1.25em][c]{\\color{white}\\playbooksans\\bfseries\\arabic*}}}
\\setlist[description,1]{after=\\vspace{10pt}}
\\setlistdepth{10}
\\newenvironment{resumeenum}{\\begin{enumerate}[resume*]}{\\end{enumerate}}

% Define a Custom Raised Bullet Command
\\newcommand{\\raisedtinybullet}{\\raisebox{0.25ex}{\\color{playbookblue}\\tiny$\\bullet$}}

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
\\setlist[itemize,1]{label=\\raisedtinybullet, leftmargin=1.65em, labelsep=0.65em, itemsep=2pt, after=\\vspace{7pt}}
\\let\\playbookoldsquare\\square
\\renewcommand{\\square}{\\color{playbookblue}\\playbookoldsquare}

% Define custom colors for quote environment
\\definecolor{powderblue}{HTML}{EAF1FA}
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
\\tcbset{
  playbooknotes/.style={
    enhanced,
    breakable,
    colback=playbookwash,
    colframe=playbookblue,
    boxrule=0pt,
    borderline west={3pt}{0pt}{playbookblue},
    sharp corners,
    left=14pt,
    right=14pt,
    top=14pt,
    bottom=14pt,
    before skip=4pt,
    after skip=10pt
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
\\vspace{-0.7\\baselineskip}\\fontsize{17}{20}\\selectfont\\itshape
}%
{% end code
\\par\\bigskip
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
\\titlespacing*{\\section}{0pt}{24pt}{10pt}
\\titlespacing*{\\subsection}{0pt}{24pt}{10pt}
\\titlespacing*{\\subsubsection}{0pt}{10pt}{5pt}
\\titlespacing{\\paragraph}{0pt}{0pt}{.5em}[]

\\newcommand{\\mysectiontitle}[1]{%
  \\markboth{#1}{#1}%
  \\markright{#1}%
  \\Needspace{6\\baselineskip}%
  \\raggedright\\playbookdisplay\\color{playbookblue}\\fontsize{35}{36}\\selectfont\\MakeUppercase{#1}%
  \\par\\vspace{3pt}{\\color{playbookblue}\\rule{\\linewidth}{1.4pt}}
}

\\newcommand{\\mysubsectiontitle}[1]{%
  \\markright{#1}%
  \\Needspace{5\\baselineskip}%
  \\playbookdisplay\\color{playbookblue}\\fontsize{25}{27}\\selectfont\\raggedright\\MakeUppercase{#1}
}

\\newcommand{\\mysubsubsectiontitle}[1]{%
  \\Needspace{4\\baselineskip}%
  \\noindent\\colorbox{playbookpowder}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{\\strut\\playbooksans\\bfseries\\color{playbookink}\\fontsize{14}{17}\\selectfont\\MakeUppercase{#1}}}
}

\\newcommand{\\PlaybookContinued}[1]{%
  \\noindent{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{10}{12}\\selectfont\\addfontfeatures{LetterSpace=60}\\MakeUppercase{#1 --- CONTINUED}}\\par
  \\vspace{5pt}{\\color{playbookblue}\\rule{\\linewidth}{1pt}}\\vspace{8pt}
}
\\newcommand{\\PlaybookPhaseBreak}[1]{\\clearpage}
\\newcommand{\\PlaybookContinuationBreak}{\\clearpage}
\\newcommand{\\PlaybookMonthBreak}[1]{}
\\newcommand{\\PlaybookEngineBreak}{\\clearpage}
\\newcommand{\\PlaybookTableHead}[1]{\\cellcolor{playbookblue}\\color{white}\\textbf{#1}}
\\newcommand{\\PlaybookBackmatterStart}{}
\\newcommand{\\PlaybookBackmatterMiddle}{}
\\newcommand{\\PlaybookBackmatterEnd}{}

\\titleformat{\\section}
  {\\normalfont}
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
\\titleformat*{\\paragraph}{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{15}{17}\\raggedright}
\\titleformat*{\\subparagraph}{\\playbooksans\\bfseries\\color{playbookmuted}\\fontsize{12}{14}\\raggedright}

% Hyperref Configuration
\\hypersetup{
  colorlinks,
  citecolor = elegantblue,
  filecolor = elegantblue,
  linkcolor = playbookblue,
  urlcolor  = playbookblue
}

% Playbook cover
\\makeatletter
\\newcommand{\\playbookcovertitle}{\\@title}
\\newcommand{\\PlaybookCoverTitle}[1]{\\renewcommand{\\playbookcovertitle}{#1}}
\\newcommand{\\PlaybookWarmTitle}[1]{}
\\renewcommand\\maketitle{%
  \\begin{titlepage}
    \\thispagestyle{empty}
    \\vspace*{0.08in}
    {\\playbooksans\\bfseries\\fontsize{8}{9}\\selectfont\\addfontfeatures{LetterSpace=55}THOUGHT LEADERSHIP COUNTDOWN\\hfill\\playbookperiod\\par}
    \\vspace{7pt}{\\color{playbookink}\\rule{\\linewidth}{0.7pt}}
    \\vfill
    {\\centering\\playbookdisplay\\color{playbookink}\\fontsize{49}{48}\\selectfont\\MakeUppercase{\\playbookcovertitle}\\par}
    \\vspace{16pt}
    {\\centering\\color{playbookblue}\\rule{0.76\\linewidth}{1.5pt}\\par}
    \\vspace{13pt}
    {\\centering\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{15}{17}\\selectfont\\addfontfeatures{LetterSpace=150}\\playbookperiod\\par}
    \\vfill
    \\begin{tcolorbox}[colback=white,colframe=playbookline,boxrule=0.7pt,arc=2pt,left=10pt,right=10pt,top=10pt,bottom=10pt]
      \\begin{tabularx}{\\linewidth}{*{4}{>{\\centering\\arraybackslash}X}}
        {\\playbooksans\\bfseries\\color{playbookblue}\\shortstack{0\\\\COUNTDOWN}} &
        {\\playbooksans\\bfseries\\color{playbookblue}\\shortstack{1\\\\LAUNCH}} &
        {\\playbooksans\\bfseries\\color{playbookblue}\\shortstack{2\\\\COMPOUND}} &
        {\\playbooksans\\bfseries\\color{playbookblue}\\shortstack{3\\\\HARVEST}}
      \\end{tabularx}
    \\end{tcolorbox}
    \\vspace{12pt}
    {\\centering\\playbooksans\\bfseries\\color{playbookmuted}\\fontsize{8}{9}\\selectfont\\addfontfeatures{LetterSpace=55}PRINTED. VISIBLE. USED.\\par}
  \\end{titlepage}
  \\setcounter{page}{1}%
}
\\makeatother

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
  (jay/latex-apply-prose-defaults "thought-leadership-playbook"))

(when (fboundp 'jay/latex-register-wrap-class)
  (jay/latex-register-wrap-class "thought-leadership-playbook"))
