;;; thought-leadership-playbook-warm.el --- Org LaTeX class -*- lexical-binding: t; -*-

(provide 'thought-leadership-playbook-warm)

(add-to-list 'org-latex-classes
             '("thought-leadership-playbook-warm"
               "
\\providecommand{\\DocumentMetadata}[1]{}
\\DocumentMetadata{lang=en-US}
\\documentclass[10pt]{article}
\\usepackage[includeheadfoot, top=0.68in, bottom=0.58in, left=1in, right=1in, headsep=0.16in]{geometry}
\\setlength{\\footskip}{0.28in}

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
\\usepackage{xstring}
\\usepackage{multicol}
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
  \\thispagestyle{fancy}% The printed-playbook header runs from page one
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
  \\fontsize{8.35}{10.15}\\selectfont}
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
    Numbers = Lining,
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
% Warm editorial palette derived from the Claude Design reference.
\\definecolor{playbookpaper}{HTML}{FFFFFF}
\\definecolor{playbookink}{HTML}{2A2520}
\\definecolor{playbookblue}{HTML}{7C2E2E}
\\definecolor{playbookbluebright}{HTML}{7C2E2E}
\\definecolor{playbookpowder}{HTML}{F1EADC}
\\definecolor{playbookwash}{HTML}{F6F0E4}
\\definecolor{playbookline}{HTML}{E7DFCF}
\\definecolor{playbookred}{HTML}{7C2E2E}
\\definecolor{playbookmuted}{HTML}{8A8073}
\\definecolor{playbookgold}{HTML}{C9AF7E}
\\definecolor{playbookblack}{HTML}{23201C}
\\definecolor{playbookledlabel}{HTML}{C9AF7E}

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
\\newcommand{\\playbookperiod}{JULY—DECEMBER 2026}
\\newcommand{\\playbookstrap}{PRINTED \\& ON THE WALL \\textperiodcentered{} JULY—DECEMBER 2026}
\\newcommand{\\PlaybookPeriod}[1]{\\renewcommand{\\playbookperiod}{#1}}
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\makeatletter
\\def\\jay@leftheader@split#1\\\\#2\\jay@leftheader@end{\\def\\jayresolvedleftheader{#1}}
\\newcommand{\\jayapplyleftheader}{%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheader}
    {\\expandafter\\jay@leftheader@split\\@title\\\\\\jay@leftheader@end}
    {\\def\\jayresolvedleftheader{\\the\\leftheader}}%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheaderurl}
    {\\lhead{{\\playbooksans\\color{playbookmuted}\\fontsize{6.7}{8}\\selectfont\\addfontfeatures{LetterSpace=5.5}\\MakeUppercase{\\jayresolvedleftheader \\textperiodcentered{} Fable}}}}
    {\\lhead{{\\playbooksans\\color{playbookmuted}\\fontsize{6.7}{8}\\selectfont\\addfontfeatures{LetterSpace=5.5}\\href{\\the\\leftheaderurl}{\\MakeUppercase{\\jayresolvedleftheader \\textperiodcentered{} Fable}}}}}%
}
\\makeatother
\\rhead{{\\playbooksans\\color{playbookmuted}\\fontsize{6.7}{8}\\selectfont\\addfontfeatures{LetterSpace=5.5}SOCRATIC AI}}
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
    \\fancyfoot[C]{{\\playbooksans\\color{playbookmuted}\\fontsize{6.4}{7.5}\\selectfont\\addfontfeatures{LetterSpace=4.5}\\playbookstrap}}%
  \\fi\\fi
}
\\newcommand{\\EnableLogoFooter}{\\jaynofooterfalse\\jaylogofootertrue\\jayapplyfooter}
\\newcommand{\\DisableFooter}{\\jaylogofooterfalse\\jaynofootertrue\\jayapplyfooter}
\\jayapplyfooter

% Paragraph and Indentation Settings
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{3.2pt plus 0.8pt minus 0.8pt}
\\setstretch{1.15}
\\clubpenalty=10000
\\widowpenalty=10000
\\displaywidowpenalty=10000
\\color{playbookink}
\\pagecolor{playbookpaper}
\\AtBeginDocument{\\RaggedRight\\fontsize{9.15}{11.25}\\selectfont}

% Table of Contents Customization
\\renewcommand{\\contentsname}{Table of Contents}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}

% Description Environment Customization
\\newif\\ifplaybookchip
\\playbookchipfalse
\\newif\\ifplaybookblanklabel
\\playbookblanklabelfalse
\\renewcommand{\\descriptionlabel}[1]{%
  \\ifplaybookblanklabel\\else\\ifstrempty{#1}{}{%
    {\\hspace{\\labelsep}\\colorbox{playbookred}{%
      \\playbookchiptrue\\strut\\playbooksans\\bfseries\\color{white}%
      \\fontsize{7.15}{8.5}\\selectfont\\hspace{1.5pt}#1\\hspace{1.5pt}}}}\\fi}
\\setlist[description]{style=standard, leftmargin=0pt, labelindent=0pt,
  labelwidth=0pt, labelsep=0pt,
  itemsep=0pt, topsep=3pt, parsep=0pt,
  before=\\vspace{1pt}, after=\\vspace{7pt}}

% List Environment Customization
\\setlist{itemsep=2pt, parsep=0pt, partopsep=0pt, topsep=3pt}
\\setlist[enumerate,1]{leftmargin=2.2em, labelsep=0.55em, itemsep=8pt,
  topsep=6pt, before=\\begin{multicols}{2}, after=\\end{multicols}\\vspace{5pt},
  label={\\protect\\playbookdisplay\\protect\\color{playbookgold}\\protect\\fontsize{19}{20}\\protect\\selectfont\\arabic*}}
\\setlist[description,1]{after=\\vspace{10pt}}
\\setlistdepth{10}
\\newenvironment{resumeenum}{\\begin{enumerate}[resume*]}{\\end{enumerate}}

% Define a Custom Raised Bullet Command
\\newcommand{\\raisedtinybullet}{\\raisebox{0.25ex}{\\color{playbookred}\\tiny$\\bullet$}}

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
\\renewcommand{\\square}{\\ifplaybookchip\\color{white}\\else\\color{playbookred}\\fi\\playbookoldsquare}

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
    colback=playbookpowder,
    colframe=playbookred,
    boxrule=0pt,
    borderline west={2.5pt}{0pt}{playbookred},
    sharp corners,
    left=14pt,
    right=14pt,
    top=14pt,
    bottom=14pt,
    before skip=4pt,
    after skip=10pt,
    before upper=\\RaggedRight
  }
}
\\tcbset{
  playbookmilestone/.style={
    enhanced,
    breakable=false,
    width=\\linewidth,
    colback=playbookwash,
    colframe=playbookwash,
    boxrule=0pt,
    borderline west={4pt}{0pt}{playbookred},
    arc=2pt,
    outer arc=2pt,
    left=16pt,
    right=16pt,
    top=12pt,
    bottom=12pt,
    before skip=14pt,
    after skip=10pt,
    before upper=\\RaggedRight
  },
  playbookfloor/.style={
    enhanced,
    breakable=false,
    width=\\linewidth,
    colback=playbookblack,
    colframe=playbookblack,
    boxrule=0pt,
    sharp corners,
    left=18pt,
    right=18pt,
    top=14pt,
    bottom=14pt,
    before skip=12pt,
    after skip=10pt
  }
}
\\newcommand{\\PlaybookCheckbox}{%
  \\raisebox{0.08ex}{\\color{playbookred}$\\square$}}
\\newcommand{\\PlaybookTask}[2]{%
  \\par\\noindent{\\color{playbookline}\\rule{\\linewidth}{0.4pt}}\\vspace{5pt}\\par
  \\begingroup
    \\RaggedRight\\fontsize{9.15}{9.8}\\selectfont
    \\hangindent=1.5em\\hangafter=1
    \\noindent\\PlaybookCheckbox\\hspace{4pt}%
    {\\bfseries\\color{playbookred}#1}~---~#2\\par
  \\endgroup
  \\vspace{5pt}}
\\newcommand{\\PlaybookMilestoneLabel}[1]{%
  \\noindent\\RaggedRight\\PlaybookCheckbox\\hspace{4pt}%
  {\\bfseries\\color{playbookred}#1}\\par\\vspace{4pt}}
\\newcommand{\\PlaybookFloorCheck}{%
  \\begin{tcolorbox}[playbookfloor]
    {\\playbooksans\\bfseries\\color{playbookledlabel}\\fontsize{7}{8.5}\\selectfont\\addfontfeatures{LetterSpace=9.5}AUGUST FLOOR CHECK}\\par
    \\vspace{7pt}
    \\begingroup
    \\renewcommand{\\arraystretch}{1.05}
    \\setlength{\\tabcolsep}{2pt}
    \\begin{tabularx}{\\linewidth}{*{6}{>{\\centering\\arraybackslash}X}}
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{18}{19}\\selectfont 8} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{18}{19}\\selectfont 8--12} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{18}{19}\\selectfont 2} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{18}{19}\\selectfont \\textasciitilde{}20} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{18}{19}\\selectfont 12} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{18}{19}\\selectfont 2} \\\\
      {\\playbooksans\\color{playbookledlabel}\\fontsize{5.8}{7}\\selectfont LONG VIDEOS} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{5.8}{7}\\selectfont SHORTS} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{5.8}{7}\\selectfont ESSAYS} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{5.8}{7}\\selectfont NOTES} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{5.8}{7}\\selectfont LINKEDIN} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{5.8}{7}\\selectfont POD PITCHES}
    \\end{tabularx}
    \\endgroup
  \\end{tcolorbox}}

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

% Editorial hierarchy.  The Org file supplies semantic levels; this class
% turns them into the compact phase/week system used by the reference PDF.
\\newcommand{\\playbookrootsection}[1]{}
\\newcommand{\\playbookphaseprefix}{}
\\newcommand{\\playbookphasetitle}{}
\\newcommand{\\playbookphasemain}{}
\\newcommand{\\playbookphasedate}{}
\\newcommand{\\playbookdisplaytitle}{}
\\newcommand{\\playbookdisplaydate}{}
\\newcommand{\\playbookweeklabel}{}
\\newcommand{\\playbookweektitle}{}
\\newcommand{\\PlaybookResolvePhase}[1]{%
  \\StrBefore{#1}{ (}[\\playbookphasemain]%
  \\StrBetween{#1}{(}{)}[\\playbookphasedate]%
  \\def\\playbookdisplaytitle{\\playbookphasemain}%
  \\def\\playbookdisplaydate{\\playbookphasedate}%
  \\IfStrEq{\\playbookphasemain}{LAUNCH COUNTDOWN}{\\def\\playbookdisplaytitle{Launch Countdown}}{}%
  \\IfStrEq{\\playbookphasemain}{LAUNCH}{\\def\\playbookdisplaytitle{Launch}}{}%
  \\IfStrEq{\\playbookphasemain}{CADENCE \\& COMPOUNDING}{\\def\\playbookdisplaytitle{Cadence \\& Compounding}}{}%
  \\IfStrEq{\\playbookphasemain}{HARVEST}{\\def\\playbookdisplaytitle{Harvest}}{}%
  \\IfStrEq{\\playbookphasedate}{July 15-August 3}{\\def\\playbookdisplaydate{Jul 15--Aug 3}}{}%
  \\IfStrEq{\\playbookphasedate}{September-October}{\\def\\playbookdisplaydate{September--October}}{}%
  \\IfStrEq{\\playbookphasedate}{November-December}{\\def\\playbookdisplaydate{November--December}}{}%
}

\\newcommand{\\playbooksection}[1]{%
  \\IfStrEq{#1}{How to use this document}{}{%
    \\IfSubStr{#1}{PHASE }{%
      \\Needspace{7\\baselineskip}%
      \\StrBefore{#1}{:}[\\playbookphaseprefix]%
      \\StrBehind{#1}{: }[\\playbookphasetitle]%
      \\PlaybookResolvePhase{\\playbookphasetitle}%
      \\par\\vspace{8pt}%
      \\noindent\\raisebox{0.45ex}{{\\playbooksans\\bfseries\\color{playbookred}\\fontsize{7.4}{9}\\selectfont\\addfontfeatures{LetterSpace=8.5}\\playbookphaseprefix}}%
      \\hspace{0.75em}{\\playbookdisplay\\bfseries\\color{playbookink}\\fontsize{20}{22}\\selectfont\\playbookdisplaytitle}%
      \\hfill{\\playbookdisplay\\itshape\\color{playbookmuted}\\fontsize{10}{11.5}\\selectfont\\playbookdisplaydate}\\par
      \\vspace{5pt}{\\color{playbookink}\\rule{\\linewidth}{0.7pt}}\\vspace{6pt}%
    }{%
      \\IfSubStr{#1}{THE RECURRING WEEK}{%
        \\Needspace{7\\baselineskip}%
        \\par\\vspace{10pt}%
        \\noindent{\\playbooksans\\color{playbookgold}\\fontsize{7.2}{9}\\selectfont\\addfontfeatures{LetterSpace=8.5}ENGINE}%
        \\hspace{0.8em}{\\playbookdisplay\\bfseries\\color{playbookink}\\fontsize{19}{21}\\selectfont The Recurring Week}\\par
        \\vspace{5pt}{\\color{playbookink}\\rule{\\linewidth}{0.7pt}}\\vspace{6pt}%
      }{%
        \\Needspace{4\\baselineskip}%
        \\par\\vspace{7pt}%
        \\noindent{\\playbooksans\\bfseries\\color{playbookred}\\fontsize{7.4}{9}\\selectfont\\addfontfeatures{LetterSpace=9}\\MakeUppercase{#1}}\\par
        \\vspace{4pt}{\\color{playbookline}\\rule{\\linewidth}{0.55pt}}\\vspace{4pt}%
      }%
    }%
  }%
}

\\newcommand{\\playbooksubsection}[1]{%
  \\Needspace{9\\baselineskip}%
  \\par\\vspace{4pt}%
  \\IfSubStr{#1}{:}{%
    \\StrBefore{#1}{:}[\\playbookweeklabel]%
    \\StrBehind{#1}{: }[\\playbookweektitle]%
    \\noindent{\\playbooksans\\bfseries\\color{playbookred}\\fontsize{7.2}{9}\\selectfont\\addfontfeatures{LetterSpace=7.5}\\MakeUppercase{\\playbookweeklabel}}\\par
    \\vspace{2pt}{\\itshape\\color{playbookmuted}\\playbookweektitle}\\par
  }{%
    \\noindent{\\playbooksans\\bfseries\\color{playbookred}\\fontsize{7.2}{9}\\selectfont\\addfontfeatures{LetterSpace=7.5}\\MakeUppercase{#1}}\\par
  }%
  \\vspace{3pt}{\\color{playbookline}\\rule{\\linewidth}{0.45pt}}\\vspace{3pt}%
}

\\newcommand{\\PlaybookContinued}[1]{%
  \\noindent{\\playbooksans\\bfseries\\color{playbookred}\\fontsize{7.3}{9}\\selectfont\\addfontfeatures{LetterSpace=7.5}\\MakeUppercase{#1 --- CONTINUED}}\\par
  \\vspace{4pt}{\\color{playbookink}\\rule{\\linewidth}{0.7pt}}\\vspace{6pt}
}
\\newcommand{\\PlaybookPhaseBreak}[1]{%
  \\IfStrEq{#1}{0}{\\clearpage}{%
    \\IfStrEq{#1}{1}{\\clearpage}{}%
  }%
}
\\newcommand{\\PlaybookContinuationBreak}{\\Needspace{8\\baselineskip}}
\\newcommand{\\PlaybookMonthBreak}[1]{}
\\newcommand{\\PlaybookEngineBreak}{\\Needspace{18\\baselineskip}}
\\newcommand{\\PlaybookTableHead}[1]{{\\playbooksans\\bfseries\\color{playbookred}\\fontsize{7.2}{8.7}\\selectfont\\addfontfeatures{LetterSpace=4.5}\\MakeUppercase{#1}}}
\\newcommand{\\PlaybookDeliverable}[1]{{\\bfseries\\color{playbookred}#1}}
\\newcommand{\\PlaybookBackmatterStart}{\\clearpage\\noindent\\begin{minipage}[t]{0.47\\textwidth}}
\\newcommand{\\PlaybookBackmatterMiddle}{\\end{minipage}\\hfill\\begin{minipage}[t]{0.47\\textwidth}}
\\newcommand{\\PlaybookBackmatterEnd}{\\end{minipage}\\par\\vspace{10pt}}

\\titleformat*{\\paragraph}{\\playbooksans\\bfseries\\color{playbookred}\\fontsize{10}{12}\\raggedright}
\\titleformat*{\\subparagraph}{\\playbooksans\\bfseries\\color{playbookmuted}\\fontsize{9}{11}\\raggedright}

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
\\newcommand{\\playbookwarmtitle}{Thought Leadership Countdown \\& Launch Plan}
\\newcommand{\\PlaybookWarmTitle}[1]{\\renewcommand{\\playbookwarmtitle}{#1}}
\\renewcommand\\maketitle{%
  \\thispagestyle{fancy}%
  \\vspace*{0.18in}%
  {\\playbooksans\\bfseries\\color{playbookred}\\fontsize{7.4}{9}\\selectfont\\addfontfeatures{LetterSpace=12.5}A SOCRATIC PLAYBOOK\\par}
  \\vspace{13pt}%
  {\\playbookdisplay\\bfseries\\color{playbookink}\\fontsize{31}{32}\\selectfont\\playbookwarmtitle\\par}
  \\vspace{7pt}%
  {\\playbookdisplay\\itshape\\color{playbookmuted}\\fontsize{11.2}{13.2}\\selectfont July--December 2026 --- printed and on the wall.\\par}
  \\vspace{13pt}%
  {\\playbooksans\\color{playbookmuted}\\fontsize{6.8}{8}\\selectfont\\addfontfeatures{LetterSpace=8}JAY DIXIT \\hspace{1.7em}/\\hspace{1.7em} FABLE \\hspace{1.7em}/\\hspace{1.7em} SOCRATIC PLAYBOOK\\par}
  \\vspace{13pt}{\\color{playbookink}\\rule{\\linewidth}{0.8pt}}\\vspace{7pt}%
}
\\makeatother

[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\playbookrootsection{%s}" . "\\playbookrootsection{%s}")
               ("\\playbooksection{%s}" . "\\playbooksection{%s}")
               ("\\playbooksubsection{%s}" . "\\playbooksubsection{%s}")
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
  (jay/latex-apply-prose-defaults "thought-leadership-playbook-warm"))

(when (fboundp 'jay/latex-register-wrap-class)
  (jay/latex-register-wrap-class "thought-leadership-playbook-warm"))

(defun jay/playbook-warm--style-task-lead-ins (text)
  "Turn selected shouting task leads in TEXT into sentence-case bold leads."
  (dolist (replacement
           '(("START THE DAILY LAYER." . "\\textbf{Start the daily layer.}")
             ("MANIFESTO SHIPS." . "\\textbf{Manifesto ships.}")
             ("GROUP M EMAIL SENDS" . "\\textbf{Group M email sends}")
             ("YOUTUBE LAUNCHES." . "\\textbf{YouTube launches.}")
             ("ESSAY \\#1" . "\\textbf{Essay \\#1}")))
    (setq text
          (replace-regexp-in-string
           (regexp-quote (car replacement))
           (cdr replacement)
           text t t)))
  text)

(defun jay/playbook-warm-item-filter (text backend info)
  "Render milestone and floor-check list items as warm playbook set-pieces."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string= (plist-get info :latex-class)
                    "thought-leadership-playbook-warm"))
      (cond
       ((string-match
         "\\`\\\\item\\[{\\$\\\\square\\$ \\([^]}]+\\)}\\] \\(\\(?:MONTHLY REVIEW\\|DECISION DEADLINE\\)\\(?:.\\|\n\\)*\\)\\'"
         text)
        (format (concat "\\playbookblanklabeltrue\\item[]\\playbookblanklabelfalse\n"
                        "\\Needspace{5\\baselineskip}\n"
                        "\\begin{tcolorbox}[playbookmilestone]\n"
                        "\\PlaybookMilestoneLabel{%s}%%\n"
                        "{\\fontsize{8.85}{9.4}\\selectfont %s}"
                        "\\end{tcolorbox}\n")
                (match-string 1 text)
                (match-string 2 text)))
       ((string-match-p "\\`\\\\item August floor check:" text)
        "\\playbookblanklabeltrue\\item[]\\playbookblanklabelfalse\n\\PlaybookFloorCheck\n")
       ((string-match
         "\\`\\\\item\\[{\\$\\\\square\\$ \\([^]}]+\\)}\\] \\(\\(?:.\\|\n\\)*\\)\\'"
         text)
        (format (concat "\\playbookblanklabeltrue\\item[]\\playbookblanklabelfalse\n"
                        "\\PlaybookTask{%s}{%s}\n")
                (match-string 1 text)
                (jay/playbook-warm--style-task-lead-ins
                 (match-string 2 text))))
       (t text))
    text))

(defun jay/playbook-warm-table-cell-filter (text backend info)
  "Accent recurring-week deliverables in the warm playbook table."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string= (plist-get info :latex-class)
                    "thought-leadership-playbook-warm"))
      (let ((result text)
            (case-fold-search nil))
        (dolist (deliverable '("MINDSET VIDEO" "ESSAY" "WORK-SESSION VIDEO"
                               "RECORDING SESSION"))
          (setq result
                (replace-regexp-in-string
                 (regexp-quote deliverable)
                 (format "\\PlaybookDeliverable{%s}" deliverable)
                 result t t)))
        result)
    text))

(defun jay/playbook-warm-table-filter (text backend info)
  "Make the recurring-week prose columns ragged right in the warm playbook."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string= (plist-get info :latex-class)
                    "thought-leadership-playbook-warm"))
      (replace-regexp-in-string
       (regexp-quote "\\begin{tabularx}{\\textwidth}{lXXr}")
       "\\begin{tabularx}{\\textwidth}{lYYr}"
       text t t)
    text))

(add-to-list 'org-export-filter-item-functions
             #'jay/playbook-warm-item-filter)

(add-to-list 'org-export-filter-table-cell-functions
             #'jay/playbook-warm-table-cell-filter)

(add-to-list 'org-export-filter-table-functions
             #'jay/playbook-warm-table-filter)
