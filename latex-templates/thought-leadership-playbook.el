;;; thought-leadership-playbook.el --- Org LaTeX class -*- lexical-binding: t; -*-

(provide 'thought-leadership-playbook)

(add-to-list 'org-latex-classes
             '("thought-leadership-playbook"
               "
\\providecommand{\\DocumentMetadata}[1]{}
\\DocumentMetadata{lang=en-US}
\\documentclass[10pt]{article}
\\usepackage[includeheadfoot, top=0.70in, bottom=0.62in, left=1in, right=1in, headsep=0.18in]{geometry}
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
\\usepackage{xstring}
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
\\AtBeginEnvironment{tabularx}{\\jaytableformat}
\\AtBeginEnvironment{tabular}{\\jaytableformat}
\\AtBeginEnvironment{longtable}{\\jaytableformat}
\\renewcommand{\\toprule}{\\specialrule{1.6pt}{0pt}{2pt}}
\\renewcommand{\\bottomrule}{\\specialrule{1.6pt}{2pt}{0pt}}
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
  \\newfontfamily\\playbookdisplay{Bodoni 72}
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
\\definecolor{playbookink}{HTML}{16233B}
\\definecolor{playbookbody}{HTML}{2A3346}
\\definecolor{playbookblue}{HTML}{1B3A6B}
\\definecolor{playbookbluebright}{HTML}{1B3A6B}
\\definecolor{playbookpowder}{HTML}{E8EEF6}
\\definecolor{playbookwash}{HTML}{EEF3FA}
\\definecolor{playbookline}{HTML}{EAEEF4}
\\definecolor{playbookred}{HTML}{D84A3A}
\\definecolor{playbookmuted}{HTML}{8791A3}
\\definecolor{playbookledlabel}{HTML}{8FA9CE}

\\newcommand{\\labelitemv}{\\textbullet}
\\newcommand{\\labelitemvi}{\\textbullet}
\\newcommand{\\labelitemvii}{\\textbullet}
\\newcommand{\\labelitemviii}{\\textbullet}
\\newcommand{\\labelitemix}{\\textbullet}
\\newcommand{\\labelitemx}{\\textbullet}

% Header and Footer Configuration
\\fancyhf{} % Clear all header and footer fields
\\renewcommand{\\headrulewidth}{0.5pt}
\\renewcommand{\\headrule}{\\hbox to\\headwidth{\\color{playbookblue}\\leaders\\hrule height \\headrulewidth\\hfill}}
\\pagestyle{fancy}
\\newtoks\\leftheader
\\newtoks\\leftheaderurl
\\newtoks\\coverimage
\\def\\jayfooterlogo{/Users/jay/Dropbox/writing/prosperous/design/storytelling-nyc-logo/current-2018/_better-storytelling-nyc-period-canonical-helvetica-condensed.png}
\\newcommand{\\footerlogo}[1]{\\def\\jayfooterlogo{#1}\\jayapplyfooter}
\\def\\jayfooterbrand{}
\\newcommand{\\footerbrand}[1]{\\def\\jayfooterbrand{#1}\\jayapplyfooter}
\\newcommand{\\playbookperiod}{JULY—DECEMBER 2026}
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
    {\\lhead{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7.5}{9}\\selectfont\\addfontfeatures{LetterSpace=5.5}\\MakeUppercase{\\jayresolvedleftheader}}}}
    {\\lhead{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7.5}{9}\\selectfont\\addfontfeatures{LetterSpace=5.5}\\href{\\the\\leftheaderurl}{\\MakeUppercase{\\jayresolvedleftheader}}}}}%
}
\\makeatother
\\rhead{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7.5}{9}\\selectfont\\addfontfeatures{LetterSpace=5.5}\\playbookperiod}}
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
    \\fancyfoot[L]{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7}{8}\\selectfont\\addfontfeatures{LetterSpace=4.5}PLAYBOOK}}%
    \\fancyfoot[C]{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7}{8}\\selectfont\\addfontfeatures{LetterSpace=5}\\playbookstrap}}%
    \\fancyfoot[R]{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7}{8}\\selectfont\\addfontfeatures{LetterSpace=3}SOCRATIC AI \\textperiodcentered{} PAGE \\thepage}}%
  \\fi\\fi
}
\\newcommand{\\EnableLogoFooter}{\\jaynofooterfalse\\jaylogofootertrue\\jayapplyfooter}
\\newcommand{\\DisableFooter}{\\jaylogofooterfalse\\jaynofootertrue\\jayapplyfooter}
\\jayapplyfooter

% Paragraph and Indentation Settings
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{3.5pt plus 1pt minus 1pt}
\\setstretch{1.04}
\\color{playbookbody}
\\AtBeginDocument{\\fontsize{9.2}{11}\\selectfont}

% Table of Contents Customization
\\renewcommand{\\contentsname}{Table of Contents}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}

% Description Environment Customization
\\renewcommand{\\descriptionlabel}[1]{%
  {\\hspace{\\labelsep}\\bfseries\\textsc{#1}}}
\\setlist[description]{style=standard, leftmargin=0pt, labelwidth=0pt,
  labelsep=0pt, itemsep=0pt, topsep=2pt, parsep=0pt,
  before=\\vspace{1pt}, after=\\vspace{5pt}}

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
\\newcommand{\\PlaybookTask}[2]{%
  \\par\\noindent{\\color{playbookline}\\rule{\\linewidth}{0.4pt}}\\vspace{1.3pt}\\par
  \\noindent\\begin{minipage}[t]{1.45em}\\vspace{0.35pt}{\\color{playbookblue}$\\square$}\\end{minipage}%
  \\begin{minipage}[t]{\\dimexpr\\linewidth-1.45em\\relax}
    {\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7}{8.2}\\selectfont\\addfontfeatures{LetterSpace=3.5}\\MakeUppercase{#1}}\\par
    \\vspace{0.4pt}{\\RaggedRight\\fontsize{8.75}{10.15}\\selectfont #2}
  \\end{minipage}\\par\\vspace{0.5pt}}
\\newcommand{\\PlaybookDeferred}[1]{%
  \\noindent{\\color{playbookblue}$\\square$}\\hspace{0.45em}{\\fontsize{8.7}{10.2}\\selectfont #1}\\par\\vspace{2pt}}

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
    breakable=false,
    colback=playbookink,
    colframe=playbookink,
    colupper=white,
    boxrule=0pt,
    sharp corners,
    left=16pt,
    right=16pt,
    top=15pt,
    bottom=15pt,
    before skip=4pt,
    after skip=10pt,
    before upper=\\RaggedRight
  },
  playbookmilestone/.style={
    enhanced,
    breakable=false,
    colback=playbookwash,
    colframe=playbookwash,
    boxrule=0pt,
    borderline west={4pt}{0pt}{playbookblue},
    sharp corners,
    left=14pt,
    right=14pt,
    top=10pt,
    bottom=10pt,
    before skip=4pt,
    after skip=6pt,
    before upper=\\RaggedRight
  },
  playbookfloor/.style={
    enhanced,
    breakable=false,
    colback=playbookink,
    colframe=playbookink,
    boxrule=0pt,
    sharp corners,
    left=18pt,
    right=18pt,
    top=9pt,
    bottom=10pt,
    before skip=6pt,
    after skip=7pt
  }
}

\\newcommand{\\PlaybookMilestoneLabel}[1]{%
  \\noindent{\\color{playbookblue}$\\square$}\\hspace{0.45em}%
  {\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8.2}{9.5}\\selectfont\\addfontfeatures{LetterSpace=3.5}\\MakeUppercase{#1}}%
  \\par\\vspace{4pt}}

\\newcommand{\\PlaybookFloorCheck}{%
  \\begin{tcolorbox}[playbookfloor]
    {\\playbooksans\\bfseries\\color{playbookledlabel}\\fontsize{8}{9.5}\\selectfont\\addfontfeatures{LetterSpace=9.5}AUGUST FLOOR CHECK}\\par
    \\vspace{6pt}
    \\begingroup
    \\renewcommand{\\arraystretch}{1.05}
    \\setlength{\\tabcolsep}{2pt}
    \\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}*{6}{c}@{}}
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{20}{21}\\selectfont 8} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{20}{21}\\selectfont 8--12} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{20}{21}\\selectfont 2} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{20}{21}\\selectfont \\textasciitilde{}20} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{20}{21}\\selectfont 12} &
      {\\playbookdisplay\\bfseries\\color{white}\\fontsize{20}{21}\\selectfont 2} \\\\
      {\\playbooksans\\color{playbookledlabel}\\fontsize{6}{7.2}\\selectfont LONG VIDEOS} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{6}{7.2}\\selectfont SHORTS} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{6}{7.2}\\selectfont ESSAYS} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{6}{7.2}\\selectfont NOTES} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{6}{7.2}\\selectfont LINKEDIN} &
      {\\playbooksans\\color{playbookledlabel}\\fontsize{6}{7.2}\\selectfont POD PITCHES}
    \\end{tabular*}
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

% Titlesec Configuration
\\titlespacing*{\\section}{0pt}{24pt}{10pt}
\\titlespacing*{\\subsection}{0pt}{24pt}{10pt}
\\titlespacing*{\\subsubsection}{0pt}{10pt}{5pt}
\\titlespacing{\\paragraph}{0pt}{0pt}{.5em}[]

\\newcommand{\\mysectiontitle}[1]{%
  \\markboth{#1}{#1}%
  \\markright{#1}%
  \\Needspace{6\\baselineskip}%
  \\raggedright\\playbookdisplay\\color{playbookink}\\fontsize{35}{36}\\selectfont\\MakeUppercase{#1}%
  \\par\\vspace{3pt}{\\color{playbookblue}\\rule{\\linewidth}{2pt}}
}

\\newif\\ifplaybookbackmatter
\\playbookbackmatterfalse
\\newcommand{\\playbookphaseprefix}{}
\\newcommand{\\playbookphaserest}{}
\\newcommand{\\playbookphasetitle}{}
\\newcommand{\\playbookphasedates}{}
\\newcommand{\\mysubsectiontitle}[1]{%
  \\markright{#1}%
  \\Needspace{5\\baselineskip}%
  \\ifplaybookbackmatter
    \\noindent\\colorbox{playbookpowder}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{%
      \\strut\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8.2}{9.7}\\selectfont\\addfontfeatures{LetterSpace=5.5}\\MakeUppercase{#1}}}%
  \\else
    \\IfSubStr{#1}{PHASE }{%
      \\StrBefore{#1}{:}[\\playbookphaseprefix]%
      \\StrBehind{#1}{: }[\\playbookphaserest]%
      \\StrBefore{\\playbookphaserest}{ (}[\\playbookphasetitle]%
      \\StrBetween{\\playbookphaserest}{(}{)}[\\playbookphasedates]%
      \\noindent{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8.5}{10}\\selectfont\\addfontfeatures{LetterSpace=7.5}%
        \\MakeUppercase{\\playbookphaseprefix\\space --- \\playbookphasedates}}\\par
      \\vspace{3pt}{\\playbookdisplay\\color{playbookink}\\fontsize{33}{34}\\selectfont\\raggedright\\MakeUppercase{\\playbookphasetitle}}\\par
      \\vspace{5pt}{\\color{playbookblue}\\rule{\\linewidth}{2pt}}%
    }{%
      \\playbookdisplay\\color{playbookink}\\fontsize{27}{29}\\selectfont\\raggedright\\MakeUppercase{#1}%
    }%
  \\fi
}

\\newcommand{\\playbookweeklabel}{}
\\newcommand{\\playbookweektitle}{}
\\newcommand{\\mysubsubsectiontitle}[1]{%
  \\Needspace{9\\baselineskip}%
  \\IfSubStr{#1}{:}{%
    \\StrBefore{#1}{:}[\\playbookweeklabel]%
    \\StrBehind{#1}{: }[\\playbookweektitle]%
    \\noindent\\colorbox{playbookpowder}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{%
      \\strut{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8.3}{9.8}\\selectfont\\addfontfeatures{LetterSpace=3.5}\\MakeUppercase{\\playbookweeklabel}}%
      \\hspace{0.65em}{\\playbooksans\\color{playbookmuted}\\fontsize{8.3}{9.8}\\selectfont\\MakeUppercase{\\playbookweektitle}}}}%
  }{%
    \\noindent\\colorbox{playbookpowder}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{%
      \\strut\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8.3}{9.8}\\selectfont\\addfontfeatures{LetterSpace=3.5}\\MakeUppercase{#1}}}%
  }%
}

\\newcommand{\\PlaybookContinued}[1]{%
  \\noindent{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8.5}{10}\\selectfont\\addfontfeatures{LetterSpace=7.5}\\MakeUppercase{#1 --- CONTINUED}}\\par
  \\vspace{5pt}{\\color{playbookline}\\rule{\\linewidth}{0.5pt}}\\vspace{8pt}
}
\\newcommand{\\PlaybookPhaseBreak}[1]{\\clearpage}
\\newcommand{\\PlaybookContinuationBreak}{\\clearpage}
\\newcommand{\\PlaybookMonthBreak}[1]{}
\\newcommand{\\PlaybookEngineBreak}{\\clearpage}
\\newcommand{\\PlaybookTableHead}[1]{{\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{7.5}{9}\\selectfont\\addfontfeatures{LetterSpace=3.5}\\MakeUppercase{#1}}}
\\newcommand{\\PlaybookDeliverable}[1]{{\\bfseries\\color{playbookblue}#1}}
\\newcommand{\\PlaybookDuration}[1]{{\\itshape\\color{playbookmuted}#1}}
\\newcommand{\\PlaybookProtocol}[1]{%
  \\par\\vspace{5pt}\\begin{tcolorbox}[enhanced,breakable=false,colback=white,colframe=white,
    boxrule=0pt,borderline west={2.5pt}{0pt}{playbookblue},sharp corners,
    left=10pt,right=6pt,top=4pt,bottom=4pt,before skip=0pt,after skip=5pt]
    {\\RaggedRight\\fontsize{8.7}{10.3}\\selectfont #1}
  \\end{tcolorbox}}
\\newcommand{\\PlaybookBackmatterStart}{\\par\\vspace{14pt}\\playbookbackmattertrue\\noindent\\begin{minipage}[t]{0.47\\textwidth}\\RaggedRight}
\\newcommand{\\PlaybookBackmatterMiddle}{\\end{minipage}\\hfill\\begin{minipage}[t]{0.47\\textwidth}\\RaggedRight}
\\newcommand{\\PlaybookBackmatterEnd}{\\end{minipage}\\playbookbackmatterfalse\\par\\vspace{12pt}}

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
    \\vspace*{0.18in}
    {\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8}{9}\\selectfont\\addfontfeatures{LetterSpace=6.5}THOUGHT LEADERSHIP COUNTDOWN\\hfill\\playbookperiod\\par}
    \\vspace{7pt}{\\color{playbookblue}\\rule{\\linewidth}{0.5pt}}
    \\vfill
    {\\centering\\playbookdisplay\\color{playbookink}\\fontsize{72}{68}\\selectfont THOUGHT\\par LEADERSHIP\\par}
    \\vspace{12pt}
    {\\centering\\playbookdisplay\\itshape\\color{playbookblue}\\fontsize{30}{32}\\selectfont Countdown\\par}
    \\vspace{17pt}
    {\\centering\\color{playbookblue}\\rule{120pt}{3pt}\\par}
    \\vspace{18pt}
    {\\centering\\itshape\\color{playbookbody}\\fontsize{12}{14}\\selectfont July--December 2026 --- printed and on the wall.\\par}
    \\vfill
    {\\color{playbookline}\\rule{\\linewidth}{0.5pt}}\\vspace{8pt}
    {\\playbooksans\\bfseries\\color{playbookblue}\\fontsize{8}{9}\\selectfont\\addfontfeatures{LetterSpace=5.5}%
      JAY DIXIT\\hfill FABLE\\hfill SOCRATIC PLAYBOOK\\par}
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

(defun jay/playbook-blue-item-filter (text backend info)
  "Render milestone and floor-check list items as Blue playbook set-pieces."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string= (plist-get info :latex-class)
                    "thought-leadership-playbook"))
      (cond
       ((string-match
         "\\`\\\\item\\[{\\$\\\\square\\$ \\([^]}]+\\)}\\] \\(\\(?:MONTHLY REVIEW\\|DECISION DEADLINE\\)\\(?:.\\|\n\\)*\\)\\'"
         text)
        (format (concat "\\item[]\n"
                        "\\Needspace{5\\baselineskip}\n"
                        "\\begin{tcolorbox}[playbookmilestone]\n"
                        "\\PlaybookMilestoneLabel{%s}%%\n"
                        "{\\fontsize{9.4}{11.2}\\selectfont %s}"
                        "\\end{tcolorbox}\n")
                (match-string 1 text)
                (match-string 2 text)))
       ((string-match-p "\\`\\\\item August floor check:" text)
        "\\item[]\n\\PlaybookFloorCheck\n")
       ((string-match
         "\\`\\\\item\\[{\\$\\\\square\\$ \\([^]}]+\\)}\\] \\(\\(?:.\\|\n\\)*\\)\\'"
         text)
        (format (concat "\\item[]\n"
                        "\\PlaybookTask{%s}{%s}\n")
                (match-string 1 text)
                (match-string 2 text)))
       ((string-match
         "\\`\\\\item\\[\\({\\$\\\\square\\$}\\)\\] \\(\\(?:.\\|\n\\)*\\)\\'"
         text)
        (format "\\item[]\n\\PlaybookDeferred{%s}\n"
                (match-string 2 text)))
       (t text))
    text))

(add-to-list 'org-export-filter-item-functions
             #'jay/playbook-blue-item-filter)

(defun jay/playbook-blue-table-cell-filter (text backend info)
  "Accent deliverables and durations in the Blue recurring-week table."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string= (plist-get info :latex-class)
                    "thought-leadership-playbook"))
      (let ((result text)
            (case-fold-search nil))
        (dolist (deliverable '("MINDSET VIDEO" "ESSAY" "WORK-SESSION VIDEO"
                               "RECORDING SESSION"))
          (setq result
                (replace-regexp-in-string
                 (regexp-quote deliverable)
                 (format "\\PlaybookDeliverable{%s}" deliverable)
                 result t t)))
        (if (string-match-p
             "\\`\\(?:[0-9]\\|\\\\textasciitilde{}\\).+\\(?:min\\|hrs\\)\\'"
             result)
            (format "\\PlaybookDuration{%s}" result)
          result))
    text))

(defun jay/playbook-blue-table-filter (text backend info)
  "Make the recurring-week prose columns ragged right in the Blue playbook."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string= (plist-get info :latex-class)
                    "thought-leadership-playbook"))
      (replace-regexp-in-string
       (regexp-quote "\\begin{tabularx}{\\textwidth}{lXXr}")
       "\\begin{tabularx}{\\textwidth}{lYYr}"
       text t t)
    text))

(defun jay/playbook-blue-paragraph-filter (text backend info)
  "Turn the recurring-week Notes protocol into a compact rule callout."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string= (plist-get info :latex-class)
                    "thought-leadership-playbook")
           (string-match "Notes protocol:" text))
      (let ((totals (string-trim-right
                     (substring text 0 (match-beginning 0))))
            (protocol (string-trim
                       (substring text (match-beginning 0)))))
        (format "%s\n\\PlaybookProtocol{%s}\n" totals protocol))
    text))

(add-to-list 'org-export-filter-table-cell-functions
             #'jay/playbook-blue-table-cell-filter)

(add-to-list 'org-export-filter-table-functions
             #'jay/playbook-blue-table-filter)

(add-to-list 'org-export-filter-paragraph-functions
             #'jay/playbook-blue-paragraph-filter)
