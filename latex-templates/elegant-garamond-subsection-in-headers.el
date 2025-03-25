(provide 'elegant-garamond-subsection)

(add-to-list 'org-latex-classes
             '("elegant-garamond-subsection"
               "
\\documentclass[12pt]{article}
\\usepackage[includeheadfoot, margin=1.5in, hmargin=1.5in, vmargin=0.5in]{geometry} % Set margins

% Package Inclusions
\\usepackage{wrapfig}
\\usepackage{float} % For precise figure placement
\\usepackage{changepage}
\\usepackage{algorithm}
\\usepackage{pdfpages}
\\usepackage{amsmath}
\\usepackage{ifxetex}
\\usepackage{setspace}
\\usepackage{url}
\\usepackage{paralist}
\\usepackage{tikz}
\\usepackage{calc}
\\usepackage{eso-pic}
\\usepackage{etoolbox}
\\usepackage{xcolor}
\\usepackage{microtype} % Improve typography
\\usepackage{booktabs} % Professional-looking tables
\\usepackage{fancyhdr} % Custom headers and footers
\\usepackage{xspace} % Consistent spacing after commands
\\usepackage{listings}
\\usepackage{fancyvrb}
\\usepackage{enumerate}
\\usepackage{ctable}
\\usepackage{tocloft}
\\usepackage[normalem]{ulem}
\\usepackage{enumitem}
\\usepackage{titlesec}
\\usepackage{lipsum}
\\usepackage[breaklinks=true, linktocpage, xetex]{hyperref}
\\usepackage[most]{tcolorbox} % For enhanced environments

\\newcounter{level} % Define the custom counter
\\usepackage{forloop}

\\setlength{\\headheight}{14.49998pt}

% Font Settings
\\ifxetex
  \\usepackage{fontspec}
  \\defaultfontfeatures{Mapping=tex-text, Scale=MatchLowercase}
  \\setsansfont{TeX Gyre Pagella}

  % Set main font to Garamond Premier Pro
  \\setromanfont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = GaramondPremrPro,
    ItalicFont = GaramondPremrPro-It,
    BoldFont = GaramondPremrPro-Bd,
    BoldItalicFont = GaramondPremrPro-BdIt,
    Extension = .otf
  ]{Garamond Premier Pro}

  % Set monospaced font to Helvetica Neue LT Pro
  \\setmonofont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-Lt,
    BoldFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
\\else
  \\usepackage[mathletters]{ucs}
  \\usepackage[utf8x]{inputenc}
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

\\usepackage{enumitem} % Ensures advanced list customization
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
\\renewcommand{\\subsectionmark}[1]{\\markboth{#1}{}}
\\lhead{\\href{\\the\\leftheaderurl}{\\the\\leftheader}}
\\rhead{\\leftmark}
\\cfoot{\\thepage}

% Paragraph and Indentation Settings
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{12pt plus 2pt minus 1pt} % Space between paragraphs
\\onehalfspacing
\\setstretch{1.2}

% Table of Contents Customization
\\renewcommand{\\contentsname}{Table of Contents}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}

% Description Environment Customization
\\renewcommand{\\descriptionlabel}[1]{%
  {\\hspace{\\labelsep}\\bfseries\\textsc{#1}}}
\\setlist[description]{style=nextline, before=\\vspace{\\baselineskip}}

% List Environment Customization
\\setlist{noitemsep, topsep=-8pt, after=\\vspace{12pt}} % Control spacing in lists
\\setlistdepth{10}

% Define a Custom Raised Bullet Command
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
\\forloop{level}{1}{\\value{level} <= 10}{%
  \\setlist[itemize,\\arabic{level}]{label=\\raisedtinybullet}
}

% Quote Environment Customization Using tcolorbox

\\tcbset{
  myquote/.style={
    colback=azure,
    colframe=spacegrey,
    colupper=black,
    boxrule=0.5pt,
    rounded corners,
    fontupper=\\singlespacing\\fontsize{11}{13}\\selectfont\\ttfamily,
    width=0.8\\textwidth,
    enlarge left by=0pt,
    enlarge right by=0pt,
    box align=center,
    halign=flush left,
    before skip=0pt,        % Removes external vertical space above box
    after skip=1em,         % Maintains external space below box
    parbox=false,           % Allows natural paragraph handling
    parskip=0.8em,          % Controls spacing between paragraphs
    top=0pt,                % Removes internal vertical padding at top
    bottom=0pt,             % Optionally adjust bottom padding
    center
  }
}
\\renewenvironment{quote}
{%
  \\begin{tcolorbox}[myquote]
}
{%
  \\end{tcolorbox}
}

% Titlesec Configuration
\\titlespacing*{\\section}{0pt}{48pt}{0pt}
\\titlespacing*{\\subsubsection}{0pt}{16pt}{0pt}
\\titlespacing{\\paragraph}{0pt}{0pt}{.5em}[]

\\newcommand{\\mysectiontitle}[1]{%
  \\raggedright\\fontsize{40}{26}\\selectfont #1
}

\\titleformat{\\section}
  {\\normalfont\\ttfamily\\color{spacegrey}}
  {}
  {0em}
  {\\mysectiontitle}

\\titleformat*{\\subsection}{\\sffamily\\setstretch{0.1}\\fontsize{24}{36}\\raggedright\\sffamily}
\\titleformat*{\\subsubsection}{\\ttfamily\\bfseries\\fontsize{18}{16}\\raggedright\\ttfamily\\color{spacegrey}}
\\titleformat*{\\paragraph}{\\ttfamily\\bfseries\\fontsize{19}{12}\\raggedright}
\\titleformat*{\\subparagraph}{\\sffamily\\fontsize{16}{12}\\raggedright\\ttfamily\\bfseries}

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

(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f")) ;; for multiple passes
