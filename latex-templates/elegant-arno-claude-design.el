;;; elegant-arno-claude-design.el --- Org LaTeX class -*- lexical-binding: t; -*-

(provide 'elegant-arno-claude-design)

(add-to-list 'org-latex-classes
             '("elegant-arno-claude-design"
               "
\\providecommand{\\DocumentMetadata}[1]{}
\\DocumentMetadata{lang=en-US}
\\documentclass[12pt]{article}
\\usepackage[includeheadfoot, margin=1in]{geometry} % Standard proposal/document margins
\\setlength{\\footskip}{0.95in}

% Package Inclusions
\\usepackage{wrapfig}
\\usepackage{float} % For precise figure placement
\\usepackage{changepage}
\\usepackage{algorithm}
\\usepackage{pdfpages}
\\usepackage{amsmath}
\\usepackage{amssymb} % Provides Org's \\square checkbox symbol
\\usepackage{iftex}
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
\\usepackage{needspace} % Keeps headings with the text that follows
\\usepackage{marginnote} % Notes in the source rail (see \\EnableSourceRail)
\\usepackage{lipsum}
\\usepackage[breaklinks=true, linktocpage]{hyperref}
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

\\setlength{\\headheight}{14.49998pt}

% Table configuration for automatic text wrapping
\\newcolumntype{Y}{>{\\RaggedRight\\arraybackslash}X}
\\newcolumntype{Z}{>{\\Centering\\arraybackslash}X}
\\newcolumntype{W}{>{\\RaggedLeft\\arraybackslash}X}
\\renewcommand{\\tabularxcolumn}[1]{m{#1}}
\\newcommand{\\jaytableformat}{%
  \\setlength{\\tabcolsep}{6pt}%
  \\renewcommand{\\arraystretch}{1.05}%
  \\fontsize{10}{12}\\selectfont\\jaytablefigures}
\\AtBeginEnvironment{tabularx}{\\jaytableformat}
\\AtBeginEnvironment{tabular}{\\jaytableformat}
\\AtBeginEnvironment{longtable}{\\jaytableformat}
\\setlength{\\LTpre}{0pt}
\\setlength{\\LTpost}{0pt}

% Font Settings
\\ifPDFTeX
  \\usepackage[mathletters]{ucs}
  \\usepackage[utf8x]{inputenc}
  \\newcommand{\\jayheadingfont}{\\ttfamily}
  \\newcommand{\\jayfooterbrandstyle}[1]{{\\sffamily\\bfseries\\color{jayfooterbrandgrey}\\fontsize{50}{50}\\selectfont \\MakeUppercase{#1}}}
  \\newcommand{\\jaylabelfont}{\\sffamily}
  \\newcommand{\\jaycaps}{\\sffamily}
  \\newcommand{\\jaytablefigures}{}
  \\newcommand{\\UseLiningFigures}{}
\\else
  \\usepackage{fontspec}
  \\defaultfontfeatures{Ligatures=TeX, Scale=MatchLowercase}
  \\setsansfont{Arno Pro Subhead}
  \\newfontfamily\\jayfooterbrandfont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
  \\newcommand{\\jayfooterbrandstyle}[1]{{\\jayfooterbrandfont\\color{jayfooterbrandgrey}\\addfontfeatures{LetterSpace=-5}\\fontsize{50}{50}\\selectfont \\MakeUppercase{#1}}}

  % Set main font to Adobe Arno Pro
  \\setromanfont[
    Numbers = OldStyle,
    Ligatures = {Common}
  ]{Arno Pro}

  % Set monospaced font to Helvetica Neue LT Pro
  \\setmonofont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-MdCn,
    BoldFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}

  % Condensed sans for labels, running heads, tags and the source rail
  \\newfontfamily\\jaylabelfont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-MdCn,
    BoldFont = HelveticaNeueLTPro-BdCn,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
  \\newcommand{\\jaycaps}{\\jaylabelfont\\addfontfeatures{LetterSpace=9}} % Tracked, for all-caps labels only
  \\newcommand{\\jaytablefigures}{\\addfontfeatures{Numbers={Lining,Monospaced}}} % Figures line up in table columns
  \\newcommand{\\UseLiningFigures}{\\setromanfont[Numbers = Lining, Ligatures = {Common}]{Arno Pro}}
  \\ifLuaTeX
    \\newfontfamily\\jayheadingfont[
      Renderer = HarfBuzz,
      Path = /System/Library/Fonts/,
      UprightFont = SFNS.ttf,
      BoldFont = SFNS.ttf,
      BoldFeatures = {Weight=700}
    ]{SFNS.ttf}
  \\else
    \\newfontfamily\\jayheadingfont[
      Path = /System/Library/Fonts/,
      UprightFont = SFNS.ttf,
      BoldFont = SFNS.ttf,
      BoldFeatures = {FakeBold=2}
    ]{SFNS.ttf}
  \\fi
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

% Working palette: every color has one job
\\definecolor{jayink}{HTML}{1C1B1A}       % body text
\\definecolor{jayinksoft}{HTML}{4A4744}   % running heads, folios, bullets, sources
\\definecolor{jayrule}{HTML}{BDB6B0}      % hairline rules
\\definecolor{jayaccent}{HTML}{C0261F}    % chinagraph red: labels, links, fine marks
\\definecolor{jaypanel}{HTML}{FBF1F2}     % stone rose: quote and callout panels
\\definecolor{jayhighlight}{HTML}{FBB6CE} % pale pink: highlighter behind dark ink
\\definecolor{jaycaution}{HTML}{F1E9AE}   % legal pad yellow: caution tag fill
\\definecolor{jaygold}{HTML}{C89B34}      % gold: caution tag border

% Highlighter, tags and labels (Org macros hl, tag, caution, stop, label)
\\ifLuaTeX
  \\usepackage{luacolor}
  \\usepackage{lua-ul}
  \\newcommand{\\jayhl}[1]{\\highLight[jayhighlight]{#1}}
\\else
  \\newcommand{\\jayhl}[1]{\\colorbox{jayhighlight}{#1}}
\\fi
\\newcommand{\\jaytagbox}[4]{\\begingroup\\setlength{\\fboxsep}{1.6pt}\\setlength{\\fboxrule}{0.5pt}\\fcolorbox{#1}{#2}{\\jaycaps\\bfseries\\fontsize{7.5}{8}\\selectfont\\color{#3}\\strut\\MakeUppercase{#4}}\\endgroup}
\\newcommand{\\jaytag}[1]{\\jaytagbox{jayink}{white}{jayink}{#1}}
\\newcommand{\\jaycautiontag}[1]{\\jaytagbox{jaygold}{jaycaution}{jayink}{#1}}
\\newcommand{\\jaystoptag}[1]{\\jaytagbox{jayaccent}{white}{jayaccent}{#1}}
\\newcommand{\\jaylabel}[1]{{\\jaycaps\\bfseries\\fontsize{9}{11}\\selectfont\\color{jayaccent}\\MakeUppercase{#1}}}

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
\\newcommand{\\jayheadstyle}{\\jaycaps\\fontsize{8.5}{10}\\selectfont\\color{jayinksoft}}
\\pagestyle{fancy}
\\newtoks\\leftheader
\\newtoks\\leftheaderurl
\\newtoks\\coverimage
\\def\\jayfooterlogo{/Users/jay/Dropbox/writing/prosperous/design/storytelling-nyc-logo/current-2018/_better-storytelling-nyc-period-canonical-helvetica-condensed.png}
\\newcommand{\\footerlogo}[1]{\\def\\jayfooterlogo{#1}\\jayapplyfooter}
\\def\\jayfooterbrand{}
\\newcommand{\\footerbrand}[1]{\\def\\jayfooterbrand{#1}\\jayapplyfooter}
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\makeatletter
\\def\\jay@leftheader@split#1\\\\#2\\jay@leftheader@end{\\def\\jayresolvedleftheader{#1}}
\\newcommand{\\jayapplyleftheader}{%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheader}
    {\\expandafter\\jay@leftheader@split\\@title\\\\\\jay@leftheader@end}
    {\\def\\jayresolvedleftheader{\\the\\leftheader}}%
  \\expandafter\\ifstrempty\\expandafter{\\the\\leftheaderurl}
    {\\lhead{\\jayheadstyle\\MakeUppercase{\\jayresolvedleftheader}}}
    {\\lhead{\\jayheadstyle\\href{\\the\\leftheaderurl}{\\color{jayinksoft}\\MakeUppercase{\\jayresolvedleftheader}}}}%
}
\\makeatother
\\rhead{\\jayheadstyle\\MakeUppercase{\\rightmark}}
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
    \\cfoot{\\jayfooterunlesslast{{\\jayheadstyle\\thepage}}}%
  \\fi\\fi
}
\\newcommand{\\EnableLogoFooter}{\\jaynofooterfalse\\jaylogofootertrue\\jayapplyfooter}
\\newcommand{\\DisableFooter}{\\jaylogofooterfalse\\jaynofootertrue\\jayapplyfooter}
\\jayapplyfooter

% Paragraph and Indentation Settings
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{6.5pt plus 1pt minus 1pt} % Space between paragraphs
\\setstretch{1.16}

% Table of Contents Customization
\\renewcommand{\\contentsname}{Table of Contents}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}

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

% Define a Custom Raised Bullet Command
\\newcommand{\\raisedtinybullet}{\\textcolor{jayinksoft}{\\small\\textbullet}} % Arno's own bullet, not Computer Modern's

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
\\setlist[itemize,1]{label=\\raisedtinybullet, after=\\vspace{20pt}, itemsep=3pt, leftmargin=1.5em} % Must follow the loop; otherwise its level-1 reset removes the post-list gap
\\setlist*[itemize,2]{itemsep=1.5pt, leftmargin=1.35em}
\\setlist*[itemize,3]{leftmargin=1.25em}
\\setlist*[enumerate,1]{itemsep=3pt}

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

\\tcbset{
  jaypanel/.style={
    enhanced,
    breakable,
    parbox=false,
    sharp corners,
    frame hidden,
    boxrule=0pt,
    colback=jaypanel,
    colupper=jayink,
    left=12pt,
    right=12pt,
    top=9pt,
    bottom=10pt,
    before skip=12pt,
    after skip=14pt
  }
}

\\renewenvironment{quote}{\\begin{tcolorbox}[jaypanel]}{\\end{tcolorbox}}

% Org special blocks: #+begin_callout (label via #+ATTR_LATEX: :options [Label]) and #+begin_caveat
\\newtcolorbox{callout}[1][]{jaypanel, before upper={\\ifstrempty{#1}{}{\\jaylabel{#1}\\par\\vspace{3pt}}}}
\\newenvironment{caveat}
  {\\par\\noindent\\jaycautiontag{Caveat}\\hspace{0.5em}\\itshape\\ignorespaces}
  {\\par}

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
% Titles in Arno (its optical sizes switch automatically), labels in condensed caps.
% Every \\fontsize is followed by \\selectfont; without it the size never applies.
\\titlespacing*{\\section}{0pt}{40pt}{14pt}
\\titlespacing*{\\subsection}{0pt}{30pt plus 6pt minus 4pt}{10pt}
\\titlespacing*{\\subsubsection}{0pt}{26pt plus 5pt minus 3pt}{8pt}
\\titlespacing*{\\paragraph}{0pt}{16pt plus 3pt minus 2pt}{5pt}
\\titlespacing*{\\subparagraph}{0pt}{12pt plus 2pt}{3pt}

\\newcommand{\\mysectiontitle}[1]{\\markboth{#1}{#1}\\markright{#1}#1}
\\newcommand{\\mysubsectiontitle}[1]{\\markright{#1}#1}
\\newcommand{\\mysubsubsectiontitle}[1]{#1}
\\newcommand{\\myparagraphtitle}[1]{\\MakeUppercase{#1}}

\\titleformat{\\section}[block]
  {\\normalfont\\fontsize{32}{36}\\selectfont\\raggedright\\color{jayink}}
  {}{0pt}{\\mysectiontitle}
\\titleformat{\\subsection}[block]
  {\\normalfont\\fontsize{22}{26}\\selectfont\\raggedright\\color{jayink}}
  {}{0pt}{\\mysubsectiontitle}
\\titleformat{\\subsubsection}[block]
  {\\color{jayink}\\titlerule[1.2pt]\\vspace{7pt}\\normalfont\\bfseries\\fontsize{15.5}{19}\\selectfont\\raggedright}
  {}{0pt}{\\mysubsubsectiontitle}
\\titleformat{\\paragraph}[block]
  {\\normalfont\\jaycaps\\bfseries\\fontsize{9.5}{11.5}\\selectfont\\raggedright\\color{jayaccent}}
  {}{0pt}{\\myparagraphtitle}
\\titleformat{\\subparagraph}[block]
  {\\normalfont\\bfseries\\normalsize\\raggedright}
  {}{0pt}{}

% Never strand a heading at the foot of a page
\\pretocmd{\\section}{\\Needspace*{8\\baselineskip}}{}{}
\\pretocmd{\\subsection}{\\Needspace*{6\\baselineskip}}{}{}
\\pretocmd{\\subsubsection}{\\Needspace*{5\\baselineskip}}{}{}
\\pretocmd{\\paragraph}{\\Needspace*{4\\baselineskip}}{}{}
\\pretocmd{\\subparagraph}{\\Needspace*{3\\baselineskip}}{}{}
% A paragraph that is only bold text (see the export filter below) stays with what follows
\\newcommand{\\jayleadin}[1]{\\par\\Needspace*{3\\baselineskip}\\noindent#1\\par\\nobreak}

% Hyperref Configuration
\\hypersetup{
  colorlinks,
  citecolor = jayaccent,
  filecolor = jayaccent,
  linkcolor = jayaccent,
  urlcolor  = jayaccent
}

% Opt-in, per document, with #+LATEX_HEADER:
%   \\UseLiningFigures   lining numerals for data-heavy briefs (default stays old-style)
%   \\RaggedRightBody    ragged-right text instead of justified
%   \\EnableSourceRail   wide left margin for {{{src(...)}}} notes beside the text
\\newcommand{\\RaggedRightBody}{\\setlength{\\RaggedRightRightskip}{0pt plus 4em}\\setlength{\\emergencystretch}{1.5em}\\AtBeginDocument{\\RaggedRight}}
\\newcommand{\\jaysrc}[1]{\\leavevmode\\marginnote{\\RaggedLeft\\jaylabelfont\\fontsize{8.5}{10.5}\\selectfont\\color{jayinksoft}#1}}
\\newcommand{\\EnableSourceRail}{\\geometry{left=2.3in, right=0.9in, marginparwidth=1.5in, marginparsep=0.25in}\\reversemarginpar}

% Body text in near-black ink
\\AtBeginDocument{\\color{jayink}}

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
      '("%latex -interaction nonstopmode %f"
        "%latex -interaction nonstopmode %f")) ;; for multiple passes

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
  (jay/latex-apply-prose-defaults "elegant-arno-claude-design"))

(when (fboundp 'jay/latex-register-wrap-class)
  (jay/latex-register-wrap-class "elegant-arno-claude-design"))

;;; Inline macros for any Org file: {{{hl(68.01%)}}} {{{tag(Measured)}}}
;;; {{{caution(Vendor)}}} {{{stop(Unverified)}}} {{{label(Before Tuesday)}}}
;;; {{{src(Pew Research Center, July 22, 2025)}}}.  Commas inside the argument
;;; need no escaping.  Other exports (HTML) just show the plain text.
(require 'subr-x)

(defun jay/latex--wrap-macro (command)
  "Return an Org macro template that wraps its whole argument in \\COMMAND{}."
  (lambda (&rest args)
    (format "@@latex:\\%s{@@%s@@latex:}@@" command (mapconcat #'identity args ","))))

(with-eval-after-load 'ox
  (dolist (m (list (cons "hl" (jay/latex--wrap-macro "jayhl"))
                   (cons "tag" (jay/latex--wrap-macro "jaytag"))
                   (cons "caution" (jay/latex--wrap-macro "jaycautiontag"))
                   (cons "stop" (jay/latex--wrap-macro "jaystoptag"))
                   (cons "label" (jay/latex--wrap-macro "jaylabel"))
                   (cons "src" (jay/latex--wrap-macro "jaysrc"))))
    (setf (alist-get (car m) org-export-global-macros nil nil #'string=) (cdr m))))

;;; A paragraph that is nothing but bold text is standing in for a heading.
;;; Wrap it in \jayleadin so LaTeX keeps it with the paragraph after it
;;; instead of stranding it at the foot of a page.
(defun jay/latex--bold-leadin-filter (tree backend info)
  "Mark bold-only paragraphs in TREE as lead-ins for the elegant-arno-claude-design class."
  (when (and (org-export-derived-backend-p backend 'latex)
             (equal (plist-get info :latex-class) "elegant-arno-claude-design"))
    (org-element-map tree 'paragraph
      (lambda (p)
        (let ((kids (seq-remove (lambda (k) (and (stringp k) (string-blank-p k)))
                                (org-element-contents p))))
          (when (and (eq (org-element-type (org-element-property :parent p)) 'section)
                     (= (length kids) 1)
                     (eq (org-element-type (car kids)) 'bold))
            (org-element-set-contents
             p
             (org-element-create 'export-snippet '(:back-end "latex" :value "\\jayleadin{"))
             (car kids)
             (org-element-create 'export-snippet '(:back-end "latex" :value "}"))))))
      info))
  tree)

(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-parse-tree-functions #'jay/latex--bold-leadin-filter))
