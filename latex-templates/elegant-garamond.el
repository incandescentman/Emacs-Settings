(provide 'elegant-garamond)

(add-to-list 'org-latex-classes
             '("elegant-garamond"
               "
\\documentclass[12pt]{article}
\\usepackage[includeheadfoot, margin=1in]{geometry} % Standard proposal/document margins

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
\\usepackage{array} % Column helpers for wrapping tables
\\usepackage{ragged2e} % Ragged alignment with hyphenation
\\usepackage{tabularx} % Auto-resizing/wrapping table columns
\\usepackage{longtable} % Multi-page tables
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
    UprightFont = HelveticaNeueLTPro-MdCn,
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
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\lhead{\\scshape\\href{\\the\\leftheaderurl}{\\the\\leftheader}}
\\rhead{\\scshape{\\leftmark}}
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
\\forloop{level}{1}{\\value{level} < 11}{%
  \\setlist[itemize,\\arabic{level}]{label=\\raisedtinybullet}
}

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
% For section (40pt heading -> ~28pt subtitle, 70% size)
\\newenvironment{subtitle}%
{% begin code
\\vspace{-0.7\\baselineskip}\\fontsize{28}{32}\\selectfont\\itshape
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
\\titlespacing*{\\section}{0pt}{48pt}{0pt}
\\titlespacing*{\\subsubsection}{0pt}{16pt}{0pt}
\\titlespacing{\\paragraph}{0pt}{0pt}{.5em}[]

\\newcommand{\\mysectiontitle}[1]{%
  \\raggedright\\fontsize{40}{26}\\selectfont #1
}

\\titleformat{\\section}
  {\\normalfont\\ttfamily\\scshape\\color{spacegrey}}
  {}
  {0em}
  {\\mysectiontitle}

\\titleformat*{\\subsection}{\\sffamily\\setstretch{0.1}\\fontsize{24}{36}\\raggedright\\sffamily}
\\titleformat*{\\subsubsection}{\\ttfamily\\bfseries\\scshape\\fontsize{18}{16}\\raggedright\\ttfamily\\color{spacegrey}}
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

(with-eval-after-load 'ox-latex
  (defun jay/elegant-garamond--tabular-align-to-tabularx (align)
    "Map simple ALIGN string to left-aligned wrapping tabularx columns."
    (apply #'string
           (mapcar (lambda (ch)
                     (pcase ch
                       (?l ?Y)
                       (?c ?Y)
                       (?r ?Y)
                       (_ ch)))
                   (string-to-list align))))

  (defun jay/elegant-garamond--booktabsify-tabularx (text)
    "Apply light booktabs rules to a converted tabularx table string."
    (if (string-match-p "\\\\toprule\\|\\\\midrule\\|\\\\bottomrule" text)
        text
      (let ((updated text))
        (when (string-match "\\\\begin{tabularx}[^\n]*\n" updated)
          (setq updated
                (replace-match
                 (concat (match-string 0 updated) "\\toprule\n")
                 t t updated)))
        (setq updated
              (replace-regexp-in-string
               "\\\\hline" "\\midrule" updated t t))
        (when (string-match "\n\\\\end{tabularx}" updated)
          (setq updated
                (replace-match
                 "\n\\bottomrule\n\\end{tabularx}"
                 t t updated)))
        updated)))

  (defun jay/elegant-garamond--wrap-disabled-p (table)
    "Return non-nil when TABLE has :wrap explicitly disabled."
    (let* ((raw (org-element-property :attr_latex table))
           (joined (downcase
                    (mapconcat #'identity
                               (cond
                                ((null raw) nil)
                                ((listp raw) raw)
                                (t (list raw)))
                               " "))))
      (and (stringp joined)
           (string-match-p
            "\\(?:^\\|[[:space:]]\\):wrap[[:space:]]+\\(?:nil\\|no\\|false\\|off\\|0\\)\\(?:[[:space:]]\\|$\\)"
            joined))))

  (defun jay/elegant-garamond--convert-tabular-to-tabularx (text)
    "Convert simple tabular environments in TEXT into wrapping tabularx."
    (if (string-match
         "\\\\begin{tabular}\\(\\[[^]]*\\]\\)?{\\([^}\n]+\\)}" text)
        (let* ((options (or (match-string 1 text) ""))
               (align (match-string 2 text)))
          (if (string-match-p "\\`[|lcr]+\\'" align)
              (let* ((wrapped-align (jay/elegant-garamond--tabular-align-to-tabularx align))
                     (new-begin
                      (format "\\begin{tabularx}%s{\\linewidth}{%s}"
                              options wrapped-align))
                     (updated
                      (concat (substring text 0 (match-beginning 0))
                              new-begin
                              (substring text (match-end 0)))))
                (if (string-match "\\\\end{tabular}" updated)
                    (jay/elegant-garamond--booktabsify-tabularx
                     (concat (substring updated 0 (match-beginning 0))
                             "\\end{tabularx}"
                             (substring updated (match-end 0))))
                  updated))
            text))
      text))

  (defun jay/elegant-garamond-wrap-org-tables-advice (orig-fun table contents info)
    "Wrap Org tables in elegant-garamond class unless :wrap is disabled."
    (let ((rendered (funcall orig-fun table contents info)))
      (if (and (string= (plist-get info :latex-class) "elegant-garamond")
               (not (jay/elegant-garamond--wrap-disabled-p table)))
          (jay/elegant-garamond--convert-tabular-to-tabularx rendered)
        rendered)))

  (unless (advice-member-p
           #'jay/elegant-garamond-wrap-org-tables-advice 'org-latex-table)
    (advice-add 'org-latex-table :around
                #'jay/elegant-garamond-wrap-org-tables-advice)))
