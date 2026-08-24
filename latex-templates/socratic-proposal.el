;;; socratic-proposal.el --- Proposal class derived from elegant-garamond -*- lexical-binding: t; -*-

;; Mirrors the HTML proposal design: cover page, quiet running head,
;; label/value metadata blocks, ruled agenda table, unruled section
;; headings with generous air, footer with name + email + page number.
;;
;; Usage in an org file:
;;   #+LaTeX_CLASS: socratic-proposal
;;   #+LATEX_HEADER: \coversubtitle{A keynote and hands-on workshop for ...}
;;   #+LATEX_HEADER: \preparedfor{Joel Plagenz, Jenny Andreassen Burke, ...\\Environmental Defense Fund}
;;   #+LATEX_HEADER: \preparedby{Jay Dixit, Socratic AI\\August 17, 2026}
;;   #+LATEX_HEADER: \sessiondetails{Tuesday, September 29 (afternoon) ... 80 participants}
;;   #+LATEX_HEADER: \runningheadright{EDF Marketing \& Communications Retreat}
;;   #+LATEX_HEADER: \footerident{Jay Dixit \textperiodcentered\ Socratic AI \textperiodcentered\ jay@socraticai.co}
;;   #+OPTIONS: toc:t num:t
;;
;; The cover is emitted automatically at \begin{document} when \preparedfor
;; is set; \coverwithtoc / \coverwithouttoc control the contents list.

(let* ((template-file (or load-file-name buffer-file-name))
       (template-directory
        (if template-file
            (file-name-directory template-file)
          default-directory))
       (load-path (cons template-directory load-path)))
  (require 'elegant-garamond))
(defconst socratic-proposal--overrides "
% ============================================================
% Socratic AI proposal overrides
% ============================================================
\\usepackage{multicol}
\\usepackage{colortbl}

\\definecolor{jayink}{HTML}{111315}
\\definecolor{jaybody}{HTML}{1A1C1E}
\\definecolor{jaylabelgrey}{HTML}{8A8175}
\\definecolor{jayhairline}{HTML}{D8D1C2}
\\definecolor{jaymuted}{HTML}{6B675F}
\\definecolor{jayagendaaccent}{HTML}{477D8C}

\\color{jaybody}

% Use the installed Helvetica Neue LT Pro Condensed family for labels,
% running furniture, agenda times, and other sans-serif details.
\\ifxetex
  \\setsansfont[
    Path = /Users/jay/Library/Fonts/,
    UprightFont = HelveticaNeueLTPro-Cn,
    BoldFont = HelveticaNeueLTPro-BdCn,
    ItalicFont = HelveticaNeueLTPro-CnO,
    BoldItalicFont = HelveticaNeueLTPro-BdCnO,
    Extension = .otf
  ]{Helvetica Neue LT Pro}
\\fi

% --- Small-caps sans label (the 'PREPARED FOR' style) -------
\\newcommand{\\jaylabel}[1]{%
  {\\sffamily\\fontsize{8.5}{11}\\selectfont\\color{jaylabelgrey}%
   \\addfontfeature{LetterSpace=14}\\MakeUppercase{#1}}}

% --- Section headings: no rule, no color, generous air ------
\\titlespacing*{\\section}{0pt}{30pt}{7pt}
\\titlespacing*{\\subsection}{0pt}{18pt}{4pt}
\\setcounter{secnumdepth}{0}
\\renewcommand{\\mysectiontitle}[1]{%
  \\markboth{#1}{#1}%
  \\raggedright\\normalfont\\bfseries\\color{jayink}\\fontsize{17.5}{21}\\selectfont #1}
\\renewcommand{\\mysubsectiontitle}[1]{%
  \\markright{#1}%
  \\raggedright\\normalfont\\itshape\\color{jayink}\\fontsize{14}{18}\\selectfont #1}
\\titleformat{\\section}{}{}{0em}{\\mysectiontitle}
\\titleformat{\\subsection}{}{}{0em}{\\mysubsectiontitle}

% --- Running head and footer -------------------------------
\\newcommand{\\jayrunheadright}{}
\\newcommand{\\runningheadright}[1]{\\renewcommand{\\jayrunheadright}{#1}}
\\newcommand{\\jayfooterident}{}
\\newcommand{\\footerident}[1]{\\renewcommand{\\jayfooterident}{#1}}
\\newcommand{\\jayrunfont}{\\sffamily\\fontsize{8.5}{11}\\selectfont\\color{jaylabelgrey}}
\\fancyhf{}
\\renewcommand{\\headrulewidth}{0.4pt}
\\renewcommand{\\footrulewidth}{0pt}
\\makeatletter
\\AtBeginDocument{%
  \\lhead{{\\jayrunfont\\addfontfeature{LetterSpace=14}\\MakeUppercase{\\@title}}}%
  \\rhead{{\\jayrunfont\\addfontfeature{LetterSpace=14}\\MakeUppercase{\\jayrunheadright}}}%
  \\lfoot{{\\jayrunfont\\addfontfeature{LetterSpace=10}\\MakeUppercase{\\jayfooterident}}}%
  \\rfoot{{\\jayrunfont\\thepage}}%
}
\\makeatother

% --- Cover page --------------------------------------------
\\newcommand{\\jaycoverlogo}{/Users/jay/Dropbox/github/socratic-video/assets/socratic-ai-horizontal-logo.png}
\\newcommand{\\covertitlelogo}[1]{\\renewcommand{\\jaycoverlogo}{#1}}
\\newcommand{\\jaycoversubtitle}{}
\\newcommand{\\coversubtitle}[1]{\\renewcommand{\\jaycoversubtitle}{#1}}
\\newcommand{\\jaypreparedfor}{}
\\newcommand{\\preparedfor}[1]{\\renewcommand{\\jaypreparedfor}{#1}}
\\newcommand{\\jaypreparedby}{}
\\newcommand{\\preparedby}[1]{\\renewcommand{\\jaypreparedby}{#1}}
\\newcommand{\\jaysessiondetails}{}
\\newcommand{\\sessiondetails}[1]{\\renewcommand{\\jaysessiondetails}{#1}}
\\newcommand{\\jaymetaseparator}{%
  \\nobreak\\hspace{0.6em}%
  \\raisebox{0.45ex}{\\tiny\\textbullet}%
  \\hspace{0.6em}\\nobreak}
\\newif\\ifjaycovertoc \\jaycovertoctrue
\\newcommand{\\coverwithtoc}{\\jaycovertoctrue}
\\newcommand{\\coverwithouttoc}{\\jaycovertocfalse}

\\makeatletter
% The proposal cover lists top-level sections only.  Apply this at document
% start so a broader document-local tocdepth cannot pull subsections into it.
\\AtBeginDocument{\\setcounter{tocdepth}{1}}
\\newcommand{\\jaymakecover}{%
  \\begingroup
  \\thispagestyle{empty}%
  \\setlength{\\parskip}{0pt}%
  \\ifdefempty{\\jaycoverlogo}{}{%
    \\noindent\\includegraphics[width=1.95in,keepaspectratio]{\\jaycoverlogo}\\par}
  \\vspace{1.1in}
  {\\raggedright\\color{jayink}\\fontsize{34}{37}\\selectfont\\@title\\par}
  \\vspace{13pt}
  {\\raggedright\\itshape\\color{jaymuted}\\fontsize{16}{21}\\selectfont\\jaycoversubtitle\\par}
  \\vfill
  \\ifjaycovertoc
    \\jaylabel{Contents}\\par\\vspace{7pt}%
    {\\setlength{\\columnsep}{28pt}%
     \\renewcommand{\\cftsecfont}{\\sffamily\\fontsize{9.5}{17}\\selectfont}%
     \\cftpagenumbersoff{section}%
     \\setlength{\\cftbeforesecskip}{0pt}%
     \\begin{multicols}{2}\\@starttoc{toc}\\end{multicols}}%
    \\vspace{16pt}%
  \\fi
  {\\color{jayink}\\hrule height 0.5pt}
  \\vspace{14pt}
  \\noindent\\begin{tabularx}{\\textwidth}{@{}Y@{\\hspace{28pt}}Y@{}}
    \\jaylabel{Prepared for} & \\jaylabel{Prepared by} \\\\[4pt]
    \\begin{minipage}[t]{\\linewidth}\\raggedright\\jaypreparedfor\\end{minipage} &
    \\begin{minipage}[t]{\\linewidth}\\raggedright\\jaypreparedby\\end{minipage} \\\\
  \\end{tabularx}\\par
  \\vspace{16pt}
  \\ifdefempty{\\jaysessiondetails}{}{%
    \\noindent\\jaylabel{Session}\\par\\vspace{4pt}%
    \\noindent\\begin{minipage}{\\textwidth}%
      \\raggedright\\fontsize{10}{12}\\selectfont\\jaysessiondetails
    \\end{minipage}\\par}
  \\endgroup
  \\clearpage}
\\AtBeginDocument{\\ifdefempty{\\jaypreparedfor}{}{\\jaymakecover}}
% With toc:t, Org emits a second \\tableofcontents followed by \\newpage in
% the document body.  Keep toc:t so section entries are written for the cover,
% but suppress that redundant body command and exactly its page break.
\\newif\\ifjayskipnextnewpage
\\let\\jayoriginalnewpage\\newpage
\\AtBeginDocument{%
  \\renewcommand{\\tableofcontents}{\\global\\jayskipnextnewpagetrue}%
  \\renewcommand{\\newpage}{%
    \\ifjayskipnextnewpage
      \\global\\jayskipnextnewpagefalse
    \\else
      \\jayoriginalnewpage
    \\fi}}
\\makeatother

% --- Ruled agenda table ------------------------------------
% #+ATTR_LATEX: :environment jayagenda   on a two-column org table,
% or hand-written:  \\begin{jayagenda}{ll} 20 MIN & ... \\\\ \\end{jayagenda}
% Org appends its generated column specification (for example, {ll}) to a
% custom table environment, so accept and intentionally ignore that argument.
\\newenvironment{jayagenda}[1]
  {\\vspace{6pt}\\renewcommand{\\arraystretch}{1.5}%
   \\arrayrulecolor{jayhairline}%
   \\tabularx{\\textwidth}{@{}>{\\jaytimecell}m{0.72in}@{\\hspace{18pt}}Y@{}}\\toprule}
  {\\bottomrule\\endtabularx\\vspace{6pt}}
\\newcommand{\\jaytimecell}{\\sffamily\\bfseries\\fontsize{8.5}{11}\\selectfont\\color{jayagendaaccent}}
\\newcommand{\\jayagendarow}[2]{#1 & #2 \\\\ \\midrule}

% --- Pull quote: centered italic, no box -------------------
\\renewenvironment{quote}
  {\\begin{center}\\begin{minipage}{0.78\\textwidth}%
   \\vspace{10pt}\\centering\\itshape\\color{jayink}\\fontsize{15}{21}\\selectfont}
  {\\vspace{10pt}\\end{minipage}\\end{center}}
\\newcommand{\\jaypullquote}[1]{\\begin{quote}#1\\end{quote}}

% --- Bio portrait: left-aligned with wrapped biography text -------
\\newcommand{\\jaybiophoto}[1]{%
  \\begin{wrapfigure}[17]{l}{0.44\\textwidth}%
    \\vspace{-10pt}%
    \\centering
    \\includegraphics[width=\\linewidth,trim=0 160 0 50,clip]{#1}%
  \\end{wrapfigure}}

% --- Investment figure ------------------------------------
\\newcommand{\\jayfee}[1]{%
  \\par\\vspace{6pt}{\\color{jayink}\\bfseries\\fontsize{34}{36}\\selectfont #1\\par}\\vspace{10pt}}

\\setlength{\\parskip}{7pt plus 1pt minus 1pt}
\\setstretch{1.14}

% The base class uses negative list top spacing.  Restore one paragraph's
% breathing room before top-level bullets while keeping its 12pt post-list gap.
\\setlist[itemize,1]{topsep=0pt,after=\\vspace{12pt}}
")

(let* ((base (assoc "elegant-garamond" org-latex-classes))
       (preamble (nth 1 base))
       (spliced (replace-regexp-in-string
                 "\\[NO-DEFAULT-PACKAGES\\]"
                 (concat socratic-proposal--overrides "\n[NO-DEFAULT-PACKAGES]")
                 preamble t t)))
  (setq org-latex-classes
        (cons (append (list "socratic-proposal" spliced) (cddr base))
              (assoc-delete-all "socratic-proposal" org-latex-classes))))

(provide 'socratic-proposal)

;;; socratic-proposal.el ends here
