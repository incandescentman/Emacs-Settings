;;; socratic-invoice.el --- Socratic AI invoice class -*- lexical-binding: t; -*-

;; Use #+LATEX_CLASS: socratic-invoice and XeLaTeX.
;; Service table: :environment tabularx :width \linewidth :align Xr :booktabs t
;; Mark total cells bold in the Org source; never infer totals from row position.
(require 'ox-latex)

(defgroup socratic-invoice nil
  "Socratic AI invoice export." :group 'org-export-latex)

(defcustom socratic-invoice-logo-file
  (expand-file-name "assets/socratic-ai-logo-horizontal-print.png"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Horizontal logo. Reload this template after customizing."
  :type 'file :group 'socratic-invoice)

(let ((preamble "\\documentclass[12pt,letterpaper]{article}
\\usepackage[letterpaper,includeheadfoot,hmargin=0.85in,top=0.45in,bottom=0.45in,headheight=54pt,headsep=18pt,footskip=24pt]{geometry}
\\usepackage{fontspec}
\\setmainfont{Arno Pro}[Numbers={Lining,Monospaced}]
\\setsansfont{Arno Pro}[Numbers={Lining,Monospaced}]
\\setmonofont{Menlo}[Scale=MatchLowercase]
\\usepackage{xcolor}
\\definecolor{socraticink}{HTML}{171715}
\\definecolor{socraticpink}{HTML}{FBB6CE}
\\color{socraticink}
\\usepackage{graphicx}
\\usepackage{array,tabularx,longtable,booktabs}
\\usepackage{etoolbox}
\\usepackage{enumitem}
\\usepackage{titlesec}
\\usepackage{fancyhdr,lastpage}
\\usepackage{hyperref}
\\hypersetup{colorlinks=true,allcolors=socraticink}
\\urlstyle{same}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{5pt}
\\setlength{\\tabcolsep}{7pt}
\\renewcommand{\\arraystretch}{1.15}
\\newcolumntype{Y}{>{\\raggedright\\arraybackslash}X}

\\AtBeginEnvironment{tabularx}{\\small}
\\AtBeginEnvironment{longtable}{\\small}
\\setlength{\\LTpre}{0pt}
\\setlength{\\LTpost}{0pt}
\\setlist{nosep,leftmargin=1.4em}
\\setcounter{secnumdepth}{0}
\\newcommand{\\socraticsection}[1]{%
  \\makebox[0pt][l]{\\raisebox{20pt}[0pt][0pt]{\\color{socraticpink}\\rule{25pt}{1.5pt}}}%
  #1}
\\titleformat{\\section}{\\normalfont\\fontsize{20}{24}\\selectfont}{}{0pt}{\\socraticsection}
\\titlespacing*{\\section}{0pt}{16pt}{4pt}
\\titleformat{\\subsection}{\\normalfont\\fontsize{15}{18}\\selectfont}{}{0pt}{}
\\titlespacing*{\\subsection}{0pt}{9pt}{3pt}
\\titleformat{\\subsubsection}{\\normalfont\\itshape\\fontsize{12}{15}\\selectfont}{}{0pt}{}
\\newcommand{\\letterheadlogo}[1]{\\def\\socraticinvoicelogo{#1}}
\\letterheadlogo{LOGOPATH}
\\newcommand{\\invoicecontact}{\\href{mailto:jay@socraticai.co}{jay@socraticai.co}\\enspace\\textperiodcentered\\enspace\\href{https://socraticai.co}{socraticai.co}}
\\pagestyle{fancy}
\\fancyhf{}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}
\\fancyhead[L]{\\includegraphics[width=2.75in,height=48pt,keepaspectratio]{\\socraticinvoicelogo}}
\\fancyfoot[L]{\\footnotesize\\invoicecontact}
\\fancyfoot[R]{\\footnotesize Page \\thepage\\ of \\pageref*{LastPage}}
\\renewcommand{\\maketitle}{}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"))
  (setq preamble (replace-regexp-in-string "LOGOPATH"
                   (expand-file-name socratic-invoice-logo-file) preamble t t))
  (setf (alist-get "socratic-invoice" org-latex-classes nil nil #'equal)
        (list preamble
              (cons "\\section{%s}" "\\section*{%s}")
              (cons "\\subsection{%s}" "\\subsection*{%s}")
              (cons "\\subsubsection{%s}" "\\subsubsection*{%s}")
              (cons "\\paragraph{%s}" "\\paragraph*{%s}")
              (cons "\\subparagraph{%s}" "\\subparagraph*{%s}")
              )))

(let ((load-path (cons (file-name-directory (or load-file-name buffer-file-name)) load-path)))
  (require 'jay-latex-prose-defaults)
  (jay/latex-apply-prose-defaults "socratic-invoice"))

(provide 'socratic-invoice)
