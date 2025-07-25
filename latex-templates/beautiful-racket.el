(provide 'beautiful-racket)

(add-to-list 'org-latex-classes
             '("beautiful-racket"
               "

\\documentclass[12pt]{article}

% Use the geometry package to customize the page layout, margins, and other aspects of your document's appearance


\\usepackage{wrapfig}


% To have more control over figure placement in your document, use the float package and its [H] option to place figures exactly where you want them in the text:
\\usepackage{float}


% \\usepackage{glossaries}
% \\makeglossaries

\\usepackage{todonotes}
% \\usepackage[asterism]{sectionbreak}
% \\sectionbreakmark{❦}


\\usepackage{wrapfig}
\\usepackage{changepage}
\\usepackage{algorithm}
\\usepackage{pdfpages}
\\usepackage{amsmath}
\\usepackage{ifxetex}
\\usepackage{setspace}
\\ifxetex
\\usepackage{fontspec,xltxtra,xunicode}
\\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
\\setromanfont{Garamond Premier Pro}
 \\setsansfont{TeX Gyre Pagella}
  \\setmonofont{TeX Gyre Heros}

\\else
  \\usepackage[mathletters]{ucs}
  \\usepackage[utf8x]{inputenc}
\\fi
\\usepackage{url}
\\usepackage{paralist}
\\usepackage{tikz}
\\usepackage{calc}
\\usepackage{eso-pic}
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
\\definecolor{elegantblue}{HTML}{4380b9}
\\definecolor{spacegrey}{HTML}{434346}

\\newtoks\\leftheader
\\newtoks\\leftheaderurl
\\newtoks\\coverimage


\\setlength{\\marginparwidth}{2cm} % Set marginparwidth for todonotes

% \\usepackage{magaz}


%Define Bold face
\\DeclareTextFontCommand{\\textbf}{\\sffamily\\bfseries}
\\DeclareTextFontCommand{\\textit}{\\itshape}

\\usepackage{microtype} %  Improve the overall typography and appearance of your document by enabling micro-typographic features, such as character protrusion and font expansion:


\\hyphenpenalty=1000
\\tolerance=1000
\\exhyphenpenalty=100
\\pretolerance=150
\\emergencystretch=10pt



\\pagenumbering{arabic}
\\makeatletter



\\setcounter{secnumdepth}{0}


\\usepackage[font={small,tt}]{caption}

\\usepackage{booktabs} % Customized table styles: If you plan to use tables in your document, you might want to consider customizing their appearance with the `booktabs` package for professional-looking tables

% Use the fancyhdr package to customize the headers and footers of your document for a professional appearance

\\usepackage{fancyhdr}
\\fancyhf{} % sets both header and footer to nothing
\\renewcommand{\\headrulewidth}{0pt}
\\pagestyle{fancy}


\\setlength{\\headheight}{14.49998pt} % Set headheight for fancyhdr

\\fancypagestyle{plain}{%
  \\fancyhf{}%
  % Remove the header rule:
  \\renewcommand{\\headrulewidth}{0pt}%
  % Put page number in the center footer if you want:
  \\cfoot{\\thepage}%
}

\\pagestyle{fancy}




\\renewcommand{\\subsectionmark}[1]{\\markboth{#1}{}} % for subsections in the headers instead of sections
% \\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}
\\lhead{\\scshape\\href{\\the\\leftheaderurl}{\\the\\leftheader}}
\\chead{}
\\rhead{{\\scshape{\\leftmark}}} % Add section heading
% \\rhead{\\@title: {{\\leftmark}}}
\\lfoot{}
\\cfoot{\\thepage} % Add page numbers
\\rfoot{}


% Ensure consistent spacing after periods in your document by using the xspace package:
\\usepackage{xspace}

\\newenvironment{fauxsubtitle}
{

\\Large
\\itshape
}




\\usepackage{listings}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{12pt plus 2pt minus 1pt} % space between paragraphs

% spacing: how to read {12pt plus 4pt minus 2pt}
%           12pt is what we would like the spacing to be
%           plus 4pt means that TeX can stretch it by at most 4pt
%           minus 2pt means that TeX can shrink it by at most 2pt
%       This is one example of the concept of, 'glue', in TeX


\\usepackage{setspace}
\\onehalfspacing
\\setstretch{1.2}

\\usepackage{fancyvrb}
\\usepackage{enumerate}
\\usepackage{ctable}


\\setlength{\\paperwidth}{8.5in}
\\setlength{\\paperheight}{11in}
\\usepackage[includeheadfoot,hmargin=1.5in,top=0.5in,bottom=1in]{geometry}

\\usepackage{tocloft}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}
\\usepackage[normalem]{ulem}


\\makeatletter
\\newcommand{\\globalcolor}[1]{%
  \\color{#1}\\global\\let\\default@color\\current@color
}
\\makeatother

\\newcommand{\\textsubscr}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}

\\usepackage{enumitem}

\\newlist{mylist}{enumerate}{10}


% control line spacing in bulleted list
\\setlist{noitemsep, topsep=-8pt, after=\\vspace{12pt}} % for no spacing between list items
% see: https://tex.stackexchange.com/questions/199118/modifying-whitespace-before-and-after-list-separately-using-enumitem-package
%\\setlist{topsep=0pt} % for a line between list items


\\renewcommand{\\labelitemi}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemiii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemiv}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemv}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemvi}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemvii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemviii}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemix}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}
\\renewcommand{\\labelitemx}{\\raise 0.25ex\\hbox{\\tiny$\\bullet$}}

\\setlistdepth{10}
\\setlist[itemize,1]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,2]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,3]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,4]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,5]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,6]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,7]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,8]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,9]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\setlist[itemize,10]{label=\\raise 0.25ex\\hbox\\tiny$\\bullet$}
\\renewlist{itemize}{itemize}{10}


\\renewcommand{\\contentsname}{Table of Contents}



\\renewcommand{\\descriptionlabel}[1]{%
 {\\hspace{\\labelsep}\\bfseries\\textsc{#1}}}


\\setlist[description]{style=nextline, before=\\vspace{\\baselineskip}}


\\definecolor{azure}{HTML}{f2feff}

\\usepackage{tikz}
\\usetikzlibrary{backgrounds}
\\makeatletter

\\tikzset{%
  fancy quotes/.style={
    text width=\\fq@width pt,
    align=justify,
    inner sep=1em,
    anchor=north west,
    minimum width=\\linewidth,
  },
  fancy quotes width/.initial={.8\\linewidth},
  fancy quotes marks/.style={
    scale=8,
    text=black,
    inner sep=0pt,
  },
  fancy quotes opening/.style={
    fancy quotes marks,
  },
  fancy quotes closing/.style={
    fancy quotes marks,
  },
  fancy quotes background/.style={
    show background rectangle,
    inner frame xsep=0pt,
    background rectangle/.style={
      fill=azure,
      rounded corners,
    },
  }
}

\\newenvironment{fancyquotes}[1][]{%
\\noindent
\\tikzpicture[fancy quotes background]
\\node[fancy quotes opening,anchor=north west] (fq@ul) at (0,0) {``};
\\tikz@scan@one@point\\pgfutil@firstofone(fq@ul.east)
\\pgfmathsetmacro{\\fq@width}{\\linewidth - 2*\\pgf@x}
\\node[fancy quotes,#1] (fq@txt) at (fq@ul.north west) \\bgroup}
{\\egroup;
\\node[overlay,fancy quotes closing,anchor=east] at (fq@txt.south east) {''};
\\endtikzpicture}
\\makeatother

%\\setlength{\\intextsep}{10pt plus 1.0pt minus 2.0pt}


\\newenvironment{indentedsection}
{  {\\adjustwidth{2em}{0pt}}
  {\\endadjustwidth}
}

\\usepackage[HTML]{xcolor}    % For \\definecolor with the HTML model
\\usepackage{tcolorbox}       % For creating and customizing colored boxes
\\usepackage{setspace}        % For \\singlespacing (optional, if not already loaded)

\\usepackage{setspace}
\\usepackage{lipsum}


% Define custom colors if not already defined
\\definecolor{powderblue}{HTML}{f5f7ff}
\\definecolor{stormybluegrey}{HTML}{708090}
\\definecolor{moonrockgrey}{HTML}{5D5D5D}



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
    before skip=1em, % space before the box
    after skip=1em,  % space after the box
    parskip=1em
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


\\usepackage[sc]{titlesec}



\\newlength\\TitleOverhang
\\setlength\\TitleOverhang{1.5cm}

\\newcommand\\Overhang[1]{%
 \\llap{\\makebox[\\TitleOverhang][l]{#1}}%
}


% \\titlespacing{command}{left spacing}{before spacing}{after spacing}[right]
\\titlespacing*{\\section}{0pt}{150pt}{40pt}
%\\titlespacing*{\\subsection}{0pt}{0pt}{-6pt}
\\titlespacing*{\\subsubsection}{0pt}{16pt}{0pt}

\\titlespacing{\\paragraph}{0pt}{0pt}{.5em}[]

\\newcommand{\\mysectiontitle}[1]{%
  \\parbox{10cm}{\\raggedleft\\fontsize{40}{48}\\selectfont #1}
}

\\usepackage{titlesec}
\\usepackage{xcolor}

\\titleformat{\\section}
  {\\normalfont\\ttfamily\\scshape\\color{spacegrey}}
  {}
  {0em}
  {\\thispagestyle{plain}\\hfill\\mysectiontitle}

{\\raggedleft\\parbox[t]{10cm}{\\ttfamily\\scshape\\fontsize{40}{36}\\selectfont\\color{spacegrey}}}


\\titleformat*{\\subsection}{\\sffamily\\setstretch{0.7}\\fontsize{24}{36}\\raggedright\\sffamily}

\\titleformat*{\\subsubsection}{\\ttfamily\\scshape\\fontsize{18}{16}\\raggedright\\ttfamily}\\color{spacegrey}

\\titleformat*{\\paragraph}{\\ttfamily\\bfseries\\fontsize{19}{12}\\raggedright}
\\titleformat*{\\subparagraph}{\\sffamily\\fontsize{16}{12}\\raggedright\\ttfamily\\bfseries}

\\DeclareTextFontCommand{\\nonsection}{\\sffamily\\fontsize{19}{19}\\raggedright\\sffamily\\textlf}

\\DeclareTextFontCommand{\\nonsubsection}{\\sffamily\\itshape\\fontsize{18}\\raggedright\\sffamily}

\\DeclareTextFontCommand{\\nonsubsubsection}{\\sffamily\\fontsize{18}\\raggedright\\sffamily}


\\newenvironment{changemargin}[2]{%
\\begin{list}{}{%
\\setlength{\\topsep}{0pt}%
\\setlength{\\leftmargin}{#1}%
\\setlength{\\rightmargin}{#2}%
\\setlength{\\listparindent}{\\parindent}%
\\setlength{\\itemindent}{\\parindent}%
\\setlength{\\parsep}{\\parskip}%
}%
\\item[]}{\\end{list}}

\\newenvironment{tagline}% environment name
{% begin code
  \\vspace{-36pt}
  \\Large
  \\begin{changemargin}{1cm}{0cm} % Adjust these values as you see fit
  \\begin{itshape}%
    \\par\\vspace{\\baselineskip}\\noindent\\ignorespaces
}%
{% end code
  \\end{itshape}\\end{changemargin}\\vspace{24pt}\\ignorespacesafterend % Close changemargin
}


\\newenvironment{fauxtitle}% environment name
{% begin code
%\\vspace{12pt}
\\Large
\\begin{bfseries}%
  \\par\\vspace{\\baselineskip}\\noindent\\ignorespaces
}%
{% end code
  \\end{bfseries}\\vspace{12pt}\\ignorespacesafterend
}



\\newenvironment{fauxcenter}% environment name
{% begin code

\\Large
\\begin{center}

}%
{% end code
\\end{center}\\ignorespacesafterend
}




\\newenvironment{causationstatement}% environment name
{% begin code
\\vspace{-30pt}
\\ttfamily
\\bfseries
\\begin{center}

}%
{% end code
\\end{center}\\ignorespacesafterend
}






\\usepackage[breaklinks=true,linktocpage,xetex]{hyperref}
\\hypersetup{colorlinks, citecolor=elegantblue,filecolor=elegantblue,linkcolor=elegantblue,urlcolor=elegantblue}

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
