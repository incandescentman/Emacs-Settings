(TeX-add-style-hook
 "invoice-template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "includeheadfoot" "margin=1.0in" "hmargin=1.0in" "vmargin=0.5in") ("ucs" "mathletters") ("inputenc" "utf8x") ("ulem" "normalem") ("titlesec" "science") ("hyperref" "breaklinks=true" "linktocpage" "xetex")))
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "geometry"
    "float"
    "algorithm"
    "amsmath"
    "ifxetex"
    "fontspec"
    "xltxtra"
    "xunicode"
    "ucs"
    "inputenc"
    "url"
    "paralist"
    "graphicx"
    "tikz"
    "calc"
    "eso-pic"
    "etoolbox"
    "xcolor"
    "textcase"
    "fancyhdr"
    "listings"
    "fancyvrb"
    "enumerate"
    "ctable"
    "tocloft"
    "ulem"
    "enumitem"
    "titlesec"
    "hyperref")
   (TeX-add-symbols
    '("textsubscr" 1)
    '("globalcolor" 1)
    "textbf"
    "textit")
   (LaTeX-add-labels
    "sec:orgheadline1"
    "sec:orgheadline2"
    "sec:orgheadline3")))

