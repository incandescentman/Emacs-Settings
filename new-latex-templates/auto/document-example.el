(TeX-add-style-hook
 "document-example"
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
    "textit"
    "BackgroundPic"
    "maketitle")
   (LaTeX-add-labels
    "sec-1"
    "sec-1-1"
    "sec-1-2"
    "sec-1-3"
    "sec-1-4"
    "sec-1-5"
    "sec-2"
    "sec-2-1"
    "sec-2-2"
    "sec-2-3"
    "sec-2-4"
    "sec-2-5"
    "sec-3"
    "sec-3-1"
    "sec-3-2"
    "sec-3-2-1"
    "sec-3-2-2"
    "sec-3-2-3"
    "sec-3-2-4"
    "sec-3-2-5"
    "sec-3-3"
    "sec-3-3-1"
    "sec-3-3-2"
    "sec-3-3-3"
    "sec-4"
    "sec-4-1"
    "sec-4-2"
    "sec-4-3"
    "sec-4-4"
    "sec-4-5"
    "sec-4-6"
    "sec-4-6-1"
    "sec-4-6-2"
    "sec-4-6-3"
    "sec-4-6-4"
    "sec-4-7"
    "sec-4-7-1"
    "sec-4-7-1-1"
    "sec-4-7-1-2"
    "sec-4-7-1-3"
    "sec-5"
    "sec-5-1"
    "sec-5-2"
    "sec-5-3"
    "sec-5-3-1")))

