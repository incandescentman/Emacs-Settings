(TeX-add-style-hook
 "document-example"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "includeheadfoot" "margin=1.0in" "hmargin=1.0in" "vmargin=0.5in") ("ucs" "mathletters") ("inputenc" "utf8x") ("ulem" "normalem") ("titlesec" "science") ("hyperref" "breaklinks=true" "linktocpage" "xetex")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
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
    "sec:orgheadline6"
    "sec:orgheadline1"
    "sec:orgheadline2"
    "sec:orgheadline3"
    "sec:orgheadline4"
    "sec:orgheadline5"
    "sec:orgheadline12"
    "sec:orgheadline7"
    "sec:orgheadline8"
    "sec:orgheadline9"
    "sec:orgheadline10"
    "sec:orgheadline11"
    "sec:orgheadline24"
    "sec:orgheadline13"
    "sec:orgheadline19"
    "sec:orgheadline14"
    "sec:orgheadline15"
    "sec:orgheadline16"
    "sec:orgheadline17"
    "sec:orgheadline18"
    "sec:orgheadline23"
    "sec:orgheadline20"
    "sec:orgheadline21"
    "sec:orgheadline22"
    "sec:orgheadline40"
    "sec:orgheadline25"
    "sec:orgheadline26"
    "sec:orgheadline27"
    "sec:orgheadline28"
    "sec:orgheadline29"
    "sec:orgheadline34"
    "sec:orgheadline30"
    "sec:orgheadline31"
    "sec:orgheadline32"
    "sec:orgheadline33"
    "sec:orgheadline39"
    "sec:orgheadline38"
    "sec:orgheadline35"
    "sec:orgheadline36"
    "sec:orgheadline37"
    "sec:orgheadline45"
    "sec:orgheadline41"
    "sec:orgheadline42"
    "sec:orgheadline44"
    "sec:orgheadline43")))

