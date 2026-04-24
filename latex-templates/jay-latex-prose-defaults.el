;;; jay-latex-prose-defaults.el --- Shared Org->LaTeX prose defaults -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defconst jay/latex-prose-defaults
  (mapconcat #'identity
             '("% Shared prose typography defaults"
               "\\IfFileExists{nowidow.sty}{%"
               "  \\usepackage[all]{nowidow}"
               "}{%"
               "  \\widowpenalty=10000"
               "  \\clubpenalty=10000"
               "}"
               "\\emergencystretch=3em"
               "% Prevent ugly short hyphen tails on the next line."
               "\\righthyphenmin=5")
             "\n")
  "Shared LaTeX defaults for prose-oriented document classes.")

(defconst jay/latex-prose-default-legacy-blocks
  (list
   (string-join
    '("% Shared prose typography defaults"
      "\\IfFileExists{nowidow.sty}{%"
      "  \\usepackage[all]{nowidow}"
      "}{%"
      "  \\widowpenalty=10000"
      "  \\clubpenalty=10000"
      "}"
      "\\emergencystretch=3em"
      "% Discourage hyphenation; runts prevented by enforcing a 4-char right minimum."
      "\\hyphenpenalty=10000"
      "\\righthyphenmin=4"
      "\\setlist{before=\\RaggedRight}")
    "\n")
   (string-join
    '("% Shared prose typography defaults"
      "\\IfFileExists{nowidow.sty}{%"
      "  \\usepackage[all]{nowidow}"
      "}{%"
      "  \\widowpenalty=10000"
      "  \\clubpenalty=10000"
      "}"
      "\\emergencystretch=3em"
      "% Strongly penalize hyphenation on the penultimate line so a hyphenated word never strands its tail alone on the last line of a paragraph."
      "\\finalhyphendemerits=1000000"
      "% Ragged-right inside lists so bullets never hyphenate."
      "\\setlist{before=\\RaggedRight}")
    "\n")
   (string-join
    '("% Shared prose typography defaults"
      "\\IfFileExists{nowidow.sty}{%"
      "  \\usepackage[all]{nowidow}"
      "}{%"
      "  \\widowpenalty=10000"
      "  \\clubpenalty=10000"
      "}"
      "\\emergencystretch=3em"
      "% Prevent ugly short hyphen tails on the next line."
      "\\righthyphenmin=5")
    "\n")
   (string-join
    '("% Shared prose typography defaults"
      "\\IfFileExists{nowidow.sty}{%"
      "  \\usepackage[all]{nowidow}"
      "}{%"
      "  \\widowpenalty=10000"
      "  \\clubpenalty=10000"
      "}"
      "\\emergencystretch=3em"
      "% Strongly penalize hyphenation on the penultimate line so a hyphenated word never strands its tail alone on the last line of a paragraph."
      "\\finalhyphendemerits=1000000")
    "\n")
   (string-join
    '("% Shared prose typography defaults"
      "\\IfFileExists{nowidow.sty}{%"
      "  \\usepackage[all]{nowidow}"
      "}{%"
      "  \\widowpenalty=10000"
      "  \\clubpenalty=10000"
      "}"
      "\\emergencystretch=3em")
    "\n"))
  "Exact legacy shared prose-default blocks to strip before reinserting.")

(defun jay/latex--strip-prose-defaults (body)
  "Remove legacy widow/orphan controls and shared-block remnants from BODY."
  (let ((result body))
    (dolist (block jay/latex-prose-default-legacy-blocks result)
      (setq result
            (replace-regexp-in-string
             (concat "\n?" (regexp-quote block) "\n?")
             "\n"
             result
             t
             t)))))

(defun jay/latex-apply-prose-defaults (class-name)
  "Normalize shared prose defaults for CLASS-NAME inside `org-latex-classes'."
  (let (found normalized)
    (dolist (entry org-latex-classes)
      (if (equal (car entry) class-name)
          (unless found
            (setq found t)
            (let ((updated (copy-sequence entry)))
              (setcar (cdr updated)
                      (concat (string-trim-right
                               (jay/latex--strip-prose-defaults (nth 1 updated)))
                              "\n\n"
                              jay/latex-prose-defaults
                              "\n"))
              (push updated normalized)))
        (push entry normalized)))
    (setq org-latex-classes (nreverse normalized))))

(provide 'jay-latex-prose-defaults)
;;; jay-latex-prose-defaults.el ends here
