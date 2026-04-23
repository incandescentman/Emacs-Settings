;;; jay-latex-prose-defaults.el --- Shared Org->LaTeX prose defaults -*- lexical-binding: t; -*-

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
               "% Strongly penalize hyphenation on the penultimate line so a hyphenated word never strands its tail alone on the last line of a paragraph."
               "\\finalhyphendemerits=1000000"
               "% Ragged-right inside lists so bullets never hyphenate."
               "\\setlist{before=\\RaggedRight}")
             "\n")
  "Shared LaTeX defaults for prose-oriented document classes.")

(defun jay/latex--strip-prose-defaults (body)
  "Remove legacy widow/orphan controls and shared-block remnants from BODY."
  (let ((clean body))
    (dolist (pattern '("% Shared prose typography defaults\n?"
                       "\\\\IfFileExists{nowidow\\.sty}{%\n?"
                       "  \\\\usepackage\\[all\\]{nowidow}\n?"
                       "}{%\n?"
                       "  \\\\widowpenalty=10000\n?"
                       "  \\\\clubpenalty=10000\n?"
                       "}\n?"
                       "\\\\usepackage\\[all\\]{nowidow}\n?"
                       "\\\\widowpenalty=10000\n?"
                       "\\\\clubpenalty=10000\n?"
                       "\\\\emergencystretch=10pt\n?"
                       "\\\\emergencystretch=3em\n?"
                       "% Discourage hyphenation; runts prevented by enforcing a 4-char right minimum\\.\n?"
                       "\\\\hyphenpenalty=10000\n?"
                       "\\\\righthyphenmin=4\n?"
                       "% Strongly penalize hyphenation on the penultimate line so a hyphenated word never strands its tail alone on the last line of a paragraph\\.\n?"
                       "\\\\finalhyphendemerits=1000000\n?"
                       "% Ragged-right inside lists so bullets never hyphenate\\.\n?"
                       "\\\\setlist{before=\\\\RaggedRight}\n?"))
      (setq clean (replace-regexp-in-string pattern "" clean t t)))
    clean))

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
