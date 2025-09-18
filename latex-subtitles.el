(defun my/org-latex-process-subtitles (tree backend info)
  "Process ~ lines as subtitles in LaTeX export."
  (when (org-export-derived-backend-p backend 'latex)
    (org-element-map tree 'headline
      (lambda (headline)
        (let ((contents (org-element-contents headline)))
          (when contents
            (let ((first-elem (car contents)))
              (when (and (eq (org-element-type first-elem) 'paragraph)
                         (string-match "^~ \\(.+\\)"
                                       (org-element-interpret-data first-elem)))
                (let ((subtitle-text (match-string 1
                                                   (org-element-interpret-data first-elem))))
                  (org-element-set-contents first-elem
                                            (concat "\\begin{subtitle}\n"
                                                    subtitle-text
                                                    "\n\\end{subtitle}")))))))
        nil)))
  tree)


(defun my-org-latex-subtitle-filter (contents backend info)
  "Convert lines starting with ~ to subtitle environment in LaTeX export."
  (when (eq backend 'latex)
    (replace-regexp-in-string
     "^\\s-*~\\s-*\\(.+\\)$"
     "\\\\begin{subtitle}\n\\1\n\\\\end{subtitle}"
     contents)))

(add-to-list 'org-export-filter-final-output-functions
             'my-org-latex-subtitle-filter)


(add-to-list 'org-export-filter-parse-tree-functions
             'my/org-latex-process-subtitles)
