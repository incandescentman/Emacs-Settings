;;; jay-latex-poetry-blocks.el --- Treat src verse/poetry as verse blocks  -*- lexical-binding: t; -*-

(require 'subr-x)

(defun jay/org-latex--poetry-language-p (language)
  "Return non-nil when LANGUAGE should export as a verse block."
  (member (downcase (or language "")) '("verse" "poetry")))

(defun jay/org-latex-rewrite-poetry-src-blocks (backend)
  "Rewrite src verse/poetry blocks to VERSE blocks for LaTeX BACKEND exports."
  (when (org-export-derived-backend-p backend 'latex)
    (let (replacements)
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (src)
          (when (jay/org-latex--poetry-language-p
                 (org-element-property :language src))
            (let* ((begin (org-element-property :begin src))
                   (end (org-element-property :end src))
                   (value (org-element-property :value src))
                   (body (if (and value (not (string-suffix-p "\n" value)))
                             (concat value "\n")
                           (or value ""))))
              (push (list begin
                          end
                          (concat "#+BEGIN_VERSE\n"
                                  body
                                  "#+END_VERSE\n"))
                    replacements)))))
      ;; Apply edits from end to start so earlier buffer positions stay valid.
      (dolist (replacement (sort replacements (lambda (a b) (> (car a) (car b)))))
        (goto-char (nth 0 replacement))
        (delete-region (nth 0 replacement) (nth 1 replacement))
        (insert (nth 2 replacement))))))

(with-eval-after-load 'ox
  (add-hook 'org-export-before-parsing-functions
            #'jay/org-latex-rewrite-poetry-src-blocks))

(provide 'jay-latex-poetry-blocks)
;;; jay-latex-poetry-blocks.el ends here
