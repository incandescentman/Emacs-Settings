;;; jay-latex-table-wrap.el --- Shared Org->LaTeX table wrapping helpers -*- lexical-binding: t; -*-

(defvar jay/latex-wrap-enabled-classes nil
  "Org LaTeX classes where plain Org tables should auto-wrap.")

(defun jay/latex-register-wrap-class (class)
  "Enable table auto-wrapping for Org LaTeX CLASS."
  (add-to-list 'jay/latex-wrap-enabled-classes class))

(defun jay/latex--tabular-align-to-tabularx (align)
  "Map simple ALIGN string to left-aligned wrapping tabularx columns."
  (apply #'string
         (mapcar (lambda (ch)
                   (pcase ch
                     (?l ?Y)
                     (?c ?Y)
                     (?r ?Y)
                     (_ ch)))
                 (string-to-list align))))

(defun jay/latex--booktabsify-tabularx (text)
  "Apply light booktabs rules to a converted tabularx table string."
  (if (string-match-p "\\\\toprule\\|\\\\midrule\\|\\\\bottomrule" text)
      text
    (let ((updated text))
      (when (string-match "\\\\begin{tabularx}[^\n]*\n" updated)
        (setq updated
              (replace-match
               (concat (match-string 0 updated) "\\toprule\n")
               t t updated)))
      (setq updated
            (replace-regexp-in-string
             "\\\\hline" "\\midrule" updated t t))
      (when (string-match "\n\\\\end{tabularx}" updated)
        (setq updated
              (replace-match
               "\n\\bottomrule\n\\end{tabularx}"
               t t updated)))
      updated)))

(defun jay/latex--wrap-disabled-p (table)
  "Return non-nil when TABLE has :wrap explicitly disabled."
  (let* ((raw (org-element-property :attr_latex table))
         (joined (downcase
                  (mapconcat #'identity
                             (cond
                              ((null raw) nil)
                              ((listp raw) raw)
                              (t (list raw)))
                             " "))))
    (and (stringp joined)
         (string-match-p
          "\\(?:^\\|[[:space:]]\\):wrap[[:space:]]+\\(?:nil\\|no\\|false\\|off\\|0\\)\\(?:[[:space:]]\\|$\\)"
          joined))))

(defun jay/latex--convert-tabular-to-tabularx (text)
  "Convert simple tabular environments in TEXT into wrapping tabularx."
  (if (string-match
       "\\\\begin{tabular}\\(\\[[^]]*\\]\\)?{\\([^}\n]+\\)}" text)
      (let* ((options (or (match-string 1 text) ""))
             (align (match-string 2 text)))
        (if (string-match-p "\\`[|lcr]+\\'" align)
            (let* ((wrapped-align (jay/latex--tabular-align-to-tabularx align))
                   (new-begin
                    (format "\\begin{tabularx}%s{\\linewidth}{%s}"
                            options wrapped-align))
                   (updated
                    (concat (substring text 0 (match-beginning 0))
                            new-begin
                            (substring text (match-end 0)))))
              (if (string-match "\\\\end{tabular}" updated)
                  (jay/latex--booktabsify-tabularx
                   (concat (substring updated 0 (match-beginning 0))
                           "\\end{tabularx}"
                           (substring updated (match-end 0))))
                updated))
          text))
    text))

(defun jay/latex-wrap-org-tables-advice (orig-fun table contents info)
  "Wrap Org tables for registered classes unless TABLE opts out."
  (let* ((rendered (funcall orig-fun table contents info))
         (class (plist-get info :latex-class)))
    (if (and class
             (member class jay/latex-wrap-enabled-classes)
             (not (jay/latex--wrap-disabled-p table)))
        (jay/latex--convert-tabular-to-tabularx rendered)
      rendered)))

(with-eval-after-load 'ox-latex
  (unless (advice-member-p #'jay/latex-wrap-org-tables-advice 'org-latex-table)
    (advice-add 'org-latex-table :around #'jay/latex-wrap-org-tables-advice)))

(provide 'jay-latex-table-wrap)

