;;; jay-latex-table-tabularray.el --- Shared Org->LaTeX tabularray helpers -*- lexical-binding: t; -*-

(defvar jay/latex-tabularray-enabled-classes nil
  "Org LaTeX classes where plain Org tables should export as tabularray.")

(defun jay/latex-register-tabularray-class (class)
  "Enable tabularray conversion for Org LaTeX CLASS."
  (add-to-list 'jay/latex-tabularray-enabled-classes class))

(defun jay/latex--tabularray-wrap-disabled-p (table)
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

(defun jay/latex--tabularray-colspec (align)
  "Build a tabularray colspec from simple ALIGN strings."
  (let ((clean (replace-regexp-in-string "[[:space:]]" "" align)))
    (when (string-match-p "\\`[|lcrLCRYZWX]+\\'" clean)
      (let (parts)
        (dolist (ch (string-to-list clean))
          (pcase ch
            (?| nil)
            ((or ?c ?C ?Z) (push "X[c]" parts))
            ((or ?r ?R ?W) (push "X[r]" parts))
            ((or ?l ?L ?Y ?X) (push "X[l]" parts))))
        (when parts
          (mapconcat #'identity (nreverse parts) " "))))))

(defun jay/latex--tabularray-convert-tabular (text)
  "Convert tabular environments in TEXT into tabularray tblr."
  (if (string-match "\\\\begin{tabular}\\(\\[[^]]*\\]\\)?{\\([^}\\n]+\\)}" text)
      (let* ((align (match-string 2 text))
             (colspec (jay/latex--tabularray-colspec align)))
        (if colspec
            (let* ((new-begin
                    (format "\\begin{tblr}{width=\\linewidth,colspec={%s}}" colspec))
                   (updated (concat (substring text 0 (match-beginning 0))
                                    new-begin
                                    (substring text (match-end 0)))))
              (if (string-match "\\\\end{tabular}" updated)
                  (concat (substring updated 0 (match-beginning 0))
                          "\\end{tblr}"
                          (substring updated (match-end 0)))
                updated))
          text))
    text))

(defun jay/latex--tabularray-convert-tabularx (text)
  "Convert tabularx environments in TEXT into tabularray tblr."
  (if (string-match "\\\\begin{tabularx}{[^}\\n]+}{\\([^}\\n]+\\)}" text)
      (let* ((align (match-string 1 text))
             (colspec (jay/latex--tabularray-colspec align)))
        (if colspec
            (let* ((new-begin
                    (format "\\begin{tblr}{width=\\linewidth,colspec={%s}}" colspec))
                   (updated (concat (substring text 0 (match-beginning 0))
                                    new-begin
                                    (substring text (match-end 0)))))
              (if (string-match "\\\\end{tabularx}" updated)
                  (concat (substring updated 0 (match-beginning 0))
                          "\\end{tblr}"
                          (substring updated (match-end 0)))
                updated))
          text))
    text))

(defun jay/latex--tabularray-convert-longtable (text)
  "Convert longtable environments in TEXT into tabularray longtblr."
  (if (string-match "\\\\begin{longtable}\\(\\[[^]]*\\]\\)?{\\([^}\\n]+\\)}" text)
      (let* ((align (match-string 2 text))
             (colspec (jay/latex--tabularray-colspec align)))
        (if colspec
            (let* ((new-begin
                    (format "\\begin{longtblr}{width=\\linewidth,colspec={%s}}" colspec))
                   (updated (concat (substring text 0 (match-beginning 0))
                                    new-begin
                                    (substring text (match-end 0)))))
              (if (string-match "\\\\end{longtable}" updated)
                  (concat (substring updated 0 (match-beginning 0))
                          "\\end{longtblr}"
                          (substring updated (match-end 0)))
                updated))
          text))
    text))

(defun jay/latex--convert-to-tabularray (text)
  "Convert supported table environments in TEXT into tabularray variants."
  (let ((updated text))
    (setq updated (jay/latex--tabularray-convert-longtable updated))
    (setq updated (jay/latex--tabularray-convert-tabularx updated))
    (setq updated (jay/latex--tabularray-convert-tabular updated))
    updated))

(defun jay/latex-tabularray-org-tables-advice (orig-fun table contents info)
  "Convert Org LaTeX table output into tabularray for registered classes."
  (let* ((rendered (funcall orig-fun table contents info))
         (class (plist-get info :latex-class)))
    (if (and class
             (member class jay/latex-tabularray-enabled-classes)
             (not (jay/latex--tabularray-wrap-disabled-p table)))
        (jay/latex--convert-to-tabularray rendered)
      rendered)))

(with-eval-after-load 'ox-latex
  (unless (advice-member-p #'jay/latex-tabularray-org-tables-advice 'org-latex-table)
    (advice-add 'org-latex-table :around #'jay/latex-tabularray-org-tables-advice)))

(provide 'jay-latex-table-tabularray)
