
    (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))

(add-to-list 'load-path "~/elisp/org/contrib/lisp" t)

(require 'ox-koma-letter)

(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t) t))

(eval-after-load 'ox-koma-letter
  '(progn
     (add-to-list 'org-latex-classes
                  '("my-letter"
                    "\\documentclass\{scrlttr2\}
     \\usepackage[english]{babel}
     \\setkomavar{frombank}{(1234)\\,567\\,890}
     \[DEFAULT-PACKAGES]
     \[PACKAGES]
     \[EXTRA]"))

     (setq org-koma-letter-default-class "my-letter")))
