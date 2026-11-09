;;; fiverr-koma.el --- KOMA letter preset -*- lexical-binding: nil; -*-
(eval-after-load 'ox '(require 'ox-koma-letter))
(eval-after-load 'ox-latex '(add-to-list 'org-latex-packages-alist '("english" "babel" t) t))


(eval-after-load 'ox '(require 'ox-koma-letter))
(eval-after-load 'ox-latex '(add-to-list 'org-latex-packages-alist '("english" "babel" t) t))

(eval-after-load 'ox-koma-letter
  '(progn
     (add-to-list 'org-latex-classes
                  '("my-letter"
                    "\\documentclass[foldmarks=false,refline=dateleft,fromrule=afteraddress,enlargefirstpage=on,fontsize=10pt,fromalign=center,subject=left,parskip=full]\{scrlttr2\}
     \\usepackage[english]{babel}
     \\nonfrenchspacing
     \\setkomafont{backaddress}{\\normalsize \\usekomafont{fromaddress}}
     \\setkomafont{fromrule}{\\vspace{0.2in}}
     \\setkomavar{placeseparator}{\\\\}
     \[DEFAULT-PACKAGES]
     \[PACKAGES]
     \\usepackage{fontspec}
     \\setmainfont{Adobe Garamond Pro}
     \[EXTRA]
     \\KOMAoption{backaddress}{false}"))

     (setq org-koma-letter-default-class "my-letter")))

