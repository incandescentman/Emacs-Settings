

;; leave an empty line between folded subtrees
(setq org-cycle-separator-lines 1)




(define-skeleton my-something "Docstring." nil
  "beginning" _ "end") 

(define-skeleton my-beginl "Docstring." nil
"#+BEGIN_SRC emacs-lisp 
" _ " 
#+END_SRC")

(define-skeleton my-begine "Docstring." nil
"#+BEGIN_EXAMPLE
" _ " 
#+END_EXAMPLE")


(define-skeleton my-bq "Docstring." nil
"#+BEGIN_QUOTE
" _ " 
#+END_QUOTE
Source:")



(define-skeleton my-beginverse "Docstring." nil
"#+BEGIN_VERSE
"_" 
#+END_VERSE")


(define-skeleton my-org-slide "Docstring." nil
  "* " _ "                                      :slide:")
