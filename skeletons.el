(define-skeleton my-orgfootnote "Docstring." nil
  "[fn:: " _ "] ")


(define-skeleton my-org-slide-notes "Docstring." nil
  "* " _ "                               :slide:
** Notes                                                              :notes:
")


(define-skeleton slidy-image "Docstring." nil
  "<figure >
<img src='"_"'>
<figcaption></figcaption>
</figure>") 






(define-skeleton my-something "Docstring." nil
  "beginning" _ "end") 

(define-skeleton my-beginl "Docstring." nil
"#+BEGIN_SRC emacs-lisp 
" _ " \n#+END_SRC")

(define-skeleton my-begine "Docstring." nil
"#+BEGIN_EXAMPLE
" _ " 
#+END_EXAMPLE")


(define-skeleton my-bq "Docstring." nil
"#+BEGIN_QUOTE
" _ "
#+END_QUOTE
Source: ")



(define-skeleton my-beginverse "Docstring." nil
"#+BEGIN_VERSE
"_" 
#+END_VERSE")


(define-skeleton my-org-slide "Docstring." nil
  "* " _ "                                      :slide:")


