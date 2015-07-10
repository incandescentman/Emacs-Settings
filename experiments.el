

(defun erika-send-mail ()
      "email subtree and HTMLize"
      (interactive)
(email-heading)
(org-mime-htmlize)
) 

 
(defun email-heading-to-erika ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive)
  ; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (org-mark-subtree)
    (let ((content (buffer-substring (point) (mark)))
	  (TO (org-entry-get (point) "TO" t))
	  (CC (org-entry-get (point) "CC" t))
	  (BCC (org-entry-get (point) "BCC" t))
	  (SUBJECT (nth 4 (org-heading-components)))
	  (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
	  (continue nil)
	  (switch-function nil)
	  (yank-action nil)
	  (send-actions '((email-send-action . nil)))
	  (return-action '(email-heading-return)))
      
      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
      (message-goto-body)
      (insert content)
      (when CC
	(message-goto-cc)
	(insert CC))
      (when BCC
	(message-goto-bcc)
	(insert BCC))
      (if TO
	  (message-goto-body)
	(message-goto-to))))) 



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
