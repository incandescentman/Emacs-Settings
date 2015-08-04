(require 'dictem)

(defun dictem-elisp-variable-documentation (func)
  (let* ((help-buffer "*Help*")
	 (temp-buffer-show-function (lambda (b) ()))
	 )
    (if (boundp func)
	(save-excursion
	  (describe-variable func)
	  (set-buffer help-buffer)
	  (prog1
	      (buffer-substring (point-min) (point-max))
	    (kill-this-buffer help-buffer))
	  )
      nil)))

(defun dictem-elisp-function-documentation (func)
  (let* ((help-buffer "*Help*")
	 (temp-buffer-show-function (lambda (b) ()))
	 )
    (if (functionp func)
	(save-excursion
	  (describe-function func)
	  (set-buffer help-buffer)
	  (prog1
	      (buffer-substring (point-min) (point-max))
	    (kill-this-buffer))
	  )
      nil)))

(defun dictem-elisp-DEFINE (query)
  (let ((sym (intern-soft query))
	(doc nil))
;         (ret (if (and sym (functionp sym))
;		  (documentation sym)
;		nil)))
    (cond ((null sym)
	   (dictem-make-error
	    20 (format "SYmbol '%s is not defined" query)))
	  ((functionp sym)
	   (setq doc (dictem-elisp-function-documentation sym))
	   (if doc doc
	     (dictem-make-error
	      20 (format "'%s is not documented as a function" query))))
	  (t
	   (setq doc (dictem-elisp-function-documentation sym))
	   (if doc doc
	     (dictem-make-error
	      20 (format "'%s is documented as neither function not variable" query)))
	   ))))
;	   (documentation sym))
;          (t (dictem-make-error
;              20 (format "There is no function '%s" query))))))

(defun dictem-string-match-prefix (pattern string)
  (eq 0 (string-match (regexp-quote pattern) string)))
(defun dictem-string-match-substring (pattern string)
  (string-match (regexp-quote pattern) string))
(defun dictem-string-match-suffix (pattern string)
  (string-match (regexp-quote pattern) string)
  (= (length string) (match-end 0)))
(defun dictem-string-match-word (pattern string)
  (string-match (concat "\\b\\(" (regexp-quote pattern) "\\)\\b")
		string))

(defun dictem-elisp-MATCH-UNI (query fun)
  (let ((i    0)
	(l    nil)
;	(re   (regexp-quote query))
	(item nil))
    (while (< i (length obarray))
      (progn
	(setq item (symbol-name (elt obarray i)))
	(if (funcall fun (regexp-quote query) item)
	    (setq l (cons item l)))
	(setq i (+ i 1))))
    l))

(defun dictem-elisp-MATCH (query strategy)
  (let ((l (dictem-elisp-MATCH-UNI
	    query
	    (cond ((string= strategy "exact")
		   (symbol-function 'string=))
		  ((string= strategy "word")
		   (symbol-function 'dictem-string-match-word))
		  ((string= strategy "prefix")
		   (symbol-function 'dictem-string-match-prefix))
		  ((string= strategy "suffix")
		   (symbol-function 'dictem-string-match-suffix))
		  ((string= strategy "substring")
		   (symbol-function 'dictem-string-match-substring))))))
    (if l l
      (dictem-make-error
       20 (format "No matches for %s/%s" query strategy)))))

;(dictem-elisp-MATCH "at"     "word")
;(dictem-elisp-MATCH "file"   "suffix")
;(dictem-elisp-MATCH "dictem" "prefix")
;(dictem-elisp-MATCH "s-s"    "substring")
;(dictem-elisp-MATCH "pike"   "substring")

(provide 'dictem-elisp)
