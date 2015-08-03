; dictem.el - DICT protocol client (rfc-2229) for [X]Emacs

; This code was initially based on
; dictionary.el written by Torsten Hilbrich <Torsten.Hilbrich@gmx.net>
; but now probably doesn't contain original code.
; Most of the code has been written
; from scratch by Aleksey Cheusov <vle@gmx.net>, 2004-2008.
;
; DictEm is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; DictEm is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
; 02111-1307, USA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NOTE! Documentation is in README file.
;
; Latest information about dictem project and sources
; are available at
;
; http://freshmeat.net/projects/dictem
; http://sourceforge.net/projects/dictem
; http://mova.org/~cheusov/pub/dictem
;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Custom Things        ;;;;;

(defgroup dictem nil
  "Client for accessing the DICT server."
  :tag "DictEm"
  :group 'help
  :group 'hypermedia)

(defgroup dictem-faces nil
  "Face options for dictem DICT client."
  :tag "DictEm faces"
  :group 'dictem
  :group 'faces)

(defcustom dictem-server nil
  "The DICT server"
  :group 'dictem
  :type '(restricted-sexp :match-alternatives (stringp 'nil)))

(defcustom dictem-port 2628
  "The port of the DICT server"
  :group 'dictem
  :type 'number)

(defcustom dictem-client-prog "dict"
  "The command line DICT client.
dictem accesses DICT server through this executable.
dict-1.9.14 or later (or compatible) is strongly recomented."
  :group 'dictem
  :type 'string)

(defcustom dictem-client-prog-args-list nil
  "A list of additional arguments (strings) passed to dict client.
For example '(\"--some-option\")."
  :group 'dictem
  :type  'list)

(defcustom dictem-option-mime nil
  "If `t' the OPTION MIME command (see RFC-2229 for details)
will be sent to the DICT server. i.e. \"dict\" program
will be run with \"-M\" option.
As a result server's response will be prepanded with MIME header
followed by a blank line.
Because of bugs in dict -M (version < 1.10.3) utility,
dict-1.10.3 or later is strongly recommended
"
  :group 'dictem
  :type  'boolean)

(defcustom dictem-default-strategy nil
  "The default search strategy."
  :group 'dictem
  :type  'string)

(defcustom dictem-default-database nil
  "The default database name."
  :group 'dictem
  :type  'string)

(defcustom dictem-user-databases-alist
  nil
  "ALIST of user's \"virtual\"databases.
Valid value looks like this:
'((\"en-ru\" .  (\"mueller7\" \"korolew_en-ru\"))
  ((\"en-en\" . (\"foldoc\" \"gcide\" \"wn\")))
  ((\"gazetteer\" . \"gaz\")))
"
  :group 'dictem
  :type '(alist :key-type string))

(defcustom dictem-exclude-databases
  nil
  "ALIST of regexps for databases
that will not appear in autocompletion list.
"
  :group 'dictem
  :type '(alist :key-type string))

(defcustom dictem-use-user-databases-only
  nil
  "If `t', only user's dictionaries from dictem-user-databases-alist
will be used by dictem-select-database"
  :group 'dictem
  :type 'boolean)

(defcustom dictem-mode-hook
  nil
  "Hook run in dictem mode buffers."
  :group 'dictem
  :type 'hook)

(defcustom dictem-use-existing-buffer
  nil
  "If `t' the `dictem-run' function will not create new *dictem* buffer.
Instead, existing buffer will be erased and used to show results.
"
  :group 'dictem
  :type 'boolean)

(defcustom dictem-empty-initial-input
  nil
  "If `t' the `dictem-read-query' leave initial input empty"
  :group 'dictem
  :type 'boolean)

(defcustom dictem-use-content-history t
  "If not nil and dictem-use-existing-buffer is also not nil,
buffer content and (point) is saved in dictem-content-history variable
when DEFINE hyperlinks are accessed.
It is restored by dictem-last function.
On slow machines it may better to set this variable to nil"
  :group 'dictem)

;;;;;            Faces             ;;;;;

(defface dictem-reference-definition-face
  '((((background light)) (:foreground "blue"))
    (((background dark))  (:foreground "cyan")))

  "The face that is used for displaying a reference to
a phrase in a DEFINE search."
  :group 'dictem-faces)

(defface dictem-reference-m1-face
  '((((background light)) (:foreground "darkgreen"))
    (((background dark))  (:foreground "lightblue")))

  "The face that is used for displaying a reference to
a phrase in a MATCH search."
  :group 'dictem-faces)

(defface dictem-reference-m2-face
  '((((background light)) (:foreground "blue"))
    (((background dark))  (:bold true :foreground "gray")))

  "The face that is used for displaying a reference to
a single word in a MATCH search."
  :group 'dictem-faces)

(defface dictem-reference-dbname-face
  '((((background light)) (:foreground "darkgreen"))
    (((background dark))  (:bold t :foreground "white")))

  "The face that is used for displaying a reference to database"
  :group 'dictem-faces)

(defface dictem-database-description-face
  '((((background light)) (:bold t :foreground "darkblue"))
    (((background dark))  (:bold t :foreground "white")))

  "The face that is used for displaying a database description"
  :group 'dictem-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;           Variables          ;;;;;

(defconst dictem-version "1.0.4"
  "DictEm version information.")

(defvar dictem-strategy-alist
  nil
  "ALIST of search strategies")

(defvar dictem-database-alist
  nil
  "ALIST of databases")

(defvar dictem-strategy-history
  nil
  "List of strategies entered from minibuffer")

(defvar dictem-database-history
  nil
  "List of database names entered from minibuffer")

(defvar dictem-query-history
  nil
  "List of queries entered from minibuffer")

(defvar dictem-last-database
  "*"
  "Last used database name")

(defvar dictem-last-strategy
  "."
  "Last used strategy name")

(defvar dictem-mode-map
  nil
  "Keymap for dictem mode")

(defvar dictem-temp-buffer-name
  "*dict-temp*"
  "Temporary dictem buffer name")

(defvar dictem-current-dbname
  nil
  "This variable keeps a database name of the definition
currently processed
by functions run from dictem-postprocess-each-definition-hook.")

(defvar dictem-error-messages
  nil
  "A list of error messages collected by dictem-run")

(defvar dictem-hyperlinks-alist
  nil
  "ALIST of hyperlinks collected from dictem buffer by
the function dictem-postprocess-collect-hyperlinks
(add this function to the hook dictem-postprocess-definition-hook).
This variable is local to buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-prepand-special-strats (l)
  (cons '(".") l))

(defun dictem-prepand-special-dbs (l)
  (cons '("*") (cons '("!") l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Functions         ;;;;;;

(defmacro save-dictem (&rest funs)
  `(let ((dictem-port                    2628)
	 (dictem-server                  nil)
	 (dictem-database-alist          nil)
	 (dictem-strategy-alist          nil)
	 (dictem-use-user-databases-only nil)
	 (dictem-user-databases-alist    nil)
	 )
     (progn ,@funs)
     ))

(defun dictem-client-text ()
  "Returns a portion of text sent to the server for identifying a client"
  (concat "dictem " dictem-version ", DICT client for emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Functions related to userdb    ;;

(defun dictem-make-userdb (name short-name match define)
  "Make user database object"
  (list name 'dictem-userdb
	short-name match define))

(defun dictem-userdb-p (obj)
  "Returns t if obj is the dictem error object"
  (and obj (listp obj) (cdr obj) (listp (cdr obj))
       (eq (cadr obj) 'dictem-userdb)))

(defun dictem-userdb-member (obj name)
  "Extract member from userdb object by its name"
  (cond ((dictem-userdb-p obj)
	 (nth (cdr (assoc name
			  '(("name"   . 0) ("short-name" . 2)
			    ("match"  . 3) ("define"     . 4))))
	      obj))
	(t (error "Invalid type of argument"))))

(defun dictem-userdb-DEFINE (buffer db query host port)
  (let* ((fun   (dictem-userdb-member db "define"))
	 (name  (dictem-userdb-member db "name"))
	 (sname (dictem-userdb-member db "short-name"))
	 (ret (save-excursion (funcall fun query)))
	 (buf (dictem-get-buffer buffer)))
    (save-excursion
      (set-buffer buf)
      (cond ((dictem-error-p ret)
;	     (insert "From " sname " [" name "]:\n\n"
;		     (dictem-error-message ret) "\n\n")
;	     (insert (dictem-error-message ret) "\n")
	     (insert (dictem-error-message ret) "\n")
	     (dictem-error-status ret))
	    ((null ret)
	     (insert "No matches found" "\n")
	     20)
	    ((listp ret)
	     (dolist (definition ret)
	       (insert "From " sname " [" name "]:\n\n"
		       (dictem-indent-string definition) "\n\n"))
	     0)
	    ((stringp ret)
	     (insert "From " sname " [" name "]:\n\n"
		     (dictem-indent-string ret) "\n\n")
	     0)
	    (t
	     (error "Invalid type of returned value1"))))))

(defun dictem-userdb-MATCH (buffer db query strat host port)
  (let* ((fun   (dictem-userdb-member db "match"))
	 (name  (dictem-userdb-member db "name"))
	 (ret (save-excursion (funcall fun query strat)))
	 (buf (dictem-get-buffer buffer)))
    (save-excursion
      (set-buffer buf)
      (cond ((dictem-error-p ret)
	     (insert (dictem-error-message ret) "\n")
	     (dictem-error-status ret))
	    ((listp ret)
	     (insert (concat name ":\n"))
	     (dolist (match ret); (insert (car db) ":\n" ))
	       (progn
		 (insert "  " match "\n"))
		 )
	     0)
	    (t
	     (error "Invalid type of returned value2"))))))

(defun dictem-userdb-SEARCH (buffer db query strat host port)
  (let* ((funm  (dictem-userdb-member db "match"))
	 (name  (dictem-userdb-member db "name"))
	 (sname (dictem-userdb-member db "short-name"))
	 (sname nil)
	 (ret   (funcall funm query strat))
	 (buf   (dictem-get-buffer buffer)))
    (save-excursion
      (set-buffer buf)
      (cond ((dictem-error-p ret)
	     (insert (dictem-error-message ret) "\n")
	     (dictem-error-status ret))
	    ((listp ret)
	     (dolist (match ret)
	       (dictem-userdb-DEFINE buffer db
				     match host port))
	     0)
	    (t
	     (error "Something strange happened"))
	    ))))

(defun dictem-userdb-SHOW-INFO (buffer db host port)
  (let ((sname (dictem-userdb-member db "short-name"))
	(buf   (dictem-get-buffer buffer)))
    (save-excursion
      (set-buffer buf)
      (cond ((dictem-error-p sname)
	     (insert (dictem-error-message sname) "\n")
	     (dictem-error-status sname))
	    ((stringp sname)
	     (insert sname)
	     0)
	    (t
	     (error "Something strange happened"))
	    ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions related to error object  ;;

(defun dictem-make-error (error_status &optional buffer-or-string)
  "Creates dictem error object"
  (cond
   ((stringp buffer-or-string)
    (list 'dictem-error error_status buffer-or-string))
   ((bufferp buffer-or-string)
    (dictem-make-error
     error_status
     (save-excursion
       (set-buffer buffer-or-string)
       (goto-char (point-min))
       (dictem-get-line)
       )))
   ((eq nil buffer-or-string)
    (list 'dictem-error error_status buffer-or-string))
   (t
    (error "Invalid type of argument"))
   ))

(defun dictem-error-p (OBJECT)
  "Returns t if OBJECT is the dictem error object"
  (and
   (not (null OBJECT))
   (listp OBJECT)
   (eq (car OBJECT) 'dictem-error)
   ))

(defun dictem-error-message (err)
  "Extract error message from dictem error object"
  (cond
   ((dictem-error-p err)
    (nth 2 err))
   (t
    (error "Invalid type of argument"))
   ))

(defun dictem-error-status (err)
  "Extract error status from dictem error object"
  (cond
   ((dictem-error-p err)
    (nth 1 err))
   (t
    (error "Invalid type of argument"))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-collect-matches ()
  ; nreverse, setcar and nconc are used to reduce a number of cons
  (goto-char (point-min))
  (let ((dictem-temp nil))
    (loop
     (let ((line (dictem-get-line)))
       (if (string-match "^[^ ]+:" line)
	   (progn
	     (if (consp dictem-temp)
		 (setcar (cdar dictem-temp)
			 (nreverse (cadar dictem-temp))))
	     (setq
	      dictem-temp
	      (cons
	       (list
		(substring line (match-beginning 0) (- (match-end 0) 1))
		(nreverse 
		 (dictem-tokenize (substring line (match-end 0)))))
	       dictem-temp)))
	 (if (consp dictem-temp)
	     (setcar (cdar dictem-temp)
		     (nconc (nreverse (dictem-tokenize line))
			    (cadar dictem-temp))
		     ))
	 ))
     (if (or (> (forward-line 1) 0)
	     (> (current-column) 0))
	 (return (nreverse dictem-temp)))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-get-buffer (buf)
  (cond
   ((bufferp buf) buf)
   (buf (current-buffer))
   (t (get-buffer-create dictem-temp-buffer-name))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             call-process functions

(defun dictem-local-dict-basic-option (host port option-mime)
  (let ((server-host (if host host (dictem-get-server))))
    (append
     (list "-P" "-" 
	   "--client" (dictem-client-text))
     (if server-host
	 (list "-h" server-host "-p" (dictem-get-port port)))
     (if option-mime '("-M"))
     dictem-client-prog-args-list
     )))

(defun dictem-call-process-SHOW-SERVER (buffer host port)
  (apply 'call-process
	 `(,dictem-client-prog
	   nil
	   ,(dictem-get-buffer buffer)
	   nil
	   ,@(dictem-local-dict-basic-option host port nil)
	   "-I")))

(defun dictem-call-process-SHOW-INFO (buffer db host port)
  (apply 'call-process
	 `(,dictem-client-prog
	   nil
	   ,(dictem-get-buffer buffer)
	   nil
	   ,@(dictem-local-dict-basic-option host port nil)
	   "-i" ,db)))

(defun dictem-call-process-SHOW-STRAT (buffer host port)
  (apply 'call-process
	 `(,dictem-client-prog
	   nil
	   ,(dictem-get-buffer buffer)
	   nil
	   ,@(dictem-local-dict-basic-option host port nil)
	   "-S")))

(defun dictem-call-process-SHOW-DB (buffer host port)
  (apply 'call-process
	 `(,dictem-client-prog
	   nil
	   ,(dictem-get-buffer buffer)
	   nil
	   ,@(dictem-local-dict-basic-option host port nil)
	   "-D")))

(defun dictem-call-process-MATCH (buffer db query strat host port)
  (apply 'call-process
	 `(,dictem-client-prog
	   nil
	   ,(dictem-get-buffer buffer)
	   nil
	   ,@(dictem-local-dict-basic-option host port nil)
	   "-m"
	   "-d" ,(if db db "*")
	   "-s" ,(if strat strat ".")
	   ,query)))

(defun dictem-call-process-DEFINE (buffer db query host port)
  (apply 'call-process
	 `(,dictem-client-prog
	   nil
	   ,(dictem-get-buffer buffer)
	   nil
	   ,@(dictem-local-dict-basic-option host port dictem-option-mime)
	   "-d" ,(if db db "*")
	   ,query)))

(defun dictem-call-process-SEARCH (buffer db query strat host port)
  (apply 'call-process
	 `(,dictem-client-prog
	   nil
	   ,(dictem-get-buffer buffer)
	   nil
	   ,@(dictem-local-dict-basic-option host port dictem-option-mime)
	   "-d" ,(if db db "*")
	   "-s" ,(if strat strat ".")
	   ,query)))

;;;;;        GET Functions         ;;;;;

(defun dictem-get-matches (query &optional database strategy server port)
  "Returns ALIST of matches"
  (let ((exit_status
	 (dictem-call-process-MATCH nil database query strategy server port)
	 ))
    (cond
     ((= exit_status 20) ;20 means "no matches found", See dict(1)
      (kill-buffer dictem-temp-buffer-name)
      nil)
     ((= exit_status 0)
      (progn
	(save-excursion
	  (set-buffer dictem-temp-buffer-name)
	  (let ((matches (dictem-collect-matches)))
	    (kill-buffer dictem-temp-buffer-name)
	    matches))))
     (t
      (let
	  ((err (dictem-make-error exit_status
				   (get-buffer dictem-temp-buffer-name))))
	(kill-buffer dictem-temp-buffer-name)
	err))
     )))

(defun dictem-get-strategies (&optional server port)
  "Obtains strategy ALIST from a DICT server
and returns alist containing strategies and their descriptions"
  (let ((exit_status
	 (dictem-call-process-SHOW-STRAT nil server port)
	 ))
    (cond
     ((= exit_status 0)
      (save-excursion
	(set-buffer dictem-temp-buffer-name)
	(goto-char (point-min))
	(let ((regexp "^ \\([^ ]+\\) +\\(.*\\)$")
	      (l nil))
	  (while (search-forward-regexp regexp nil t)
	    (setq l (cons
		     (list
		      (buffer-substring-no-properties
		       (match-beginning 1) (match-end 1))
		      (buffer-substring-no-properties
		       (match-beginning 2) (match-end 2)))
		     l)))
	  (kill-buffer dictem-temp-buffer-name)
	  l)))
     (t
      (let
	  ((err (dictem-make-error exit_status
				   (get-buffer dictem-temp-buffer-name))))
	(kill-buffer dictem-temp-buffer-name)
	err))
    )))

(defun dictem-get-databases (&optional server port)
  "Obtains database ALIST from a DICT server
and returns alist containing database names and descriptions"
  (let ((exit_status
	 (dictem-call-process-SHOW-DB nil server port)
	 ))
    (cond
     ((= exit_status 0)
      (save-excursion
	(set-buffer dictem-temp-buffer-name)
	(goto-char (point-min))
	(let ((regexp "^ \\([^ ]+\\) +\\(.*\\)$")
	      (l nil))
	  (while (search-forward-regexp regexp nil t)
	    (let ((dbname (buffer-substring-no-properties
			   (match-beginning 1) (match-end 1)))
		  (dbdescr (buffer-substring-no-properties
			    (match-beginning 2) (match-end 2))))
	      (if (not (string= "--exit--" dbname))
		  (setq l (cons (list dbname dbdescr) l)))))
	  (kill-buffer dictem-temp-buffer-name)
	  l)))
     (t
      (let
	  ((err (dictem-make-error exit_status
				   (get-buffer dictem-temp-buffer-name))))
	(kill-buffer dictem-temp-buffer-name)
	err))
     )))

(defun dictem-get-default-strategy (&optional def-strat)
  "Gets the default search strategy"
  (if def-strat
      def-strat
    (if dictem-default-strategy
	dictem-default-strategy
      (if dictem-last-strategy
	  dictem-last-strategy
	"."))))

(defun dictem-extract-dbname (database)
  (cond
   ((consp database) (dictem-extract-dbname (car database)))
   ((stringp database) database)
   (t (error "The database should be either stringp or consp"))
   ))

(defun dictem-get-default-database (&optional def-db)
  "Returns the default database"

  (if def-db
      (dictem-extract-dbname def-db)
    (if dictem-default-database
	(dictem-extract-dbname dictem-default-database)
      (if dictem-last-database
	  (dictem-extract-dbname dictem-last-database)
	"*"))))

;;;;;      Low Level Functions     ;;;;;

(defun dictem-db-should-be-excluded (dbname)
  "Returns t if a dbname should is not interesting for user.
See dictem-exclude-databases variable"
  (let ((ret nil))
    (dolist (re dictem-exclude-databases)
      (if (string-match re dbname)
	  (setq ret t)))
    ret))

(defun dictem-delete-alist-predicate (l pred)
  "makes a copy of l with no items for which (pred item) is true"
  (let ((ret nil))
    (dolist (item l)
      (if (not (funcall pred (car item)))
	  (setq ret (cons item ret))))
    ret))

(defun dictem-get-line ()
  "Replacement for (thing-at-point 'line)"
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line) (point))
     (progn (end-of-line) (point)))))

(defun dictem-list2alist (l)
  (cond
   ((null l) nil)
   (t (cons
       (list (car l) nil)
       (dictem-list2alist (cdr l))))))

(defun dictem-indent-string (str)
  (let ((start 0))
    (while (string-match "\n" str start)
      (progn
	(setq start ( + 2 (match-end 0)))
	(setq str (replace-match "\n  " t t str)))))
  (concat "  " str))

(defun dictem-replace-spaces (str)
  (while (string-match "[ \n][ \n]+" str)
    (setq str (replace-match " " t t str)))
  (if (string-match "^ +" str)
      (setq str (replace-match "" t t str)))
  (if (string-match " +$" str)
      (setq str (replace-match "" t t str)))
  str)

(defun dictem-remove-value-from-alist (l)
  (let ((ret nil))
    (dolist (i l)
      (setq ret (cons (list (car i)) ret)))
    (reverse ret)
    ))
;(defun dictem-remove-value-from-alist (l)
;  (cond
;   ((symbolp l) l)
;   (t (cons (list (caar l))
;	    (dictem-remove-value-from-alist (cdr l))))))

(defun dictem-select (prompt alist default history)
  (let*
      ((completion-ignore-case t)
       (str (completing-read
	     (concat prompt " [" default "]: ")
	     alist nil t nil history default))
       (str-cons (assoc str alist)))
    (cond
     ((and str-cons (consp str-cons) (cdr str-cons))
      str-cons)
     ((and str-cons (consp str-cons))
      (car str-cons))
     (t nil))))

(defun dictem-tokenize (s)
  (if (string-match "\"[^\"]+\"\\|[^ \"]+" s )
;	(substring s (match-beginning 0) (match-end 0))
      (cons (substring s (match-beginning 0) (match-end 0))
	    (dictem-tokenize (substring s (match-end 0))))
    nil))

;(defun dictem-search-forward-regexp-cs (REGEXP &optional BOUND NOERROR COUNT)
;  "Case-sensitive variant for search-forward-regexp"
;  (let ((case-replace nil)
;	(case-fold-search nil))
;    (search-forward-regexp REGEXP BOUND NOERROR COUNT)))

;(defun dictem-replace-match-cs (NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
;  "Case-sensitive variant for replace-match"
;  (let ((case-replace nil)
;	(case-fold-search nil))
;    (replace-match NEWTEXT FIXEDCASE LITERAL STRING SUBEXP)))

(defun dictem-get-port (&optional port)
  (let ((p (if port port dictem-port)))
    (cond
     ((and (stringp p) (string= "" p)) 2628)
     ((null p) 2628)
     ((stringp p) p)
     ((numberp p) (number-to-string p))
     (t (error "The value of dictem-port variable should be \
either a string or a number"))
     )))

(defun dictem-get-server ()
  (cond
   ((and (stringp dictem-server) (string= "" dictem-server)) nil)
   ((stringp dictem-server) dictem-server)
   ((null dictem-server) nil)
   (t (error "The value of dictem-server variable should be \
either a string or a nil"))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Main Functions        ;;;;;

;;;;;; Functions for Initializing ;;;;;;

(defun dictem-initialize-strategies-alist (&optional server port)
  "Obtain strategy ALIST from a DICT server
and sets dictem-strategy-alist variable."
  (interactive)
  (setq dictem-strategy-alist (dictem-get-strategies
			       server
			       (dictem-get-port port))))

(defun dictem-initialize-databases-alist (&optional server port)
  "Obtain database ALIST from a DICT server
and sets dictem-database-alist variable."
  (interactive)
  (setq dictem-database-alist
	(dictem-get-databases server (dictem-get-port port)))
  (if (dictem-error-p dictem-database-alist)
      dictem-database-alist
    (setq dictem-database-alist
	  (dictem-delete-alist-predicate
	   dictem-database-alist
	   'dictem-db-should-be-excluded))))

(defun dictem-initialize ()
  "Initializes dictem, i.e. obtains
a list of available databases and strategiss from DICT server
and makes other tasks."
  (interactive)
  (let ((dbs (dictem-initialize-databases-alist))
	(strats (dictem-initialize-strategies-alist)))
    (if (dictem-error-p dbs)
	dbs strats)))

(defun dictem-reinitialize-err ()
  "Initializes dictem if it is not initialized yet
and run (error ...) if an initialization fails"
  (interactive)
  (if (or (dictem-error-p dictem-database-alist)
	  (null dictem-database-alist))
      (if (dictem-error-p (dictem-initialize))
	  (error (dictem-error-message dictem-database-alist)))))

;;; Functions related to Minibuffer ;;;;

(defun dictem-select-strategy (&optional default-strat)
  "Switches to minibuffer and asks the user
to enter a search strategy."
  (dictem-reinitialize-err)
  (dictem-select
   "strategy"
   (dictem-prepand-special-strats
    (dictem-remove-value-from-alist dictem-strategy-alist))
   (dictem-get-default-strategy default-strat)
   'dictem-strategy-history))

(defun dictem-select-database (spec-dbs user-dbs &optional default-db)
  "Switches to minibuffer and asks user
to enter a database name."
  (dictem-reinitialize-err)
  (let* ((dbs (dictem-remove-value-from-alist dictem-database-alist))
	 (dbs2 (if user-dbs
		   (if dictem-use-user-databases-only
		       dictem-user-databases-alist
		     (append dictem-user-databases-alist dbs)
		     )
		 dbs)))
    (dictem-select
     "db"
     (if spec-dbs (dictem-prepand-special-dbs dbs2) dbs2)
     (dictem-get-default-database default-db)
     'dictem-database-history)))

(defun dictem-read-query (&optional default-query)
  "Switches to minibuffer and asks user to enter a query."
  (if (featurep 'xemacs)
      (read-string
       (concat "query [" default-query "]: ")
       nil 'dictem-query-history default-query)
    (read-string
     (concat "query [" default-query "]: ")
     (if dictem-empty-initial-input nil default-query)
     'dictem-query-history default-query t)))


;;;;;;;;           Hooks        ;;;;;;;;

(defcustom dictem-postprocess-definition-hook
  nil
  "Hook run in dictem mode buffers containing DEFINE result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-definition-separator
	     dictem-postprocess-definition-hyperlinks
	     dictem-postprocess-each-definition
	     dictem-postprocess-definition-remove-header
	     dictem-postprocess-collect-hyperlinks))

(defcustom dictem-postprocess-match-hook
  nil
  "Hook run in dictem mode buffers containing MATCH result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-match))

(defcustom dictem-postprocess-show-info-hook
  nil
  "Hook run in dictem mode buffers containing SHOW INFO result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-definition-hyperlinks
	     dictem-postprocess-collect-hyperlinks))

(defcustom dictem-postprocess-show-server-hook
  nil
  "Hook run in dictem mode buffers containing SHOW SERVER result."
  :group 'dictem
  :type 'hook)

;;;;;;;;    Search Functions     ;;;;;;;

(defun dictem-call-dict-internal (fun databases)
  (let ((exit-status -1))
    (cond
     ((null databases) 0)
     ((stringp databases)
      (dictem-call-dict-internal fun (list databases)))
     ((listp databases)
      (dolist (db databases)
	(let ((ex_st (funcall fun db)))
	  (cond
	   ((= ex_st 0)
	    (setq exit-status 0))
	   (t (if (/= 0 exit-status)
		  (setq exit-status ex_st)))
	   )))
      (if (= exit-status -1) 0 exit-status)
      )
     (t (error "wrong type of argument"))
     )
    ))

;(defun dictem-call-dict-internal (fun databases)
;  (dolist (db databases)
;    (funcall fun db)))
;  (funcall fun databases))

(defun dictem-make-url (host port database cmd_sign query &optional strategy)
  "Returns dict:// URL"
  (concat
   "dict://" host ":"
   (dictem-get-port (if port port "2628"))
   "/" cmd_sign ":" query ":" database
   (if strategy (concat ":" (if strategy strategy ".")))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-base-do-selector (cmd hook &optional database &rest args)
  (let* ((splitted-url nil)
	 (databases    nil)
	 (user-db      (assoc database dictem-user-databases-alist))
	 )
    (goto-char (point-max))
    (cond ((dictem-userdb-p database)
	   (apply 'dictem-base-do-default-server
		  (append (list cmd hook database) args)))

	  ((and database (listp database))
	   (dictem-call-dict-internal
	    `(lambda (db)
	       (apply 'dictem-base-do-selector 
		      (append (list ,cmd hook db) args)))
	    (cdr database))
	   (setq dictem-last-database (car database)))

	  ((and database (stringp database)
		(setq splitted-url (dictem-parse-url database)))
	   (apply 'dictem-base-do-foreign-server
		  (append
		   (list cmd hook
			 (nth 1 splitted-url)
			 (dictem-get-port (nth 2 splitted-url))
			 (nth 3 splitted-url))
		   args)))

	  (user-db
	   (let ((exit_status
		  (apply 'dictem-base-do-selector
			 (append
			  (list cmd hook user-db) args))))
	     (progn
	       (setq dictem-last-database database)
	       exit_status)
	     ))

	  (t
	   (apply 'dictem-base-do-default-server
		  (append (list cmd hook database) args)))
	  )))

(defun dictem-base-do-foreign-server (cmd hook server port database &rest args)
  (let ((dictem-last-database nil)
	(dictem-last-strategy nil))
    (save-dictem (setq dictem-server server)
		 (setq dictem-port   port)
		 (setq database      database)
		 (dictem-initialize)
		 (apply 'dictem-base-do-default-server
			(append (list cmd hook database) args))
		 )))

(defun dictem-base-do-default-server (cmd hook
					  &optional database query strategy)
  (let* ((beg (point))
	 (fun (if (dictem-userdb-p database)
		  (dictem-cmd2userdb cmd)
		(dictem-cmd2function cmd)))

	 (exit_status
	  (save-excursion (apply fun (append (list t)
			     (if database (list database))
			     (if query (list query))
			     (if strategy (list strategy))
			     (list nil) (list nil))))
	  ))

    (cond ((= 0 exit_status)
	   (save-excursion
	     (narrow-to-region beg (point-max))
	     (run-hooks hook)
	     (widen)))
	  ((= 21 exit_status)
	   (save-excursion
	     (narrow-to-region beg (point-max))
	     (run-hooks 'dictem-postprocess-match-hook)
	     (widen)))
	  (t
	   (if (/= beg (point))
	       (setq dictem-error-messages
		     (append
		      (list
		       (dictem-make-url (dictem-get-server)
					(dictem-get-port) database "?" query)
		       (buffer-substring-no-properties beg (point)))
		      dictem-error-messages)))
	   (kill-region beg (point))))

    (if database (setq dictem-last-database database))
    (if strategy (setq dictem-last-strategy strategy))
    exit_status
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-base-search (databases query strategy)
  "MATCH + DEFINE commands"

  (dictem-base-do-selector
   "search"
   'dictem-postprocess-definition-hook
   databases query strategy))

(defun dictem-base-define (databases query c)
  "DEFINE command"

  (dictem-base-do-selector
   "define"
   'dictem-postprocess-definition-hook
   databases query))

(defun dictem-base-match (databases query strategy)
  "MATCH command"

  (dictem-base-do-selector
   "match"
   'dictem-postprocess-match-hook
   databases query strategy))

(defun dictem-base-show-databases (a b c)
  "SHOW DB command"

  (dictem-base-do-selector
   "show-db"
   nil))

(defun dictem-base-show-strategies (a b c)
  "SHOW STRAT command"

  (dictem-base-do-selector
   "show-strat"
   nil))

(defun dictem-base-show-info (databases b c)
  "SHOW INFO command"

  (dictem-base-do-selector
   "show-info"
   'dictem-postprocess-show-info-hook
   databases))

(defun dictem-base-show-server (a b c)
  "SHOW SERVER command"

  (dictem-base-do-selector
   "show-server"
   'dictem-postprocess-show-server-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-get-error-message (exit_status)
  (cond
   ((= exit_status 0) "All is fine")
   ((= exit_status 20) "No matches found")
   ((= exit_status 21) "Approximate matches found")
   ((= exit_status 22) "No databases available")
   ((= exit_status 23) "No strategies available")

   ((= exit_status 30) "Unexpected response code from server")
   ((= exit_status 31) "Server is temporarily unavailable")
   ((= exit_status 32) "Server is shutting down")
   ((= exit_status 33) "Syntax error, command not recognized")
   ((= exit_status 34) "Syntax error, illegal parameters")
   ((= exit_status 35) "Command not implemented")
   ((= exit_status 36) "Command parameter not implemented")
   ((= exit_status 37) "Access denied")
   ((= exit_status 38) "Authentication failed")
   ((= exit_status 39) "Invalid database name")
   ((= exit_status 40) "Invalid strategy name")
   ((= exit_status 41) "Connection to server failed")
   (t                  (concat "Ooops!" (number-to-string exit_status)))
   ))

(defun dictem-local-internal (err-msgs exit_status)
  (if err-msgs
      (concat (car err-msgs) "\n"
	      (cadr err-msgs)
	      "\n"
	      (dictem-local-internal
	       (cddr err-msgs)
	       nil)
	      )
    (if exit_status
	(dictem-get-error-message exit_status)
      nil)))

(defun dictem-generate-full-error-message (exit_status)

  (concat "Error messages:\n\n"
	  (dictem-local-internal dictem-error-messages exit_status)))

(defun dictem-run (search-fun &optional database query strategy)
  "Creates new *dictem* buffer and run search-fun"

  (let ((ex_status -1))

    (defun dictem-local-run-functions (funs database query strategy)
      (cond
       ((functionp funs)
	(let ((ex_st (funcall funs database query strategy)))
	  (if (/= ex_status 0)
	      (setq ex_status ex_st))))
       ((and (consp funs) (functionp (car funs)))
	(dictem-local-run-functions (car funs) database query strategy)
	(dictem-local-run-functions (cdr funs) database query strategy))
       ((null funs)
	nil)
       (t (error "wrong argument type"))
       )
      ex_status)

    (let ((coding-system nil))
      (if (and (functionp 'coding-system-list)
	       (member 'utf-8 (coding-system-list)))
	  (setq coding-system 'utf-8))
      (let ((selected-window (frame-selected-window))
	    (coding-system-for-read coding-system)
	    (coding-system-for-write coding-system)
	    ; here we remember values of variables local to buffer
	    (server           dictem-server)
	    (port             dictem-port)
	    (dbs              dictem-database-alist)
	    (strats           dictem-strategy-alist)
	    (user-dbs         dictem-user-databases-alist)
	    (user-only        dictem-use-user-databases-only)
	    (use-existing-buf dictem-use-existing-buffer)
;	    (option-mime      dictem-option-mime)
	    (dict-buf         nil)
	    )
	(if dictem-use-existing-buffer
	    (dictem-ensure-buffer)
	  (dictem))
	(setq dict-buf (buffer-name))
;	(set-buffer-file-coding-system coding-system)
	(make-local-variable 'dictem-default-strategy)
	(make-local-variable 'dictem-default-database)
	(make-local-variable 'case-replace)
	(make-local-variable 'case-fold-search)

	; the following lines are to inherit values local to buffer
	(set (make-local-variable 'dictem-server) server)
	(set (make-local-variable 'dictem-port)   port)
	(set (make-local-variable 'dictem-database-alist) dbs)
	(set (make-local-variable 'dictem-strategy-alist) strats)
	(set (make-local-variable 'dictem-user-databases-alist) user-dbs)
	(set (make-local-variable 'dictem-use-user-databases-only) user-only)
	(set (make-local-variable 'dictem-use-existing-buffer) use-existing-buf)

;	(set (make-local-variable 'dictem-option-mime) option-mime)

	(set (make-local-variable 'dictem-hyperlinks-alist) nil)

	;;;;;;;;;;;;;;
	(setq case-replace nil)
	(setq case-fold-search nil)
	(setq dictem-error-messages nil)
	(dictem-local-run-functions search-fun database query strategy)
	(switch-to-buffer dict-buf)
	(if (and (not (equal ex_status 0)) (= (point-min) (point-max)))
	    (insert (dictem-generate-full-error-message ex_status)))
	(goto-char (point-min))
	(setq buffer-read-only t)
	ex_status
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dictem-next-section ()
  "Move point to the next definition"
  (interactive)
  (forward-char)
  (if (search-forward-regexp "^From " nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun dictem-previous-section ()
  "Move point to the previous definition"
  (interactive)
  (backward-char)
  (if (search-backward-regexp "^From " nil t)
      (beginning-of-line)
    (goto-char (point-min))))

(defun dictem-hyperlinks-menu ()
  "Hyperlinks menu with autocompletion"
  (interactive)
  (let ((link (completing-read "Go to:" dictem-hyperlinks-alist)))
    (if (and link (setq link (assoc link dictem-hyperlinks-alist)))
	(dictem-run-define
	 (cadr link)
	 dictem-last-database))
    ))

(defun dictem-next-link ()
  "Move point to the next hyperlink"
  (interactive)
  (let ((pt nil)
	(limit (point-max)))
    (if (and (setq pt (next-single-property-change
		       (point) 'link nil limit))
	     (/= limit pt))
	(if (get-char-property pt 'link)
	    (goto-char pt)
	  (goto-char (next-single-property-change pt 'link nil limit))))
    ))

(defun dictem-previous-link ()
  "Move point to the previous hyperlink"
  (interactive)
  (let ((pt nil)
	(limit (point-min)))
    (if (and (setq pt (previous-single-property-change
		       (point) 'link nil limit))
	     (/= limit pt))
	(if (get-char-property pt 'link)
	    (goto-char pt)
	  (goto-char (previous-single-property-change pt 'link nil limit))))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dictem-help ()
  "Display a dictem help"
  (interactive)
  (describe-function 'dictem-mode))

(defun dictem-mode ()
  "This is a mode for dict client implementing
the protocol defined in RFC 2229.

The following basic commands are available in the buffer.

  \\[dictem-help]         display the help information

  \\[dictem-kill]         kill the dictem buffer
  \\[dictem-kill-all-buffers]         kill all dictem buffers
  \\[dictem-quit]         bury the dictem buffer

  \\[dictem-last]         restore content of the previously visited dictem buffer

  \\[dictem-run-search]         make a new SEARCH, i.e. ask for a database, strategy and query
            and show definitions
  \\[dictem-run-match]         make a new MATCH, i.e. ask for database, strategy and query
            and show matches
  \\[dictem-run-define]         make a new DEFINE, i.e. ask for a database and query
            and show definitions
  \\[dictem-run-show-server]         show information about DICT server
  \\[dictem-run-show-info]         ask for a database and show information about it
  \\[dictem-run-show-databases]         show databases DICT server provides
  \\[dictem-run-show-strategies]         show search strategies DICT server provides

  \\[dictem-next-section]         move point to the next definition
  \\[dictem-previous-section]         move point to the previous definition
  \\[dictem-next-link]       move point to the next hyper link
  \\[dictem-previous-link]       move point to the previous hyper link

  \\[dictem-hyperlinks-menu]         display the menu with hyperlinks

  \\[scroll-up]                 scroll dictem buffer up
  \\[scroll-down]                 scroll dictem buffer down
  \\[dictem-define-on-click] or \\[dictem-define-on-press]    visit a link (DEFINE using all dictionaries)


Also some advanced commands are available.

  \\[dictem-initialize] Initializes dictem, i.e. obtains
a list of available databases and strategiss from DICT server
and makes other tasks
  \\[dictem-initialize-strategies-alist] Obtain strategy ALIST from a DICT server and sets dictem-strategy-alist variable
  \\[dictem-initialize-databases-alist] Obtain database ALIST from a DICT server and sets dictem-database-alist variable


The following key bindings are currently in effect in the buffer:
\\{dictem-mode-map}
"

  (interactive)

  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map dictem-mode-map)
  (setq major-mode 'dictem-mode)
  (setq mode-name "dictem")

  (add-hook 'kill-buffer-hook 'dictem-kill t t)
  (run-hooks 'dictem-mode-hook)
  )

(defvar dictem-window-configuration
  nil
  "The window configuration to be restored upon closing the buffer")

(defvar dictem-selected-window
  nil
  "The currently selected window")

(defvar dictem-content-history
  nil
  "A list of lists (buffer_content point)")

(defconst dictem-buffer-name
  "*dictem buffer*")

(defconst dictem-url-regexp
  "^\\(dict\\)://\\([^/:]*\\)\\(:\\([0-9]+\\)\\)?/\\(.*\\)$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dictem-cmd2function-alist
  '(("show-server" dictem-call-process-SHOW-SERVER)
    ("show-info"   dictem-call-process-SHOW-INFO)
    ("show-strat"  dictem-call-process-SHOW-STRAT)
    ("show-db"     dictem-call-process-SHOW-DB)
    ("match"       dictem-call-process-MATCH)
    ("define"      dictem-call-process-DEFINE)
    ("search"      dictem-call-process-SEARCH)
    ))

(defconst dictem-cmd2userdb-alist
  '(("match"       dictem-userdb-MATCH)
    ("define"      dictem-userdb-DEFINE)
    ("search"      dictem-userdb-SEARCH)
    ("show-info"   dictem-userdb-SHOW-INFO)
    ))

(defun dictem-cmd2xxx (cmd alist)
  (let ((fun (assoc cmd alist)))
    (if fun
	(symbol-function (cadr fun))
      (error "Unknown command \"%s\"" cmd)
      )
    ))

(defun dictem-cmd2function (cmd)
  (dictem-cmd2xxx cmd dictem-cmd2function-alist))
(defun dictem-cmd2userdb (cmd)
  (dictem-cmd2xxx cmd dictem-cmd2userdb-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-parse-url (url)
  "Parses string like dict://dict.org:2628/foldoc
and returns a list containing protocol, server, port and path on nil if fails"
  (if (string-match dictem-url-regexp url)
      (list
       (match-string 1 url) ; protocol
       (match-string 2 url) ; host
       (match-string 4 url) ; port
       (match-string 5 url) ; path (database name for dict://)
       )
    nil))

(defun dictem ()
  "Create a new dictem buffer and install dictem-mode"
  (interactive)

  (let (
	(buffer (generate-new-buffer dictem-buffer-name))
	(window-configuration (current-window-configuration))
	(selected-window (frame-selected-window)))
    (switch-to-buffer-other-window buffer)
    (dictem-mode)

    (make-local-variable 'dictem-window-configuration)
    (make-local-variable 'dictem-selected-window)
    (make-local-variable 'dictem-content-history)
    (setq dictem-window-configuration window-configuration)
    (setq dictem-selected-window selected-window)
    ))

;(unless dictem-mode-map
(setq dictem-mode-map (make-sparse-keymap))
(suppress-keymap dictem-mode-map)

; Kill the buffer
(define-key dictem-mode-map "k" 'dictem-kill)

; Kill all dictem buffers
(define-key dictem-mode-map "x" 'dictem-kill-all-buffers)

; Bury the buffer
(define-key dictem-mode-map "q" 'dictem-quit)

; LAST, works like in Info-mode
(define-key dictem-mode-map "l" 'dictem-last)

; Show help message
(define-key dictem-mode-map "h" 'dictem-help)

; SEARCH = MATCH + DEFINE
(define-key dictem-mode-map "s" 'dictem-run-search)

; MATCH
(define-key dictem-mode-map "m" 'dictem-run-match)

; DEFINE
(define-key dictem-mode-map "d" 'dictem-run-define)

; SHOW SERVER
(define-key dictem-mode-map "r" 'dictem-run-show-server)

; SHOW INFO
(define-key dictem-mode-map "i" 'dictem-run-show-info)

; Move point to the next DEFINITION
(define-key dictem-mode-map "n" 'dictem-next-section)

; Move point to the previous DEFINITION
(define-key dictem-mode-map "p" 'dictem-previous-section)

; Move point to the next HYPER LINK
(define-key dictem-mode-map "\M-n" 'dictem-next-link)

; Move point to the previous HYPER LINK
(define-key dictem-mode-map "\M-p" 'dictem-previous-link)

; Hyperlinks menu
(define-key dictem-mode-map "e" 'dictem-hyperlinks-menu)

; Scroll up dictem buffer
(define-key dictem-mode-map " " 'scroll-up)

; Scroll down dictem buffer
(define-key dictem-mode-map "\177" 'scroll-down)

; Define on click
(if (featurep 'xemacs)
    (define-key dictem-mode-map [button2]
      'dictem-define-on-click)
  (define-key dictem-mode-map [mouse-2]
    'dictem-define-on-click))

(define-key dictem-mode-map "\C-m"
  'dictem-define-on-press)

(defun dictem-mode-p ()
  "Return non-nil if current buffer has dictem-mode"
  (eq major-mode 'dictem-mode))

(defun dictem-ensure-buffer ()
  "If current buffer is not a dictem buffer, create a new one."
  (if (dictem-mode-p)
      (progn
	(if dictem-use-content-history
	    (setq dictem-content-history
		  (cons (list (buffer-substring
			       (point-min) (point-max))
			      (point)) dictem-content-history)))
	(setq buffer-read-only nil)
	(erase-buffer))
    (dictem)))

(defun dictem-quit ()
  "Bury the current dictem buffer."
  (interactive)
  (if (featurep 'xemacs)
      (bury-buffer)
    (quit-window)))

(defun dictem-kill ()
  "Kill the current dictem buffer."
  (interactive)

  (if (eq major-mode 'dictem-mode)
      (progn
	(setq major-mode nil)
	(let ((configuration dictem-window-configuration)
	      (selected-window dictem-selected-window))
	  (kill-buffer (current-buffer))
	  (if (window-live-p selected-window)
	      (progn
		(select-window selected-window)
		(set-window-configuration configuration)))))))

(defun dictem-last ()
  "Go back to the last buffer visited visited."
  (interactive)
  (if (eq major-mode 'dictem-mode)
      (if dictem-content-history
	  (progn
	    (setq buffer-read-only nil)
	    (delete-region (point-min) (point-max))
	    (insert (car (car dictem-content-history)))
	    (goto-char (cadr (car dictem-content-history)))
	    (setq dictem-content-history (cdr dictem-content-history))
	    )
	  )
    ))

(defun dictem-kill-all-buffers ()
  "Kill all dictem buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((buf-name (buffer-name buffer)))
      (if (and (<= (length dictem-buffer-name) (length buf-name))
	       (string= dictem-buffer-name
			(substring buf-name 0 (length dictem-buffer-name))))
	  (kill-buffer buf-name))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     Top-level Functions     ;;;;;;

(defun dictem-run-match (query database strat)
  "Asks a user about database name, search strategy and query,
creates new *dictem* buffer and
shows matches in it."
  (interactive
   (list 
    (dictem-read-query (thing-at-point 'word))
    (dictem-select-database t t (dictem-get-default-database))
    (dictem-select-strategy (dictem-get-default-strategy))))
  (dictem-run 'dictem-base-match database query strat))

(defun dictem-run-define (query database)
  "Asks a user about database name and query,
creates new *dictem* buffer and
shows definitions in it."
  (interactive
   (list
    (dictem-read-query (thing-at-point 'word))
    (dictem-select-database t t (dictem-get-default-database))))
  (dictem-run 'dictem-base-define database query nil))

(defun dictem-run-search (query database strat)
  "Asks a user about database name, search strategy and query,
creates new *dictem* buffer and
shows definitions in it."
  (interactive
   (list
    (dictem-read-query (thing-at-point 'word))
    (dictem-select-database t t (dictem-get-default-database))
    (dictem-select-strategy (dictem-get-default-strategy))))
  (dictem-run 'dictem-base-search database query strat))

(defun dictem-run-show-info (database)
  "Asks a user about database name
creates new *dictem* buffer and
shows information about it."
  (interactive (list
		(dictem-select-database
		 nil nil
		 (dictem-get-default-database))))
  (dictem-run 'dictem-base-show-info database))

(defun dictem-run-show-server ()
  "Creates new *dictem* buffer and
shows information about DICT server in it."
  (interactive)
  (dictem-run 'dictem-base-show-server))

(defun dictem-run-show-databases ()
  "Creates new *dictem* buffer and
shows a list of databases provided by DICT."
  (interactive)
  (dictem-run 'dictem-base-show-databases))

(defun dictem-run-show-strategies ()
  "Creates new *dictem* buffer and
shows a list of search stratgeies provided by DICT."
  (interactive)
  (dictem-run 'dictem-base-show-strategies))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(easy-menu-define
 dictem-menu
 dictem-mode-map
 "DictEm Menu"
 `("DictEm"
   ["DictEm..." dictem-help t]
   "--"
   ["Next Section"     dictem-next-section t]
   ["Previous Section" dictem-previous-section t]
   "--"
   ["Match"            dictem-run-match t]
   ["Definition"       dictem-run-define t]
   ["Search"           dictem-run-search t]
   "--"
   ["Information about server"   dictem-run-show-server t]
   ["Information about database" dictem-run-show-info t]
   ["A list of available databases" dictem-run-show-databases t]
   "--"
   ["Bury Dictem Buffer" dictem-quit t]
   ["Kill Dictem Buffer" dictem-kill t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;      Optional Features       ;;;;;
(defun dictem-create-link (start end face function &optional data add-props)
  "Create a link in the current buffer starting from `start' going to `end'.
The `face' is used for displaying, the `data' are stored together with the
link.  Upon clicking the `function' is called with `data' as argument."
  (let ((properties
	 (append (list 'face face
		       'mouse-face 'highlight
		       'link-data data
		       'link-function function
		       'dictem-server dictem-server
		       'dictem-port   dictem-port)
	 add-props)))
    (remove-text-properties start end properties)
    (add-text-properties start end properties)))

;;;;;;;   Postprocessing Functions     ;;;;;;;

(defun dictem-postprocess-definition-separator ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "^\\(From\\)\\( [^\n]+\\)\\(\\[[^\n]+\\]\\)"))

      (while (search-forward-regexp regexp nil t)
	(let ((beg (match-beginning 1))
	      (end (match-end 1))
	      (beg-dbdescr (match-beginning 2))
	      (end-dbdescr (match-end 2))
	      (beg-dbname (match-beginning 3))
	      (end-dbname (match-end 3))
	      )
	  (put-text-property beg end
			     'face 'dictem-database-description-face)
	  (put-text-property beg-dbdescr end-dbdescr
			     'face 'dictem-database-description-face)
	  (setq dictem-current-dbname
		(dictem-replace-spaces
		 (buffer-substring-no-properties
		  (+ beg-dbname 1) (- end-dbname 1))))
	  (dictem-create-link
	   beg-dbname end-dbname
	   'dictem-reference-dbname-face
	   'dictem-base-show-info
	   (list (cons 'dbname dictem-current-dbname))))
	))))

(defvar dictem-hyperlink-beginning
  "{"
  "String that begins hyperlink.
This variable is used by
the function 'dictem-postprocess-definition-hyperlinks'")

(defvar dictem-hyperlink-end
  "}"
  "String that ends hyperlink.
This variable is used by
the function 'dictem-postprocess-definition-hyperlinks'")

(defvar dictem-hyperlink-define-func
  'dictem-base-define
  "Function called when user clicks on hyperlinks inside the definition.
This variable is used by
the function 'dictem-postprocess-definition-hyperlinks'")

(defun dictem-postprocess-collect-hyperlinks ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat "\\(" dictem-hyperlink-beginning "\\([^{}|]+\\)"
		  dictem-hyperlink-end
		  "\\|\\(" dictem-hyperlink-beginning
		  "\\([^{}|\n]+\\)|\\([^{}|\n]+\\)" dictem-hyperlink-end
		  "\\)\\)")))

      (while (search-forward-regexp regexp nil t)
	(cond ((match-beginning 2)
	       (let* ((word (dictem-replace-spaces
			     (buffer-substring-no-properties
			      (match-beginning 2)
			      (match-end 2)))))
		 (setq dictem-hyperlinks-alist
		       (cons (list word word) dictem-hyperlinks-alist))
		 ))
	      ((match-beginning 3)
	       (let* ((word-beg (match-beginning 4))
		      (word-end (match-end 4))
		      (link-beg (match-beginning 5))
		      (link-end (match-end 5))
		      (word (dictem-replace-spaces
			     (buffer-substring-no-properties
			      word-beg word-end)))
		      (link (dictem-replace-spaces
			     (buffer-substring-no-properties
			      link-beg link-end)))
		      )
		 (setq dictem-hyperlinks-alist
		       (cons (list word link) dictem-hyperlinks-alist))
		 )))))
    ))

(defun dictem-postprocess-definition-hyperlinks ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp
	   (concat dictem-hyperlink-beginning "\\([^{}|]+\\)" dictem-hyperlink-end
		   "\\|"
		   "^From [^\n]+\\[\\([^\n]+\\)\\]"
		   "\\|"
		   "\\(" dictem-hyperlink-beginning "\\([^{}|\n]+\\)|\\([^{}|\n]+\\)"
		   dictem-hyperlink-end "\\)")))

      (while (search-forward-regexp regexp nil t)
	(cond ((match-beginning 1)
	       (let* ((beg (match-beginning 1))
		      (end (match-end 1))
		      (match-beg (match-beginning 0))
		      (word (buffer-substring-no-properties beg end)))
		 (replace-match word t t)
		 (dictem-create-link
		  match-beg (+ match-beg (length word))
		  'dictem-reference-definition-face
		  dictem-hyperlink-define-func
		  (list (cons 'word (dictem-replace-spaces word))
			(cons 'dbname dictem-current-dbname))
		  '(link t))
		 ))
	      ((match-beginning 2)
	       (if (null dictem-current-dbname)
		   (setq dictem-current-dbname
			 (dictem-replace-spaces
			  (buffer-substring-no-properties (match-beginning 2)
							  (match-end 2))))))
	      ((match-beginning 3)
	       (let* ((beg (match-beginning 5))
		      (end (match-end 5))
		      (match-beg (match-beginning 3))
	              (repl-beg (match-beginning 4))
		      (repl-end (match-end 4))
		      (repl (buffer-substring-no-properties repl-beg repl-end))
		      (word (buffer-substring-no-properties beg end)))
		 (replace-match repl t t)
		 (dictem-create-link
		  match-beg (+ match-beg (length repl))
		  'dictem-reference-definition-face
		  dictem-hyperlink-define-func
		  (list (cons 'word (dictem-replace-spaces word))
			(cons 'dbname dictem-current-dbname))
		  '(link t))))
	      )))))

(defun dictem-postprocess-match ()
  (save-excursion
    (goto-char (point-min))
    (let ((last-database dictem-last-database)
	  (regexp "\\(\"[^\"\n]+\"\\)\\|\\([^ \"\n]+\\)"))

      (while (search-forward-regexp regexp nil t)
	(let* ((beg (match-beginning 0))
	       (end (match-end 0))
	       (first-char (buffer-substring-no-properties beg beg)))
	  (cond
	   ((save-excursion (goto-char beg) (= 0 (current-column)))
	    (setq last-database
		  (dictem-replace-spaces
		   (buffer-substring-no-properties beg (- end 1))))
	    (dictem-create-link
	     beg (- end 1)
	     'dictem-reference-dbname-face 'dictem-base-show-info
	     (list (cons 'dbname last-database))))
	   ((match-beginning 1)
	    (dictem-create-link
	     beg end
	     'dictem-reference-m1-face 'dictem-base-define
	     (list (cons 'word
			 (dictem-replace-spaces
			  (buffer-substring-no-properties
			   (+ beg 1) (- end 1))))
		   (cons 'dbname last-database))))
	   (t
	    (dictem-create-link
	     beg end
	     'dictem-reference-m2-face 'dictem-base-define
	     (list (cons 'word
			 (dictem-replace-spaces
			  (buffer-substring-no-properties
			   beg end )))
		   (cons 'dbname last-database))))
	   ))))))

(defun dictem-postprocess-definition-remove-header ()
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (let (eol (point))
      (goto-char (point-min))
      (if (search-forward-regexp "[0-9] definitions? found" eol t)
	  (progn
	    (goto-char (point-min))
	    (let ((kill-whole-line t))
	      (kill-line 1))
	    )))))

;;;;;       On-Click Functions     ;;;;;
(defun dictem-define-on-press ()
  "Is called upon pressing Enter."
  (interactive)

  (let* (
	 (properties (text-properties-at (point)))
	 (data (plist-get properties 'link-data))
	 (fun  (plist-get properties 'link-function))
	 (dictem-server (plist-get properties 'dictem-server))
	 (dictem-port   (plist-get properties 'dictem-port))
	 (word   (assq 'word data))
	 (dbname (assq 'dbname data))
	 )
    (if (or word dbname)
	(dictem-run fun
		    (if dbname (cdr dbname) dictem-last-database)
		    (if word (cdr word) nil)
		    nil))))

(defun dictem-define-on-click (event)
  "Is called upon clicking the link."
  (interactive "@e")

  (mouse-set-point event)
  (dictem-define-on-press))

;(defun dictem-define-with-db-on-click (event)
;  "Is called upon clicking the link."
;  (interactive "@e")
;
;  (mouse-set-point event)
;  (let* (
;	 (properties (text-properties-at (point)))
;	 (word (plist-get properties 'link-data)))
;    (if word
;	(dictem-run 'dictem-base-define (dictem-select-database) word nil))))

;(define-key dictem-mode-map [C-down-mouse-2]
;  'dictem-define-with-db-on-click)


;;;     Function for "narrowing" definitions ;;;;;

(defcustom dictem-postprocess-each-definition-hook
  nil
  "Hook run in dictem mode buffers containing SHOW SERVER result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-definition-separator
	     dictem-postprocess-definition-hyperlinks))

(defun dictem-postprocess-each-definition ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp-from-dbname "^From [^\n]+\\[\\([^\n]+\\)\\]")
	  (beg nil)
	  (end (make-marker))
	  (dbname nil))
      (if (search-forward-regexp regexp-from-dbname nil t)
	  (let ((dictem-current-dbname
		 (buffer-substring-no-properties
		  (match-beginning 1) (match-end 1))))
	    (setq beg (match-beginning 0))
	    (while (search-forward-regexp regexp-from-dbname nil t)
	      (set-marker end (match-beginning 0))
;	    (set-marker marker (match-end 0))
	      (setq dbname
		    (buffer-substring-no-properties
		     (match-beginning 1) (match-end 1)))

	      (save-excursion
		(narrow-to-region beg (marker-position end))
		(run-hooks 'dictem-postprocess-each-definition-hook)
		(widen))

	      (setq dictem-current-dbname dbname)
	      (goto-char end)
	      (forward-char)
	      (setq beg (marker-position end))
	      )
	    (save-excursion
	      (narrow-to-region beg (point-max))
	      (run-hooks 'dictem-postprocess-each-definition-hook)
	      (widen))
	    )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dictem)
