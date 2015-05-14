;;; neopastebin.el --- pastebin.com interface to emacs

;;; Copyright (C) 2013 by Daniel Hilst <danielhilst at gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA

;;; Besides being a new interface, some parts were borrowed from old
;;; interface so I think is fair put the names here. 
;;; Copyright (C) 2008 by Nic Ferrier <nic@ferrier.me.uk>
;;; Copyright (C) 2010 by Ivan Korotkov <twee@tweedle-dee.org>
;;; Copyright (C) 2012 by Filonenko Michael <filonenko.mikhail@gmail.com>

;;;
;;; USAGE:
;;;
;;;     LOGIN
;;;     ~~~~~
;;;
;;; Puts this on your .emacs file
;;;
;;; ((pastebin-create-login :dev-key "YOUR DEV KEY"
;;;                     :username "YOUR USER NAME")
;;;
;;; Login will only ocurr when you try to paste something or
;;; list your pastes. So is save to put this on your .emacs file
;;; without need to wait emacs connect to pastebin on each startup.
;;;
;;;
;;; -*- SECURITY DISCLAIMER -*-
;;;
;;; Password will be asked first time you login. It will also ask for store password
;;; on disk. This will be saved on ~/.emacs.d/pastebin-data/pass by default.
;;;
;;; !!! YOUR PASSWORD ITS SAVED ON PLAIN TEXT  !!!
;;;
;;; Since pastebin uses http instead of https, your password can be gathered from sniffers
;;; on network, so you are not safe in any case, this is pastebin.com api and is insecure.
;;; Use at own risk!
;;;
;;; 
;;;     LISTING
;;;     ~~~~~~~
;;;
;;; M-x pastebin-list-buffer-refresh -> Fetch and list pastes on "list buffer"
;;;
;;; After logged you can list your pastes with command `pastebin-list-buffer-refresh', just
;;; type pastebin-l and press TAB.
;;;
;;; Here is a list of keybinds from list buffer
;;;
;;; RET -> fetch paste and switch to it
;;; r ->   refresh list and list buffer
;;; d ->   delete paste
;;; t ->   order by title
;;; D ->   order by date
;;; f ->   order by format
;;; k ->   order by key
;;; p ->   order by private
;;;
;;;
;;;     CREATING NEW PASTE
;;;     ~~~~~~~~~~~~~~~~~~
;;;
;;; M-x pastebin-new -> will create a new paste from current buffer
;;;
;;; The name of the paste is given from current buffer name
;;; The format from buffers major mode
;;; Prefix argument makes private 
;;;

;;;
;;; Naming convention:
;;;
;;; pastebin-- prefix for internal stuff
;;; pastebin- prefix for user interface and customs
;;;
;;;
;;; @TODO list:
;;;  
;;; - pastebin minor mode
;;;   - Must save files on pastebin with keybinds. I'm
;;;     wondering if setting pastebin.com as an abstract storage
;;;     is a good idea. If so, C-x C-s should save pastebin buffers
;;;     to pastebin.com without question. Elisp files manual chapter would help-me
;;;     to do that http://www.gnu.org/software/emacs/manual/html_node/elisp/Files.html#Files
;;;   - Keybind for printing url on mini buffer
;;;
;;; DEPENDENCIES:
;;;
;;; eieio.el
;;; wid-edit
;;;   

(require 'eieio)
(require 'wid-edit)

(defgroup pastebin nil
  "Pastebin -- pastebin.com client"
  :tag "Pastebin"
  :group 'tools)

;; Customs 

(defcustom pastebin-default-paste-list-limit 100
  "The number of pastes to retrieve by default"
  :type 'number
  :group 'pastebin)

(defcustom pastebin-post-request-login-url "http://pastebin.com/api/api_login.php"
  "Login url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-paste-url "http://pastebin.com/api/api_post.php"
  "Paste url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-data-dir "~/.emacs.d/pastebin-data"
  "Diretory to keep data"
  :type 'string
  :group 'pastebin)

;; Global variables

(defvar pastebin--mode-map nil
  "The pastebin keymap.")
(unless pastebin--mode-map
  (setq pastebin--mode-map (make-sparse-keymap))
  (define-key pastebin--mode-map (kbd "C-x p u") 'pastebin-show-url))

(define-minor-mode pastebin-mode
  "Pastebin buffer mode, used to upload pastes automatically with S-C-x C-s"
  :lighter " pastebin"
  :group 'pastebin
  :keymap pastebin--mode-map)

(defvar pastebin--type-assoc
  '((actionscript-mode . " actionscript")
    (ada-mode . "ada")
    (asm-mode . "asm")
    (sh-mode . "bash")
    (autoconf-mode . "bash")
    (bibtex-mode . "bibtex")
    (cmake-mode . "cmake")
    (c-mode . "c")
    (c++-mode . "cpp")
    (cobol-mode . "cobol")
    (conf-colon-mode . "properties")
    (conf-javaprop-mode . "properties")
    (conf-mode . "ini")
    (conf-space-mode . "properties")
    (conf-unix-mode . "ini")
    (conf-windows-mode . "ini")
    (cperl-mode . "perl")
    (csharp-mode . "csharp")
    (css-mode . "css")
    (delphi-mode . "delphi")
    (diff-mode . "diff")
    (ebuild-mode . "bash")
    (eiffel-mode . "eiffel")
    (emacs-lisp-mode . "lisp")
    (erlang-mode . "erlang")
    (erlang-shell-mode . "erlang")
    (espresso-mode . "javascript")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (gnuplot-mode . "gnuplot")
    (graphviz-dot-mode . "dot")
    (haskell-mode . "haskell")
    (html-mode . "html4strict")
    (idl-mode . "idl")
    (inferior-haskell-mode . "haskell")
    (inferior-octave-mode . "octave")
    (inferior-python-mode . "python")
    (inferior-ruby-mode . "ruby")
    (java-mode . "java")
    (js2-mode . "javascript")
    (jython-mode . "python")
    (latex-mode . "latex")
    (lisp-mode . "lisp")
    (lisp-interaction-mode . "lisp")
    (lua-mode . "lua")
    (makefile-mode . "make")
    (makefile-automake-mode . "make")
    (makefile-gmake-mode . "make")
    (makefile-makepp-mode . "make")
    (makefile-bsdmake-mode . "make")
    (makefile-imake-mode . "make")
    (matlab-mode . "matlab")
    (nxml-mode . "xml")
    (oberon-mode . "oberon2")
    (objc-mode . "objc")
    (ocaml-mode . "ocaml")
    (octave-mode . "matlab")
    (pascal-mode . "pascal")
    (perl-mode . "perl")
    (php-mode . "php")
    (plsql-mode . "plsql")
    (po-mode . "gettext")
    (prolog-mode . "prolog")
    (python-2-mode . "python")
    (python-3-mode . "python")
    (python-basic-mode . "python")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (scheme-mode . "lisp")
    (shell-mode . "bash")
    (smalltalk-mode . "smalltalk")
    (sql-mode . "sql")
    (tcl-mode . "tcl")
    (visual-basic-mode . "vb")
    (xml-mode . "xml")
    (yaml-mode . "properties")
    (text-mode . "text"))
  "Alist composed of major-mode names and corresponding pastebin highlight formats.")

(defvar pastebin--default-user nil
  "The default user begin used")

(defvar pastebin--local-buffer-paste nil
  "Every pastebin buffer has a paste object associated with it")
(make-variable-buffer-local 'pastebin--local-buffer-paste)

(defvar pastebin--list-buffer-user nil
  "Every pastebin list buffer has a user object associated with it")
(make-variable-buffer-local 'pastebin--list-buffer-user)

(defvar pastebin--list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'pastebin-delete-paste-at-point) 
    (define-key map (kbd "r") 'pastebin-list-buffer-refresh) 
    (define-key map (kbd "f") 'pastebin-list-buffer-refresh-sort-by-format)
    (define-key map (kbd "t") 'pastebin-list-buffer-refresh-sort-by-title)
    (define-key map (kbd "k") 'pastebin-list-buffer-refresh-sort-by-key)
    (define-key map (kbd "D") 'pastebin-list-buffer-refresh-sort-by-date)
    (define-key map (kbd "p") 'pastebin-list-buffer-refresh-sort-by-private)
    map)
  "Key map for pastebin list buffer")

(defconst pastebin--raw-paste-url "http://pastebin.com/raw.php?i="
  "Concatenate this with paste key to get the raw paste")

;;
;; EIEIO Layer
;;

;; PASTE-USER class

(defclass pastebin--paste-user ()
  ((dev-key :initarg :dev-key "Your developer key from http://pastebin.com/api")
   (usr-key :initarg :usr-key "Your user key, retrived from pastebin")
   (password :initarg :password "Your password, clear text honey!!")
   (username :initarg :username "Your username")
   (paste-list :initarg :paste-list "The list of pastes for this user")
   (list-buffer :initarg :list-buffer "Done by do-list-buffer")
   (sort-by :initarg :sort-by "Order to sort :paste-list")
  )
  "Class representing a pastebin.com user")

(defmethod is-logged ((user pastebin--paste-user))
  "Return true if user is logged in"
  (slot-boundp user :usr-key))

(defmethod fetch-list-xml ((user pastebin--paste-user))
  "Fetch the list of pastes as xml, and return that buffer"
  (let* ((params (concat "api_dev_key=" (oref user dev-key)
                         "&api_user_key=" (oref user usr-key)
                         "&api_results_limit=" (format "%d" pastebin-default-paste-list-limit)
                         "&api_option=list")))
    (with-current-buffer (pastebin--url-retrieve-synchronously pastebin-post-request-paste-url
                                                               "POST"
                                                               params)
      (pastebin--strip-CRs)
      (goto-char (point-min))
      (current-buffer)
      )))

(defmethod refresh-paste-list ((user pastebin--paste-user))
  "Set/Refresh paste-list attr to the list of paste objects retrieved from pastebin.com"
  (oset user :paste-list nil)
  (with-current-buffer (fetch-list-xml user)
    (goto-char (point-min))
    (let ((i (point-min))
          plist)
      (while (re-search-forward "</paste>" nil t)
        (let ((paste-sexp (xml-parse-region i (point)))
              p)
          (setq i (point))
          (condition-case err
              (progn
                (setq p (pastebin--sexp-to-paste paste-sexp))
                (oset p :user user)
                (oset p :last-fetched (float-time))
                (setq plist (append plist (list p))))
            (wrong-type-argument
             (error "Error while creating paste object on `refresh-paste-list' %s" err)
             (debug)))
          )
        (oset user :paste-list plist)
        )
      )
    )
  )

(defmacro pastebin--sort-by-string-attr (user attr)
  "sort :paste-list by `attr' in reverse order"
  `(progn
     (unless (keywordp ,attr)
       (error "pastebin--sort-by-stirng-attr attr is not a keyword"))

     (unless (member ,attr '(:key :title :format_long :format_short :url :date :private))
      (error "pastebin--sort-by-string-attr attr is not in '(:key :title :format_long :format_short :url)"))

     (oset ,user :paste-list (sort (oref ,user :paste-list) (lambda (p1 p2)
                                                          (string< (downcase (oref p1 ,attr))
                                                                   (downcase (oref p2 ,attr))))))
     )
  )

(defmethod do-list-buffer ((user pastebin--paste-user))
  "Create a buffer with a list of pastes and return it
Some keybinds are setted"
  (unless (is-logged user)
    (error "do-list-buffer called with unloged user"))

  (unless (slot-boundp user :list-buffer)
    (oset user :list-buffer (format "Pastebin %s pastes" (oref user :username))))

  (unless (get-buffer (oref user :list-buffer))
    (generate-new-buffer (oref user :list-buffer))
    (message "%s buffer created" (oref user :list-buffer)))

  (let ((inhibit-read-only t)
        old-point)
    (with-current-buffer (get-buffer (oref user :list-buffer))

      (setq old-point (point))

      (erase-buffer)

      (widget-minor-mode 1)
      (use-local-map pastebin--list-map)
      
      (setq pastebin--list-buffer-user user)

      (widget-insert (format "%5.5s | %-8.8s | %-32.32s | %-7.7s | %-30.30s\n"
                             "VIEW" "ID" "TITLE" "FORMAT" "DATE"))
      (dolist (paste (oref user :paste-list))
        (widget-create 'link 
                       :notify (lambda (wid &rest ignore)
                                 (pastebin--fetch-paste-at-point))
                       :paste paste
                       :follow-link t
                       :value (format "%4.4s | %-8.8s | %-32.32s | %-7.7s | %-20.20s"
                                      (cond 
                                       ((string= (oref paste :private) "0")
                                        "PUBL")
                                       ((string= (oref paste :private) "1")
                                        "ULST")
                                       ((string= (oref paste :private) "2")
                                        "PRIV")
                                       (t
                                        "_ERR"))
                                      (oref paste :key)
                                      (or (oref paste :title) "")
                                      (oref paste :format_short)
                                      (format-time-string "%c" (seconds-to-time (string-to-number (oref paste :date))))
                                      )
                       )

        (widget-insert "\n")
        )
      (widget-setup)
      (goto-char (or old-point (point-min)))
      (current-buffer)
      ) ;; (with-current-buffer (get-buffer (oref user :list-buffer))
    ) ;; (let ((inhibit-read-only t)
  )

(defmethod login ((user pastebin--paste-user))
  "Given user and password login and sets usr-key"
  (if (slot-boundp user :usr-key)
      (oref user :usr-key)
    (let* ((params (concat "api_dev_key=" (oref user :dev-key)
                           "&api_user_name=" (url-hexify-string (oref user :username))
                           "&api_user_password=" (url-hexify-string (oref user :password)))))

      (with-current-buffer (pastebin--url-retrieve-synchronously pastebin-post-request-login-url
                                                                 "POST"
                                                                 params)
        (oset user :usr-key (buffer-substring-no-properties (point-min) (point-max)))))))

(defmethod paste-new ((user pastebin--paste-user) &optional private)
  "Upload a new paste to pastebin.com"
  (let* ((ptitle (buffer-name))
         (pbuffer (current-buffer))
         (pprivate (or private "1"))
         (params (concat "api_dev_key=" (oref user :dev-key)
                         "&api_user_key=" (oref user :usr-key)
                         "&api_paste_name=" (url-hexify-string ptitle)
                         "&api_paste_format=" (url-hexify-string (pastebin--get-format-string-from-major-mode))
                         "&api_paste_code=" (url-hexify-string (with-current-buffer pbuffer
                                                                 (buffer-string)))
                         "&api_option=paste"
                         "&api_paste_private=" pprivate)))
    (with-current-buffer (pastebin--url-retrieve-synchronously pastebin-post-request-paste-url
                                                               "POST"
                                                               params)
      (current-buffer))))



;; PASTE CLASS

(defclass pastebin--paste ()
  ((key :initarg :key)
   (date :initarg :date)
   (title :initarg :title)
   (size :initarg :size)
   (expire_date :initarg :expire_date)
   (private :initarg :private)
   (format_long :initarg :format_long)
   (format_short :initarg :format_short)
   (url :initarg :url)
   (buffer :initarg :buffer)
   (last-fetched :initarg :last-fetched)
   (user :initarg :user :type pastebin--paste-user)
   (hits :initarg :hits))
  "Class representing a paste from pastebin.com 
The contents of paste are not stored. Instead the method
`paste-fetch' fetch and retrieve the buffer with paste contents")

(defmethod get-mode ((p pastebin--paste))
  "return the mode from `pastebin--type-assoc'"
  (if (slot-boundp p :format_short)
      (car (rassoc (oref p :format_short) pastebin--type-assoc))
    (error "No format short for paste %s with key %s" (oref p :title) (oref p :key))))

(defmethod fetch-and-process ((p pastebin--paste))
  "Fetch buffer a do needed processing before switching to it"
  (with-current-buffer (paste-fetch p)
    (switch-to-buffer (current-buffer))))

(defmethod paste-fetch ((p pastebin--paste))
  "Fetch the raw content from paste and return buffer containing"
  (let* ((content-buf (pastebin--url-retrieve-synchronously (concat pastebin--raw-paste-url (oref p key))
                                                            "GET"
                                                            ""))
         (inhibit-read-only t)
         (pbuf (if (and (slot-boundp p :buffer)
                        (buffer-live-p (oref p :buffer)))
                   (oref p :buffer)
                 (oset p :buffer (get-buffer-create (oref p :title))))))
    (with-current-buffer pbuf
      (erase-buffer)
      (insert-buffer-substring content-buf)
      (pastebin--strip-paste-CRs)
      (funcall (get-mode p))
      (setq pastebin--local-buffer-paste p) ;; buffer local
      (pastebin-mode 1)
      (current-buffer))))

(defmethod paste-delete ((p pastebin--paste))
  "Detele paste from pastebin.com"
  (unless (and (slot-boundp p :user)
               (slot-boundp p :key)
               (slot-boundp (oref p :user) :dev-key) 
               (slot-boundp (oref p :user) :usr-key))
    (error "paste-delete called with ubound slot object"))

  (let* ((params (concat "api_dev_key=" (oref (oref p :user) :dev-key)
                         "&api_user_key=" (oref (oref p :user) :usr-key)
                         "&api_paste_key=" (oref p :key)
                         "&api_option=delete")))
    (with-current-buffer (pastebin--url-retrieve-synchronously pastebin-post-request-paste-url
                                                               "POST"
                                                               params)
      (buffer-string)) ;; Pastebin send somthing like paste xxx deleted
    ))

;; Local functions and helpers

(defun pastebin--get-format-string-from-major-mode ()
  "Returns the format string from major mode
Error if major-mode is nil"
  (unless major-mode
    (error "pastebin--get-format-string-from-major-mode called with nil major-mode"))
  (or (cdr (assoc major-mode pastebin--type-assoc))
      "text"))

(defun pastebin--sexp-get-attr-h (paste-sexp attr &optional onerror)
  "Return the attribute `attr' from `paste-sexp'
If onerror is given (should be a string) is used when no such attribute
is found.
Attributes are described here: http://pastebin.com/api#9
`attr' must be a symbol
Ex: (pastebin-paste-get-attr some-paste-sexp 'paste_tittle)"
  (unless (symbolp attr)
    (error "attr should be a symbol"))
  (when (and onerror
             (not (stringp onerror)))
    (error "onerror should be a string"))
  (let ((a (or (car (last (assoc attr (nthcdr 2 (car paste-sexp)))))
               onerror)))
    (unless a
      (error "No attribute %s on paste sexp '%s'" attr paste-sexp))
    (format "%s" a)))

(defun pastebin--strip-paste-CRs (&optional buffer)
  "Get rid of CR
I use this after fetching a paste to get rid of annoying ^M"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
        (replace-match ""))
      buffer)))

(defun pastebin--strip-CRs (&optional buffer)
  "Get rid of CRLF
I need this for xml-parse-region reponse without getting
a lot of spaces and CRLF on pastes sexps. See `pastebin--sexp-to-paste'"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward "\r\n" nil t)
        (replace-match ""))
      buffer)))

(defun pastebin--strip-http-header (&optional buffer)
  "Given a buffer with an HTTP response, remove the header and return the buffer
If no buffer is given current buffer is used"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n")
    (kill-region (point-min) (point))
    buffer)))

(defun pastebin--get-paste-at-point ()
  "Get the paste at point at current-buffer"
  (let ((wid (widget-at)))
    (if (not wid)
        (error "No paste at point")
      (widget-get wid :paste))))

(defun pastebin--fetch-paste-at-point ()
  "Fetch and switch to paste at point"
  (let ((p (pastebin--get-paste-at-point)))
    (fetch-and-process p)))

(defun pastebin--sexp-to-paste (paste-sexp)
  "Given and sexp returned from `xml-parse-region' on pastebin.com response, constructs and return a pastebin--paste object.
See `fetch-list-xml' for more information"
  (unless (consp paste-sexp)
    (error "pastebin--sexp-to-paste called without cons type"))
  (condition-case err
      (pastebin--paste (concat "paste@" (pastebin--sexp-get-attr-h paste-sexp 'paste_key))
             :key (pastebin--sexp-get-attr-h paste-sexp 'paste_key)
             :date (pastebin--sexp-get-attr-h paste-sexp 'paste_date)
             :title (pastebin--sexp-get-attr-h paste-sexp 'paste_title "UNTITLED")
             :size (pastebin--sexp-get-attr-h paste-sexp 'paste_size)
             :expire_date (pastebin--sexp-get-attr-h paste-sexp 'paste_expire_date)
             :private (pastebin--sexp-get-attr-h paste-sexp 'paste_private)
             :format_long (pastebin--sexp-get-attr-h paste-sexp 'paste_format_long)
             :format_short (pastebin--sexp-get-attr-h paste-sexp 'paste_format_short)
             :url (pastebin--sexp-get-attr-h paste-sexp 'paste_url)
             )
    ((debug error)
     (error "Cant construct paste from sexp %s\nError: %s" paste-sexp err))))

(defun pastebin--store-password (passwd)
  "Stores password on `pastebin-data-dir'/pass"
  (pastebin--mkdatadir)
  (with-temp-buffer
    (insert passwd)
    (write-file (concat pastebin-data-dir "/pass"))))

(defun pastebin--read-password-from-file ()
  "Read password from `pastebin-data-dir'/pass"
  (with-temp-buffer
    (goto-char (point-min))
    (insert-file-contents-literally (concat pastebin-data-dir "/pass"))
    (buffer-string)))

(defun pastebin--password-file-exists-p ()
  "return t if pastebin-data-dir exists"
  (file-exists-p (concat pastebin-data-dir "/pass")))

(defun pastebin--mkdatadir ()
  "Create the `pastebin-data-dir'"
  (ignore-errors
    (make-directory pastebin-data-dir t)))

(defun pastebin--ask-for-password (prompt)
  "Ask user for a password and if want to store it"
  (let* ((lexical-binding t)
         (p (read-passwd prompt)))
    (when (yes-or-no-p "Store password on disk? ")
        (pastebin--store-password p))
    p))

(defun pastebin--url-retrieve-synchronously (url method params)
  "Retrieve a buffer from pastebin, raising an error if an error ocurr"
  (unless (stringp url)
    (error "pastebin--url-retrieve-synchronously `url' need to be a string"))

  (unless (stringp method)
    (error "pastebin--url-retrieve-synchronously `method' need to be a string"))

    (unless (stringp params)
    (error "pastebin--url-retrieve-synchronously `params' need to be a string"))

  (let* ((inhibit-read-only t)
         (url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params)
         (content-buf (url-retrieve-synchronously url)))
    (unless (pastebin--http-200-p content-buf) ;; check HTTP header
      (when debug-on-error
        (with-current-buffer (get-buffer-create "*pastebin-debug*")
          (erase-buffer)
          (goto-char (point-min))
          (insert "Bad HTTP response below\n")
          (insert-buffer-substring content-buf)))
      (error (concat 
              "pastebin--url-retrieve-synchronously HTTP Bad response (not 200) on header\n"
              (if debug-on-error
                  "See header at *pastebin-debug*"
                "")))) ;; header is OK ...
    (with-current-buffer content-buf   
      (goto-char (point-min))
      (pastebin--strip-http-header)
      (pastebin--error-if-bad-response (current-buffer)) ;; two `with-current-buffer' on same buffer :-/ slow
      )
    content-buf ;; return the buffer
    ))

(defun pastebin--error-if-bad-response (buf)
  "Raises a error if is a bad response from pastebin"
  (unless (or (bufferp buf)
              (stringp buf))
    (error "pastebin--bad-presponse-p `buf' need be a buffer or a string"))
  
  (with-current-buffer buf
    (if (or 
         (save-excursion
           (re-search-forward "Bad API request," nil t))
         (save-excursion
           (re-search-forward "No pastes found." nil t))
         (save-excursion
           (re-search-forward "URL Post limit, maximum pastes per 24h reached" nil t)))
        (error "Pastebin bad response: %s" (buffer-string))
      nil)))


(defun pastebin--http-200-p (header-buf)
  "Search for HTTP 200 status on header-str - a buffer"
  (unless (bufferp header-buf)
    (error "pastebin--http-200-p: `header-buf' need be a buffer :/"))

  (with-current-buffer header-buf
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "HTTP.*200 OK" nil t))))

(defun pastebin--get-pst-url (buf)
  "Return url string from buf"
  (unless (and (or (bufferp buf)
                   (stringp buf))
                (get-buffer buf))
     (error (concat "pastebin--get-pst-url `buf' need\n"
                    "be a existing buffer or buffer name as string")))

  (with-current-buffer buf
    (save-excursion
      (buffer-substring-no-properties (point-min) (point-max)))))

;; User interface 

(defun pastebin-show-url ()
  "On a buffer from a fetched paste, show the url o echo area"
  (interactive)
  (if pastebin--local-buffer-paste
      (message (format "Paste URL: %s" (oref pastebin--local-buffer-paste :url)))
    (message (format "Current buffer is not a paste buffer"))))

(defun pastebin-list-buffer-refresh ()
  "Refresh the list buffer screen
Operates on current buffer"
  (interactive)
  (unless (is-logged pastebin--default-user)
    (login pastebin--default-user))
  (refresh-paste-list pastebin--default-user)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  (message "%d pastes fetched!" (length (oref pastebin--default-user :paste-list)))
  )


(defun pastebin-list-buffer-refresh-sort-by-title ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :title)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-format ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :format_short)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-key ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :key)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-date ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :date)
  (oset pastebin--default-user :paste-list (reverse (oref pastebin--default-user :paste-list)))
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-private ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :private)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-delete-paste-at-point ()
  "Delete the paste at point"
  (interactive)
  (let* ((lexical-binding t)
         (p (pastebin--get-paste-at-point)))
    (when (y-or-n-p (format "Do you really want to delete paste %s from %s\n" 
                            (oref p :title)
                            (format-time-string "%c" (seconds-to-time (string-to-number (oref p :date))))))
      (message "%s" (paste-delete (pastebin--get-paste-at-point))))
    (pastebin-list-buffer-refresh)))

(defun pastebin-new (p)
  "Create a new paste from buffer"
  (interactive "P")
  (unless (is-logged pastebin--default-user)
    (login pastebin--default-user))
  (save-excursion
    (goto-char (point-min))
    (pastebin-mode 1)
    (let* ((lexical-binding t)
           (pbuf (paste-new pastebin--default-user (and p "1")))
           (url (pastebin--get-pst-url pbuf))
           (x-select-enable-clipboard t)
           (link-point (re-search-forward "http://[A-Za-z0-9_-]+\.[A-Za-z0-9]+" nil t)))
      (kill-new url)
      (message "URL: %s%s" url
               (if link-point
                   (concat (format "\nYour buffer contains an link at line %d\n" (line-number-at-pos link-point))
                           (format "please visit link above and fill the captcha"))
                 "")))))

(defun pastebin-create-login (&rest args)
  "Create a login data. The effective login will be done when needed
NOTE: `args' is a keryword list using :username and :dev-key that should
be strings"
  ;; Keyword arguments work arround
  ;; I want to get rid of cl dependence here
  (let* ((lexical-bind t)
         username
         dev-key
         password)
    (while args
      (cond ((eq (car-safe args) :username)
             (setq username (car-safe (cdr-safe args))))
            ((eq (car-safe args) :dev-key)
             (setq dev-key (car-safe (cdr-safe args))))
            ) ;; (cond .. 
      (setq args (cdr-safe args)))
    ;; Function body
    (unless (and username dev-key)
      (error "pastebin-login argument missing. (dev-key or username)"))
    (let ((lexical-binding t)
          (p (if (pastebin--password-file-exists-p)
                         (pastebin--read-password-from-file)
                       (pastebin--ask-for-password "Pastebin password: "))))
      (setq pastebin--default-user (pastebin--paste-user username
                                                         :username username
                                                         :dev-key dev-key
                                                         :password p)))
    (message "User %s created, login is on demand. Have a nice day!" username)
    ) ;; (let* ((lexical-bind t)
  ) ;; (defun pastebin-create-login &rest args)

;; Setup minor mode keymap
(or (assq 'pastebin-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'pastebin-mode pastebin--mode-map)
                                     minor-mode-map-alist)))

(provide 'neopastebin)

;;; END of neopastebin

