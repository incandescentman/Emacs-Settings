(require 'xmlgen)
(require 'uuidgen)
(require 'cl)
(require 'pp)


;;; Returns plist:
;;;   (<table symbol> ({(<abbrev> <expansion> <count>)}*))
(cl-defun all-abbrevs (&key where)
  (mapcan (lambda (table-symbol)
            (let ((table (symbol-value table-symbol))
                  defs)
              ;; HACK! We're shamefully breaking the abstraction
              ;; barrier, but the abbrevs-table inteface is too opaque
              ;; for our purposes.
              (mapatoms (lambda (abbrev-symbol)
                          (let ((expansion (symbol-value abbrev-symbol)))
                            (when (and expansion
                                       (or (null where)
                                           (funcall where abbrev-symbol)))
                              (push (list (symbol-name abbrev-symbol) ; abbrev
                                          expansion
                                          (abbrev-get abbrev-symbol :count))
                                    defs)))) ;count
                        table)
              (when defs
                (list table-symbol defs))))
          abbrev-table-name-list))


(defvar textexpander-export-as-case-sensitive-list 
  (list "id" "wed" "im" "sd" "uk" "US" "OS" "CK"))


(defun abbreviation-mode (abbrev)
  (if (or (and (= (length abbrev) 1)
               (string= abbrev (downcase abbrev)))
          (member abbrev textexpander-export-as-case-sensitive-list))
      0
    2))


(defun translate-abbrev-table-definitions (defs)
  (loop for (abbrev expansion count) in defs
        unless (string= abbrev expansion)
        collect `(dict (key "abbreviation")
                       (string ,abbrev)
                       (key "abbreviationMode")
                       (integer ,(abbreviation-mode abbrev))
                       (key "creationDate")
                       (date ,(format-time-string "%FT%TZ" (current-time) t))
                       (key "flags")
                       (integer 0)      ; TODO No idea what this is...
                       (key "label")
                       (string "")
                       (key "lastUsed")
                       (date ,(format-time-string "%FT%TZ" (current-time) t))
                       (key "modificationDate")
                       (date ,(format-time-string "%FT%TZ" (current-time) t))
                       (key "plainText")
                       (string ,expansion)
                       (key "snippetType")
                       (integer 0)      ; TODO No idea what this is...
                       (key "useCount")
                       (integer ,count)
                       (key "uuidString")
                       (string ,(upcase (uuidgen-1)))
                       )))


(defun insert-abbrev-defs-converted-to-text-expander (defs)
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  (insert "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n")
  (insert
   (xmlgen `(plist :version "1.0"
                   (dict (key "groupInfo")
                         (dict (key "expandAfterMode")
                               (integer 0)
                               (key "groupName")
                               (string "emacs-abbrevs"))
                         (key "snippetsTE2")
                         (array ,@(translate-abbrev-table-definitions defs))))
           nil 0 t)))


(defun insert-abbrev-defs-as-table-definitions (table defs)
  (insert
   (pp-to-string
    `(define-abbrev-table ',table
       '(,@(mapcar (lambda (def)
                     `(,(first def)
                       ,(second def)
                       nil
                       ,(third def)))
                   defs))))))


(defvar *max-exported-abbrev-length* 4)


(defun all-exportable-abbrevs ()
  (apply 'all-abbrevs
         (when *max-exported-abbrev-length*
           (list :where (lambda (abbrev-symbol)
                          (<= (length (symbol-name abbrev-symbol))
                              *max-exported-abbrev-length*))))))


(defun export-abbrevs-to-emacs (abbrevs)
  (let ((buffer (generate-new-buffer "exported.abbrev_defs")))
    (switch-to-buffer buffer)
    (loop for (table defs) on abbrevs by #'cddr
          do (insert-abbrev-defs-as-table-definitions table defs)
             (terpri (current-buffer)))))


(defun export-abbrevs-to-text-expander (abbrevs)
  (let ((buffer (generate-new-buffer "emacs-abbrevs.textexpander")))
    (switch-to-buffer buffer)
    (loop for (table defs) on abbrevs by #'cddr
          do (insert-abbrev-defs-converted-to-text-expander defs))))


(defun export-abbrevs ()
  "Exports all abbrevs, with length lesser or equal to
  `*max-exported-abbrev-length*', to:
  - a new buffer containing only `define-abbrev-table' forms;
  - a different new buffer, containing TextExpander snippets."
  (interactive)
  (let ((abbrevs (all-exportable-abbrevs)))
    (export-abbrevs-to-emacs abbrevs)
    (export-abbrevs-to-text-expander abbrevs)))


(defun export-all-abbrevs-to-text-expander ()
  (interactive)
  (export-abbrevs-to-text-expander (all-exportable-abbrevs)))

