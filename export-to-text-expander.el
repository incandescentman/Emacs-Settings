(require 'xmlgen)
(require 'uuidgen)
(require 'cl)


;;; Returns plist:
;;;   (<table symbol> ({(<abbrev> <expansion> <count>)}*))
(defun all-abbrevs ()
  (mapcan (lambda (table-symbol)
            (let ((table (symbol-value table-symbol)))
              ;; HACK! We're shamefully breaking the abstraction
              ;; barrier, but the abbrevs-table inteface is too opaque
              ;; for our purposes.
              (list table-symbol
                    (let (defs)
                      (mapatoms (lambda (abbrev-symbol)
                                  (let ((expansion (symbol-value abbrev-symbol)))
                                    (when expansion
                                      (push (list (symbol-name abbrev-symbol) ; abbrev
                                                  expansion
                                                  (abbrev-get abbrev-symbol :count))
                                            defs)))) ;count
                                table)
                      defs))))
          abbrev-table-name-list))


(defun translate-abbrev-table-definitions (defs)
  (loop for (abbrev expansion count) in defs
        collect `(dict (key "abbreviation")
                       (string ,abbrev)
                       (key "abbreviationMode")
                       (integer ,(if (and (= (length abbrev) 1)
                                          (string= abbrev (downcase abbrev)))
                                     0
                                   2))
                       (key "creationDate")
                       (date ,(format-time-string "%FT%T%z"))
                       (key "flags")
                       (integer 0)      ; TODO No idea what this is...
                       (key "label")
                       (string "")
                       (key "lastUsed")
                       (date ,(format-time-string "%FT%T%z"))
                       (key "modificationDate")
                       (date ,(format-time-string "%FT%T%z"))
                       (key "plainText")
                       (string ,expansion)
                       (key "snippetType")
                       (integer 0)      ; TODO No idea what this is...
                       (key "useCount")
                       (integer ,count)
                       (key "uuidString")
                       (string ,(uuidgen-1))
                       )))


(defun insert-abbrev-defs-converted-to-text-expander (defs)
  (dolist (form (translate-abbrev-table-definitions defs))
    (insert (xmlgen form nil 0 t))
    (terpri (current-buffer))))


(defvar *max-exported-abbrev-length* 4)


(defun export-all-abbrevs-to-text-expander ()
  (interactive)
  (let ((buffer (generate-new-buffer "snippets.xml")))
    (switch-to-buffer buffer)
    (loop for (table defs) on (all-abbrevs) by #'cddr
          do (insert-abbrev-defs-converted-to-text-expander
              (if *max-exported-abbrev-length*
                  (remove-if (lambda (def)
                               (> (length (first def))
                                  *max-exported-abbrev-length*))
                             defs)
                defs)))))

