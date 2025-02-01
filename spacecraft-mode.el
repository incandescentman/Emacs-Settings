(defun smart-expand ()
  (interactive)

  (unless

    (or
       (looking-back "\)\n*")
(looking-back "[[:punct:]]*\)[ ]*[[:punct:]]*[\n\t ]*[[:punct:]]*>*")
(looking-back ":t[ ]*")
(looking-back "][\n\t ]*[[:punct:]]*[\n\t ]*") ; don't expand past closing square brackets ]

(looking-back ">[\n\t ]*[[:punct:]]*[\n\t ]*") ; don't expand past closing email addresses]


;; (looking-back "\\\w") ; for some reason this matches all words, not just ones that start with a backslash
)
    (expand-abbrev)
)
)

(require 'captain)   ;; load Captain

(defvar spacecraft-mode-map
  (let ((map (make-sparse-keymap)))
    ;; If you want to override some keys:
    ;; (define-key map (kbd ".") 'smart-period)
    ;; ...
    map)
  "Keymap for `spacecraft-mode'.")

(defun spacecraft-predicate ()
  "Should the captain auto-capitalize right now?"
  ;; Reuse your existing `auto-capitalize-predicate` checks:
  (and (not buffer-read-only)
       (functionp auto-capitalize-predicate)
       (funcall auto-capitalize-predicate)))

(defun spacecraft-sentence-start ()
  "Return the start of the current sentence.
By default, Captain uses `bounds-of-thing-at-point', but you can
override with your `my/beginning-of-sentence-p'."
  ;; E.g. if you want a naive approach, do:
  (car (bounds-of-thing-at-point 'sentence))

  ;; OR if you have something like `my/beginning-of-sentence-p`, you might
  ;; do a custom search.  For now, let's do the naive approach:
)

;;;***autoload
(define-minor-mode spacecraft-mode
  "A specialized mode for writing English prose with Captain for delayed capitalization."
  :init-value nil
  :lighter " SpaceCraft"
  :keymap spacecraft-mode-map
  (if spacecraft-mode
      (progn
        ;; 1) Enable Captain in this buffer
        (captain-mode 1)
        ;; 2) Set Captain's variables
        (setq-local captain-predicate #'spacecraft-predicate)
        (setq-local captain-sentence-start-function #'spacecraft-sentence-start)

        ;; 3) Turn on your 'smart punctuation' bindings:
        ;;   e.g. (define-key spacecraft-mode-map (kbd ".") 'smart-period)
        ;;        ...
        ;; Or rely on your existing global or org-mode bindings
        )
    ;; If turning off:
    (captain-mode -1)))

(setq never-downcase-words '("Internet" "Jay" "Dixit" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday" "York" "Canada" "I" "U" "I'm" "I'll" "I've" "I'd" "OK"))

(setq auto-capitalize-predicate
      (lambda ()
        (and
         (not (org-checkbox-p))
         (save-match-data
           (not (and
;; (org-or-orgalist-p)
                 (looking-back
                 "\\[\\[.*\\]\\][^.\n]*\\.?"))))
         (save-match-data
           (not (looking-back
                 "\\([Ee]\\.g\\|[Uu]\\.S\\|[Uu]\\.K\\|Ph\\.D\\|\\bal\\|Mr\\|Mrs\\|[M]s\\|cf\\|[N]\\.B\\|[U]\\.N\\|[E]\\.R\\|[M]\\.C\\|[Vv]S\\|[Ii]\.e\\|\\.\\.\\)\\.[^.\n]*\\|E.R\\|\!\"[ ]*\\|\?\"[ ]*"
                 (- (point) 20)))))))

(setq auto-capitalize-words '("fn" "\bI\b" "setq" "iPhone" "IPad" "nil" "use" "ediff" "btw" "nyc" "file" "http" "provide" "load" "require" "alias" "looking-at" "blockquote" "http" "https" "eBay" "omg" "zk" "http" "https" "looking" "or" "youarehere"))

(defun downcase-or-endless-downcase ()
(interactive)
(if

; If
(or
(looking-back "\\.\\.\\.[ ]*[\n\t ]*")
(looking-back "i.e.[ ]*")
(looking-back "[0-9]\.[ ]*")
(looking-back "e.g.[ ]*")
(looking-back "vs.[ ]*")
(looking-back "U.K.[ ]*")
(looking-back "U.S.[ ]*")
(looking-back "vs.[ ]*")
(looking-back "^")
)
    (call-interactively 'downcase-word); then
    (call-interactively 'endless/downcase); else

)
)

(defun capitalize-sentence ()
  (interactive)
(unless (my/beginning-of-sentence-p)
(org-backward-sentence))
  (endless/capitalize)
(org-forward-sentence 1)
(jay/right-char)
)
(define-key key-minor-mode-map (kbd "M-C") 'capitalize-word)

(defun downcase-sentence ()
  (interactive)
(unless (my/beginning-of-sentence-p)
(org-backward-sentence))
  (downcase-word 1)
(org-forward-sentence 1)
(jay/right-char)
)

(define-key key-minor-mode-map (kbd "M-L") 'downcase-sentence)

(defvar auto-capitalize--cap-next-word nil
  "Non-nil means the next typed word should be capitalized immediately.")

(defun auto-capitalize--maybe-capitalize-next-word ()
  "If `auto-capitalize--cap-next-word' is non-nil and we just typed a new word, capitalize it."
  (when auto-capitalize--cap-next-word
    (let ((ch (char-before (point))))
      (when (and ch (eq (char-syntax ch) ?w))
        ;; Upcase the character we just typed
        (save-excursion
          (backward-char 1)
          (replace-match (upcase (string ch)) t t))
        (setq auto-capitalize--cap-next-word nil)))))

;; (add-hook 'post-self-insert-hook #'auto-capitalize--maybe-capitalize-next-word)

(defun downcase-or-endless-downcase ()
(interactive)
(if

; If
(or
(looking-back "\\.\\.\\.[ ]*[\n\t ]*")
(looking-back "i.e.[ ]*")
(looking-back "[0-9]\.[ ]*")
(looking-back "e.g.[ ]*")
(looking-back "vs.[ ]*")
(looking-back "U.K.[ ]*")
(looking-back "U.S.[ ]*")
(looking-back "vs.[ ]*")
(looking-back "^")
)
    (call-interactively 'downcase-word); then
    (call-interactively 'endless/downcase); else

)
)

(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to text-mode."
  (let ((f "\\(%s\\)\\(%s\\)")
        (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space))))
        (replace-match rp nil nil nil 1))))

(defun endless/capitalize ()
  "Capitalize region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'capitalize-word)))

(defun endless/downcase ()
  "Downcase region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'downcase-word)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'upcase-word)))

(defun capitalize-or-endless/capitalize ()
(interactive)
(if

; If
(or
(looking-back "^")
)
    (call-interactively 'capitalize-word); then
    (call-interactively 'endless/capitalize); else

)
)

(global-set-key "\M-c" 'capitalize-or-endless/capitalize)
(global-set-key "\M-l" 'downcase-or-endless-downcase)
(global-set-key (kbd "M-u") 'endless/upcase)
(global-set-key (kbd "M-U") 'caps-lock-mode) ;; hell yes!! This is awesome!

(defun endless/upgrade ()
  "Update all packages, no questions asked."
  (interactive)
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)))

(defun smart-period-or-smart-space ()
"double space adds a period!"
(interactive)
  (if
(looking-back "[A-Za-z0-9] ")
(smart-period)
(smart-space)
))

(defun smart-space ()
  "Insert space and then clean up whitespace."
  (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))

;; (if (org-at-heading-p)
 ;;    (insert-normal-space-in-org-heading)

  (unless
      (or
(let ((case-fold-search nil)
(looking-back "\\bi\.e[[:punct:][:punct:]]*[ ]*") ; don't add extra spaces to ie.
)
(looking-back "\\bvs.[ ]*") ; don't add extra spaces to vs.
(looking-back "\\be\.\g[[:punct:]]*[ ]*") ; don't add extra spaces to eg.

(looking-back "^[[:punct:]]*[ ]*") ; don't expand previous lines - brilliant!

(looking-back ">") ; don't expand days of the week inside timestamps

(looking-back "][\n\t ]*") ; don't expand past closing square brackets ]
       ))
  (smart-expand))

(insert "\ ")
(just-one-space)
)




;; this is probably convuluted logic to invert the behavior of the SPC key when in org-heading
(defun insert-smart-space-in-org-heading ()
 "Insert space and then clean up whitespace."
 (interactive)
(unless
   (or
(looking-back "\\bvs.[ ]*") ; don't add extra spaces to vs.
(looking-back "\\bi\.e[[:punct:][:punct:]]*[ ]*") ; don't add extra spaces to ie.
(looking-back "\\be\.\g[[:punct:][:punct:]]*[ ]*") ; don't add extra spaces to eg.

(looking-back "^[[:punct:][:punct:]]*[ ]*") ; don't expand previous lines--brilliant!

(looking-back ">") ; don't expand days of the week inside timestamps

(looking-back "][\n\t ]*") ; don't expand past closing square brackets ]
    )
 (smart-expand))
(insert "\ ")
 (just-one-space))



(define-key org-mode-map (kbd "<SPC>") 'smart-space)
;; (define-key orgalist-mode-map (kbd "<SPC>") 'smart-period-or-smart-space)
(global-set-key (kbd "M-SPC") 'insert-space)
(define-key org-mode-map (kbd "<M-SPC>") 'insert-space)
;; (define-key orgalist-mode-map (kbd "<M-SPC>") 'insert-space)

(defun my/fix-space ()
  "Delete all spaces and tabs around point, leaving one space except at the beginning of a line and before a punctuation mark."
  (interactive)
(let (inhibit-modification-hooks)
  (just-one-space)
  (when (and (or
              (looking-back "^[[:space:]]+")
              (looking-back "-[[:space:]]+")
              (looking-at "[.,:;!?»)-]")
              (looking-back"( ")
              (looking-at " )")
              )
             (not (looking-back "^-[[:space:]]+"))
             (not (looking-back " - "))

)
    (delete-horizontal-space))))

(defun insert-space ()
  (interactive)
(if (org-at-heading-p)
(insert-smart-space-in-org-heading)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
  (insert " ")
))
(defun insert-normal-space-in-org-heading ()
 (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))
 (insert " ")
)
;; this is probably convuluted logic to invert the behavior of the SPC key when in org-heading


(defun insert-period ()
"Inserts a fuckin' period!"
 (interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))

 (insert ".")
)


(defun insert-comma ()
 (interactive)
(cond (mark-active
   (progn (delete-region (mark) (point)))))
 (insert ",")
)

(defun insert-exclamation-point ()
 (interactive)
(cond (mark-active
  (progn (delete-region (mark) (point)))))
 (insert "!")
)


(defun insert-colon ()
"Insert a goodamn colon!"
 (interactive)
(cond (mark-active
  (progn (delete-region (mark) (point)))))
 (insert ":")
)

(defun insert-question-mark ()
"Insert a freaking question mark!!"
 (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))
 (insert "?")
)

    (defun kill-clause ()
      (interactive)
      (smart-expand)
(when (or (looking-at ",")
          (looking-at ";")
          (looking-at ":"))
  (org-delete-char 1))
(when (or (looking-back ",")
     (looking-back ";")
     (looking-back ":"))
 (org-delete-backward-char 1))


(when (looking-back " ")
  (left-char 1))

      (if
	  (let ((sm (string-match "*+\s" (thing-at-point 'line)))) (and sm (= sm 0)))
	  (kill-line)

	(let ((old-point (point))
	      (kill-punct (my/beginning-of-sentence-p)))
	  ;; Stop at a period followed by a space, or the end of the line
	  (when (re-search-forward "--\\|[][,;:?!…\"”()}\\.]+\\|$" nil t)
	    (kill-region old-point
			 (if kill-punct
			     (match-end 0)
			   (match-beginning 0)))))
	(my/fix-space)
	(save-excursion
	  (when (my/beginning-of-sentence-p)
	    (capitalize-unless-org-heading)))

(cond
 ((looking-back "\\, \\, ")
 (new-org-delete-backward-char 2)
 (my/fix-space)
 t)

((looking-back "!\\. ")
 (new-org-delete-backward-char 2)
 (my/fix-space)
 t)

 ((looking-back ":: ")
 (new-org-delete-backward-char 2)
 (my/fix-space)
 t))

(when
    (looking-back "[[:punct:]]")
  (progn
(forward-char 1)
(my/fix-space)
(backward-char 1)))
    ;; fix a bug that leaves this: " ?"
    (when (looking-back " \\?")
        (left-char 1)
    (new-org-delete-backward-char 1)
    (right-char 1))


    ;; fix a bug that leaves this: " , "
    (when (looking-back " , ")
    (left-char 2)
    (my/fix-space)
    (right-char 2))

    ;; fix a bug that leaves this: ":, "
    (when (looking-back ":, ")
    (left-char 1)
    (delete-backward-char 1)
    (right-char 1))

    ;; fix a bug that leaves this: ",."
    (when (looking-back "\\,\\. ")
    (left-char 2)
    (delete-backward-char 1)
    (right-char 2)
    )


    ;; fix a bug that leaves this: ", . "
    (when (looking-back "\\, \\. ")
    (left-char 2)
    (delete-backward-char 2)
    (right-char 2)
    )


    ;; fix a bug that leaves this: " ; "
    (when
	(looking-back " [[:punct:]] ")
    (left-char 2)
    (delete-backward-char 1)
    (right-char 2)
    )




    (when
    (and
    (looking-back "---")
    (looking-at "-"))

    (delete-backward-char 4)
    (delete-char 1)
    (insert-space))

    ;; leave the cursor before the comma or period, not after it
    (when
    (looking-back "[[:punct:]] ")
    (left-char 2))
    (when
    (looking-back "[[:punct:]]")
    (left-char 1))



    ;; fix a bug that leaves this: ".,"
 (when
	(looking-at "\\.\\,")
 (delete-forward-char 1)
 )
;; works!!



  ;; fix a bug that leaves this: ":."
 (when
	(looking-at ":\\.")
 (delete-forward-char 1)
 )
;; works!!


;; a more general solution, haven't tested it yet:
;; (when
;;   (looking-at "[[:punct:]]\\.")
;; (delete-forward-char 1) )





    ;; when on a punctuation mark with a space before it, delete the space
    (when
	(and
    (looking-at "[[:punct:]]")
    (looking-back " ")
)
  (delete-backward-char 1))
    )

  (when
    (or
     (looking-at ":\\,")
     (looking-at ";\\,")
     (looking-at "\\,\\,")
     (looking-at "\\.\\.")
     (looking-at "\\,;")
     (looking-at "\\,:")
     (looking-at "\\?\\?")
)
(right-char 1)
      (delete-char 1)
      (left-char 1)
)
  ;; Add this near the end of the function, before the final right parenthesis
(when (looking-at ",")
  (when (looking-back ", ")
    (delete-backward-char 2)
    (insert ", "))))

(defvar *smart-punctuation-marks*
  ".,;:!?-")

(setq *smart-punctuation-exceptions*
  (list "?!" ".." "..." "............................................." "---" "--" ";;" "!!" "!!!" "??" "???" "! :" ". :" ") ; "))

(defun smart-punctuation (new-punct &optional not-so-smart)
  (smart-expand)
  (save-restriction
    (when (and (eq major-mode 'org-mode)
               (org-at-heading-p))
      (save-excursion
        (org-beginning-of-line)
        (let ((heading-text (fifth (org-heading-components))))
          (when heading-text
            (search-forward heading-text)
            (narrow-to-region (match-beginning 0) (match-end 0))))))
    (cl-flet ((go-back (regexp)
                (re-search-backward regexp nil t)
                (ignore-errors ; might signal `end-of-buffer'
                  (forward-char (length (match-string 0))))))
      (if not-so-smart
          ;; Simple path: just insert `new-punct`
          (let ((old-point (point)))
            (go-back "[^ \t]")
            (insert new-punct)
            (goto-char old-point)
            (forward-char (length new-punct)))
        ;; "Smart" punctuation
        (let ((old-point (point)))
          (go-back (format "[^ \t%s]\\|\\`" *smart-punctuation-marks*))
          (let ((was-after-space
                 (and (< (point) old-point)
                      (find ?\s (buffer-substring (point) old-point)))))
            ;; Attempt to match existing punctuation
            (if (re-search-forward
                 (format "\\([ \t]*\\)\\([%s]*\\)" *smart-punctuation-marks*)
                 nil t)
                (let* ((old-punct (match-string 2))
                       (was-after-punct (>= old-point (point))))
                  ;; Replace subexp #1 (the whitespace)
                  (replace-match "" nil t nil 1)
                  ;; Replace subexp #2 (the punctuation), if it exists
                  (when (match-beginning 2)
                    (replace-match
                     (or (when (and was-after-punct
                                    (not (string= old-punct "")))
                           ;; Possibly combine old punctuation with new
                           (let ((potential-new-punct (concat old-punct new-punct)))
                             (find-if (lambda (exception)
                                        (search potential-new-punct exception))
                                      *smart-punctuation-exceptions*)))
                         new-punct)
                     nil t nil 2))
                  ;; Restore spaces if we had them
                  (if was-after-space
                      (my/fix-space)
                    (when (looking-at "[ \t]*\\<")
                      (save-excursion (my/fix-space)))))
              ;; Fallback: if the re-search-forward fails, just insert punctuation
              (goto-char old-point)
              (insert new-punct)))))))
    (when (and (eq major-mode 'org-mode)
               (org-at-heading-p))
      ;; Possibly do more things for Org headings here...
      ))

(defun smart-period ()
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (unless (or (looking-back "\\bvs.[ ]*" nil)
              (looking-back "\\bi\\.e[[:punct:]]*[ ]*" nil)
              (looking-back "\\be\\.g[[:punct:]]*[ ]*" nil))
    (smart-punctuation ".")
    ;; Optionally, call `auto-capitalize--handler' or set a "cap next word" flag:
; (auto-capitalize--handler (point) (point) 0)
)
  (save-excursion
    (unless (or (looking-at "[ ]*$")
                (looking-at "\"[[:punct:]]*[ ]*$")
                (looking-at "\)[ ]*$"))
      (capitalize-unless-org-heading)))
  ;; If two periods or commas in a row, remove the second one:
  (when (or (and (looking-at "\\.")
                 (looking-back "\\." nil))
            (and (looking-at ",")
                 (looking-back "," nil)))
    (delete-char 1)))

(defun auto-capitalize--handler (beg end length)
  "The `after-change-functions' handler for `auto-capitalize-mode'."
  (when (and auto-capitalize-state
             (or (null auto-capitalize-predicate)
                 (funcall auto-capitalize-predicate)))
    ;; If we inserted punctuation recognized as an end-of-sentence...
    (when (and (eq this-command 'self-insert-command)
               (member (char-before end) '(?. ?! ??)))
; (setq auto-capitalize--cap-next-word t)
)

    ;; Original logic to capitalize the *previous* word if needed:
    (cond
     ((auto-capitalize--inserted-non-word-p beg end length)

      (auto-capitalize--capitalize-previous-word))
     ;; or yank logic...
     )))

(defun auto-capitalize--maybe-capitalize-next-word ()
  "If `auto-capitalize--cap-next-word' is non-nil and we're typing a new word, capitalize it."
  (when auto-capitalize--cap-next-word
    (let ((ch (char-before (point))))
      ;; If we typed a letter in a new word
      (when (and ch (eq (char-syntax ch) ?w))
        ;; Replace the just-typed letter with uppercase
        (save-excursion
          (backward-char 1)
          (let ((case-fold-search nil))
            (replace-match (upcase (string ch)) t t)))
;(setq auto-capitalize--cap-next-word nil)
))))

;; (add-hook 'post-self-insert-hook #'auto-capitalize--maybe-capitalize-next-word)

(define-key org-mode-map (kbd ".") 'smart-period)

(defun smart-comma ()
  (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))

  (smart-punctuation ",")
(unless
(or

(looking-at "\]*[[:punct:]]*[ ]*$")
(looking-at "[[:punct:]]*[ ]*$")
(looking-at "[ ]*I\\b")          ; never downcase the word "I"
(looking-at "[ ]*I\'")          ; never downcase the word "I'
(looking-at "[[:punct:]]*[ ]*\"")          ; beginning of a quote
)

(save-excursion (downcase-word 1)))
(when

;; if two periods or two commas in a row, delete the second one
(or
(and
(looking-at "\\.")
(looking-back "\\.")
)
(and
(looking-at ",")
(looking-back ",")
))
(delete-char 1)
)

)


(define-key org-mode-map (kbd ",") 'comma-or-smart-comma)
;; (define-key orgalist-mode-map (kbd ",") 'comma-or-smart-comma)

(defun smart-question-mark ()
  (interactive)
  (cond (mark-active
         (progn (delete-region (mark) (point)))))

  (smart-punctuation "?")
  (save-excursion
    (unless
        (or
         (looking-at "[ ]*$")
         (looking-at "\][[:punct:]]*[ ]*$")
         (looking-at "[[:punct:]]*[ ]*$")
         (looking-at "\"[[:punct:]]*[ ]*$")
         (looking-at "\)[ ]*$")
         (looking-at "\)")
         ) ; or
    (capitalize-unless-org-heading)
      ) ; unless
    ) ; save excursion
  ) ; defun

;; works!!

(define-key org-mode-map (kbd "?") 'smart-question-mark)
;; (define-key orgalist-mode-map (kbd "?") 'smart-question-mark)

(defun smart-exclamation-point ()
  (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))

  (smart-punctuation "!")
(save-excursion
(unless (looking-at "[ ]*$")
(capitalize-unless-org-heading))
))

(define-key org-mode-map (kbd "!") 'smart-exclamation-point)
;; (define-key orgalist-mode-map (kbd "!") 'smart-exclamation-point)

(defun smart-semicolon ()
  (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))
  (smart-punctuation ";")
(unless
(or
(looking-at "[[:punct:]]*[ ]*$")
(looking-at "[ ]*I\\b")     ; never downcase the word "I"
(looking-at "[ ]*I\'")     ; never downcase the word "I'
(looking-at "[[:punct:]]*[ ]*\"")     ; beginning of a quote
)

(save-excursion (downcase-word 1))))

(define-key org-mode-map (kbd ";") 'smart-semicolon)
;; (define-key orgalist-mode-map (kbd ";") 'smart-semicolon)

(defun smart-colon ()
  (interactive)
(cond (mark-active
  (progn (delete-region (mark) (point)))))
  (smart-punctuation ":")
(unless
(or
(looking-at "[[:punct:]]*[ ]*$")
(looking-at "[ ]*I\\b")     ; never downcase the word "I"
(looking-at "[ ]*I\'")     ; never downcase the word "I'
(looking-at "[[:punct:]]*[ ]*\"")     ; beginning of a quote
)

;; (save-excursion (downcase-word 1))
))


(define-key org-mode-map (kbd ":") 'colon-or-smart-colon)



(define-key org-mode-map (kbd ",") 'comma-or-smart-comma)
;; (define-key orgalist-mode-map (kbd ":") 'smart-colon)

(defun comma-or-smart-comma ()
(interactive)
(if
(or
(bolp)
(org-at-heading-p)
(looking-at " \"")
)
(insert ",")
(smart-comma))
)

(defun line-starts-with-hash-p ()
 (save-excursion
  (beginning-of-line)
  (looking-at-p "#")))

(defun colon-or-smart-colon ()
 (interactive)
 (if (or (bolp)
     (org-at-heading-p)
     (line-starts-with-hash-p))
   (insert ":")
  (smart-colon)))

(defun backward-kill-word-correctly ()
  "Kill word."
  (interactive)
(with-silent-modifications
  (if (re-search-backward "\\>\\W*[[:punct:]]+\\W*\\=" nil t)
      (kill-region (match-end 0) (match-beginning 0))
    (backward-kill-word 1))
  (my/fix-space)

;; I added this ↓↓↓ #######################
(when (and
(not (looking-back "--")) ; I added this
(not (looking-back "^"))) ; I added this
;; I added this ↑↑↑ #######################

(smart-space)
)
(my/fix-space
)))

(defun my/delete-backward ()
  "When there is an active region, delete it and then fix up the whitespace"
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-backward-char 1))
  (save-excursion
    (when (or (looking-at "[[:space:]]")
              (looking-back "[[:space:]]"))
(unless (looking-back "\\w ")
      (my/fix-space)))))

(defcustom capitalize-after-deleting-single-char nil
  "Determines whether capitalization should occur after deleting a single character.")

(defun my/delete-backward-and-capitalize ()
  "When there is an active region, delete it and then fix up the whitespace"
  (interactive)
(when (looking-back "^[*]+ ")
(kill-line 0)
(insert " ") ; this line is super hacky I put it here because when I tried to use "unless", the rest of the function, and then this at the end, it didn't work; however, this does produce the behavior I desire
)

  (let ((capitalize capitalize-after-deleting-single-char))
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (setf capitalize t))
      (new-org-delete-backward-char 1))
    (save-excursion
      (when (or (looking-at "[[:space:]]")
    (looking-back "[[:space:]]"))
;; unless there's already exactly one space between words, since I need to be able to delete backward past spaces
(unless (and
(looking-back "\\w ")
(looking-at "\\w")
)
  (my/fix-space))))
    (when (and capitalize (my/beginning-of-sentence-p))
      (save-excursion
        (capitalize-unless-org-heading))))
(when

(or
(and
(looking-at "\\.")
(looking-back "\\.")
)
(and
(looking-at ",")
(looking-back ",")
))
(delete-char 1)
)
)

(defun backward-kill-word-correctly-and-capitalize ()
  "Backward kill word correctly. Then check to see if the point is at the beginning of the sentence. If yes, then kill-word-correctly and endless/capitalize to capitalize the first letter of the word that becomes the first word in the sentence. Otherwise simply kill-word-correctly."
  (interactive)
(call-interactively 'backward-kill-word-correctly)
  (let ((fix-capitalization (my/beginning-of-sentence-p)))
    (when fix-capitalization
      (save-excursion (capitalize-unless-org-heading)))))

(defadvice capitalize-word (after capitalize-word-advice activate)
  "After capitalizing the new first word in a sentence, downcase the next word which is no longer starting the sentence."

  (unless

      (or
       (looking-at "[ ]*\"")          ; if looking at a quote? Might not work

       (looking-at "[[:punct:]]*[ ]*I\\b")          ; never downcase the word "I"
       (looking-at "[[:punct:]]*[ ]*I'")          ; never downcase words like I'm, I'd
       (looking-at "[[:punct:]]*[ ]*\"*I'")    ; never downcase words like I'm, I'd

(looking-at "[ ]*I\'")   ; never downcase the word "I'

       (looking-at "[[:punct:]]*[ ]*\"I\\b")          ; never downcase the word "I"
       (looking-at "[[:punct:]]*[ ]*OK\\b")          ; never downcase the word "OK"

       ;; (looking-at "\\") ; how do you search for a literal backslash?
       (looking-at (sentence-end))

       (looking-at "[[:punct:]]*[ ]*$") ; don't downcase past line break

       (looking-at "[[:punct:]]*[ ]*\"$") ; don't downcase past quotation then line break
       (looking-at "[[:punct:]]*[ ]*)$") ; don't downcase past a right paren then line break
       (looking-at "[[:punct:]]*[ ]*\")$") ; don't downcase past a quotation then a right paren then a line break

       (looking-at "[[:punct:]]*[ ]*http") ; never capitalize http

(looking-at "\"[[:punct:]]*[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"

(looking-at "\)[ ]*$") ; a right paren followed by "zero or more" whitespace, then end of line

(looking-at ")[ ]*$") ; a right paren followed by "zero or more" whitespace, then end of line
(looking-at ")$") ; a right paren followed by "zero or more" whitespace, then end of line

(looking-at "[ ]*-*[ ]*$") ; dashes at the end of a line


       (looking-at (user-full-name))

       )

    (save-excursion
      (downcase-word 1))))

(defun capitalize-unless-org-heading ()
  (interactive)
;(when capitalist-mode
  (unless
      (or
       (looking-at "[[:punct:]]*[\n\t ]*\\*")
       (let ((case-fold-search nil))
         (looking-at "[ ]*[\n\t ]*[[:punct:]]*[\n\t ]*[A-Z]")
         (looking-at "[A-Z].*"))
       (looking-at "[\n\t ]*[[:punct:]]*[\n\t ]*#\\+")
       (looking-at "[\n\t ]*[[:punct:]]*[\n\t ]*\(")
       (looking-at "[\n\t ]*[[:punct:]]*[\n\t ]*<")
       (looking-at "[\n\t ]*[[:punct:]]*[\n\t ]*file:")
       (looking-at "[\n\t ]*\\[fn")
       (looking-at "[\n\t ]*)$")
       (looking-at "[\n\t ]*\"$")
       (looking-at "\"[\n\t ]*$")
       (looking-at "[[:punct:]]*[ ]*http")
       (looking-at "[[:punct:]]*[ ]*\")$"); don't capitalize past
       (looking-at "[ ]*I\'")
       (looking-at
        (concat
         "\\("
         (reduce (lambda (a b) (concat a "\\|" b))
                 auto-capitalize-words)
         "\\)")))
    (capitalize-word 1)))
;)

(defun downcase-save-excursion ()
  (interactive)
(unless
(or
(looking-at "[[:punct:]]*[ ]*$")
(looking-at "[ ]*I\\b") ; never downcase the word "I"
(looking-at "[[:punct:]]*[ ]*[[:punct:]]*I'")  ; never downcase I'm I've etc.
(looking-at "[[:punct:]]*[ ]*$") ; zero or more whitespaces followed by zero or more punctuation followed by zero or more whitespaces followed by a line break
(looking-at "\"[[:punct:]]*[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"
(looking-at "\)[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"
(looking-at (sentence-end)) ; quotation mark followed by "zero or more whitespace then end of line?"
       (looking-at (user-full-name))


)
  (save-excursion
      (downcase-word 1))
  ))
