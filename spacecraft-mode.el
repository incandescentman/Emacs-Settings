(require 'captain)   ;; load Captain
(add-hook 'text-mode-hook
          (lambda ()
            (captain-mode 1)
            (setq captain-predicate (lambda () t))))

(add-hook 'prog-mode-hook
          (lambda ()
            (captain-mode 1)
            ;; E.g., only in comments:
            (setq captain-predicate
                  (lambda () (nth 4 (syntax-ppss))))))

(defun my-simple-sentence-start ()
  "Return a naive guess of sentence start: any punctuation plus whitespace,
or nil if none was found."
  (save-excursion
    (if (re-search-backward "[.?!]\\s-+" nil t)
        (progn
          (skip-chars-forward ".?! \t\r\n")
          (point))    ;; This is the new sentence start
      nil)))           ;; Otherwise, no sentence boundary found, so return nil

(defun my-sentence-start-with-bullets ()
  "Return the start of a sentence by punctuation or the line-beginning.
Skip capitalization if the line is an Org bullet."
  (save-excursion
    (let* ((found-punct
            (re-search-backward "[.?!]\\s-+" nil t))
           (pos
            (if found-punct
                (progn
                  (skip-chars-forward ".?! \t\r\n")
                  (point))
              ;; fallback to beginning-of-line
              (beginning-of-line)
              (point))))
      ;; If we're on a bullet, return nil so Captain doesn't capitalize:
      (when (looking-at-p "^[ \t]*[-+*][ \t]+")
        (setq pos nil))
      pos)))

(defun my-sentence-start-with-bullets-and-buffer-start ()
  (save-excursion
    (if (re-search-backward "[.?!]\\s-+" nil t)
        (progn
          (skip-chars-forward ".?! \t\r\n")
          (point))
      ;; Fallback: start-of-line or start-of-buffer
      (or
       (when (> (point) (point-min))
         (beginning-of-line)
         (unless (looking-at-p "^[ \t]*[-+*][ \t]+")
           (point)))
       ;; If that was nil, treat point-min as the boundary:
       (point-min)))))


(add-hook 'org-mode-hook
  (lambda ()
    (require 'captain)
    (captain-mode 1)
    ;; Only auto-capitalize if not in an Org src block:
    (setq captain-predicate
          (lambda () (not (org-in-src-block-p))))
    (setq captain-sentence-start-function
          #'my-sentence-start-with-bullets-and-buffer-start)))

(defvar spacecraft-mode-map
  (let ((map (make-sparse-keymap)))
    ;; If you want to override some keys:
    ;; (define-key map (kbd ".") 'smart-period)
    ;; ...
    map)
  "Keymap for `spacecraft-mode'.")


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

(defun looking-back-safe (regexp &optional limit noerror)
  "Like `looking-back' but no error if mismatch, up to LIMIT or `(line-beginning-position)'."
  (looking-back regexp (or limit (line-beginning-position)) (or noerror t)))

(setq never-downcase-words '("Internet" "Jay" "Dixit" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday" "York" "Canada" "I" "U" "I'm" "I'll" "I've" "I'd" "OK"))

(setq auto-capitalize-predicate
      (lambda ()
        (and
         (not (org-checkbox-p))
         (save-match-data
           (not (and
;; (org-or-orgalist-p)
                 (looking-back-safe
"\\[\\[[^]]*\\]\\]"))))

         (save-match-data
           (not (looking-back-safe
                 "\\([Ee]\\.g\\|[Uu]\\.S\\|[Uu]\\.K\\|Ph\\.D\\|\\bal\\|Mr\\|Mrs\\|[M]s\\|cf\\|[N]\\.B\\|[U]\\.N\\|[E]\\.R\\|[M]\\.C\\|[Vv]S\\|[Ii]\\.e\\|\\.\\.\\)\\.[^.\n]*\\|E.R\\|\\!\"[ ]*\\|\\?\"[ ]*"
                 (- (point) 20)))))))

(setq auto-capitalize-words '("fn" "\\bI\\b" "setq" "iPhone" "IPad" "nil" "use" "ediff" "btw" "nyc" "file" "http" "provide" "load" "require" "alias" "looking-at" "blockquote" "http" "https" "eBay" "omg" "zk" "http" "https" "looking" "or" "youarehere"))

(defun downcase-or-endless-downcase ()
  (interactive)
  (if
      (or
       (looking-back-safe "\\.\\.\\.[ ]*[\n\t ]*")
       (looking-back-safe "i.e.[ ]*")
       (looking-back-safe "[0-9]\\.[ ]*")
       (looking-back-safe "e.g.[ ]*")
       (looking-back-safe "vs.[ ]*")
       (looking-back-safe "U.K.[ ]*")
       (looking-back-safe "U.S.[ ]*")
       (looking-back-safe "vs.[ ]*")
       (looking-back-safe "^"))
      (call-interactively 'downcase-word)
    (call-interactively 'endless/downcase)))

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
  (with-silent-modifications
    ;; 1) If region is active, delete it
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))

    ;; 2) If allowed, expand
    (unless (or
             (looking-back "\\bi\\.e[[:punct:][:punct:]]*[ ]*" nil)
             (looking-back "\\bvs.[ ]*" nil)
             (looking-back "\\be\\.g[[:punct:]]*[ ]*" nil)
             (looking-back "^[[:punct:]]*[ ]*" nil)
             (looking-back ">" nil)
             (looking-back "][\n\t ]*" nil))
      (smart-expand))

    ;; 3) Insert a space, condense to one space
    (insert " ")
    (just-one-space))

  ;; 4) Now run post-self-insert-hook outside the silent block
  (run-hooks 'post-self-insert-hook))


;; this is probably convuluted logic to invert the behavior of the SPC key when in org-heading
(defun insert-smart-space-in-org-heading ()
 "Insert space and then clean up whitespace."
 (interactive)
(unless
   (or
(looking-back-safe "\\bvs.[ ]*") ; don't add extra spaces to vs.
(looking-back-safe "\\bi\\.e[[:punct:][:punct:]]*[ ]*") ; don't add extra spaces to ie.
(looking-back-safe "\\be\\.\\g[[:punct:][:punct:]]*[ ]*") ; don't add extra spaces to eg.

(looking-back-safe "^[[:punct:][:punct:]]*[ ]*") ; don't expand previous lines--brilliant!

(looking-back-safe ">") ; don't expand days of the week inside timestamps

(looking-back-safe "][\n\t ]*") ; don't expand past closing square brackets ]
    )
 (smart-expand))
(insert "\ ")
 (just-one-space))


(define-key org-mode-map (kbd "<SPC>") 'smart-space)
;; Or, if you just want a literal space:
;; (define-key org-mode-map (kbd "<SPC>") 'insert-space)

(global-set-key (kbd "M-SPC") 'insert-space)

(defun my/fix-space ()
  "Delete all spaces and tabs around point, leaving one space except at the beginning of a line and before a punctuation mark."
  (interactive)
(let (inhibit-modification-hooks)
  (just-one-space)
  (when (and (or
              (looking-back-safe "^[[:space:]]+")
              (looking-back-safe "-[[:space:]]+")
              (looking-at "[.,:;!?»)-]")
              (looking-back-safe"( ")
              (looking-at " )")
              )
             (not (looking-back-safe "^-[[:space:]]+"))
             (not (looking-back-safe " - "))

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
(when (or (looking-back-safe ",")
     (looking-back-safe ";")
     (looking-back-safe ":"))
 (org-delete-backward-char 1))


(when (looking-back-safe " ")
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
 ((looking-back-safe "\\, \\, ")
 (new-org-delete-backward-char 2)
 (my/fix-space)
 t)

((looking-back-safe "!\\. ")
 (new-org-delete-backward-char 2)
 (my/fix-space)
 t)

 ((looking-back-safe ":: ")
 (new-org-delete-backward-char 2)
 (my/fix-space)
 t))

(when
    (looking-back-safe "[[:punct:]]")
  (progn
(forward-char 1)
(my/fix-space)
(backward-char 1)))
    ;; fix a bug that leaves this: " ?"
    (when (looking-back-safe " \\?")
        (left-char 1)
    (new-org-delete-backward-char 1)
    (right-char 1))


    ;; fix a bug that leaves this: " , "
    (when (looking-back-safe " , ")
    (left-char 2)
    (my/fix-space)
    (right-char 2))

    ;; fix a bug that leaves this: ":, "
    (when (looking-back-safe ":, ")
    (left-char 1)
    (delete-backward-char 1)
    (right-char 1))

    ;; fix a bug that leaves this: ",."
    (when (looking-back-safe "\\,\\. ")
    (left-char 2)
    (delete-backward-char 1)
    (right-char 2)
    )


    ;; fix a bug that leaves this: ", . "
    (when (looking-back-safe "\\, \\. ")
    (left-char 2)
    (delete-backward-char 2)
    (right-char 2)
    )


    ;; fix a bug that leaves this: " ; "
    (when
	(looking-back-safe " [[:punct:]] ")
    (left-char 2)
    (delete-backward-char 1)
    (right-char 2)
    )




    (when
    (and
    (looking-back-safe "---")
    (looking-at "-"))

    (delete-backward-char 4)
    (delete-char 1)
    (insert-space))

    ;; leave the cursor before the comma or period, not after it
    (when
    (looking-back-safe "[[:punct:]] ")
    (left-char 2))
    (when
    (looking-back-safe "[[:punct:]]")
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
    (looking-back-safe " ")
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
  (when (looking-back-safe ", ")
    (delete-backward-char 2)
    (insert ", "))))

(defun smart-punctuation (new-punct &optional not-so-smart)
    (smart-expand)
    (save-restriction
      (when (and (eql major-mode 'org-mode)
                 (org-at-heading-p))
        (save-excursion
          (org-beginning-of-line)
          (let ((heading-text (fifth (org-heading-components))))
            (when heading-text
              (search-forward heading-text)
              (narrow-to-region (match-beginning 0) (match-end 0))))))
      (cl-flet ((go-back (regexp)
                  (re-search-backward regexp nil t)
                  (ignore-errors      ; might signal `end-of-buffer'
                    (forward-char (length (match-string 0))))))
        (if not-so-smart
            (let ((old-point (point)))
              (go-back "[^ \t]")
              (insert new-punct)
              (goto-char old-point)
              (forward-char (length new-punct)))
          (let ((old-point (point)))
            (go-back (format "[^ \t%s]\\|\\`" *smart-punctuation-marks*))
            (let ((was-after-space (and (< (point) old-point)
                                        (find ?  (buffer-substring (point) old-point)))))
              (re-search-forward (format "\\([ \t]*\\)\\([%s]*\\)"
                                         *smart-punctuation-marks*)
                                 nil t)
              (let* ((old-punct (match-string 2))
                     (was-after-punct (>= old-point (point))))
                (replace-match "" nil t nil 1)
                (replace-match (or (when (and was-after-punct
                                              (not (string= old-punct "")))
                                     (let ((potential-new-punct (concat old-punct new-punct)))
                                       (find-if (lambda (exception)
                                                  (search potential-new-punct exception))
                                                *smart-punctuation-exceptions*)))
                                   new-punct)
                               nil t nil 2)
                (if was-after-space
                    (my/fix-space)
                  (when (looking-at "[ \t]*\\<")
                    (save-excursion (my/fix-space))))))))))
    (when (and (eql major-mode 'org-mode)
               (org-at-heading-p))
))

(defvar *smart-punctuation-marks*
  ".,;:!?-")

(setq *smart-punctuation-exceptions*
  (list "?!" ".." "..." "............................................." "---" "--" ";;" "!!" "!!!" "??" "???" "! :" ". :" ") ; "))

(defun smart-period ()
  (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))
(unless
      (or
(looking-back "\\bvs.[ ]*") ; Don't add extra periods to vs.
(looking-back "\\bi\.e[[:punct:]]*[ ]*") ; don't add extra periods to ie.
(looking-back "\\be\.\g[[:punct:]]*[ ]*") ; don't add extra periods to eg.

       )
  (smart-punctuation ".")
(run-hooks 'post-self-insert-hook)
)
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

;; if two periods or two commas in a row, delete the second one
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


(define-key org-mode-map (kbd ".") 'smart-period)


(define-key org-mode-map (kbd ".") 'smart-period)

(defun smart-comma ()
  (interactive)
(cond (mark-active
 (progn (delete-region (mark) (point)))))

  (smart-punctuation ",")
(run-hooks 'post-self-insert-hook)
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
         (looking-at "\\][[:punct:]]*[ ]*$")
         (looking-at "[[:punct:]]*[ ]*$")
         (looking-at "\"[[:punct:]]*[ ]*$")
         (looking-at ")[ ]*$")
         (looking-at "\\)")
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
(looking-at "[ ]*I\\'")     ; never downcase the word "I'
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
(looking-at "[ ]*I\\'")     ; never downcase the word "I'
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
(not (looking-back-safe "--")) ; I added this
(not (looking-back-safe "^"))) ; I added this
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
              (looking-back-safe "[[:space:]]"))
(unless (looking-back-safe "\\w ")
      (my/fix-space)))))

(defcustom capitalize-after-deleting-single-char nil
  "Determines whether capitalization should occur after deleting a single character.")

(defun my/delete-backward-and-capitalize ()
  "When there is an active region, delete it and then fix up the whitespace"
  (interactive)
(when (looking-back-safe "^[*]+ ")
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
    (looking-back-safe "[[:space:]]"))
;; unless there's already exactly one space between words, since I need to be able to delete backward past spaces
(unless (and
(looking-back-safe "\\w ")
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
(looking-back-safe "\\.")
)
(and
(looking-at ",")
(looking-back-safe ",")
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

(looking-at ")[ ]*$") ; A right paren followed by "zero or more" whitespace, then end of line

(looking-at ")[ ]*$") ; a right paren followed by "zero or more" whitespace, then end of line
(looking-at ")$") ; a right paren followed by "zero or more" whitespace, then end of line

(looking-at "[ ]*-*[ ]*$") ; dashes at the end of a line


       (looking-at (user-full-name))

       )

    (save-excursion
      (downcase-word 1))))


(defadvice capitalize-word (after capitalize-word-advice activate)
  "After capitalizing the new first word in a sentence, downcase the
next word if it's no longer the start of the sentence. We skip downcasing
certain words/contexts (like \"I\")."

  (unless (or
           ;; If looking at a quote right after capitalizing, skip downcasing
           (looking-at "[ ]*\"")

           ;; Never downcase the word "I"
           (looking-at "[[:punct:]]*[ ]*I\\b")
           (looking-at "[[:punct:]]*[ ]*I'")
           (looking-at "[[:punct:]]*[ ]*\"I\\b")
           (looking-at "[[:punct:]]*[ ]*\"I'")

           ;; Or the word "OK"
           (looking-at "[[:punct:]]*[ ]*OK\\b")

           ;; Don't downcase if we're at the end of a sentence/line
           (looking-at (sentence-end))
           (looking-at "[[:punct:]]*[ ]*$")      ; line break
           (looking-at "\"[[:punct:]]*[ ]*$")    ; quote + line break
           (looking-at ")[ ]*$")                ; right paren + line break
           ;; (looking-at ")[ ]*$") <-- removed to avoid unmatched )
           (looking-at "\")[ ]*$")              ; quote + right paren + line break
(looking-at ")[[:space:]]*$")    ; Right paren, optional whitespace to EOL
(looking-at "\")[[:space:]]*$")  ; Quote, then right paren, optional whitespace to EOL
           (looking-at "[[:punct:]]*[ ]*\")$")   ; alternate right-paren check
           (looking-at "[[:punct:]]*[ ]*http")   ; "http" etc.

           ;; An optional check for your full name (if you never want it downcased)
           (looking-at (user-full-name)))

    ;; Move ahead by one word and downcase it
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
       (looking-at "[ ]*I\\'")
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
(looking-at ")[ ]*$") ; a quotation mark followed by "zero or more whitespace then end of line?"
(looking-at (sentence-end)) ; quotation mark followed by "zero or more whitespace then end of line?"
       (looking-at (user-full-name))


)
  (save-excursion
      (downcase-word 1))
  ))

(defun pasteboard-paste-no-spaces-with-smart-quotes ()
  "Paste from system clipboard with no extra spaces, but still replace smart quotes and links.

Useful in prose when you're adjacent to punctuation but still want the text cleaned up."
  (interactive)
  (let ((beg (point)))
    ;; Do a raw paste with no extra spaces:
    (pasteboard-paste-no-spaces)
    ;; Now do the same transformations you'd do in pasteboard-paste-without-smart-quotes:
    (replace-smart-quotes beg (point))
    (convert-markdown-links-to-org-mode beg (point))))


(defun pasteboard-paste-spaces-maybe ()
  "Paste from pasteboard, choosing logic based on mode (prose vs. code) and punctuation.

- In prose (Org-mode w/o `org-config-files-local-mode`, or a mode derived from `text-mode`):
 - If near punctuation, call `pasteboard-paste-no-spaces-with-smart-quotes`.
 - Else, call `pasteboard-paste-without-smart-quotes`.

- In code (any other mode or Org with `org-config-files-local-mode`):
 - Always call `pasteboard-paste-no-spaces` (raw, no quote cleanup)."
  (interactive)
  (if (or (and (eq major-mode 'org-mode)
               (not (bound-and-true-p org-config-files-local-mode)))
          (derived-mode-p 'text-mode))
      ;; Prose branch
      (let* ((prev-char (char-before))
             (next-char (char-after))
             (char-set '(?: ?' ?( ?) ?| ?[ ?] ?/ ?\\ ?\" ?= ?< ?> ?{ ?}))
             (near-punctuation (or (member prev-char char-set)
                                   (member next-char char-set))))
        (if near-punctuation
            (pasteboard-paste-no-spaces-with-smart-quotes)
          (pasteboard-paste-without-smart-quotes)))
    ;; Code branch
    (pasteboard-paste-no-spaces)))

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

(remove-hook 'post-self-insert-hook #'captain--run)


;; If you only want Captain in Org, with some constraints:
;;(add-hook 'org-mode-hook
;; (lambda ()
;; (captain-mode 1)
;; (setq captain-predicate
;; (lambda ()
                    ;; For instance, don't do it in src blocks:
;; (not (org-in-src-block-p))))))
