;;; auto-capitalize.el -- Automatically capitalize (or upcase) words -*- lexical-binding: t; -*-

;; Copyright 1998,2001,2002,2005 Kevin Rodgers

;; This project was originally copied from:
;; https://www.emacswiki.org/emacs/auto-capitalize.el
;; and updated to work reliably in Emacs 24.3 and above.

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Maintainer: Yuta Yamada <cokesboy at gmail.com>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))

;; Created: 20 May 1998
;; Version: 2.21
;; Keywords: text, wp, convenience
;; URL: https://github.com/yuutayamada/auto-capitalize-el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; Commentary:

;; `auto-capitalize-mode' is a minor mode that automatically capitalizes
;; the first word at the beginning of a paragraph or sentence when a
;; subsequent whitespace or punctuation character is inserted.

;; Additionally, any word listed in `auto-capitalize-words' is automatically
;; capitalized or upcased, even in mid-sentence.  Lowercase entries in that
;; list will *not* be modified.

;; To install:
;;
;; 1. Put this file in your `load-path`
;; 2. (require 'auto-capitalize)
;; 3. Optionally add hooks such as:
;;
;;    (add-hook 'text-mode-hook #'turn-on-auto-capitalize-mode)
;;
;; You can customize various options using:
;;
;;    M-x customize-group RET auto-capitalize RET
;;
;; Enjoy!

;;; Code:

(require 'cl-lib)
(require 'rx)

(defgroup auto-capitalize nil
  "Automatically capitalize words in text."
  :group 'convenience
  :prefix "auto-capitalize-")

;;; User options

(defcustom auto-capitalize-ask nil
  "If non-nil, ask for confirmation before capitalizing."
  :group 'auto-capitalize
  :type 'boolean)

(defcustom auto-capitalize-yank nil
  "If non-nil, also capitalize the first word of yanked sentences."
  :group 'auto-capitalize
  :type 'boolean)

(defcustom auto-capitalize-words '("I")
  "List of proper nouns or acronyms that should be auto-capitalized/upcased.
If an entry in this list is uppercase or mixed case, it is always enforced.
A lowercase entry in this list is never changed (treated as a word to ignore)."
  :group 'auto-capitalize
  :type '(repeat (string :tag "Capitalized or Upcased Word")))

(defcustom auto-capitalize-predicate
  #'auto-capitalize-default-predicate-function
  "Function that decides whether auto-capitalization should apply.
This function is called with no arguments whenever a change occurs
and should return non-nil if auto-capitalization is allowed in the
current context."
  :group 'auto-capitalize
  :type '(choice (function :tag "Predicate function")
                 (const :tag "No predicate" nil)))

(defcustom auto-capitalize-allowed-chars '(?\  ?, ?. ?? ?' ?: ?\; ?- ?!)
  "Characters after which auto-capitalization is allowed.
If nil, no restriction is imposed based on the preceding inserted character."
  :group 'auto-capitalize
  :type '(choice (const :tag "No restriction" nil)
                 (repeat :tag "Allowed characters" character)))

(defcustom auto-capitalize-inhibit-buffers '("*scratch*")
  "List of buffer names in which to inhibit auto-capitalization."
  :group 'auto-capitalize
  :type '(repeat (string :tag "Buffer name")))

(defcustom auto-capitalize-predicate-functions nil
  "Hook of additional predicate functions for auto-capitalization.
Each function is called with no arguments and should return t if
capitalization should apply, nil otherwise.  If any function returns
nil, auto-capitalization is skipped."
  :group 'auto-capitalize
  :type '(choice (const :tag "No additional predicates" nil)
                 (repeat :tag "List of predicate functions" function)))

(defcustom auto-capitalize-aspell-file nil
  "Path to an aspell personal dictionary file (e.g., `~/.aspell.en.pws`).
When set, capitalized words from that dictionary are merged into
`auto-capitalize-words` at startup."
  :group 'auto-capitalize
  :type '(choice (const :tag "None" nil)
                 (file :tag "Aspell dictionary file")))

;;; Internal variables

(defvar auto-capitalize-state nil
  "Buffer-local state indicating whether `auto-capitalize-mode' is active.
A non-nil value means auto-capitalization is on.")

(defvar auto-capitalize--match-data nil
  "Internal storage for match data in yank-based capitalization.")

(defconst auto-capitalize-regex-lower "[[:lower:]]+"
  "Regex matching a purely lowercase word.")

(defconst auto-capitalize-regex-verify
  "\\<\\([[:upper:]]?[[:lower:]]+\\.\\)+\\="
  "Regex to detect abbreviations like \"e.g.\" or \"i.e.\"")

(defvar auto-capitalize-avoid-words-regex
  (rx (not (syntax word)) (or "e.g." "i.e." "vs.") (0+ " "))
  "Regex of contexts in which to avoid auto-capitalization.")

;;
;; Minor mode definition
;;

;;;***autoload
(define-minor-mode auto-capitalize-mode
  "Toggle Auto-Capitalize mode in the current buffer.
When enabled, the first word of a sentence or paragraph is capitalized
automatically upon typing a subsequent whitespace or punctuation character.
Also capitalizes or upcases any words in `auto-capitalize-words'.

If the optional prefix ARG is positive, turn on.  If zero or negative, turn off."
  :lighter " ACap"
  (if (or (not auto-capitalize-mode)
          buffer-read-only
          (member (buffer-name) auto-capitalize-inhibit-buffers))
      (progn
        (setq-local auto-capitalize-state nil)
        (remove-hook 'after-change-functions #'auto-capitalize--handler t))
    (setq-local auto-capitalize-state t)
    (add-hook 'after-change-functions #'auto-capitalize--handler nil t)))

;;;***autoload
(defun turn-on-auto-capitalize-mode ()
  "Turn on `auto-capitalize-mode' unconditionally in the current buffer."
  (interactive)
  (auto-capitalize-mode 1))

;;;***autoload
(defun turn-off-auto-capitalize-mode ()
  "Turn off `auto-capitalize-mode' unconditionally in the current buffer."
  (interactive)
  (auto-capitalize-mode -1))

;;;***autoload
(defun enable-auto-capitalize-mode ()
  "Enable auto-capitalization, but set `auto-capitalize-ask' to t.
This means the user is asked for confirmation before actually capitalizing."
  (interactive)
  (setq auto-capitalize-ask t)
  (auto-capitalize-mode 1))

;;
;; Core logic
;;

(defun auto-capitalize-default-predicate-function ()
  "Default predicate for `auto-capitalize-predicate'.
Return t if in an appropriate buffer context for capitalization.
This disallows read-only buffers, minibuffers, and imposes optional
restrictions via `auto-capitalize-allowed-chars' and `auto-capitalize-predicate-functions'."
  (and (not buffer-read-only)
       (not (minibufferp))
       ;; If in a prog-mode, only capitalize if in a string or comment.
       (if (derived-mode-p 'prog-mode)
           (let ((syntax-state (syntax-ppss)))
             (nth 8 syntax-state))  ;; non-nil if inside string/comment
         t)
       (or (null auto-capitalize-allowed-chars)
           (member last-command-event auto-capitalize-allowed-chars))
       ;; Inhibit modes like comint:
       (not (derived-mode-p 'comint-mode))
       ;; Check additional user-defined predicates.
       (run-hook-with-args-until-failure 'auto-capitalize-predicate-functions)
       ;; Check for a major-mode-specific function named
       ;; auto-capitalize-predicate-<major-mode> if it exists:
       (let ((fn (intern (format "auto-capitalize-predicate-%s" major-mode))))
         (if (fboundp fn)
             (funcall fn)
           t))))

(defun auto-capitalize--handler (beg end length)
  "The `after-change-functions' handler for `auto-capitalize-mode'.
Capitalizes the previous word if warranted.  Capitalization logic:
- If user inserted a non-word character (like whitespace or punctuation),
  and the preceding chunk of text matches a lowercased word at sentence
  boundary, capitalize.
- If `auto-capitalize-yank' is set, also handle capitalizing first words
  within newly yanked text."
  (when (and auto-capitalize-state
             (or (null auto-capitalize-predicate)
                 (funcall auto-capitalize-predicate)))
    (cond
     ;; 1) Self-inserting non-word character
     ((auto-capitalize--inserted-non-word-p beg end length)
      (when (and (> beg (point-min))
                 (eq (char-syntax (char-before beg)) ?w))
        (auto-capitalize--capitalize-previous-word)))
     ;; 2) Yank
     ((and auto-capitalize-yank
           (memq this-command '(yank yank-pop)))
      (auto-capitalize--capitalize-yanked beg end)))))

(defun auto-capitalize--inserted-non-word-p (beg end length)
  (or
   ;; If it's one of our custom commands, skip the length checks:
   (and (memq this-command '(smart-period smart-comma smart-question-mark
                             smart-exclamation-point smart-colon smart-semicolon))
        (let ((ch (char-before end)))
          ;; If the last inserted char is punctuation, consider it "non-word"
          (and ch (not (eq (char-syntax ch) ?w)))))
   ;; Otherwise, the old checks for real self-insert:
   (and (eq this-command 'self-insert-command)
        (= length 0)
        (= (- end beg) 1)
        (let ((ch (char-before end)))
          (not (eq (char-syntax ch) ?w))))))


(defun auto-capitalize--capitalize-yanked (beg end)
  "Capitalize newly yanked text between BEG and END if it starts sentences.
Tries to mimic self-insert triggers over the newly inserted text."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\Sw" end t)
      (setq auto-capitalize--match-data (match-data))
      (let* ((char-inserted (char-after (match-beginning 0))))
        (when char-inserted
          (set-match-data auto-capitalize--match-data)
          (save-excursion
            (goto-char (match-beginning 0))
            (when (eq (char-syntax (char-before)) ?w)
              (auto-capitalize--capitalize-previous-word))))))))

(defun auto-capitalize--capitalize-previous-word ()
  "Perform capitalization checks on the word immediately before point."
  (save-excursion
    (backward-word 1)
    (unless (auto-capitalize--avoid-word-context-p)
      (save-match-data
        (let ((word-start (point))
              (text-start  (auto-capitalize--back-to-text-start)))
          (cond
           ;; If the preceding word matches one in `auto-capitalize-words'
           ;; (case-insensitive), enforce that casing.
           ((and auto-capitalize-words
                 (looking-at (concat "\\("
                                     (mapconcat #'downcase
                                                auto-capitalize-words
                                                "\\|")
                                     "\\)\\>")))
            (auto-capitalize--enforce-user-specified
             (match-beginning 1) (match-end 1)))
           ;; Otherwise, if it looks like sentence start, do normal capitalize.
           ((auto-capitalize--capitalizable-p text-start word-start)
            (undo-boundary)
            (capitalize-word 1))))))))

(defun auto-capitalize--avoid-word-context-p ()
  "Return non-nil if the previous word is in an avoid context (e.g. e.g., i.e.)."
  (and auto-capitalize-avoid-words-regex
       (looking-back auto-capitalize-avoid-words-regex nil)))

(defun auto-capitalize--enforce-user-specified (m-beg m-end)
  "If matched text from M-BEG to M-END is in `auto-capitalize-words', enforce that case."
  (let ((found (buffer-substring m-beg m-end)))
    (unless (member found auto-capitalize-words)
      (undo-boundary)
      (replace-match
       (cl-find found auto-capitalize-words
                :test #'string-equal
                :key  #'downcase)
       t t))))

(defun auto-capitalize--capitalizable-p (text-start word-start)
  "Return non-nil if the word at WORD-START should be capitalized as sentence start.
TEXT-START is where we consider the sentence/paragraph boundary to begin.
We check:
- Are we at the beginning of a line/paragraph/sentence?
- Is it purely lowercase?
- Are we allowed to capitalize (`auto-capitalize-ask` or not)?"
  (goto-char text-start)
  (and
   (or (bobp)
       (and (= (current-column) left-margin)
            (or (looking-back paragraph-separate nil)
                (looking-back paragraph-start nil)))
       (save-excursion
         (narrow-to-region (point-min) word-start)
         (and (re-search-backward (sentence-end) nil t)
              (= (match-end 0) text-start)
              (let ((previous-char (char-before text-start)))
                (or (eq previous-char ?\s)
                    (eq previous-char ?\n))))))
   (save-excursion
     (goto-char word-start)
     (looking-at auto-capitalize-regex-lower))
   (or (not auto-capitalize-ask)
       (y-or-n-p (format "Capitalize \"%s\"? "
                         (buffer-substring word-start (match-end 0)))))))

(defun auto-capitalize--back-to-text-start ()
  "Move point back over surrounding punctuation/quotes and return new position."
  (while (or (cl-minusp (skip-chars-backward "\""))
             (cl-minusp (skip-syntax-backward "\"("))))
  (point))

;;
;; Aspell dictionary integration
;;

(defun auto-capitalize--read-file-as-string (file)
  "Return contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun auto-capitalize--aspell-capital-words (file)
  "Return a list of capitalized words found in aspell personal dictionary FILE."
  (when (file-exists-p file)
    (cl-loop
     for line in (split-string (auto-capitalize--read-file-as-string file) "\n" t)
     if (string-match-p "[[:upper:]]" line)
     collect line)))

;;;***autoload
(defun auto-capitalize-merge-aspell-words (&optional file)
  "Merge capitalized words from FILE into `auto-capitalize-words'.
If FILE is nil, use `auto-capitalize-aspell-file'."
  (let ((f (or file auto-capitalize-aspell-file)))
    (when (and f (file-exists-p f))
      (setq auto-capitalize-words
            (append auto-capitalize-words (auto-capitalize--aspell-capital-words f))))))

;;;***autoload
(defun auto-capitalize-setup ()
  "Merge aspell words and automatically enable `auto-capitalize-mode' after changes.
Call this in your init file if you want to globally set up Auto-Capitalize."
  (auto-capitalize-merge-aspell-words)
  (add-hook 'after-change-major-mode-hook #'auto-capitalize-mode))

;;
;; Org-specific predicate
;;
(with-eval-after-load 'org
  (defun auto-capitalize-predicate-org-mode ()
    "Allow auto-capitalization in Org buffers except inside src blocks."
    (not (and (fboundp 'org-in-src-block-p)
              (org-in-src-block-p)))))

;;
;; SKK-specific predicate
;;
(with-eval-after-load 'skk
  (add-hook
   'auto-capitalize-predicate-functions
   (lambda ()
     (or (not (bound-and-true-p skk-mode))
         (and (fboundp 'skk-current-input-mode)
              (eq 'latin (skk-current-input-mode)))))))

(provide 'auto-capitalize)

;;; auto-capitalize.el ends here

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

(add-hook 'post-self-insert-hook #'auto-capitalize--maybe-capitalize-next-word)

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
    (delete-horizontal-space)))

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
  (list "?!" ".." "..." "............................................." "--" ";;" "!!" "!!!" "??" "???" "! :" ". :" ") ; "))

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


(defun auto-capitalize--handler (beg end length)
  "The `after-change-functions' handler for `auto-capitalize-mode'."
  (when (and auto-capitalize-state
             (or (null auto-capitalize-predicate)
                 (funcall auto-capitalize-predicate)))
    ;; If we inserted punctuation recognized as an end-of-sentence...
    (when (and (eq this-command 'self-insert-command)
               (member (char-before end) '(?. ?! ??)))
      (setq auto-capitalize--cap-next-word t))

    ;; Original logic to capitalize the *previous* word if needed:
    (cond
     ((auto-capitalize--inserted-non-word-p beg end length)
      ...
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
        (setq auto-capitalize--cap-next-word nil)))))))

(add-hook 'post-self-insert-hook #'auto-capitalize--maybe-capitalize-next-word)

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
))

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
(when capitalist-mode
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
    (capitalize-word 1))))

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
