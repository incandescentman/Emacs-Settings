;;; poetry.el --- Poetry writing aids  -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE COMMENTS BELOW ARE EXTENSIVE BUT IT WILL PAY TO READ THEM  ;;
;; IF YOU WANT TO KNOW WHAT POETRY.EL CAN AND CAN'T DO!           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEFORE YOU DO ANYTHING MAKE SURE YOU HAVE THE LATEST VERSION.
;; POETRY.ZIP IS *NO LONGER* THE LATEST DISTRIBUTION!

;;  Download this: http://www.bobnewell.net/filez/poetry.el

;; Somehow I got interested in sonnet writing, and I wanted to do it
;; right. I discovered the Windoze freeware Verse Perfect and was
;; impressed, but I wanted something for emacs. This is now pretty
;; close to matching the poetry parts. There are no thesauri or
;; on-line regular dictionaries/lookups here. There are other emacs
;; tools for those. Look for dict.el, thesaurus.el, etc. There's no
;; point in my duplicating them.

;; Tested only on Linux Mint 17 and Emacs 24.3. Will probably work on
;; any reasonable distro and reasonable Emacs. Windoze?  Mac? I don't
;; use these so I don't know. You would need an equivalent to the
;; 'rhyme' command line utility and/or w3m. If you use one of these
;; systems and get this to work please let me know.

;; Comments are welcome. Send to bobnewell@bobnewell.net. But I don't
;; promise anything and this isn't an offer of support. (There, I've
;; covered myself.)

;; I'm especially interested in cases that give rise to incorrect foot
;; counts. For instance, one major problem is compound words with an
;; embedded silent 'e' like 'something' or 'moreover'. I have a
;; partial fix for this but it's limited to words with certain
;; patterns (see comments in code below) to limit side-effect errors,
;; such as scanning 'together' as 'toge-ther' and making the first e
;; silent. The downside is that longer words like 'heretofore' are not
;; scanned correctly.

;; But even harder to fix are words like 'graven' which counts
;; correctly as two, but may be intended by the poet to be pronounced
;; "grav'n" with just one foot. Another example from Tennyson is 'many
;; a' which the program counts correctly as three, but Tennyson
;; seemingly meant to be sounded as two: 'man-ya'. The fix here may
;; actually be dependent upon the century in which the poem was
;; written.

;; Please let me know what you find yourself, and if you can suggest
;; specific rules or rule corrections for better counting. And, an
;; overall better approach would be welcome! I've got a lot of kludged
;; rules right now and this gets more fragile with ev'ry change.

;; Foot counting is tested mostly against Tennyson's "Idylls of the
;; King" which turns out to be a pretty good (tough) test text with
;; rather strict iambic pentameter. poetry.el does surprisingly well,
;; and gets around 90% correct.  Tennyson made the odd counting error
;; himself (though very few), and at this point most of poetry.el's
;; errors are poetic elisions or other pronounciation variants as
;; described above, if you really can call these scanning errors.
;; This is actually pretty good, and better than many other scanners
;; that I've seen. Note that this means about 90% of the iambic
;; pentameter /lines/ are counted correctly. As there is generally one
;; error per incorrect line, something over 97% of words are counted
;; correctly. Are these stats good enough to make for a useful tool
;; for /writing/ poetry?  Maybe. You tell me!

;; Once again, the latest version should always be at

;;  http://www.bobnewell.net/filez/poetry.el

;; I don't plan on announcing new versions unless they are really
;; a major change.

(defvar poetry-version-no "0.14 alpha")

;; 2015-03-02 0.14 Tweaks per Stephan's message. Now distributed
;;                 as .el rather than .zip; remove refs to variable
;;                 poetry--line-number-cache; change 2nd arg from t to
;;                 nil in call to jit-lock-register.
;;                 A little more embedded silent 'e' tuning. Did it get
;;                 better or worse?
;; 2015-02-22 0.13 Attempt to partially solve embedded silent 'e'
;;                 problem. Has side effects and is limited in scope.
;;                 Made corrections to 'le' inversion, for plurals
;;                 and 'le' preceded by vowel.
;; 2015-02-21 0.12 Fixed unexplained error in words like 'queen'.
;;                 Fixed some errors for -ing ending preceeded
;;                 by a consonant and a non-u vowel.
;;                 Fixed some errors in words ending in silent 'e'
;;                 followed by 'ing', but the fix may have side effects.
;;                 Did a general fix for plurals of silent 'e' endings
;;                 not preceded by 'c' or 's', but the fix also may
;;                 have side effects.
;; 2015-02-20 0.11 Added version number function poetry-version.
;;                 Tweaked comments and a few definitions.
;; 2015-02-19 0.10 Completely new and much better way of geting
;;                 foot counts, though nothing like perfect
;;                 and never will be.
;;                 Undid 'let' constructs to allow easier
;;                 debugging. Bad practice? Sure.
;;                 Removed dependence on 'cl'.
;;                 Version jump represents change from 'pre-alpha'
;;                 to 'alpha' status.
;; 2015-02-18 0.05 Some expansion of comments.
;;            0.04 Fix rhyme words display space issue.
;;                 Added rhyming weblinks.
;; 2015-02-15 0.03 Fix failure to count one syllable word
;;                 that ends in 'e'.
;;                 Complete first cut at verse form display.
;; 2015-02-14 0.02 Begin to add verse forms. Incomplete
;;                 display implementation.
;; 2015-02-13 0.01 Revised to use nlinum.el and combine
;;                 into one file.
;; 2015-02-12 0.00 Initial coding and release.
;;                 Bob Newell, Honolulu, Hawai`i.

;; Three things are here right now.

;; 1. A poetry-mode that shows the foot count for each line in the
;; left margin. M-x poetry mode to turn it on and the same to turn it
;; off.

;; 2. A way to get (some) rhymes. Put the cursor on a word and then
;; M-x poetry-rhyme-word. Tie it to a key combination if you use it a
;; lot. This uses the internal dictionary from the 'rhyme' package.

;; 3. A way to get rhymes from various websites via

;; M-x poetry-rhyme-word-rhymezone
;; M-x poetry-rhyme-word-rhymer
;; M-x poetry-rhyme-word-wikirhymer
;; M-x poetry-rhyme-word-primerhyme
;; M-x poetry-rhyme-word-brhymes

;; Requires a web connection and the w3m package. If you just want to
;; use the web you can do without the command-line rhyme package, but
;; this is not ideal. 'wikirhymer' seems to be the most versatile, and
;; 'brhymes' will give you near-rhymes and soundalikes.

;; 4. A way to display verse form templates in the left margin. This
;; coexists nicely with the foot count but either can be used
;; independently. M-x poetry-form and select the form from the
;; list. C-x 0 to get rid of the display.

;; Credits:

;; Code, ideas, and inspiration were gotten from the packages listed
;; below. Only some of poetry.el is original. But I believe in using
;; existing tools to make new ones. Thanks to the original authors for
;; their work, and whatever I messed up is solely my own doing.

;; nlinum.el by Stefan Monnier
;; haiku.el by Jose E. Marchesi
;; rhyme command line package by Brian Langenberger
;; verse perfect by Bryant McGill
;; various online rhyming look-ups
;; many references on poetry, syllable division, etc.

;; Installation:

;; 1. Install the command-line 'rhyme' package.  There are some binary
;; packages around but if you have trouble, the source is here as of
;; February 2015.

;; https://launchpad.net/ubuntu/+source/rhyme/0.9-5

;; Building it may require some header packages, notably libreadline
;; and libgdm. You know what to do.

;; 2. Put this package (poetry.el) in your lisp loading path.

;; 3. Byte-compiling recommended. Speed is important as some things
;;    happen in real time.

;; 4. Put in .emacs or one of your startup-files:
;;    (require 'poetry)
;;    But I like to do this.
;;    (load-library "/path/to/file/poetry.elc")

;; 5. You must have both the emacs-w3m package and the command line
;;    w3m package installed if you want to use the web interfaces.

;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Ideas for the future:

;; 1. A better rhyming dictionary that includes imperfect rhymes. But
;;    the dictionary here is pretty good, and the net access dictionaries
;;    offer a lot more.
;; 2. Continue to include foot count improvements, but this is a very
;;    hard problem and won't ever be done. Note that we are counting
;;    feet, not syllables, which makes things even harder. I'm looking
;;    into the 2005 'Scandroid' program which has some good ideas.
;; 3. Is there a way to integrate with org-mode? Perhaps display foot
;;    counts only in a poetry block? This may not be so easy.

(add-hook 'prog-mode-hook #'display-line-numbers-mode)



;; Constants and variables. I prefer for now to define variables with
;; global scope (no 'let' stuff) as this makes debugging a lot easier,
;; and I found that debugging the foot counting functions was and is
;; quite tedious.

(defvar poetry-count)
(defvar poetry-patterns)
(defvar poetry-rhyme)
(defvar poetry-wordlist)
(defvar poetry-wordline)
(defvar poetry-vowgrouplen)
(defvar poetry-lastmatch)
(defvar poetry-pluses)
(defvar poetry-minuses)
(defvar poetry-counter)
(defvar poetry-element)
(defvar poetry-string)
(defvar poetry-collection)
(defvar poetry-selection)
(defvar poetry-pattern)

;; Begin foot count exceptions. This started with haiku.el but I made
;; many changes and proposed counting fixes, some of which will be
;; good and some undoubtedly not.

(defconst poetry-count-feet-negative
  '("cial" "tia" "cius" "cious" "giu"
    "ion" "iou" "sia$" ".ely$"
    ;; 'queen', queer' etc.
    "uee"
    ;; Final 'e' (e preceded by consonant) , initial 'yea' (year=1).
    "[^aeiou]e$" "^yea"
    ;; Unvoiced 'ed' endings. 'wasted' is 2 but 'dropped' is 1.
    "[^dtaieou]ed$"
    ;; Final 'es' for plurals after silent e, but not 'ces' 'ges' 'ses',
    ;; meaning 'noses' and 'voices' are 2 but 'games' or 'nodes' are 1.
    "[^csg]es$"

    ;; The following is a work-in-progress and will look messy.
    ;; Limited attempt at embedded silent 'e'. At the start of a word,
    ;; look for a consonant (cluster), vowel, consonant, 'e', and then
    ;; either V+CVC or CVCV. Fails on longer words like 'heretofore'.
    ;; I keep it limited because otherwise a lot of words like 'together'
    ;; scan improperly. 'eyelid' and 'pavement' also miss.
                                        ;      "^[^aeiou]+[aeiou][^aeiou]e[^aeiou]+[aeiouy][^aeiou][aeiou]$"
    "^...e[^aeiou]+[aeiouy][^aeiou][aeiou]$"
                                        ;      "^..e[^aeiou]+[aeiouy][^aeiou][aeiou]$"
    "^[^aeiou]+[aeiou][^aeiou]e[aeiou][^aeiou][aeiou][^aeiou]$"

    ))

(defconst poetry-count-feet-positive
  ;; I took 'li' out of this list. Undoubtedly I'll regret it, but I
  ;; don't know why it was there.
  '("ia" "riet" "dien" "iu" "io" "eo"
    ;; 'table' etc. But this is not good enough, like 'trestle'.
    ;;    "[aeiouym]ble"
    ;; It seems that an "le(s)" ending is actually the general case,
    ;; if not preceded by a vowel?
    "[^aeiouy]le$" "[^aeiouy]les$"
    "[aeiou]\\{3\\}"
    ;; Initial 'mc' = 'mac'.
    "^mc"
    ;; Be-ing but not see-ing or 'u-ing' words.
    "[^aeiouy][aeio]ing$"
    ;; Something that has a non-initial y followed by aeiou like lying,
    ;; crying.
    ".+y[aeiou]"
    "ism$" ; isms
    ;; This doesn't seem to work, so I kludge it just below.
    ;; Actually, I now cover it in the general 'le$' case above.
    ;;    "\\([^aeiouy]\\)\\1l$" ; middle twiddle battle bottle, etc
    ;;    "ddle$" "ttle$"
    ;; alien, salient, but not lien, or ebbuillient
    "[^l]lien"
    ;; exception for words coadjutor coagulable coagulate
    "^coa[dglx]"
    ;; coalesce coalescent coalition coaxial
    "[^gq]ua[^aeiou]"
    ;; Save word like 'beseemed' from embedded silent 'e' fix above.
    ;; This is getting kludgy and fragile.
    "^[^aeiou]+[aeiou][^aeiou]e[aeiou][^aeiou]ed$"
    "dnt$")) ; couldn't

(setq poetry-patterns '(
                        ("10a10b" "10a,10b,10a,10b")
                        ("Alfred Dorn Sonnet" "a,b,c,a,b,c,,d,d,,a,e,a,e,a,e")
                        ("French Sonnet 1" "10c,10c,,10d,10c,10c,10d")
                        ("French Sonnet 2" "10c,10c,,10d,10e,10e,10d")
                        ("French Sonnet 3" "10c,10c,,10d,10e,10d,10e")
                        ("Haiku" "5,7,5")
                        ("Italian Sonnet Basic" "a,b,b,a,a,b,b,a,,c,d,e,c,d,e")
                        ("Italian Sonnet Sicilian" "a,b,b,a,c,d,d,c,,e,f,e,f,e,f")
                        ("Italian Sonnet Sonetto Rispetto"
                         "a,d,a,b,a,b,c,c,,d,e,f,d,e,f,,d,e,d,e,d,e")
                        ("Keats Sonnet" "a,b,c,,a,b,c,,c,a,b,,c,d,e,,d,e")
                        ("Limerick" "a,a,b,b,a")
                        ("Ottava Rima" "a,b,a,b,a,b,c,c")
                        ("Petrarchian Sonnet" "a,b,b,a,a,b,b,a,,c,d,e,c,d,e")
                        ("Pushkin Sonnet" "a,b,a,b,,c,c,d,d,,e,f,f,e,,g,g")
                        ("Quatrain Type 1" "a,a,b,b")
                        ("Quatrain Type 2" "a,b,a,b")
                        ("Quatrain Type 3" "a,b,b,a")
                        ("Quatrain Type 4" "a,b,c,b")
                        ("Rime Royal" "a,b,a,b,b,c,c")
                        ("Rubaiyat" "a,a,b,a,,b,b,c,b,,c,c,a,c,,a,a")
                        ("Sestina"
                         "a,b,c,d,e,f,,f,a,e,b,d,c,,c,f,d,a,b,e,,e,c,b,f,a,d,,d,e,a,c,f,b,,b,d,f,e,c,a")
                        ("Shakesperian Sonnet" "a,b,a,b,,c,d,c,d,,e,f,e,f,,g,g")
                        ("Spenserian Sonnet" "a,b,a,b,,b,c,b,c,,c,d,c,d,,e,e")
                        ("Spenserian Stanza" "a,b,a,b,,b,c,b,c,,c")
                        ("Tanka" "5,7,5,7A,7A")
                        ("Terza Rima" "a,b,a,,b,c,b,,c,d,c,,d,e,d,,e,e")
                        ("Villanelle" "a,b,a,,a,b,a,,a,b,a,,a,b,a,,a,b,a,,a,b,a,a")
                        )
      )

;; Useless aux function.

(defun poetry-version ()
  "Give poetry tool version number"
  (interactive)
  (message (concat "poetry.el " poetry-version-no))
  )

;; Begin foot counting code.  This is a rewrite of haiku.el with a lot
;; of changes.

(defun poetry-feet-in-line ()
  "Count feet in current line and return it"
  (interactive)

  ;; Get the current line.
  (setq poetry-wordline (thing-at-point 'line))
  ;; Force lowercase.
  (setq poetry-wordline (downcase poetry-wordline))
  ;; Change hyphens to spaces.
  (setq poetry-wordline (replace-regexp-in-string "-" " " poetry-wordline))
  ;; Remove non-letters except for spaces.
  (setq poetry-wordline (replace-regexp-in-string "[^A-za-z ]" "" poetry-wordline))
  ;; Divide line into words.
  (setq poetry-wordlist (split-string poetry-wordline " " t))
  ;; Count (and add up and return) the feet in each word.
  (setq poetry-count 0)
  (mapc (function (lambda (x)
                    (setq poetry-count
                          (+ poetry-count (poetry-count-feet x)))))
        poetry-wordlist)
  poetry-count
  )

(defun poetry-count-feet (daword)
  "Count the feet in a word"
  ;; Count vowel groups.
                                        ;(if (string-match "^[^aeiou]+[aeiou][^aeiou]e.*[aeiou].*" daword)
                                        ;  (progn
                                        ;    (message (concat "Candidate embedded silent e in " daword))
                                        ;    (sleep-for 1)
                                        ;  )
                                        ;)
  (setq poetry-vowgrouplen 0)
  (setq poetry-lastmatch 0)
  (while (string-match "[aeiouy]+" daword poetry-lastmatch)
    (setq poetry-vowgrouplen (1+ poetry-vowgrouplen))
    (setq poetry-lastmatch (match-end 0))
    )
  ;; Find stuff that adds a syllable.
  (setq poetry-pluses 0)
  (mapc (function (lambda (x)
                    (if (string-match-p x daword)
                        (setq poetry-pluses (1+ poetry-pluses)))))
        poetry-count-feet-positive)
  ;; Find stuff that subtracts a syllable.
  (setq poetry-minuses 0)
  (mapc (function (lambda (x)
                    (if (string-match-p x daword)
                        (setq poetry-minuses (1- poetry-minuses)))))
        poetry-count-feet-negative)

  ;; Add up vowel groups and additions and subtractions.
  ;; But any word has 1 syllable minimum.

  (max (+ poetry-pluses poetry-minuses poetry-vowgrouplen) 1)

  )

;; End foot counting code.
;; Begin poetry mode stuff.

(defvar poetry--width 2)
(make-variable-buffer-local 'poetry--width)

(define-minor-mode poetry-mode
  "Toggle display of line foot count in the left margin.
With a prefix argument ARG, enable poetry mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Poetry mode is a buffer-local minor mode."
  :lighter nil ;; (" Poetry" poetry--desc)
  (jit-lock-unregister #'poetry--region)
  (remove-hook 'window-configuration-change-hook #'poetry--setup-window t)
                                        ;  (remove-hook 'after-change-functions #'poetry--after-change t)
                                        ;  (kill-local-variable 'poetry--line-number-cache)
  (remove-overlays (point-min) (point-max) 'poetry t)
  (kill-local-variable 'poetry--width)
  (when poetry-mode
    (add-hook 'change-major-mode-hook (lambda () (poetry-mode -1)))
    (add-hook 'window-configuration-change-hook #'poetry--setup-window nil t)
                                        ;    (add-hook 'after-change-functions #'poetry--after-change nil t)
    (jit-lock-register #'poetry--region nil))
  (poetry--setup-windows))

(defun poetry--face-height (face)
  (aref (font-info (face-font face)) 2))

(defun poetry--setup-window ()
  (let ((width (if (display-graphic-p)
                   (ceiling
                    (/ (* poetry--width 1.0
                          (poetry--face-height 'linum))
                       (frame-char-height)))
                   poetry--width)))
    (set-window-margins nil (if poetry-mode width)
                        (cdr (window-margins)))))

(defun poetry--setup-windows ()
  (dolist (win (get-buffer-window-list nil nil t))
    (with-selected-window win (poetry--setup-window))))

(defun poetry--flush ()
  (poetry--setup-windows)
  (remove-overlays (point-min) (point-max) 'poetry t)
  (run-with-timer 0 nil
                  (lambda (buf)
                    (with-current-buffer buf
                      (with-silent-modifications
                        (remove-text-properties
                         (point-min) (point-max) '(fontified)))))
                  (current-buffer)))

                                        ;(defvar poetry--line-number-cache nil)
                                        ;(make-variable-buffer-local 'poetry--line-number-cache)

                                        ;(defun poetry--after-change (&rest _args)
                                        ;  (setq poetry--line-number-cache nil)
                                        ;)

(defcustom poetry-format "%d"
  "Format of the syllable counts.
Used by the default `poetry-format-function'."
  :type 'string
  :group 'linum)

(defvar poetry-format-function
  (lambda (line width)
    (let ((str (format poetry-format line)))
      (when (< (length str) width)
        ;; Left pad to try and right-align the foot counts.
        (setq str (concat (make-string (- width (length str)) ?\ ) str)))
      (put-text-property 0 width 'face 'linum str)
      str))
  "Function to build the string representing the foot count.
Takes 2 arguments LINE and WIDTH, both of them numbers, and should return
a string.  WIDTH is the ideal width of the result.  If the result is larger,
it may cause the margin to be resized and line numbers to be recomputed.")

(defun poetry--region (start limit)
  (save-excursion
    ;; Text may contain those nasty intangible properties, but
    ;; that shouldn't stop us.
    (let ((inhibit-point-motion-hooks t))
      (goto-char start)
      (unless (bolp) (forward-line 1))
      (remove-overlays (point) limit 'poetry t)
      (while
          (and (not (eobp)) (< (point) limit)
               (let* ((ol (make-overlay (point) (1+ (point))))
                      (str (funcall poetry-format-function
                                    (poetry-feet-in-line) poetry--width))
                      (width (string-width str)))
                 (when (< poetry--width width)
                   (setq poetry--width width)
                   (poetry--flush))
                 (overlay-put ol 'poetry t)
                 (overlay-put ol 'evaporate t)
                 (overlay-put ol 'before-string
                              (propertize " " 'display
                                          `((margin left-margin) ,str)))
                 (zerop (forward-line 1)))))))
  nil)

;; End poetry mode stuff.
;; Begin poetry form stuff.

(defun poetry-form ()
  "Set up rhyme pattern in left of screen"
  (interactive)

  ;; Build a list of choices to display in echo area.

  (setq poetry-counter 0)
  (setq poetry-string "")
  (setq poetry-collection nil)
  (while (< poetry-counter (length poetry-patterns))
    (setq poetry-element (car (nth poetry-counter poetry-patterns)))
    (if (not (= poetry-counter 0))
        (setq poetry-string (concat poetry-string ",")))
    (setq poetry-string (concat poetry-string poetry-element))
    (setq poetry-collection (add-to-list 'poetry-collection poetry-element))
    (setq poetry-counter (+ 1 poetry-counter))
    )

  (setq poetry-string (concat poetry-string "\nChoose form: "))
  ;; Use completing-read method for easier minibuffer choice input.
  (setq poetry-selection (completing-read poetry-string poetry-collection nil t))
  ;; 'cdr' alone returns a single element list, so we have to use 'car' to
  ;; make it into a mere string.
  (if (setq poetry-pattern (car (cdr (assoc-string poetry-selection poetry-patterns))))
      (progn
        (delete-other-windows)
        (split-window-right 4)
        (if (get-buffer "*poetry.form*")
            (kill-buffer "*poetry.form*"))
        (switch-to-buffer "*poetry.form*")
        (insert (replace-regexp-in-string "," "\n" poetry-pattern))
        (insert "\n")
        (other-window 1)
        )
      (message "Invalid choice of form")
      )

  )
;;;;;;;;;;; Begin rhyming stuff.

(defun poetry-find-rhyme (word)
  "Simple interface to rhyme package"
  (interactive "sWord: ")
  (setq poetry-rhyme
        (replace-regexp-in-string "\r?\n$" " "
                                  (shell-command-to-string (concat "rhyme " word))))
  (setq poetry-rhyme
        (replace-regexp-in-string
         "Finding perfect rhymes for " "" poetry-rhyme))
  ;; Not enough display space?
  (display-message-or-buffer poetry-rhyme)
  )

(defun poetry-rhyme-word ()
  "Rhymes for word at point"
  (interactive)
  (poetry-find-rhyme (thing-at-point 'word))
  )

(defun poetry-rhyme-word-rhymezone ()
  "Find rhyme on RhymeZone for word at point"
  (interactive)
  ;; This is fragile as the API can change any time.
  ;; The site can even go down!
  (w3m (concat "http://www.rhymezone.com/r/rhyme.cgi?Word="
               (thing-at-point 'word)
               "&typeofrhyme=perfect&org1=syl&org2=l&org3=y"))
  )

(defun poetry-rhyme-word-rhymer ()
  "Find rhyme on Rhymer for word at point"
  (interactive)
  ;; This is fragile as the API can change any time.
  ;; The site can even go down!
  (w3m (concat "http://www.rhymer.com/RhymingDictionary/"
               (thing-at-point 'word)
               ".html"))
  )


(defun poetry-rhyme-word-wikirhymer ()
  "Find rhyme on RhymeBrain for word at point"
  (interactive)
  ;; This is fragile as the API can change any time.
  ;; The site can even go down!
  (w3m (concat "http://wikirhymer.com/words/"
               (thing-at-point 'word)))
  )

(defun poetry-rhyme-word-primerhyme ()
  "Find rhyme on PrimeRhyme for word at point"
  (interactive)
  ;; This is fragile as the API can change any time.
  ;; The site can even go down!
  (w3m (concat "http://www.prime-rhyme.com/"
               (thing-at-point 'word)
               ".html"))
  )

(defun poetry-rhyme-word-brhymes ()
  "Find rhyme on B-rhymes for word at point"
  (interactive)
  ;; This is fragile as the API can change any time.
  ;; The site can even go down!
  (w3m (concat "http://www.b-rhymes.com/rhyme/word/"
               (thing-at-point 'word)))
  )


(defun poetry-mode-pretty ()
  "the space between"
  (interactive)
  (poetry-mode 1)
  ;; (olivetti-mode 1)
  (stripe-buffer-mode 1))



;; End rhyming stuff.

(provide 'poetry)

;;; poetry.el ends here
