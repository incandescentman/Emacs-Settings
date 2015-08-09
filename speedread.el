(defun speedread () 
"Speedread a buffer by timed flashing of groups of words in the echo area"
(interactive)

;;; Read all of the documentation text below before doing anything!

;;; This program is originally Copyright (C) 2004 by Bob Newell.  
;;; The copyright has been assigned to the Free Software Foundation.

;;; The code may be used freely and without restriction by anyone,
;;; but no rights of ownership are granted, conceded, or relinquished
;;; by the copyright holders or the author.

;;; Tested on Emacs 20/21 on Win98/Win XP/Various Linux.  
;;; Emacs 19 or lower will *not* work.  Xemacs unknown.  Macintosh 
;;; unknown.  Emacs 22 probably works but not tested.
;;;
;;; PRIOR TO BYTE-COMPILING YOU MUST LOAD THE BOOKMARK PACKAGE!

;;; M-x bookmark-load

;;; will do this for you.  For performance reasons you really MUST
;;; use in byte-compiled form.  You also MUST fine-tune the 
;;; customization variables.  These are in the customization group
;;; 'speedread' in the 'local' customization section.
;;;
;;; When compiling ignore any warnings; hopefully I've found most
;;; of these by now in any case.

;;; Concept: load a (text) file into a buffer for speedreading.
;;; Be sure this package is loaded just as you would load any other
;;; package.  Put a 'load-library' command in your .emacs file if
;;; you wish to make this easier.  Load the byte-compiled version!

;;; Position the cursor to the point at which you wish to start
;;; reading and give the command 

;;; M-x speedread

;;; The file will be displayed to you in the echo area (!) a bit
;;; at a time, in "flashes" with a delay between each flash.  The
;;; customization variables control the minimum size (in 
;;; characters) of each flash group, and the pause between groups.

;;; After a certain number of groups have been displayed (there is
;;; a customization variable for this too) there is a 'hard' pause.
;;; This is quite necessary to avoid incredible eye fatigue!  

;;; At this point you can continue reading, stop, or change some
;;; reading parameters.  To continue, press spacebar or the ENTER key.
;;; To quit, type 'q' to quit and save your place with a bookmark,
;;; or 'e' to exit speed-reading without saving your current place.
;;; (Side effect: all bookmarks get saved, not just this one.  Be
;;; aware.)  The commands described immediately below are also active.
;;; '?' or 'h' will get you a help screen.

;;; Rather than waiting for a pause between flashes, if you like, 
;;; you can alter the speed, the flash group size, or the number of flashes
;;; on-the-fly at just about any time.  Type the single  keystrokes

;;;   'f' to go 20% faster,
;;;   's' to go 20% slower, 
;;; '  w' to widen the flash group 20%, 
;;;   'n' to narrow it 20%, 
;;;   'm' for 20% more flashes between pauses,
;;;   'l' for 20% less flashes between pauses,
;;;   'b' to go back and repeat the current set of flashes,
;;;   'r' to completely restart from whatever point in the buffer 
;;;       you began the session.
;;;   'q' to quit and save the bookmark at point;
;;;   'e' to exit without saving the bookmark.

;;; When starting a speedread session, if a bookmark exists you are
;;; asked if you wish to use it.  If you choose not to use it, the
;;; display starts at the current cursor position.

;;; Newline characters are converted to spaces.  This causes a little
;;; weirdness at times but leaving newlines intact makes a big mess.

;;; Again, tune the display parameters!  You may find that as your
;;; speed-reading skills improve you can increase the number of 
;;; characters in a flash group, and/or decrease the pause time
;;; between groups.

;;; Project Gutenberg is a fabulous source of texts to use.

;;; COMMENTS

;;;  At first I thought I should somehow hide the main buffer display
;;;  or find a better way to flash the text than through the 
;;;  echo area, which seems lame in concept - but actually easy and
;;;  relativey fast.  And speed really matters here; code execution
;;;  time is effectively added to pause time.  This of course can
;;;  lead to unpredictable results especially on heavily loaded systems.
;;;  As to hiding the main buffer display, this turns out not to be all
;;;  that distracting, as your attention is tightly focused (and I do
;;;  mean tightly) on the echo area.

;;;  The parameters 'out of the box' tend to result in roughly 600 words
;;;  per minute.  This is probably too fast for many people; adjust to suit.
;;;  Don't attempt too much speed initially or you will become very 
;;;  frustrated and probably give up.  As you learn how to work with
;;;  the technique you can build up the speed and the flash group size.
;;;  On the other hand, push yourself a little.  Go as fast as you can
;;;  without losing comprehension.  Different types of reading material
;;;  will require different speed settings!  You can read a scifi novel
;;;  faster than you can read existential philosophy.

;;;  Your comments on both speed-reading, the flash technique, and the
;;;  program itself are welcome.  Write 

;;;  speedread@bobnewell.net

;;;  I also have a Perl version of similar (older, less functional) code.  
;;;  If you want it let me know (assuming I can still find it).

;;; REVISION LOG
;;;
;;; 24 oct 2007 Three years have passed since I looked at this!  My big
;;;             changes for today are an update to my email address and a
;;;             few other very minor doc tweaks.
;;;             Alpha 0.23.
;;; 13 oct 2004 Fixed severe bug with % character in flash group, can't 
;;;             imagine this wasn't found earlier.  Changed 'minibuffer'
;;;             terminology to more accurate 'echo area'.  Strange echo
;;;             bug reappeared; hope the % fix killed it but not certain.
;;;             Alpha 0.22.
;;; 03 sep 2004 Remove spaces at front of a flash group.  
;;;             Seemingly fixed strange minibuffer echo bug; still not so
;;;             sure though.  
;;;             Fixed most if not all free-variable compiler complaints.
;;;             Moved speedread customization group out of local group,
;;;             making an incompatibility with prior releases.
;;;             Changed bookmarking to use bookmark-buffer-file-name,
;;;             also incompatible with previous releases.
;;;             Attempted to fix leading punctuation problem by changing
;;;             forward-word method to search-forward-regexp.  It's not a
;;;             complete solution but seems to help a lot.
;;;             Changed to fixed delay when end of buffer is reached.
;;;              Alpha 0.21 (release version)
;;; 01 sep 2004 Incorporated additional ideas from Joakim to
;;;             allow command entry at any time, not just at 
;;;             major pauses.  Required extensive changes to the
;;;             previous alpha.  Added comments and improved
;;;             legibility.  Still some bugs.
;;;              Alpha 0.20 (not for public release yet)
;;; 12 aug 2004 Major rev to include commands to change params
;;;             temporarily on the fly, and all the supporting
;;;             code to go with it, plus doc revs, help screen,
;;;             etc etc.  Thanks to Joakim Verona for numerous
;;;             good ideas for improved functionality.
;;;             This is close to a rewrite.
;;;             Added speedread-save-changes to save any altered
;;;             customization variables.
;;;              Alpha 0.10 (not for public release yet)
;;; 06 aug 2004 Added summary statistics per user input.
;;;              Alpha 0.03
;;; 30 jul 2004 Changed, reordered, added to documentation.
;;;             Fixed a few > col 80 line wraps in source code.
;;;             Minor prompt change.
;;;              Alpha 0.02
;;; 29 jul 2004 Santa Fe, New Mexico.  Initial release.
;;;              Alpha 0.01

;;; UNRESOLVED KNOWN BUGS/ISSUES

;;;  BUGS
;;;  * The bookmark is not found unless the bookmark file has been
;;;    preloaded.
;;;  * In X-windows, moving focus out of the window during flashing
;;;    causes problems.

;;;  ISSUES/COMMENTS
;;;  * There is no symmetry in increasing/decreasing rates, counts,
;;;    etc.  If you increase speed 20% and then decrease speed 20%
;;;    you end up at 96% of original speed, for example.
;;;  * Temporary changes are not saved and can be hard to reproduce.
;;;  * Pause time when waiting for command/continuation input is not
;;;    counted against the reading time.  I don't think it should be,
;;;    though.

;;; TODO LIST/IDEAS
;;;  * Maybe bookmark to a 'speedread' file rather than default file?

(require 'bookmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; user-configurable stuff here
;;
;; since speed is very much system dependent these params
;; MUST be fine-tuned to get any sort of acceptable results
;; advice: do it on-the-fly and then when you get what you like
;; save as permanent customized values

;; set the size of the flash group and the delay between groups

(defgroup speedread nil 
 "Speed reading customization variables"
)

(defcustom iread-chars 20 
 "Minimum characters per flash group"
 :group 'speedread
)
(defcustom iread-delay-milliseconds 400 
 "Milliseconds of delay between flashes"
 :group 'speedread
)
(defcustom iread-delay-seconds 0 
 "Full seconds of delay between flashes, should almost always be 0"
 :group 'speedread
)

;; this is not the real screenlength
;; it is the maximum number of lines to be flashed between 'hard' pauses
;; to avoid eye fatigue

(defcustom iread-screenlength 100 
 "Number of flash groups between pauses"
 :group 'speedread
)

;; stuff to avoid at least some of the free variable complaints

(defvar iread-average)
(defvar iread-bookmark)
(defvar iread-chars)
(defvar iread-continue)
(defvar iread-count)
(defvar iread-delay-milliseconds)
(defvar iread-delay-seconds)
(defvar iread-flash-length)
(defvar iread-flash-line)
(defvar iread-group-start)
(defvar iread-mark)
(defvar iread-param-change)
(defvar iread-position-change)
(defvar iread-quit-major)
(defvar iread-save-line)
(defvar iread-screenlength)
(defvar iread-start-start)
(defvar iread-time-now)
(defvar iread-time-used)
(defvar iread-words-read)
(defvar iread-words-recent)

;; vector definition for asynchronous command processing

(defvar iread-command-vector nil)

(setq  iread-command-vector (make-vector 256 'iread-do-continue))
(aset  iread-command-vector ?q 'iread-quit)
(aset  iread-command-vector ?e 'iread-exit)
(aset  iread-command-vector ?f 'iread-faster)
(aset  iread-command-vector ?s 'iread-slower)
(aset  iread-command-vector ?w 'iread-wider)
(aset  iread-command-vector ?n 'iread-narrower)
(aset  iread-command-vector ?m 'iread-more)
(aset  iread-command-vector ?l 'iread-less)
(aset  iread-command-vector ?b 'iread-back)
(aset  iread-command-vector ?r 'iread-restart)
(aset  iread-command-vector ?h 'iread-help)
(aset  iread-command-vector ?? 'iread-help)
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; look for a bookmark and optionally go there
;; if we don't go there start at point

  (if (setq iread-bookmark (bookmark-get-position (bookmark-buffer-file-name)))
    (if (yes-or-no-p "Bookmark exists, use it? ")
     (goto-char iread-bookmark)))
     
;; initialize

(setq iread-count 0)
(setq iread-flash-line nil)
(setq iread-flash-length 0)
(setq iread-mark (point))
(setq iread-group-start (point))
(setq iread-start-start (point))
(setq iread-time-now (float-time))
(setq iread-time-used 0)
(setq iread-words-read 0)
(setq iread-words-recent 0)
(setq iread-position-change nil)
(setq iread-continue nil)
(setq iread-quit-major nil)

(catch 'iread
 (progn
;; don't break on \r or \n, only real spaces or tabs!
  (while (search-forward-regexp "[ \t]+" nil t)
   (progn
    (setq iread-words-read (1+ iread-words-read))
    (setq iread-words-recent (1+ iread-words-recent))
    (setq iread-flash-line 
    (concat iread-flash-line 
            (buffer-substring iread-mark (point))))
    (setq iread-flash-length (length iread-flash-line))
    (setq iread-mark (point))
   )

;; output the group when complete

    (if (>= iread-flash-length iread-chars)
      (progn  

;; fix up newline problems here by substituting space
;; this might make chapter headings a little weird

	(setq iread-flash-line 
	      (subst-char-in-string (string-to-char "\n") 
	                            (string-to-char " ") iread-flash-line))
;; trim leading spaces
        (while (equal (substring iread-flash-line 0 1) " ")
	       (setq iread-flash-line (substring iread-flash-line 1)))
	(display-message-or-buffer iread-flash-line)

;; after flashing group check and process pending input, ONE 
;; command letter only - otherwise multiple or repeat keypresses
;; could create havoc

	(if (input-pending-p)
	    (funcall (aref iread-command-vector (read-char-exclusive))))

;; flush anything extra beyond a single char

        (while (input-pending-p) (read-char-exclusive))

;; then do the between-flash pause

	(sleep-for iread-delay-seconds iread-delay-milliseconds)
	(setq iread-save-line iread-flash-line)
	(setq iread-flash-line nil)
	(setq iread-flash-length 0)
	(setq iread-count (1+ iread-count))

;; major pause processing after however-many flash groups

	(if (>= iread-count iread-screenlength)
	    (progn
	       (setq iread-continue nil)
	       (setq iread-time-used (+ iread-time-used 
		   (/ (- (float-time) iread-time-now) 60)))
   	       (setq iread-time-now (float-time))
	       (setq iread-param-change nil)
	       (setq iread-position-change nil)
	    (while  (equal iread-continue nil)
	      (progn

;; show stats and prompt, and also process command input
		(setq iread-average (/ iread-words-read iread-time-used ))
	        (message
 		   (format 
"%d msec delay %d chr/flash %d flash/pause %6.1f wd/min. Command/?/ENTER:"
              (+ (* 1000 iread-delay-seconds) iread-delay-milliseconds) 
	             iread-chars iread-screenlength iread-average))
;; wait unconditionally for input with 1/10 second polling
                (while (not (input-pending-p))
		       (sleep-for 0 100))
;; try to avoid timer problem on quit/exit commands
  	        (setq iread-time-now (float-time))
		(setq iread-quit-major t)
		(funcall (aref iread-command-vector (read-char-exclusive)))
;; flush extra input
		(while (input-pending-p) (read-char-exclusive))
            ))

;; ready to continue reading here
;; reset counter, move group marker, restart timer

	      (setq iread-continue nil)
	      (setq iread-count 0)
	      (setq iread-words-recent 0)
	      (setq iread-group-start (point))
	      (setq iread-time-now (float-time))
	      (setq iread-quit-major nil)
;; repeat the last group for continuity IF we didn't move the position
	      (if (equal iread-position-change nil)
		  (progn
		    (message iread-save-line)
		    (sleep-for iread-delay-seconds iread-delay-milliseconds)
                  )
              )
	      (setq iread-position-change nil)
	 )
        )
       )
     )
  )

;; We have reached the end of the file.  
;; We still have a little bit undisplayed, so show it and then we're done.
(setq iread-flash-line (concat iread-flash-line
                    (buffer-substring iread-mark (point-max))))   
(setq iread-flash-line 
      (subst-char-in-string (string-to-char "\n") 
                            (string-to-char " ") iread-flash-line))
(while (equal (substring iread-flash-line 0 1) " ")
       (setq iread-flash-line (substring iread-flash-line 1)))
(message iread-flash-line)
(sleep-for iread-delay-seconds iread-delay-milliseconds)
(message "THE END.")
;; longer fixed pause at the end of the buffer
(sleep-for 3 0)
  )
 )

;; Show closeout stats.
(if (equal iread-quit-major nil)
  (progn
    (setq iread-time-used 
        (+ iread-time-used (/ (- (float-time) iread-time-now) 60) ))
    (setq iread-average (/ iread-words-read iread-time-used))))
(message (format "%d words read in %6.2f minutes; %6.1f words per minute." 

		 iread-words-read iread-time-used iread-average))

) ;; end of mainline speedread

;; the throws work here even though I thought they should not !?

(defun iread-quit ()
"quit and save bookmark"
    (bookmark-set (bookmark-buffer-file-name))
    (bookmark-save)
    (throw 'iread t))

(defun iread-exit ()
"quit and don't save bookmark"
   (throw 'iread t))

(defun iread-do-continue ()
"continue reading now"
 (setq iread-continue t)
)

(defun iread-faster ()
"Increase reading speed temporarily"
 (interactive)
 (defvar iread-delay-total)
 (setq iread-delay-total (+ (float iread-delay-seconds)
     (/ (float iread-delay-milliseconds) 1000)))
 (setq iread-delay-total (* 0.8 iread-delay-total))
 (setq iread-delay-seconds (truncate iread-delay-total 1000))
 (setq iread-delay-milliseconds 
   (truncate (* (- iread-delay-total (float iread-delay-seconds)) 1000)))
)

(defun iread-slower ()
"Decrease reading speed temporarily"
 (defvar iread-delay-total)
 (setq iread-delay-total (+ (float iread-delay-seconds)
     (/ (float iread-delay-milliseconds) 1000)))
 (setq iread-delay-total (* 1.20 iread-delay-total))
 (setq iread-delay-seconds (truncate iread-delay-total 1000))
 (setq iread-delay-milliseconds 
   (truncate (* (- iread-delay-total (float iread-delay-seconds)) 1000)))
)

(defun iread-wider ()
"Widen flash group temporarily"
 (setq iread-chars (truncate (* 1.2 (float iread-chars))))
)

(defun iread-narrower ()
"Narrow flash-group temporarily"
 (interactive)
 (setq iread-chars (truncate (* 0.8 (float iread-chars))))
)

(defun iread-more ()
"Increase number of flash groups per set temporarily"
 (setq iread-screenlength (truncate (* 1.2 (float iread-screenlength))))
)

(defun iread-less ()
"Decrease number of flash groups per set temporarily"
 (setq iread-screenlength (truncate (* 0.8 (float iread-screenlength))))
)

(defun iread-back ()
"Re-read last full set of flash groups"
 (goto-char iread-group-start)
 (setq iread-words-read (- iread-words-read iread-words-recent))
 (setq iread-words-recent 0)
 (setq iread-mark (point))
 (setq iread-position-change t)
 (setq iread-continue t)
)

(defun iread-restart ()
"Restart from session start"
 (goto-char iread-start-start)
 (setq iread-mark (point))
;; kill the words read if we're starting over
 (setq iread-words-read 0)
 (setq iread-position-change t)
 (setq iread-continue t)
)

(defun iread-help ()
"Get speedreading command help"
 (interactive)
  (defvar iread-saved-buffername)
  (if (get-buffer "*iread.help*")
    (kill-buffer "*iread.help*"))
  (setq iread-saved-buffername (buffer-name))
  (switch-to-buffer "*iread.help*")
  (goto-char (point-min))
  (insert "Command keys:\n\n")
  (insert " f   read faster by 20%\n")
  (insert " s   read slower by 20%\n")
  (insert " w   widen flash group by 20%\n")
  (insert " n   narrow flash group by 20%\n")
  (insert " m   read 20% more groups between pauses\n")
  (insert " l   read 20% less groups between pauses\n")
  (insert " b   go back and reread from the last pause point\n")
  (insert " r   restart from session starting point\n")
  (insert " e   exit without bookmarking\n")
  (insert " q   quit and bookmark current location\n")
  (insert " h,? get command help")
  (insert " c, spacebar   continue reading\n")
  (insert "\nAll changes except bookmarks are retained during the current\n")
  (insert "Emacs session only (but bookmarks are permanent).\n")
  (insert "If you wish to retain the other changes permanently,\n")
  (insert "use the command 'speedread-save-changes'.")
  (read-from-minibuffer "Press ENTER to leave help screen")     
  (switch-to-buffer iread-saved-buffername)
)

(defun speedread-save-changes ()
"save all customization variables changed this Emacs session"
 (interactive)
 (if (yes-or-no-p "REALLY overwrite all saved speedread settings? ")
  (progn
   (customize-save-variable 'iread-chars iread-chars)
   (customize-save-variable 'iread-delay-milliseconds iread-delay-milliseconds)
   (customize-save-variable 'iread-delay-seconds iread-delay-seconds)
   (customize-save-variable 'iread-screenlength iread-screenlength)
  )))
