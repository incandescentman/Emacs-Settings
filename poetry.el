
;;; begin poetry.el
;;; poetry.el --- Poetry writing aids  -*- lexical-binding: t -*-

;; Somehow I got interested in sonnet writing, and I wanted to 
;; do it right. I discovered the Windoze freeware Verse Perfect
;; and was very impressed, but I wanted something for emacs. This
;; isn't quite as good, but it provides the real essentials.

;; Tested only on Linux Mint 17 and Emacs 24.3. Will probably
;; work on any reasonable distro and reasonable Emacs. Windoze?
;; Mac? I don't use these so I don't know. I don't think so unless
;; there is an equivalent for the 'rhyme' command-line package.

;; Comments are welcome. Send to bobnew...@bobnewell.net. But I don't
;; promise anything and this isn't an offer of support. There, I've
;; covered myself!

;; Initial release(s): February 2015, by 
;; Bob Newell, Honolulu, Hawai`i.

;; Two things are here right now.

;; 1. A poetry-mode that shows the syllable count for each line
;; in the left margin. M-x poetry mode to turn it on and the same
;; to turn it off.

;; 2. A way to get (some) rhymes. Put the cursor on a word and
;; then M-x poetry-rhyme-word. Tie it to a key combination if you
;; use it a lot.

;; Credits:

;; Code was borrowed extensively from the packages listed just
;; below. I really haven't done a lot original here. But I believe in
;; using existing tools to make new ones. Thanks to the original
;; authors for their work, and whatever I messed up is solely my own
;; doing.

;; nlinum.el by Stefan Monnier 
;; functions-extra.el by Kyle W T Sherman
;; rhyme command line package by Brian Langenberger

;; Installation:

;; 1. Install the command-line 'rhyme' package. 
;; There are some binary packages around but if you have trouble,
;; the source is here as of February 2015.

;; https://launchpad.net/ubuntu/+source/rhyme/0.9-5

;; Building it may require some header packages, notably libreadline
;; and libgdm. You know what to do.

;; 2. Put this package (poetry.el) in your lisp loading path.

;; 3. Byte-compiling recommended.

;; 4. Put in .emacs or one of your startup-files:
;;    (require 'poetry)
;;    But I like to do this.
;;    (load-library "/path/to/file/poetry.elc")

;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Ideas for the future:
;; 1. Something in addition to 'perfect' rhymes?
;; 2. Sonnet pattern guides?
;; 3. Rhyme checking against the pattern guides?
;; 4. Connect somehow with org-mode?
;; But I have what I need for now, so I don't think these will be soon!

(require 'linum)                        ;For its face.

 (defvar poetry-count)
 (defun poetry-syllables-in-line ()
 "Count syllables in current line and return"
  (interactive)
  (setq poetry-count
    (poetry-count-syllables (line-beginning-position) (line-end-position)))
  poetry-count
)

;; The core of this is borrowed from a readability index program
;; found in some emacs utilities.

 (defun poetry-count-syllables (begin end)
 "Count the syllables per line in a range"
 (interactive)
   (let ((letter-regexp "[A-Za-z]")
         (vowel-regexp "[AEIOUYaeiouy]")
         (e-end-regexp "[Ee]\\W"))
     (save-excursion
       (let ((count 0))
         (goto-char begin)
         (while (< (point) end)
           (while (and (< (point) end)
                       (not (looking-at letter-regexp)))
             (forward-char 1))
           (let ((state (if (looking-at vowel-regexp) 2 1)))
             (when (= state 2)
               (setq count (1+ count)))
             (while (looking-at letter-regexp)
               (if (and (= state 1)
                        (looking-at vowel-regexp)
                        (not (looking-at e-end-regexp)))
                   (setq state 2
                         count (1+ count))
                 (if (and (= state 2)
                          (not (looking-at vowel-regexp)))
                     (setq state 1)))
               (forward-char 1))))
         count)))
)

(defvar poetry-rhyme)
(defun poetry-find-rhyme (word)
"Simple interface to rhyme package"
(interactive "sWord: ")
(setq poetry-rhyme 
  (replace-regexp-in-string "\r?\n$" " "
    (shell-command-to-string (concat "rhyme " word))))
(setq poetry-rhyme 
    (replace-regexp-in-string "Finding perfect rhymes for " "" poetry-rhyme))
;; Not enough display space?
(message poetry-rhyme)
)

(defun poetry-rhyme-word ()
"Rhymes for word at point"
 (interactive)
 (poetry-find-rhyme (thing-at-point 'word))
)

(provide 'poetry)



(defvar poetry--width 2)
(make-variable-buffer-local 'poetry--width)

;; (defvar poetry--desc "")

;;;###autoload
(define-minor-mode poetry-mode
  "Toggle display of line syllable count in the left margin (Linum mode).
With a prefix argument ARG, enable poetry mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Poetry mode is a buffer-local minor mode."
  :lighter nil ;; (" Poetry" poetry--desc)
  (jit-lock-unregister #'poetry--region)
  (remove-hook 'window-configuration-change-hook #'poetry--setup-window t)
  (remove-hook 'after-change-functions #'poetry--after-change t)
  (kill-local-variable 'poetry--line-number-cache)
  (remove-overlays (point-min) (point-max) 'poetry t)
  (kill-local-variable 'poetry--width)
  (when poetry-mode
   (add-hook 'change-major-mode-hook (lambda () (poetry-mode -1)))
   (add-hook 'window-configuration-change-hook #'poetry--setup-window nil t)
    (add-hook 'after-change-functions #'poetry--after-change nil t)
    (jit-lock-register #'poetry--region t))
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

(defvar poetry--line-number-cache nil)
(make-variable-buffer-local 'poetry--line-number-cache)

(defun poetry--after-change (&rest _args)
  (setq poetry--line-number-cache nil))

(defcustom poetry-format "%d"
  "Format of the syllable counts.
Used by the default `poetry-format-function'."
  :type 'string
  :group 'linum)

(defvar poetry-format-function
  (lambda (line width)
    (let ((str (format poetry-format line)))
      (when (< (length str) width)
        ;; Left pad to try and right-align the line-numbers.
        (setq str (concat (make-string (- width (length str)) ?\ ) str)))
      (put-text-property 0 width 'face 'linum str)
      str))
  "Function to build the string representing the syllable count.
Takes 2 arguments LINE and WIDTH, both of them numbers, and should return
a string.  WIDTH is the ideal width of the result.  If the result is larger,
it may cause the margin to be resized and line numbers to be recomputed.")

(defun poetry--region (start limit)
  (save-excursion
    ;; Text may contain those nasty intangible properties, but
    ;; that shouldn't prevent us from counting those lines.
    (let ((inhibit-point-motion-hooks t))
      (goto-char start)
      (unless (bolp) (forward-line 1))
      (remove-overlays (point) limit 'poetry t)
;      (let ; ((line (poetry--line-number-at-pos)))
        (while
            (and (not (eobp)) (< (point) limit)
                 (let* ((ol (make-overlay (point) (1+ (point))))
                        (str (funcall poetry-format-function
                             (poetry-syllables-in-line) poetry--width))
                        (width (string-width str)))
                   (when (< poetry--width width)
                     (setq poetry--width width)
                     (poetry--flush))
                   (overlay-put ol 'poetry t)
                   (overlay-put ol 'evaporate t)
                   (overlay-put ol 'before-string
                                (propertize " " 'display
                                            `((margin left-margin) ,str)))
 ;                  (setq line (1+ line))
                   (zerop (forward-line 1)))))))
  ;; (setq poetry--desc (format "-%d" (poetry--ol-count)))
  nil)

;;;###autoload
(define-globalized-minor-mode global-poetry-mode poetry-mode
  (lambda () (unless (minibufferp) (poetry-mode))))

(provide 'poetry)
;;; poetry.el ends here 
