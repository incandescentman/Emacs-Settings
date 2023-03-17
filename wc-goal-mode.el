;;; wc-goal-mode.el --- Running word count with goals (minor mode)
;;
;; Author: Benjamin Beckwith
;; Created: 2010-6-19
;; Version: 2.1
;; Last-Updated: 2014-08-28
;; URL: https://github.com/bnbeckwith/wc-goal-mode
;; Keywords:
;; Compatability:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Read the following for how to use the 'how-many' function
;; http://www.neverfriday.com/sweetfriday/2008/06/emacs-tip-word-counting-with-a.html
;; The following site had a good idea on how to produce number of chars
;; http://xahlee.org/emacs/elisp_count-region.html
;; Inspired by http://750words.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 
;; 2.1 Get rid of compilation warnings
;; 2.0 Name change from wc-mode to wc-goal-mode
;; 1.3 Goal functions now perform reset by default
;; 1.2 Reset functions added
;; 1.1 Counting functions tied to buffer-local variables
;;     This allows customization of the counting methods
;; 1.0 Keystrokes for all goals added.
;;     Hooks variable added.
;;     In-code documentation updated.
;; 0.9 Added keymap for basic mode features/functions
;; 0.8 Added modeline format customization
;;     Added other customizations
;; 0.7 Added stats for lines and characters
;; 0.6 Mode line goal code added
;; 0.5 Initial version focused on word-count
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defgroup wc-goal nil
  "Customization group for `wc-goal-mode'."
  :group 'wp)

(defcustom wc-goal-modeline-format "WC[%W%w/%tw]"
  "The format string for the modeline.
The detailed information for this minor mode can be shown in many
ways in the modeline. The formatting strings recognized in this
format are as follows.

  %W  Original word count (before changes)
  %L  Original line count
  %C  Original character count
  %w  Change in words
  %l  Change in lines
  %c  Change in characters
  %gc Character change goal
  %gl Line change goal
  %gw Word change goal
  %tw Total words in buffer
  %tl Total lines in buffer
  %tc Total characters in buffer

The default modeline, WC[%W%w/%tw], will display the original number
of words followed by the change in words (delta), followed by the total
number of words in the buffer.
It will looks something like WC[742+360/1100] in the modeline.
"
  :group 'wc-goal)

(defcustom wc-goal-mode-hook nil
  "Hook to run when entering wc-goal-mode."
  :type 'hook
  :group 'wc-goal)

(defface wc-goal-face
  '((t (:inherit highlight)))
  "Face for modeline when goal is reached"
  :group 'wc-goal)

(defvar wc-goal-mode-map
  (let ((map (make-sparse-keymap "Wordcount")))
    (define-key map (kbd "C-c C-w w") 'wc-goal-set-word-goal)
    (define-key map (kbd "C-c C-w l") 'wc-goal-set-line-goal)
    (define-key map (kbd "C-c C-w a") 'wc-goal-set-char-goal)
    (define-key map (kbd "C-c C-w c") 'wc-goal-count)
    map)
  "Keymap for wc-goal-mode")

(defvar wc-goal-orig-words nil "Original count of words in the buffer")
(defvar wc-goal-orig-lines nil "Original count of words in the buffer")
(defvar wc-goal-orig-chars nil "Original count of words in the buffer")
(make-variable-buffer-local 'wc-goal-orig-words)
(make-variable-buffer-local 'wc-goal-orig-lines)
(make-variable-buffer-local 'wc-goal-orig-chars)

(defvar wc-goal-words-delta 0 "Change in word count")
(defvar wc-goal-lines-delta 0 "Change in line count")
(defvar wc-goal-chars-delta 0 "Change in char count")
(make-variable-buffer-local 'wc-goal-words-delta)
(make-variable-buffer-local 'wc-goal-lines-delta)
(make-variable-buffer-local 'wc-goal-chars-delta)

(defvar wc-goal-word-goal nil "Goal for number of words added")
(defvar wc-goal-line-goal nil "Goal for number of lines added")
(defvar wc-goal-char-goal nil "Goal for numger of chars added")
(make-variable-buffer-local 'wc-goal-word-goal)
(make-variable-buffer-local 'wc-goal-line-goal)
(make-variable-buffer-local 'wc-goal-char-goal)

(defvar wc-goal-count-chars-function
  (function (lambda (rstart rend)
              "Count the characters specified by the region bounded by
RSTART and REND."
              (- rend rstart))))

(defvar wc-goal-count-words-function
  (function (lambda (rstart rend)
              "Count the words specified by the region bounded by
RSTART and REND."
              (if (boundp 'count-words)
                  (count-words rstart rend)
                (how-many "\\w+" rstart rend)))))

(defvar wc-goal-count-lines-function
  (function (lambda (rstart rend)
              "Count the lines specified by the region bounded by
RSTART and REND."
              (how-many "\n" rstart rend))))

(defvar wc-goal-modeline-format-alist
  '(("%W" . (number-to-string wc-goal-orig-words))
    ("%L" . (number-to-string wc-goal-orig-lines))
    ("%C" . (number-to-string wc-goal-orig-chars))
    ("%w" . (wc-goal-prepend-sign wc-goal-words-delta))
    ("%l" . (wc-goal-prepend-sign wc-goal-lines-delta))
    ("%c" . (wc-goal-prepend-sign wc-goal-chars-delta))
    ("%gc" . (wc-goal-prepend-sign wc-goal-char-goal))
    ("%gl" . (wc-goal-prepend-sign wc-goal-line-goal))
    ("%gw" . (wc-goal-prepend-sign wc-goal-word-goal))
    ("%tc" . (number-to-string (+ wc-goal-orig-chars wc-goal-chars-delta)))
    ("%tl" . (number-to-string (+ wc-goal-orig-lines wc-goal-lines-delta)))
    ("%tw" . (number-to-string (+ wc-goal-orig-words wc-goal-words-delta))))
  "Format and value pair
Format will be evaluated in `wc-goal-generate-modeline'")

(defvar wc-goal-mode-hooks nil "Hooks to run upon entry to wc-goal-mode")

(defun wc-goal-format-modeline-string (fmt)
  "Format the modeline string according to specification and return result"
  (let ((case-fold-search nil))
    (dolist (pair wc-goal-modeline-format-alist fmt)
      (when (string-match (car pair) fmt)
        (setq fmt (replace-match (eval (cdr pair)) t t fmt))))))

(defun wc-goal-prepend-sign (val)
  "Add a sign to the beginning of a value.
Also cheat here a bit and add nil-value processing."
  (if val
      (format "%s%d"
              (if (< val 0)
                  "-" "+")
              (abs val))
    "none"))

(defun wc-goal-reset ()
  "Reset the original word, line, and char count to their current
value."
  (interactive)
  (setq wc-goal-orig-words nil)
  (setq wc-goal-orig-lines nil)
  (setq wc-goal-orig-chars nil)
  (wc-goal-mode-update))

(defun wc-goal-set-word-goal (goal)
  "Set a goal for adding or removing words in the buffer"
  (interactive "nHow many words: ")
  (setq wc-goal-word-goal goal)
  (wc-goal-reset)
  (message "Goal set at %d words" goal))

(defun wc-goal-set-line-goal (goal)
  "Set a goal for adding or removing lines in the buffer"
  (interactive "nHow many lines: ")
  (setq wc-goal-line-goal goal)
  (wc-goal-reset)
  (message "Goal set at %d lines" goal))

(defun wc-goal-set-char-goal (goal)
  "Set a goal for adding or removing chars in the buffer"
  (interactive "nHow many characters: ")
  (setq wc-goal-char-goal goal)
  (wc-goal-reset)
  (message "Goal set at %d characters" goal))

(defun wc-goal-reached ()
  "Returns t when the goal change is reached."
  (or
   (if wc-goal-line-goal
       (if (< wc-goal-line-goal 0)
           (<= wc-goal-lines-delta wc-goal-line-goal)
         (>= wc-goal-lines-delta wc-goal-line-goal)))
   (if wc-goal-word-goal
       (if (< wc-goal-word-goal 0)
           (<= wc-goal-words-delta wc-goal-word-goal)
         (>= wc-goal-words-delta wc-goal-word-goal)))
   (if wc-goal-char-goal
       (if (< wc-goal-char-goal 0)
           (<= wc-goal-chars-delta wc-goal-char-goal)
         (>= wc-goal-chars-delta wc-goal-char-goal)))))


(defun wc-goal-count (&optional rstart rend field)
  "Count the words, lines and characters present in the region
following point. This function follows most of the rules present
in the `how-many' function. If INTERACTIVE is omitted or nil,
just return the word count, do not print it. Otherwise, if
INTERACTIVE is t, the function behaves according to interactive
behavior.

START and END specify the region to operate on.

When called interactively, this function first checks to see if
it is in Transient Mark mode.  If that is the case, then the
function operates over the marked region.  Otherwise, it will
operate over the entire buffer.
"
  (interactive)
  (if rstart
      (setq rend (max rstart rend))
    (if (and (called-interactively-p 'interactive)
             transient-mark-mode mark-active)
        (setq rstart (region-beginning)
              rend (region-end))
      (setq rstart (point-min)
            rend (point-max))))
  (let ((wcount (funcall wc-goal-count-words-function rstart rend))
        (lcount (funcall wc-goal-count-lines-function rstart rend))
        (ccount (funcall wc-goal-count-chars-function rstart rend)))
    (when (called-interactively-p 'interactive) 
      (message "%d line%s, %d word%s, %d char%s"
               lcount
               (if (= lcount 1) "" "s")
               wcount
               (if (= wcount 1) "" "s")
               ccount
               (if (= ccount 1) "" "s")))
    (if field
        (nth field (list lcount wcount ccount))
      (list lcount wcount ccount))))

(defalias 'wc 'wc-goal-count
  "Alias function `wc-goal-count' to the more legible `wc'.")

(defun wc-goal-generate-modeline ()
  (let ((modeline (wc-goal-format-modeline-string wc-goal-modeline-format)))
    (when (wc-goal-reached)
      (put-text-property 0 (length modeline) 'face 'wc-goal-face modeline))
    (list " " modeline)))

(defun wc-goal-mode-update ()
  "Return a string to update the modeline appropriately."
  (let* ((stats (wc-goal-count (point-min) (point-max))))
    (unless wc-goal-orig-lines (setq wc-goal-orig-lines (nth 0 stats)))
    (unless wc-goal-orig-words (setq wc-goal-orig-words (nth 1 stats)))
    (unless wc-goal-orig-chars (setq wc-goal-orig-chars (nth 2 stats)))
    (setq wc-goal-lines-delta (- (nth 0 stats) wc-goal-orig-lines))
    (setq wc-goal-words-delta (- (nth 1 stats) wc-goal-orig-words))
    (setq wc-goal-chars-delta (- (nth 2 stats) wc-goal-orig-chars))
    (wc-goal-generate-modeline)))

;;;###autoload
(define-minor-mode wc-goal-mode
  "Toggle wc mode With no argument, this command toggles the
mode.  Non-null prefix argument turns on the mode.  Null prefix
argument turns off the mode.

When Wc mode is enabled on a buffer, it counts the current words
in the buffer and keeps track of a differential of added or
subtracted words.

A goal of number of words added/subtracted can be set while using
this mode. Upon completion of the goal, the modeline text will
highlight indicating that the goal has been reached.

Commands:
\\{wc-goal-mode-map}

Entry to this mode calls the value of `wc-goal-mode-hook' if that
value is non-nil."
  ;; initial value (off)
  :init-value nil
  ;; The indicator for the mode line
  :lighter (:eval (wc-goal-mode-update))
  ;; The customization group
  :group 'wc-goal
  ;; The local keymap to use
  :keymap wc-goal-mode-map
  ;; The mode body code
  (if wc-goal-mode
      (run-mode-hooks 'wc-goal-mode-hooks)))

(provide 'wc-goal-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wc-goal-mode.el ends here

