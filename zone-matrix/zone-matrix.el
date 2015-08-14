;; -*- Emacs-Lisp -*-
;; zone-matrix.el --- The matrix screen saver on Emacs.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Created: Jan 25, 2011
;; Time-stamp: <2013-07-28 17:53>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `zone-matrix' is a Matrix screen saver for Emacs.
;;
;; A note on noun:
;; A light bar represents a short list of chars highlighted green in column
;; on the screen, according to the film The Matrix.

;;; Code:


(require 'zone)
(eval-when-compile (require 'cl))


(defgroup zone-matrix nil
  "Matrix screen saver for Emacs."
  :group 'faces
  :prefix "zmx-")


(defface zmx-light-bar-body-face
  '((((class grayscale) (background dark)) (:foreground "green"))
    (((class color) (min-colors 256)) (:foreground "#39C139"))
    (((class color) (min-colors 16)) (:foreground "#39C139"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t :inherit default :foreground "green"))
  "*Face used to highlight the light bar body."
  :group 'zone-matrix)


(defface zmx-light-bar-head-face-1
  '((((class grayscale) (background dark))
     (:bold t :foreground "green" :weight bold))
    (((class color) (min-colors 256))
     (:bold t :foreground "#B5E4B3" :weight bold))
    (((class color) (min-colors 16))
     (:bold t :foreground "#B5E4B3" :weight bold))
    (((class color) (min-colors 8))
     (:bold t :foreground "green" :weight bold))
    (t :bold t :inherit default :foreground "green" :weight bold))
  "*Face used to highlight the light bar head."
  :group 'zone-matrix)


(defface zmx-light-bar-head-face-2
  '((((class grayscale) (background dark))
     (:bold t :foreground "green" :weight bold))
    (((class color) (min-colors 256))
     (:bold t :foreground "lightgreen" :weight bold))
    (((class color) (min-colors 16))
     (:bold t :foreground "lightgreen" :weight bold))
    (((class color) (min-colors 8))
     (:bold t :foreground "green" :weight bold))
    (t :bold t :inherit default :foreground "green" :weight bold))
  "*Face used to highlight the light bar head."
  :group 'zone-matrix)


(defcustom zmx-light-bar-head-faces
  '(zmx-light-bar-head-face-1
    zmx-light-bar-head-face-2)
  "*Face table of light bar head face."
  :group 'zone-matrix)


(defface zmx-light-bar-tail-face
  '((((class grayscale) (background dark))
     (:foreground "green" :weight light))
    (((class color) (min-colors 256))
     (:foreground "darkgreen" :weight light))
    (((class color) (min-colors 16))
     (:foreground "darkgreen" :weight light))
    (((class color) (min-colors 8))
     (:foreground "green" :weight light))
    (t :inherit default :foreground "green" :weight light))
  "*Face used to highlight the light bar tail."
  :group 'zone-matrix)


(defcustom zmx-unicode-mode nil
  "*The flag indicates that char displayed is unicode or ansii."
  :group 'zone-matrix)


(defcustom zmx-update-time 0.02
  "*The time to wait for before the next screen update."
  :group 'zone-matrix)


(defcustom zmx-update-speed-factor 1.5
  "*The factor of light bar falling speed on every screen update."
  :group 'zone-matrix)


(defcustom zmx-light-bar-max-length 30
  "*The maximum length of light bar."
  :group 'zone-matrix)


(defcustom zmx-light-bar-max-number 10
  "*The maximum number of a light bar."
  :group 'zone-matrix)


(defcustom zmx-light-bar-raise-rchance 25
  "*Reciprocal of the chance a light bar would raise.

If its value set to positive integer N, the chance of a light bar raise
from blank screen top would be 1/N."
  :group 'zone-matrix)


(defcustom zmx-light-bar-highlight-rchance 2
  "*Reciprocal of the chance a light bar would highlights.

If its value set to positive integer N, the chance of a light bar highlights
in brighter color would be 1/N."
  :group 'zone-matrix)


(defcustom zmx-light-bar-fade-rchance 8
  "*Reciprocal of the chance a light bar would fades.

If its value set to positive integer N, the chance of a light bar fades
into blank screen would be 1/N."
  :group 'zone-matrix)


(defsubst zmx-hit-chance (rchance)
  "Try to hit the chance of RCHANCE using random number."
  (= (random rchance) 1))

(defsubst zmx-light-bar-raise ()
  "Test whether a light bar would raise in random."
  (zmx-hit-chance zmx-light-bar-raise-rchance))

(defsubst zmx-light-bar-highlight ()
  "Test whether a light bar would highlight in random."
  (zmx-hit-chance zmx-light-bar-highlight-rchance))

(defsubst zmx-light-bar-fade ()
  "Test whether a light bar would fade in random."
  (zmx-hit-chance zmx-light-bar-fade-rchance))


(defun zmx-check-settings ()
  "Check the environment variable settings."
  (unless (and (> zmx-update-time 0)
               ;;(> zmx-update-speed-factor 1)
               (> zmx-light-bar-max-length 1)
               (> zmx-light-bar-max-number 1))
    (error "error in function `zone-matrix': wrong setting.")))


(defcustom zmx-ascii-char-table
  "ABCDEFGHIGKLMNOPQRSTUVWXYZabcdefghigklmnopqrstuvwxyz0123456789"
  "*ASCII char table to form light bar."
  :group 'zone-matrix)

(defcustom zmx-japan-char-table
  "\
あいうえお\
かきくけこ\
さしすせそ\
たちつてと\
はひふへほ\
なにぬねの\
まみむめも\
らりるれろ\
やゆよ\
ん\
がぎぐげご\
ざじずぜぞ\
だぢづでど\
ばびぶべぼ\
ぱぴぷぺぽ\
\
アイウエオ\
カキクケコ\
サシスセソ\
タチツテト\
ハヒフヘホ\
ナニヌネノ\
マミムメモ\
ラリルレロ\
ヤユヨ\
ン\
ガギグゲゴ\
ザジズゼゾ\
ダヂヅデド\
バビブベボ\
パピプペポ"
  "*Japan char table to form light bar."
  :group 'zone-matrix)

(defcustom zmx-unicode-char-table zmx-japan-char-table
  "*Unicode char table to form light bar."
  :group 'zone-matrix)


(defun zmx-blank-char ()
  "Return a blank char."
  (if zmx-unicode-mode
      (encode-char 12288 'ucs)
    ? ))

(defun zmx-random (seq)
  "Return a random element in sequence SEQ."
  (elt seq (random (length seq))))

(defun zmx-random-char ()
  "Return a random char to form light bar."
  (if zmx-unicode-mode
      (zmx-random zmx-unicode-char-table)
    (zmx-random zmx-ascii-char-table)))

(defun zmx-random-char-str ()
  "Return a random char to form light bar."
  (char-to-string (zmx-random-char)))

(defun zmx-random-light-bar-head-face ()
  "Return a random light bar head face."
  (zmx-random zmx-light-bar-head-faces))


(defun zmx-update (point arg)
  "Update the argument ARG, which is a one char string, at POINT."
  (goto-char point)
  (and (search-forward (buffer-substring point (1+ point)) (1+ point) t 1)
       (replace-match arg)))

(defun zmx-at-point (point)
  "Return the contents at POINT as a string(only one char)."
  (buffer-substring point (1+ point)))


(defun zmx-move-char (text old new)
  "Move char from position OLD to NEW in TEXT."
  (aset text new (aref text old))
  (put-text-property new (1+ new) 'face
                     (get-text-property old 'face text)
                     text))


(defun zmx-set-str-face (str face)
  "Set the text property face of string STR to FACE."
  (put-text-property 0 1 'face face str)
  str)


(cl-defmacro zmx-range-dotimes
    ((var range-end &optional (range-start 0) (from-end nil)) &rest body)
  "Loop a certain number of times.
Evaluate BODY with VAR bound to successive from range-end, exclusive,
to range-start, inclusive.  Then evaluate RESULT to get return value,
default nil.

\(fn (VAR RANGE-START [RANGE-END RESULT]) BODY...)"
  (declare (indent 1))
  (let ((end-value-name (gensym))
        (start-value-name (gensym)))
    `(let ((,end-value-name (1- ,range-end))
           (,start-value-name ,range-start))
       (if ,from-end
           (do* ((,var ,end-value-name (1- ,var)))
               ((< ,var ,start-value-name))
             ,@body)
         (do* ((,var ,start-value-name (1+ ,var)))
             ((> ,var ,end-value-name))
           ,@body)))))


(defun zmx-inner-loop-buffer-impl (win-width visible-column-number)
  "The inner loop of `zmx-buffer-impl'."
  (let ((column-index 0)
        (point 0)
        (old-property nil)
        (light-bar-states (make-vector visible-column-number 0)))
    ;; reset the seed of built-in random generator
    (random t)
    (while (not (input-pending-p))
      ;; update some light bars in random column
      (dotimes (counter (* visible-column-number zmx-update-speed-factor))
        (setq column-index (random visible-column-number) ;; start at 0
              point (1+ column-index))                    ;; start at 1
        ;; Move one position down for every char in current column.
        ;; Notice face is moved with its char.
        (zmx-range-dotimes (line-index win-height 1 t)
          (zmx-update (+ point (* line-index win-width))
                      (zmx-at-point
                       (+ point (* (1- line-index) win-width)))))
        ;; A lightweight state machine of embedded switch implementation
        ;; to update the char at point `point',
        ;; that is to add a new char and attach its face if necessary.
        (setq old-property (get-text-property point 'face))
        (cond
         ((equal old-property 'zmx-light-bar-body-face)
          ;; The light bar continues to fall down.
          (zmx-update point
                      (zmx-set-str-face
                       (zmx-random-char-str)
                       (if (<= (incf (aref light-bar-states column-index))
                               zmx-light-bar-max-length)
                           (if (zmx-light-bar-fade)
                               'zmx-light-bar-tail-face
                             'zmx-light-bar-body-face)
                         'zmx-light-bar-tail-face))))
         ((equal old-property 'zmx-light-bar-tail-face)
          ;; The light bar fades.
          (zmx-update point
                      (zmx-set-str-face (make-string 1 (zmx-blank-char)) nil))
          (aset light-bar-states column-index 0))
         ((member old-property zmx-light-bar-head-faces)
          ;; The light bar raises just now.
          (zmx-update point
                      (zmx-set-str-face
                       (zmx-random-char-str)
                       (if (<= (incf (aref light-bar-states column-index))
                               zmx-light-bar-max-length)
                           (if (zmx-light-bar-highlight)
                               (zmx-random-light-bar-head-face)
                             'zmx-light-bar-body-face)
                         'zmx-light-bar-tail-face))))
         (t
          ;; A blank char falls inside the screen.
          (if (zmx-light-bar-raise)
              (progn
                (zmx-update point
                            (zmx-set-str-face (zmx-random-char-str)
                                              (zmx-random-light-bar-head-face)))
                (aset light-bar-states column-index 1))
            (zmx-update point
                        (zmx-set-str-face (make-string 1 (zmx-blank-char)) nil))))))
      ;; wait for some time to update the screen
      (sit-for zmx-update-time))))


(defun zmx-buffer-impl ()
  "The Matrix screen saver of Emacs, buffer based implemention."
  ;; Hide the mode line in order to avoid the fast update of column value
  ;; at modeline when `column-mode' is on, which could be annoying.
  ;; In `zone-matrix' the point change a lot in a really fast speed.
  (zone-hiding-mode-line
   ;; Any proper message ouput to minibuffer sits here.
   ;; Personally I prefer to just clean it.
   (message "")
   (let* (;; To minus one from `(window-width)' to avoid
          ;; the continuation char '$' or '\' from displaying
          (win-width (if zmx-unicode-mode
                         (/ (1- (window-width)) 2)
                       (1- (window-width))))
          (win-height (window-height))
          ;; the number of column with visible char
          ;; the last column would have the char newline, which is not visible.
          (visible-column-number (1- win-width))
          quit)
     ;; Simply check for environment variable settings
     (zmx-check-settings)
     ;; Clean the last screen and insert the blank new content
     (erase-buffer)
     (let ((line (concat (make-string visible-column-number (zmx-blank-char)) [?\n])))
       (dotimes (var win-height)
         (insert line)))
     ;; Inner loop
     (condition-case err
         (zmx-inner-loop-buffer-impl win-width visible-column-number)
       ((debug error) (setq quit t))))
   )
  )


(defun zmx-inner-loop-text-impl (win-width visible-column-number text)
  "The inner loop of `zmx-text-impl'."
  (let ((column-index 0)
        (point 0)
        (old-property nil)
        (light-bar-states (make-vector visible-column-number 0)))
    ;; reset the seed of built-in random generator
    (random t)
    (while (not (input-pending-p))
      ;; update some light bars in random column
      (dotimes (counter (* visible-column-number zmx-update-speed-factor))
        (setq column-index (random visible-column-number))
        ;; Move one position down for every char in current column.
        ;; Notice face is moved with its char.
        (zmx-range-dotimes (line-index win-height 1 t)
          (zmx-move-char text
                         (+ column-index (* (1- line-index) win-width))
                         (+ column-index (* line-index win-width))))
        ;; A lightweight state machine of embedded switch implementation
        ;; to update the char of text at index `column-index',
        ;; that is to add a new char and attach its face if necessary.
        (setq old-property (get-text-property column-index 'face text))
        (cond
         ((equal old-property 'zmx-light-bar-body-face)
          ;; The light bar continues to fall down.
          (aset text column-index (zmx-random-char))
          (put-text-property column-index (1+ column-index) 'face
                             (if (<= (incf (aref light-bar-states column-index))
                                     zmx-light-bar-max-length)
                                 (if (zmx-light-bar-fade)
                                     'zmx-light-bar-tail-face
                                   'zmx-light-bar-body-face)
                               'zmx-light-bar-tail-face)
                             text))
         ((equal old-property 'zmx-light-bar-tail-face)
          ;; The light bar fades.
          (aset text column-index (zmx-blank-char))
          (put-text-property column-index (1+ column-index) 'face nil text)
          (aset light-bar-states column-index 0))
         ((member old-property zmx-light-bar-head-faces)
          ;; The light bar raises just now.
          (aset text column-index (zmx-random-char))
          (put-text-property column-index (1+ column-index) 'face
                             (if (<= (incf (aref light-bar-states column-index))
                                     zmx-light-bar-max-length)
                                 (if (zmx-light-bar-highlight)
                                     (zmx-random-light-bar-head-face)
                                   'zmx-light-bar-body-face)
                               'zmx-light-bar-tail-face)
                             text))
         (t
          ;; A blank char falls inside the screen.
          (if (zmx-light-bar-raise)
              (progn
                (aset text column-index (zmx-random-char))
                (put-text-property column-index (1+ column-index) 'face
                                   (zmx-random-light-bar-head-face)
                                   text)
                (aset light-bar-states column-index 1))
            (progn
              (aset text column-index (zmx-blank-char))
              (put-text-property column-index (1+ column-index) 'face nil
                                 text))))))
      ;; clean last screen and insert the new content
      (erase-buffer)
      (insert text)
      ;; move back to the point of one column with new char in top line
      (goto-char (1+ column-index))
      ;; wait for some time to update the screen
      (sit-for zmx-update-time))))


(defun zmx-text-impl ()
  "The Matrix screen saver for Emacs, text based implemention."
  ;; Hide the mode line in order to avoid the fast update of column value
  ;; at modeline when `column-mode' is on, which could be annoying.
  ;; In `zone-matrix' the point change a lot in a really fast speed.
  (zone-hiding-mode-line
   (message "")
   (let* (;; To minus one from `(window-width)' to avoid
          ;; the continuation char '$' or '\' from displaying
          (win-width (if zmx-unicode-mode
                         (/ (1- (window-width)) 2)
                       (1- (window-width))))
          (win-height (window-height))
          (text (make-string (* win-width win-height) (zmx-blank-char)))
          ;; the number of column with visible char
          ;; the last column would have the char newline, which is not visible.
          (visible-column-number (1- win-width))
          quit)
     ;; simply check for environment variable settings
     (zmx-check-settings)
     ;; insert all newline chars into text
     (dotimes (line-index win-height)
       (aset text (1- (* (1+ line-index) win-width)) ?\n))
     ;; Inner loop
     (condition-case err
         (zmx-inner-loop-text-impl win-width visible-column-number text)
       (error (setq quit t))))))


(defun zone-matrix ()
  "The entry function for the framework `zone' to call.

Two similiar function, `zmx-text-impl' and `zmx-buffer-impl', are provided
in this module based on different implementations.
The former keeps track of a text object, which will be used to
reflesh the whole screen. The later directly modify the zone buffer.
It seems the `zmx-text-impl' runs somewhat nicer than the other, even though
it requires higher resource consumption. So it is used as the default."
  (zmx-text-impl))


(provide 'zone-matrix)
