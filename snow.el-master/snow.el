;;; snow.el --- Let it snow in Emacs!         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Displays a buffer in which it snows.

;; NOTE: Be sure to preserve whitespace in the `snow-background'
;; string, otherwise the text-properties will become corrupted and
;; cause an error on load.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'color)

;;;; Variables

(defvar snow-flakes nil)

(defvar snow-amount 2)
(defvar snow-rate 0.09)
(defvar snow-timer nil)

;;;; Customization

(defgroup snow nil
  "Let it snow!"
  :group 'games)

(defcustom snow-pile-factor 100
  "Snow is reduced in mass by this factor when it hits the ground.
The lower the number, the faster snow will accumulate."
  :type 'number)

(defface snow-face
  '((t :foreground "white"))
  "The face.")

(defcustom snow-background
  #("                                       __                                                                                             
                                     _|__|_             __                                                                            
        /\\       /\\                   ('')            _|__|_                                                                          
       /  \\     /  \\                <( . )>            ('')                                                                           
       /  \\     /  \\               _(__.__)_  _   ,--<(  . )>                                                                         
      /    \\   /    \\              |       |  )),`   (   .  )                                                                         
       `||`     `||`               ==========='`       '--`-`                                                                         
" 39 41 (face (:foreground "black")) 172 178 (face (:foreground "black")) 190 193 (face (:foreground "black")) 278 280 (face (:foreground "green")) 287 289 (face (:foreground "green")) 308 309 (face (:foreground "white")) 309 311 (face (:foreground "black")) 311 312 (face (:foreground "white")) 324 330 (face (:foreground "black")) 412 413 (face (:foreground "green")) 413 416 (face (:foreground "green")) 421 425 (face (:foreground "green")) 441 442 (face (:foreground "brown")) 442 443 (face (:foreground "white")) 444 445 (face (:foreground "black")) 446 447 (face (:foreground "white")) 447 448 (face (:foreground "brown")) 460 461 (face (:foreground "white")) 461 463 (face (:foreground "black")) 463 464 (face (:foreground "white")) 547 560 (face (:foreground "green")) 576 579 (face (:foreground "white")) 579 580 (face (:foreground "black")) 580 583 (face (:foreground "white")) 586 587 (face (:foreground "black")) 590 593 (face (:foreground "brown")) 593 594 (face (:foreground "brown")) 594 595 (face (:foreground "white")) 597 598 (face (:foreground "black")) 599 600 (face (:foreground "white")) 600 601 (face (:foreground "brown")) 681 696 (face (:foreground "green")) 721 723 (face (:foreground "black")) 723 724 (face (:foreground "brown")) 724 725 (face (:foreground "brown")) 728 729 (face (:foreground "white")) 732 733 (face (:foreground "black")) 735 736 (face (:foreground "white")) 818 820 (face (:foreground "brown")) 827 829 (face (:foreground "brown")) 845 856 (face (:foreground "black")) 856 858 (face (:foreground "brown")) 865 871 (face (:foreground "gray")))
  "Background string."
  :type 'string)

;;;; Commands

(defun let-it-snow (&optional manual)
  (interactive "P")
  (with-current-buffer (get-buffer-create "*snow*")
    (if snow-timer
        (progn
          (cancel-timer snow-timer)
          (setq snow-timer nil))
      ;; Start
      (switch-to-buffer (current-buffer))
      (buffer-disable-undo)
      (setq-local cursor-type nil)
      (erase-buffer)
      (save-excursion
        (dotimes (_i (window-text-height (get-buffer-window (current-buffer) t)))
          (insert (make-string (window-text-width (get-buffer-window (current-buffer) t)) ? )
                  "\n"))
        (snow-insert-background :start-at -1))
      (goto-char (point-min))
      (setq snow-flakes nil)
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "SPC") (lambda ()
                                   (interactive)
                                   (snow--update-buffer (current-buffer))))
      (unless manual
        (setq snow-timer
              (run-at-time nil snow-rate (apply-partially #'snow--update-buffer (get-buffer-create "*snow*"))))
        (setq-local kill-buffer-hook (lambda ()
                                       (cancel-timer snow-timer)
                                       (setq snow-timer nil)))))))

;;;; Functions

(cl-defstruct snow-flake
  x y mass char overlay)

(defsubst clamp (min number max)
  "Return NUMBER clamped to between MIN and MAX, inclusive."
  (max min (min max number)))

(defsubst snow-flake-color (mass)
  (let ((raw (/ (+ mass 155) 255)))
    (color-rgb-to-hex raw raw raw 2)))

(defun snow-flake-get-flake (z)
  (pcase z
    ((pred (< 90)) (propertize "❄" 'face (list :foreground (snow-flake-color z))))
    ((pred (< 50)) (propertize "*" 'face (list :foreground (snow-flake-color z))))
    ((pred (< 10)) (propertize "." 'face (list :foreground (snow-flake-color z))))
    (_ (propertize "." 'face (list :foreground (snow-flake-color z))))))

(defun snow--update-buffer (buffer)
  (with-current-buffer buffer
    (let ((lines (window-text-height (get-buffer-window buffer t)))
          (cols (window-width (get-buffer-window buffer t))))
      (setq snow-flakes
            (append snow-flakes
                    (cl-loop for i from 0 to (random snow-amount)
                             for x = (random cols)
                             for mass = (float (random 100))
                             for flake = (make-snow-flake :x x :y 0 :mass mass :char (snow-flake-get-flake mass))
                             do (snow-flake-draw flake)
                             collect flake)))
      (setq snow-flakes
            (cl-loop for flake in snow-flakes
                     for new-flake = (progn
                                       ;; Calculate new flake position.
                                       (when (and (> (random 100) (snow-flake-mass flake))
                                                  ;; Easiest way to just reduce the chance of X movement is to add another condition.
                                                  (> (random 3) 0))
                                         (cl-incf (snow-flake-x flake) (pcase (random 2)
                                                                         (0 -1)
                                                                         (1 1)))
                                         (setf (snow-flake-x flake) (clamp 0 (snow-flake-x flake) (1- cols))))
                                       (when (> (random 100) (/ (- 100 (snow-flake-mass flake)) 3))
                                         (cl-incf (snow-flake-y flake)))
                                       (if (< (snow-flake-y flake) (1- lines))
                                           (progn
                                             ;; Redraw flake
                                             (snow-flake-draw flake)
                                             ;; Return moved flake
                                             flake)
                                         ;; Flake hit end of buffer: delete overlay.
                                         (snow-pile flake)
                                         (delete-overlay (snow-flake-overlay flake))
                                         nil))
                     when new-flake
                     collect new-flake)))
    (setq mode-line-format (format "%s flakes" (length snow-flakes)))))

(defun snow-flake-pos (flake)
  (save-excursion
    (goto-char (point-min))
    (forward-line (snow-flake-y flake))
    (forward-char (snow-flake-x flake))
    (point)))

(defun snow-pile (flake)
  (cl-labels ((landed-at (flake)
                         (let* ((pos (snow-flake-pos flake))
                                (mass (or (get-text-property pos 'snow (current-buffer)) 0)))
                           (pcase mass
                             ((pred (< 100))
                              (cl-decf (snow-flake-y flake))
                              (landed-at flake))
                             (_ (list pos mass))))))
    (pcase-let* ((`(,pos ,ground-snow-mass) (landed-at flake))
                 (ground-snow-mass (+ ground-snow-mass (/ (snow-flake-mass flake) snow-pile-factor)))
                 (ground-snow-string (pcase ground-snow-mass
                                       ((pred (<= 100)) (propertize "❄" 'face (list :foreground (snow-flake-color 100))))
                                       ((pred (< 90)) (propertize "❄" 'face (list :foreground (snow-flake-color ground-snow-mass))))
                                       ((pred (< 50)) (propertize "*" 'face (list :foreground (snow-flake-color ground-snow-mass))))
                                       ((pred (< 10)) (propertize "." 'face (list :foreground (snow-flake-color ground-snow-mass)))))))
      (when ground-snow-string
        (setf (buffer-substring pos (1+ pos)) ground-snow-string))
      (add-text-properties pos (1+ pos) (list 'snow ground-snow-mass) (current-buffer)))))

(defun snow-flake-draw (flake)
  (let ((pos (save-excursion
               (goto-char (point-min))
               (forward-line (snow-flake-y flake))
               (forward-char (snow-flake-x flake))
               (point))))
    (if (snow-flake-overlay flake)
        (move-overlay (snow-flake-overlay flake) pos (1+ pos))
      (setf (snow-flake-overlay flake) (make-overlay pos (1+ pos)))
      (overlay-put (snow-flake-overlay flake) 'display (snow-flake-char flake)))))

(cl-defun snow-insert-background (&key (s snow-background) (start-at 0))
  (let* ((lines (split-string s "\n"))
         (height (length lines))
         (start-at (pcase start-at
                     (-1 (- (line-number-at-pos (point-max)) height 1))
                     (_ start-at))))
    (cl-assert (>= (line-number-at-pos (point-max)) height))
    (remove-overlays)
    (save-excursion
      (goto-char (point-min))
      (forward-line start-at)
      (cl-loop for line in lines
               do (progn
                    (setf (buffer-substring (point) (+ (point) (length line))) line)
                    (forward-line 1))))))


;;;; Footer

(provide 'snow)

;;; snow.el ends here

;; Ensure that the before-save-hook doesn't, e.g. delete-trailing-whitespace,
;; which breaks the background string.

;; Local Variables:
;; before-save-hook: nil
;; End:
