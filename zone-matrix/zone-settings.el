;; -*- Emacs-Lisp -*-
;; Settings for `zone'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2013-08-05 10:21>

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

;;; Code:


(require 'zone)
(require 'zone-matrix-settings)


(defcustom zone-ad-restore nil
  "*A piece of code that should run by \"after\" advice of `zone'."
  :group 'zone)


(defadvice zone (before zone-ad-clean-ui)
  "Clean up the ui before `zone' runs."
  ;;
  ;; Save the current states for future restore.
  ;; Notice the sequence of code execution in the "after" advice
  ;; is the reversion of "before" advice.
  ;;
  ;; the window config
  (add-to-list 'zone-ad-restore
               `(set-window-configuration ,(current-window-configuration)))

  ;; if ecb is enabled, turn it off.
  (let ((ecb-on nil))
    (condition-case nil
        (if ecb-minor-mode
            (setq ecb-on t))
      (error (setq ecb-on nil)))
    (when ecb-on
      (add-to-list 'zone-ad-restore
                   '(ecb))
      (ecb-deactivate)))

  ;; fullfill the window with just one current buffer
  (delete-other-windows)
  ;;
  ;; the state of menu bar, tool bar and tabbar
  (when tabbar-mode
    (tabbar-mode -1)
    (add-to-list 'zone-ad-restore '(tabbar-mode 1)))
  (when scroll-bar-mode
    (scroll-bar-mode -1)
    (add-to-list 'zone-ad-restore '(scroll-bar-mode 1)))
  (when tool-bar-mode
    (tool-bar-mode -1)
    (add-to-list 'zone-ad-restore '(tool-bar-mode 1)))
  (when menu-bar-mode
    (menu-bar-mode -1)
    (add-to-list 'zone-ad-restore '(menu-bar-mode 1)))
  ;;
  ;; Make `zone-ad-restore' a self-disabling one-shot function
  (setq zone-ad-restore `(lambda ()
                           ,@zone-ad-restore
                           (setq zone-ad-restore nil))))


(defadvice zone (after zone-ad-restore-ui)
  "Retore the ui which is cleaned by the \"before\" advice."
  ;; restore the states before `zone' runs.
  (when zone-ad-restore
    (funcall zone-ad-restore)))


(defun zone-settings ()
  "Settings for `zone'."

  ;; apply settings for `zone-matrix'
  (zone-matrix-settings)
  ;; set `zone-matrix' to be the only zone program
  (setq zone-programs [
                       zone-pgm-putz-with-case
                       zone-pgm-random-life
                       zone-matrix
                       ])

  ;; activate advices
  (ad-activate 'zone)

  ;; trigger screen saver when Emacs is idle for a while
  ;; (zone-when-idle (* 60
  ;;                    15 ;; personally I feel 15 minutes is fine
  ;;                    ))
  )

(zone-settings)


(provide 'zone-settings)
