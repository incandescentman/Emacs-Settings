;;; tabula-rasa-mode.el --- Distraction free writing mode

;; Copyrigth (C) 2011  Ido Magal

;; Author: Ido Magal <M8R-u8t2l4(at)mailinator.com>
;; Version: 0.1.0
;; Keywords: distraction free, writing
;; URL: https://github.com/idomagal/Tabula-Rasa/blob/master/tabula-rasa.el

;; This file is *NOT* part of GNU Emacs.

;;; Commentary:

;; Tabula Rasa was inspired by darkroom-mode.el, WriteRoom, and all of the other  
;; distraction free tools. It was developed out of the need for a more customizable  
;; distraction free mode for Emacs.  

;; Fullscreen caveats:  
;; Win32:       supports maximized window, not total fullscreen yet.  
;; Mac OSX:     supports fullscreen with cocoa (ns-toggle-fullscreen)  

;;; Installation  

;; To use this mode, put this file in your common lisp files dir (e.g. .emacs.d/site-lisp)  
;; and put the following in your Emacs configuration file (e.g. .emacs) file:
;; (require 'tabula-rasa)  
;; Type M-x tabula-rasa-mode to toggle the mode.  

;; For customization of colors, etc, type M-x customize-group RET tabula-rasa RET
;; Customization options include:  
;; - Text font and colors.  
;; - Cursor and region colors.  
;; - Column width  
;; - Line spacing  
;; - Minor modes to enable or disable for Tabula Rasa  
;; - Whether or not to antialias text (experimental)  

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defgroup tabula-rasa nil
  "The latest in high-tech distraction free writing."
  :version 0.1
  :group 'text)

(defcustom tabula-rasa-width 80
  "Width of writing space."
  :type 'integer
  :group 'tabula-rasa
  :set (lambda (symbol value)
         (setq tabula-rasa-width value)
         (tabula-rasa-update-window))
  :initialize 'custom-initialize-default)

(defcustom tabula-rasa-line-spacing 5
  "Vertical line spacing."
  :type 'integer
  :group 'tabula-rasa
  ;; :set (lambda (symbol value)
  ;;        (setq tabula-rasa-line-spacing value)
  ;;        (modify-frame-parameters tabula-rasa-frame '((line-spacing . value))))
  :initialize 'custom-initialize-default)

(defcustom tabula-rasa-toggle-antialiasing nil
  "Whether or not to toggle anti-aliasing on text.
Recommended for non-monotype, 'pretty' fonts."
  :type 'boolean
  :group 'tabula-rasa)

(defcustom tabula-rasa-minor-mode-states
  '(("global-highline-mode" . nil) ("global-highlight-parentheses-mode" . nil))
  "This allows you to temporarily disable or enable minor modes for Tabula Rasa.
Add the minor mode and the desired state while in Tabula Rasa mode."
  :type '(repeat (cons :format "%v"
                       (string :tag "Minor Mode")
                       (boolean :tag "State"))))  

(setq tabula-rasa-mode nil)
(setq tabula-rasa-frame nil)

(define-minor-mode tabula-rasa-mode
  "YADFM: Yet Another Distraction Free Writing Mode" 
  :lighter " TR"
  :init-value nil
  :group 'tabula-rasa
  :global t
; Awkwardly backwards logic. It would appear that define-minor-mode toggles the mode variable prior to evaluating the body.
  (if tabula-rasa-mode
      (tabula-rasa-mode-enable)
    (tabula-rasa-mode-disable t)))

(defun tabula-rasa-update-window()
;;  (interactive)
  (cond 
   ((not (frame-live-p tabula-rasa-frame))
    (tabula-rasa-mode-disable nil))
   ((or (one-window-p t tabula-rasa-frame) (window-full-width-p tabula-rasa-window))
    (set-window-margins tabula-rasa-window
                        (/ (- (frame-width tabula-rasa-frame) tabula-rasa-width) 2)              
                        (/ (- (frame-width tabula-rasa-frame) tabula-rasa-width) 2)))
   (t
    (set-window-margins tabula-rasa-window 0 0)
    (set-window-margins (next-window) 0 0))))

;width test: 80 chars
;12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789


(defun tabula-rasa-save-mmodes ()
;;  (interactive)
  (setq tr-saved-mmodes '())
  (mapc (lambda (mode)
          (if (boundp (read (car mode)))
              (setq tr-saved-mmodes (cons (cons (car mode) (symbol-value (intern (car mode)))) tr-saved-mmodes))))
        tabula-rasa-minor-mode-states))

(defun tabula-rasa-set-mmodes (mmodes-alist)
;;  (interactive)
  (mapc (lambda (mode)
          (if (boundp (read (car mode)))
              (cond
               ((cdr mode) (eval (read (concat "(" (car mode) " 1)"))))
               (t          (eval (read (concat "(" (car mode) " 0)")))))))
        mmodes-alist))

(defun tabula-rasa-mode-enable()
;;  (interactive)
  (if (eq tabula-rasa-mode 1)
      (progn
        (message "Tabula Rasa mode is already running")
        (exit)))
;; minor modes adjustment
  (tabula-rasa-save-mmodes)
  (tabula-rasa-set-mmodes tabula-rasa-minor-mode-states)
;; antialiasing
  (if tabula-rasa-toggle-antialiasing
      (setq ns-antialias-text (not ns-antialias-text)))

  (setq tabula-rasa-frame (make-frame `(
                                        (fullscreen . fullboth)
                                        (unsplittable . t)
                                        (left-fringe . 0)
                                        (right-fringe . 0)
                                        (tool-bar-lines . 0)
                                        (menu-bar-lines . 0)
                                        (vertical-scroll-bars . nil)
                                        (line-spacing . ,tabula-rasa-line-spacing)
                                        
                                        )))
  
  (setq tabula-rasa-window (frame-selected-window tabula-rasa-frame))
  (add-hook 'window-configuration-change-hook 'tabula-rasa-update-window t nil)
  (add-hook 'delete-frame-functions (
                                     lambda (frame)
                                            (if (eq frame tabula-rasa-frame)
                                                  (tabula-rasa-mode-disable nil))))
  (select-frame tabula-rasa-frame)
  (cond 
   ((string= system-type "darwin")
   )
   ((string= system-type "gnu/linux")
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
   ((string= system-type "windows-nt")
    (if (fboundp 'w32-send-sys-command)
        ;; WM_SYSCOMMAND maximaze #xf030
        (w32-send-sys-command 61488))))
  
  (tabula-rasa-update-window))

(defun tabula-rasa-mode-disable(del-frame)
;;  (interactive)
  (if (frame-live-p tabula-rasa-frame)
      (progn
        (if tabula-rasa-toggle-antialiasing
            (progn
              (setq ns-antialias-text (not ns-antialias-text))
              ;; On OSX, toggling antialiasing doesn't refresh the entire display so
              ;; we force it.
              (redraw-display)))
        (setq tabula-rasa-mode nil)
        (select-frame tabula-rasa-frame)
        (cond 
         ((string= system-type "darwin")
          )
         ((string= system-type "gnu/linux")
          (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
         ((string= system-type "windows-nt")
          (if (fboundp 'w32-send-sys-command)
              ;; WM_SYSCOMMAND restore #xf120
              (w32-send-sys-command 61728)
            (progn (set-frame-parameter tabula-rasa-frame 'width 82)
                   (set-frame-parameter tabula-rasa-frame 'fullscreen 'fullheight)))))
        
        (remove-hook 'window-configuration-change-hook 'tabula-rasa-update-window)
        (remove-hook 'delete-frame-functions 'tabula-rasa-mode-disable)
        (if del-frame (delete-frame tabula-rasa-frame))
        (tabula-rasa-set-mmodes tr-saved-mmodes)
        )))

(provide 'tabula-rasa)

;;; tabula-rasa-mode.el ends here
