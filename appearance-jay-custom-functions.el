;;;; functions to switch fonts and font-sizes in the editor

(defun courier-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Courier")
    (set-frame-width (selected-frame) 97))

(defun lucida-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Ludida Sans Typewriter"))

(defun georgia-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Georgia"))

(defun garamond-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Garamond Premier Pro"))


(defun monaco-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Monaco"))

(defun inconsolata-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Inconsolata":height 160))
(defun last-words ()
  (interactive)
  (set-face-attribute 'default nil :font "Last Words" :height 200))

(defun tiny-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 150)
  
  )

(defun miniscule-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 140)
  
  )


(defun set-font-size (value)
  "Set font size"
  (interactive "nfont size bra? ")
(set-face-attribute 'default nil  :height value)
)




(defun small-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 190)
  (set-frame-width (selected-frame) 89)
  )

(defun medium-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 215)
  (set-frame-width (selected-frame) 89)
  )

(defun large-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 350)
  (set-frame-width (selected-frame) 68)
  )

;;;; functions to adjust the transparency of the frame

(defun opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100))
  )



;;;; functions to automatically load color-themes

(defun zenburn ()
  "Open my favorite file:  ~.emacs.
The function is poorly named, didn't really want to 'load' it, just open it."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/zenburn-jay.el")
  (org-mode)
  (set-cursor-color "violet")
  )

(defun ritchie ()
  (interactive)
  (load-theme 'ritchie)
  (incarnadine-cursor)
  (org-mode)
  )

(defun solarized-dark ()
  "Open my own customized version of the Solarized color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/solarized-jay.el")
 (load-theme 'solarized-dark) 
  (org-mode)
  (incarnadine-cursor)
  )

(defun solarized-light ()
  "Open my own customized version of the Solarized color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/solarized-jay.el")
  (load-theme 'solarized-light)
  (org-mode)
  (incarnadine-cursor)
  )

(defun whiteboard ()
  "Open my own customized version of the Solarized color theme."
  (interactive)
    (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/whiteboard-jay.el")
  (org-mode)
    )

(defun spolsky ()
  "Open my own customized version of the Solarized color theme."
  (interactive)
  (load-file "/Users/jay/gnulisp/jay-custom-color-themes/spolsky-jay.el")
  (incarnadine-cursor)
    (org-mode)
  )

(defun big-cyberpunk ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-big-jay.el")
  (org-mode)
  (org-bullets-mode 1)
  (incarnadine-cursor)
  (turn-on-auto-capitalize-mode)
(boss-mode)
  )

(defun cyberpunk-jay ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-jay.el")
  (org-mode)
  (org-bullets-mode 1)
  (incarnadine-cursor)
  (turn-on-auto-capitalize-mode)
(play-sound-file "/Users/jay/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/Space Age Motor Cycle 02.wav"))

(defun cyberpunk-writeroom ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-jay.el")
  (org-mode)
  (org-bullets-mode 1)
  (incarnadine-cursor)
  (turn-on-auto-capitalize-mode)
  (writeroom-mode 1)
(play-sound-file "/Users/jay/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/Space Age Motor Cycle 02.wav"))

(defun cyberpunk-large ()
  "Open my own customized version of the Cyberpunk color theme."
  (interactive)
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-jay.el")
  (org-mode)
  (org-bullets-mode 1)
  (incarnadine-cursor)
  (turn-on-auto-capitalize-mode)
(large-type)
(play-sound-file "/Users/jay/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/Space Age Motor Cycle 02.wav"))

(defun matrix ()
  (interactive)
  (set-face-attribute 'default nil :background "black" :foreground "lime") 
  ;; (set-face-attribute 'default nil :background "black" :foreground "lime" :font "Courier" :height 180)
) 

;;;; functions to change the appearance of Emacs to various beautiful defaults automatically load 

;; Set transparency of emacs
(defun transparency-set-value (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (incarnadine-cursor)
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


(defun transparent-serenity (value)
"Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value) 
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-serenity.el")
  (toggle-frame-fullscreen)
  (toggle-frame-maximized)
  )

(defun rainy-highway-mode ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(65 65))
  (add-to-list 'default-frame-alist '(alpha 65 65))
  (load-file "~/Dropbox/emacs/prelude/personal/jay-custom-color-themes/cyberpunk-jay.el")
  (shut-the-fuck-up)
)

(defun shut-the-fuck-up()
  "disable all inline notifications"
  (interactive)
(org-mode)
  (boss-mode)
(flyspell-mode-off)
(turn-off-auto-capitalize-mode)
(writegood-mode 0)
(writeroom-mode 1)
(recenter-top-bottom))

(defun morning-pages()
  "distraction-free"
(interactive)
(org-capture)
(rainy-highway-mode)
(org-clock-in)
)

(defun medium-type ()
  (interactive)
  (set-face-attribute 'default nil  :height 260)
  (set-frame-width (selected-frame) 89)
  )



(defun leuven ()
  (interactive)
  (load-theme 'leuven)
  (incarnadine-cursor)
  (org-mode)
  )


(make-face 'hard-to-read-font)
(set-face-attribute 'hard-to-read-font nil :background "darkgrey" :foreground "grey")

(define-minor-mode hard-to-read-mode
  "This mode might be useful when you don't like certain text to be seen over your shoulders."
  :init-value nil :lighter " hard-to-read" :keymap nil
  (if hard-to-read-mode
      (progn
        (font-lock-mode nil)
        (buffer-face-mode t)
        (buffer-face-set 'hard-to-read-font))
    (progn
      (font-lock-mode t)
      (buffer-face-mode nil))))
