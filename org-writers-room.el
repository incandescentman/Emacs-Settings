(require 'owr-helper)
(defgroup org-writers-room nil
  "Book & project-writing setup for org-mode"
  :group 'org-writers-room)

(defcustom org-writers-room-column-width 35
  "Width of the side columns"
  :group 'org-writers-room
  :type 'number)

(defcustom org-writers-room-properties '(("Synopsis" . "Summary") ("Role in Book" . "Hints") ("Characters" . "chars"))
  "alist of properties to be inserted automatically on heading creation"
  :group 'org-writers-room
  :type 'alist)

(defcustom org-writers-room-aggressive   t
  "boolean -- should owr aggressively keep your attention by closing other buffers?"
  :group 'org-writers-room
  :type 'boolean)

;; ORG-WR-META Mode.  
;; Trivial minor mode to display a properties drawer in the metadata window of writers-mode
;; minor mode is probably not necessary, actually.  

(define-minor-mode org-wr-meta
  "Toggle Writer's Room Metadata Window Mode.  
Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state."
  ;; initial value
  :init-value nil
  ;;
  ;; The indicator for the mode line.
  :lighter "WR-Meta"
  ;; minor mode keybindings
   :keymap 
   '(([?\C-c ?\C-x ?g] . owr-goto-guide)
     ([?\C-c ?\C-x ?m] . owr-goto-main)
    )
  ;; 
  :group "org-writers-room"
  :global nil
  (run-hooks 'org-wr-meta-hooks 
	     )
  )

;; narrow the buffer automatically to the properties drawer
(add-hook 'org-wr-meta-hooks 'org-wr-meta-narrow)



;; I'm not using this one currently
(defun toggle-current-window-dedication ()
  "this function allows a key binding to set the dedication value of a
window.  could be useful for sustaining concentration, but I'm not sure I want to impose it on everyone just yet."
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;; (global-set-key [pause] 'toggle-current-window-dedication)


	  
	    
	     ;; grab the buffer location
      ;; go to guide window
      ;; kill the indirect buffers
      ;; destroy the old windows
      ;; unlock the current window from this buffer
      ;; ))
;; )


;; set the hooks up
;; for some reason this hook seems to be called when the mode istoggled off, too!  
;; not sure why that would be.  
(setq org-writers-room-hooks nil)

;; -- BEGIN org-wr-main
;; Trivial minor mode to display a an org node in the main window of writers-mode

(define-minor-mode org-wr-main 
  "Toggle Writer's Room Main Window Mode.  
Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state."
  ;; initial value
  :init-value nil
  ;;
  ;; The indicator for the mode line.
  :lighter "WR-Main"
  ;; minor mode keybindings
   :keymap 
   '(([?\C-c ?\C-x ?g] . owr-goto-guide)
     ([?\C-c ?\C-x ?m] . owr-goto-meta)
    )
  ;; 
  :group "org-writers-room"
  :global nil
  (run-hooks 'org-wr-main-hooks 
	     )
  )


;; Several functions to populate each new heading with a properties drawer
;; would also be nice to completely hid the properties drawer after it's created
;; cf hide-show-mode solution here: http://stackoverflow.com/questions/17478260/completely-hide-the-properties-drawer-in-org-mode


;; 
(defun org-wr-main-heading-hook ()
  "Adds a properties drawer & populates it with several properties.  Intended to be used with org-insert-heading-hook, but is also interactive."
  (interactive)
  (save-excursion
    (dolist (this-property org-writers-room-properties)
      (org-set-property (car this-property) (cdr this-property))
    )
    (org-flag-drawer 'nil)
    ;; (select-window (window-with-name "guide")
                   )
    (org-cycle-hide-drawers 'all)
    )
  


(defun org-wr-main-property-fns ()
  "adds a hook to org-insert-heading-hook that automatically adds property drawers whenever a heading is created"
  (interactive)
  (if org-wr-main
      (progn
	(make-local-variable 'org-insert-heading-hook)
	(add-to-list 'org-insert-heading-hook 'org-wr-main-heading-hook))
    (progn
      (remove-hook 'org-insert-heading-hook 'org-wr-main-heading-hook))      
  )
  )

(add-hook 'org-wr-main-hooks 'org-wr-main-property-fns)

;; --- END org-wr-main -------------


;; --- BEGIN org-wr-guide ------------
;; Trivial minor mode to display guide of an org buffer  in the side window of writers-mode

(defvar org-wr-last-meta-buffer nil)
(define-minor-mode org-wr-guide
  "Toggle Writer's Room Guide Window Mode.  
Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state."
  ;; initial value
  :init-value nil
  ;;
  ;; The indicator for the mode line.
  :lighter "WR-guide"
  ;; minor mode keybindings
  :keymap 
  '(([?\C-c ?\C-x ?b] . org-writers-room-tree-to-indirect-buffer)
   ((kbd "<return>") . org-writers-room-tree-to-indirect-buffer)
   ;; ([?\C-c ?\C-x ?g] . owr-goto-guide)
   ([?\C-c ?\C-x ?m] . owr-goto-meta)
    )
  :group "org-writers-room"
  :global nil
  (run-hooks 'org-wr-guide-hooks 
	     )
  )

;; a bunch of keymappings, somewhat random, ouch
(define-key org-wr-guide-map [remap org-cycle] 'owr-cycle)
(define-key org-wr-meta-map [remap org-beginning-of-line] 'owr-beginning-of-line)
(define-key org-wr-main-map [remap org-beginning-of-line] 'owr-beginning-of-line)
(define-key org-wr-guide-map (kbd "<return>") 'org-writers-room-tree-to-indirect-buffer) 
(add-hook 'org-wr-guide-hooks 'org-writers-room-windows)
;; (add-hook 'org-wr-guide-hooks 'org-wr-side-narrow)
(add-hook 'org-wr-guide-hooks 'owr-hide-show-top)

;; --- END org-wr-guide --------------------------


;; --- BEGIN org-writers-room mode
;; (define-minor-mode org-writers-room 
;;   "Toggle Writer's Room Mode.  
;; Interactively with no argument, this command toggles the mode.
;;      A positive prefix argument enables the mode, any other prefix
;;      argument disables it.  From Lisp, argument omitted or nil enables
;;      the mode, `toggle' toggles the state."
;;   ;; initial value
;;   :init-value nil
;;   ;;
;;   ;; The indicator for the mode line.
;;   :lighter "WriRo"
;;   ;; minor mode keybindings
;;   :keymap 
;;   '(([?\C-c ?\C-x ?b] . org-writers-room-tree-to-indirect-buffer)
;;     )
;;   ;; 
;;   :group "org-writers-room"
;;   :global nil
;;   (if (eq major-mode 'org-mode)
;;   (run-hooks 'org-writers-room-hooks ))
;;     )

;; make sure that the original buffer gets guide-mode
;; (add-hook 'org-writers-room-hooks 'org-wr-guide)

(defun org-writers-room ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn
	(if (and (boundp 'org-writers-room-on) org-writers-room-on)
	    (progn
              (setq org-writers-room-on nil)
              (if 'org-writers-room-aggressive
                  (desktop-read))
              (select-window (window-with-name "guide")
                             )
              (org-wr-guide 'toggle)
              )
	  (setq org-writers-room-on t)
          (if 'org-writers-room-aggressive
              (owr-save-and-close-others))
          (org-wr-guide 'toggle)
          )
        )
    ))




(provide 'org-writers-room)

