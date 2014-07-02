;; helper functions for org-writer-room

;; * Part 1.  Functions to make windows behave
;; helper function to set width of window
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

;; main window setup
(defun org-writers-room-windows ()
  "Configure the windows for org-writers-room into 3 panes."
  (interactive "")
  (message "writers room is on: %s" org-writers-room-on)
  (if org-writers-room-on
      ;; When owr is on, delete other windows, create new ones, and name them
      (progn
        (setq org-wr-last-meta-buffer nil)
        (setq width org-writers-room-column-width)
        (global-linum-mode 0)
        (delete-other-windows)
        (set-window-dedicated-p (selected-window) t)
        (let* ((main (split-window nil width t))
               (metadata (split-window main (- width) t)))
          (set-window-name (selected-window) "guide")
          (set-window-name main "main")
          (set-window-name metadata "metadata"))
        (select-window (window-with-name "main"))
        ;; (setq my-buffer-name-regex (concat (buffer-name) "-") )
        (org-global-cycle 3)
        (org-writers-room-tree-to-indirect-buffer))
    ;; when owr is off, get rid of old named windows and indirect buffers
    (let ((realpoint nil)
          (origbuf (current-buffer)))
      (if (or (eq origbuf org-last-indirect-buffer)
              (eq origbuf org-wr-last-meta-buffer))
          (setq realpoint (point)))
      (select-window (window-with-name "guide"))
      (delete-other-windows (selected-window))
      (set-window-dedicated-p (selected-window) nil)
      ;; (pop-to-buffer org-wr-guide-buffer)
      (if realpoint
          (goto-char realpoint))
      (org-flag-heading nil t)
      (if (eq origbuf org-wr-last-meta-buffer)
          (org-wr-kill-indirects)
        (if (buffer-live-p org-wr-last-meta-buffer)
            (kill-buffer  org-wr-last-meta-buffer))
        (if (and (buffer-live-p org-last-indirect-buffer))
            (kill-buffer org-last-indirect-buffer))
        )))
  )


;; ** 3 helper functions to enable us to work with named windows (important)
;; --------------------------------------------

(defun set-window-name (window name)
  "add a 'name' parameter to the given window"
  (set-window-parameter window 'name name))

(defun window-with-name (name)
  "retrieve the window with the given name"
  (window-with-parameter 'name name))


(defun display-buffer-in-named-window (buffer &optional name)
  "Try displaying BUFFER in a window with parameter 'name'.
  If name is nil, attempts to display in window named 'main'."
  (if name
      nil
    (setq name "main"))
  (let ((named-window (window-with-name name)))
    (when named-window
      (window--display-buffer
       buffer named-window 'reuse nil display-buffer-mark-dedicated))))


;; ** and now three functions to bind keys to
(defun owr-goto-guide ( )
  (interactive )
  (select-window (window-with-name "guide")))

(defun owr-goto-window ( wname )
  (interactive "s  Goto Window: " )
  (unless (boundp 'wname)
    (setq wname "guide"))
  (print wname)
  (select-window (window-with-name 'wname )))
(defun owr-goto-main ( )
  (interactive )
  (select-window (window-with-name "main")))

(defun owr-goto-meta ( )
  (interactive )
  (select-window (window-with-name "metadata")))
;; --------------------------------------------
;; end named windows

;; * Part 2.  Buffer Creation And Destruction Helpers

;; ** focus
;; encourage focus by saving and closing all other open buffers
(defun owr-save-and-close-others ()
  "Save1 and close all other buffers before starting writer-mode."
  (interactive)
  (save-some-buffers)
  (desktop-kill)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

;; ** indirect buffer creation
;; indirect buffer creation for org-writers-room
;; stolen from org.el, but had to rewrite to remove cloning
(defun org-writers-room-get-indirect-buffer (&optional buffer newname )
  "create an indirect buffer from current bufer, but do not clone"
  (setq buffer (or buffer (current-buffer)))
  (let ((n 1) (base (buffer-name buffer)) bname)
    (while (buffer-live-p
            (get-buffer (setq bname (concat base "-" (number-to-string n)))))
      (setq n (1+ n)))
    ;; allow buffer name to be set when called
    ;; this gives more meaningful buffer names in org-writers-room
    (if newname
        (setq bname newname)
      )

    (condition-case nil
        ;; this is the main diference form org-get-indirect-buffer
        ;; final option 'clone is missing
        ;; (make-indirect-buffer buffer bname 'clone)
        (make-indirect-buffer buffer bname )
      (error (make-indirect-buffer buffer bname)))))


;; cleanup all indirect buffers.  To use when closing owr-mode
(defun org-wr-kill-indirects ()
  "clean up all indirect buffers"
  (if (and (buffer-live-p org-last-indirect-buffer))
      (kill-buffer org-last-indirect-buffer))
  (if (buffer-live-p org-wr-last-meta-buffer)
      (kill-buffer  org-wr-last-meta-buffer))
  )


;; create the meta buffer
(defun org-wr-meta-narrow ()
  "narrow the buffer to the properties drawer of the active heading"
  (interactive)
  (setq pbegin nil)
  (save-excursion
    (org-back-to-heading t)
    (setq pbegin (search-forward-regexp ":PROPERTIES:" nil 1)))
  (unless pbegin
    (org-wr-main-heading-hook)
    (save-excursion
      (org-back-to-heading t)
      ;; (org-wr-meta-narrow)
      (setq pbegin (search-forward-regexp ":PROPERTIES:" nil 1))
      ))
  (save-excursion
    (org-back-to-heading t)
    (search-forward-regexp ":END:")
    (setq end (match-beginning 0)))
  (narrow-to-region pbegin end)
  (show-all)
  )


;; helper function to hide all drawers in guide-mode
(defun org-hide-drawers ()
  "Fold all drawers in buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-property-start-re nil 'NOERROR)
      (org-flag-drawer t)))
  )






;; * Part III: Functions that affect visibility of text
(defun owr-get-start ()
  "find the start of our first headline"
  (interactive)
  (let ((tree (org-element-parse-buffer "greater-element")))
    (org-element-map tree 'headline
      (lambda (hl)
        (org-element-property :begin hl))
      nil t))
  )

(defun owr-hide-top ()
  "make text above first headline invisible."
  (interactive)
  (add-to-invisibility-spec '(owr-invisible . nil))
  ;;(add-to-invisibility-spec 'owr-invisible)
  (let ((end (owr-get-start)))
    (print end)
    (overlay-put (make-overlay 1 (- end 1))
                 'invisible 'owr-invisible)
    (owr-show-title)

    ))

(defun owr-show-top ()
  "remove owr invisiblity overlays"
  (interactive )
  (remove-from-invisibility-spec '(owr-invisible . t))
  (remove-from-invisibility-spec 'owr-invisible)
  (remove-overlays  1 (buffer-end 1)  'invisible 'owr-invisible)
  )



(defun owr-show-title ()
  "get the title to show"
  (interactive)
  (add-to-invisibility-spec 'owr-invisible)
  (save-excursion
    (goto-char 1)
    (let ((start (search-forward "#+TITLE:"))
          (end (search-forward "
")))
      (remove-overlays start end 'invisible 'owr-invisible))
    ))

(defun owr-hide-show-top ( )
  (interactive )
  (if org-writers-room-on
      (progn  (owr-hide-top)
              (owr-hide-ellipses))
    (owr-show-top)
    (owr-show-ellipses)))

(defun owr-hide-ellipses ()
  "Don't show ellipses after hidden elements "
  (interactive)
  (remove-from-invisibility-spec '(outline . t))
  (add-to-invisibility-spec '(outline . nil))
  )
(defun owr-show-ellipses ()
  "Show ellipses after hidden elements"
  (interactive)
  (remove-from-invisibility-spec '(outline . nil))
  (add-to-invisibility-spec '(outline . t))
  )


;; * Part 4:  Hacked org-cycle functions

(defun owr-cycle (&optional arg)
  "TAB-action and visibility cycling for Org-mode.

This is the command invoked in Org-mode by the TAB key.  Its main purpose
is outline visibility cycling, but it also invokes other actions
in special contexts.

- When this function is called with a prefix argument, rotate the entire
  buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.
  When called with two `C-u C-u' prefixes, switch to the startup visibility,
  determined by the variable `org-startup-folded', and by any VISIBILITY
  properties in the buffer.
  When called with three `C-u C-u C-u' prefixed, show the entire buffer,
  including any drawers.

- When inside a table, re-align the table and move to the next field.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
  If there is no subtree, switch directly from CHILDREN to FOLDED.

- When point is at the beginning of an empty headline and the variable
  `org-cycle-level-after-item/entry-creation' is set, cycle the level
  of the headline by demoting and promoting it to likely levels.  This
  speeds up creation document structure by pressing TAB once or several
  times right after creating a new headline.

- When there is a numeric prefix, go up to a heading with level ARG, do
  a `show-subtree' and return to the previous cursor position.  If ARG
  is negative, go up that many levels.

- When point is not at the beginning of a headline, execute the global
  binding for TAB, which is re-indenting the line.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is at the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg
  (C-u TAB, same as S-TAB) also when called without prefix arg.
  But only if also the variable `org-cycle-global-at-bob' is t."
  (interactive "P")
  (org-load-modules-maybe)
  (unless (or (run-hook-with-args-until-success 'org-tab-first-hook)
              (and org-cycle-level-after-item/entry-creation
                   (or (org-cycle-level)
                       (org-cycle-item-indentation))))
    (let* ((limit-level
            (or org-cycle-max-level
                (and (boundp 'org-inlinetask-min-level)
                     org-inlinetask-min-level
                     (1- org-inlinetask-min-level))))
           (nstars (and limit-level
                        (if org-odd-levels-only
                            (and limit-level (1- (* limit-level 2)))
                          limit-level)))
           (org-outline-regexp
            (if (not (derived-mode-p 'org-mode))
                outline-regexp
              (concat "\\*" (if nstars (format "\\{1,%d\\} " nstars) "+ "))))
           (bob-special (and org-cycle-global-at-bob (not arg) (bobp)
                             (not (looking-at org-outline-regexp))))
           (org-cycle-hook
            (if bob-special
                (delq 'org-optimize-window-after-visibility-change
                      (copy-sequence org-cycle-hook))
              org-cycle-hook))
           (pos (point)))

      (if (or bob-special (equal arg '(4)))
          ;; special case:  use global cycling
          (setq arg t))

      (cond

       ((equal arg '(16))
        (setq last-command 'dummy)
        (org-set-startup-visibility)
        (org-unlogged-message "Startup visibility, plus VISIBILITY properties"))

       ((equal arg '(64))
        (show-all)
        (org-unlogged-message "Entire buffer visible, including drawers"))

       ;; we don't need this part, since we never want to show or be in tables
       ;; Table: enter it or move to the next field.
       ;; ((org-at-table-p 'any)
       ;;  (if (org-at-table.el-p)
       ;;      (message "Use C-c ' to edit table.el tables")
       ;;    (if arg (org-table-edit-field t)
       ;;      (org-table-justify-field-maybe)
       ;;      (call-interactively 'org-table-next-field))))

       ;; I doubt we need this either but can't fund fn so not uncommenting
       ((run-hook-with-args-until-success
         'org-tab-after-check-for-table-hook))

       ;; keep this
       ;; Global cycling: delegate to `owr-cycle-internal-global'.
       ((eq arg t) (owr-cycle-cycle-internal-global))

       ;; shouldn't need this either
       ;; Drawers: delegate to `org-flag-drawer'.
       ((save-excursion
          (beginning-of-line 1)
          (looking-at org-drawer-regexp))
        (org-flag-drawer                ; toggle block visibility
         (not (get-char-property (match-end 0) 'invisible))))

       ;; again, don't want this -- never want to show subtree
       ;; Show-subtree, ARG levels up from here.
       ;; ((integerp arg)
       ;;  (save-excursion
       ;;    (org-back-to-heading)
       ;;    (outline-up-heading (if (< arg 0) (- arg)
       ;;                       (- (funcall outline-level) arg)))
       ;;    (org-show-subtree)))

       ;; this is weird stuff, but shouldn't matter for us, leaving for now
       ;; Inline task: delegate to `org-inlinetask-toggle-visibility'.
       ((and (featurep 'org-inlinetask)
             (org-inlinetask-at-task-p)
             (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
        (org-inlinetask-toggle-visibility))

       ((org-try-cdlatex-tab))

       ;; At an item/headline: delegate to `owr-cycle-internal-local'.
       ((and (save-excursion (beginning-of-line 1)
                             (looking-at org-outline-regexp))
             (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
        (owr-cycle-internal-local))

       ;; I have a feeling all of this stuff is superfluous, but leaving in for now
       ;; From there: TAB emulation and template completion.
       (buffer-read-only (org-back-to-heading))

       ((run-hook-with-args-until-success
         'org-tab-after-check-for-cycling-hook))

       ((org-try-structure-completion))

       ((run-hook-with-args-until-success
         'org-tab-before-tab-emulation-hook))

       ((and (eq org-cycle-emulate-tab 'exc-hl-bol)
             (or (not (bolp))
                 (not (looking-at org-outline-regexp))))
        (call-interactively (global-key-binding "\t")))

       ((if (and (memq org-cycle-emulate-tab '(white whitestart))
                 (save-excursion (beginning-of-line 1) (looking-at "[ \t]*"))
                 (or (and (eq org-cycle-emulate-tab 'white)
                          (= (match-end 0) (point-at-eol)))
                     (and (eq org-cycle-emulate-tab 'whitestart)
                          (>= (match-end 0) pos))))
            t
          (eq org-cycle-emulate-tab t))
        (call-interactively (global-key-binding "\t")))

       (t (save-excursion
            (org-back-to-heading)
            (org-cycle)))))))

;; this is broken right now -- not showing all descendants -- was working but lost the working version

(defun owr-cycle-internal-local ()
  "Do the local cycling action."
  (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
    ;; First, determine end of headline (EOH), end of subtree or item
    ;; (EOS), and if item or heading has children (HAS-CHILDREN).
    (save-excursion
      (if (org-at-item-p)
          (progn
            (beginning-of-line)
            (setq struct (org-list-struct))
            (setq eoh (point-at-eol))
            (setq eos (org-list-get-item-end-before-blank (point) struct))
            (setq has-children (org-list-has-child-p (point) struct)))
        (org-back-to-heading)
        (setq eoh (save-excursion (outline-end-of-heading) (point)))
        (setq eos (save-excursion (1- (org-end-of-subtree t t))))
        (setq has-children
              (save-excursion
                (let ((level (funcall outline-level)))
                  (outline-next-heading)
                  (and (org-at-heading-p t)
                       (> (funcall outline-level) level))))
              ))
      ;; Determine end invisible part of buffer (EOL)
      (beginning-of-line 2)
      ;; XEmacs doesn't have `next-single-char-property-change'
      (if (featurep 'xemacs)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2))
        (while (and (not (eobp)) ;; this is like `next-line'
                    (get-char-property (1- (point)) 'invisible))
          (goto-char (next-single-char-property-change (point) 'invisible))
          (and (eolp) (beginning-of-line 2))))
      (setq eol (point)))


    ;; Find out what to do next and set `this-command'
    (cond

     ;; if this is an empty engry, don't change status of anything
     ((= eos eoh)
      ;; Nothing is hidden behind this heading
      (unless (org-before-first-heading-p)
        (run-hook-with-args 'org-pre-cycle-hook 'empty))
      (org-unlogged-message "EMPTY ENTRY")
      (setq org-cycle-subtree-status nil)
      (save-excursion
        (goto-char eos)
        (outline-next-heading)
        (if (outline-invisible-p) (org-flag-heading nil))))

     ;; if the entire subtree is hidden, show all  children
     ((and (or (>= eol eos)
               (not (string-match "\\S-" (buffer-substring eol eos))))
           (or has-children
               (not (setq children-skipped
                          org-cycle-skip-children-state-if-no-children))))
      ;; Entire subtree is hidden in one line: switch to children view
      (unless (org-before-first-heading-p)
        (run-hook-with-args 'org-pre-cycle-hook 'children))
      (if (org-at-item-p)
          (org-list-set-item-visibility (point-at-bol) struct 'children)
        ;; (org-show-entry)
        (org-with-limited-levels (show-children))
        ;; FIXME: This slows down the func way too much.
        ;; How keep drawers hidden in subtree anyway?
        ;; (when (memq 'org-cycle-hide-drawers org-cycle-hook)
        ;;   (org-cycle-hide-drawers 'subtree))

        ;; shouldn't need this stuff as we never want lists
        ;; Fold every list in subtree to top-level items.
        ;; (when (eq org-cycle-include-plain-lists 'integrate)
        ;;   (save-excursion
        ;;     (org-back-to-heading)
        ;;     (while (org-list-search-forward (org-item-beginning-re) eos t)
        ;;       (beginning-of-line 1)
        ;;       (let* ((struct (org-list-struct))
        ;;           (prevs (org-list-prevs-alist struct))
        ;;           (end (org-list-get-bottom-point struct)))
        ;;      (mapc (lambda (e) (org-list-set-item-visibility e struct 'folded))
        ;;            (org-list-get-all-items (point) struct prevs))
        ;;      (goto-char (if (< end eos) end eos))))))
        )
      (org-unlogged-message "CHILDREN")
      (save-excursion
        (goto-char eos)
        (outline-next-heading)
        (if (outline-invisible-p) (org-flag-heading nil)))
      (setq org-cycle-subtree-status 'children)
      (unless (org-before-first-heading-p)
        (run-hook-with-args 'org-cycle-hook 'children)))
     ;; ((or children-skipped
     ;;     and (eq last-command this-command)
     ;;     (eq org-cycle-subtree-status 'children))
     ;;  (save-excursion)
     ;;  (goto-char (org-element-property 'begin (org-element-at-point)))
     ;;  )
     ((or children-skipped
          (and (eq last-command this-command)
               (eq org-cycle-subtree-status 'children)))
      ;; We just showed the children, or no children are there,
      ;; now show everything.
      (unless (org-before-first-heading-p)
        (run-hook-with-args 'org-pre-cycle-hook 'subtree))
      ;;       (outline-flag-region eoh eos nil)
      (show-branches)
      (org-unlogged-message
       (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
      (setq org-cycle-subtree-status 'subtree)
      (unless (org-before-first-heading-p)
        (run-hook-with-args 'org-cycle-hook 'subtree)))
     (t
      ;; Default action: hide the subtree.
      (run-hook-with-args 'org-pre-cycle-hook 'folded)
      (outline-flag-region eoh eos t)
      (org-unlogged-message "FOLDED")
      (setq org-cycle-subtree-status 'folded)
      (unless (org-before-first-heading-p)
        (run-hook-with-args 'org-cycle-hook 'folded))))))

(defun mwp-test-e ()
  (interactive)
  (goto-char (org-element-property :contents-end (org-element-at-point)))
  )

(defun owr-cycle-internal-global ()
  "Do the global cycling action."
  ;; Hack to avoid display of messages for .org  attachments in Gnus
  (let ((ga (string-match "\\*fontification" (buffer-name))))
    (cond
     ((and (eq last-command this-command)
           (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (run-hook-with-args 'org-pre-cycle-hook 'contents)
      (unless ga (org-unlogged-message "CONTENTS..."))
      (org-content)
      (unless ga (org-unlogged-message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents)
      (run-hook-with-args 'org-cycle-hook 'contents))

     ;; ((and (eq last-command this-command)
     ;;       (eq org-cycle-global-status 'contents))
     ;;  ;; We just showed the table of contents - now show everything
     ;;  (run-hook-with-args 'org-pre-cycle-hook 'all)
     ;;  (show-all)
     ;;  (unless ga (org-unlogged-message "SHOW ALL"))
     ;;  (setq org-cycle-global-status 'all)
     ;;  (run-hook-with-args 'org-cycle-hook 'all))

     (t
      ;; Default action: go to overview
      (run-hook-with-args 'org-pre-cycle-hook 'overview)
      (org-overview)
      (unless ga (org-unlogged-message "OVERVIEW"))
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))))

;; (defun org-cycle-internal-local ()
;;   "Do the local cycling action."
;;   (interactive)
;;   (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
;;     ;; First, determine end of headline (EOH), end of subtree or item
;;     ;; (EOS), and if item or heading has children (HAS-CHILDREN).
;;     (save-excursion
;;       (if (org-at-item-p)
;;           (progn
;;             (beginning-of-line)
;;             (setq struct (org-list-struct))
;;             (setq eoh (point-at-eol))
;;             (setq eos (org-list-get-item-end-before-blank (point) struct))
;;             (setq has-children (org-list-has-child-p (point) struct)))
;;         (org-back-to-heading)
;;         (setq eoh (save-excursion (outline-end-of-heading) (point)))
;;         (setq eos (save-excursion (1- (org-end-of-subtree t t))))
;;         (setq has-children
;;               (or (save-excursion
;;                     (let ((level (funcall outline-level)))
;;                       (outline-next-heading)
;;                       (and (org-at-heading-p t)
;;                            (> (funcall outline-level) level))))
;;                   (save-excursion
;;                     (org-list-search-forward (org-item-beginning-re) eos t)))))
;;       ;; Determine end invisible part of buffer (EOL)
;;       (beginning-of-line 2)
;;       ;; XEmacs doesn't have `next-single-char-property-change'
;;       (if (featurep 'xemacs)
;;           (while (and (not (eobp)) ;; this is like `next-line'
;;                       (get-char-property (1- (point)) 'invisible))
;;             (beginning-of-line 2))
;;         (while (and (not (eobp)) ;; this is like `next-line'
;;                     (get-char-property (1- (point)) 'invisible))
;;           (goto-char (next-single-char-property-change (point) 'invisible))
;;           (and (eolp) (beginning-of-line 2))))
;;       (setq eol (point)))
;;     ;; Find out what to do next and set `this-command'
;;     (cond
;;      ;; This is an empty heading, so nothing to do
;;      ((= eos eoh)
;;       ;; Nothing is hidden behind this heading
;;       (unless (org-before-first-heading-p)
;;         (run-hook-with-args 'org-pre-cycle-hook 'empty))
;;       (org-unlogged-message "EMPTY ENTRY")
;;       (setq org-cycle-subtree-status nil)
;;       (save-excursion
;;         (goto-char eos)
;;         (outline-next-heading)
;;         (if (outline-invisible-p) (org-flag-heading nil))))
;;      ((and (or (>= eol eos)
;;                (not (string-match "\\S-" (buffer-substring eol eos))))
;;            (or has-children
;;                (not (setq children-skipped
;;                           org-cycle-skip-children-state-if-no-children))))

;;       ;; Entire subtree is hidden in one line: switch to children view
;;       (unless (org-before-first-heading-p)
;;         (run-hook-with-args 'org-pre-cycle-hook 'children))
;;       (if (org-at-item-p)
;;           (org-list-set-item-visibility (point-at-bol) struct 'children)
;;         ;; (org-show-entry)
;;         (org-with-limited-levels (show-children))
;;         ;; FIXME: This slows down the func way too much.
;;         ;; How keep drawers hidden in subtree anyway?
;;         ;; (when (memq 'org-cycle-hide-drawers org-cycle-hook)
;;         ;;   (org-cycle-hide-drawers 'subtree))

;;         ;; Fold every list in subtree to top-level items.
;;         (when (eq org-cycle-include-plain-lists 'integrate)
;;           (save-excursion
;;             (org-back-to-heading)
;;             (while (org-list-search-forward (org-item-beginning-re) eos t)
;;               (beginning-of-line 1)
;;               (let* ((struct (org-list-struct))
;;                      (prevs (org-list-prevs-alist struct))
;;                      (end (org-list-get-bottom-point struct)))
;;                 (mapc (lambda (e) (org-list-set-item-visibility e struct 'folded))
;;                       (org-list-get-all-items (point) struct prevs))
;;                 (goto-char (if (< end eos) end eos)))))))
;;       (org-unlogged-message "CHILDREN")
;;       (save-excursion
;;         (goto-char eos)
;;         (outline-next-heading)
;;         (if (outline-invisible-p) (org-flag-heading nil)))
;;       (setq org-cycle-subtree-status 'children)
;;       (unless (org-before-first-heading-p)
;;         (run-hook-with-args 'org-cycle-hook 'children)))

;;      ;; show descendants
;;      (;;(and (eq last-command 'this-command))
;;       (eq org-cycle-subtree-status 'children)
;;       (message "TRYING GRANDCHILDREN")
;;       ;; We just showed the children, or no children are there,
;;       ;; now show descendants if there are any, and subtree if there aren't
;;       (let ((has-grandchildren nil))
;;         (save-excursion
;;           (let ((level (funcall outline-level)))
;;             (while (> eos (point))
;;               (outline-next-heading)
;;               (if  (and (org-at-heading-p t)
;;                         (> (funcall outline-level) level))
;;                   (setq has-grandchildren t)))))
;;         (print has-grandchildren)
;;         (if 'has-grandchildren
;;             (progn
;;               (message "HAS GRANDCHILDREN")
;;               (unless (org-before-first-heading-p)
;;                 (run-hook-with-args 'org-pre-cycle-hook 'descendants))
;;               ;; (outline-flag-region eoh eos nil)
;;               (show-branches)
;;               (org-unlogged-message "DESCENDANTS")
;;               (setq org-cycle-subtree-status 'descendants)
;;               (unless (org-before-first-heading-p)
;;                 (run-hook-with-args 'org-cycle-hook 'descendants)))
;;           (message "DOES NOT HAVE GRANDCHILDREN")
;;           (run-hook-with-args 'org-pre-cycle-hook 'folded)
;;           (outline-flag-region eoh eos t)
;;           (org-unlogged-message "FOLDED")
;;           (setq org-cycle-subtree-status 'folded)
;;           ))
;;       )

     ;; all other options exhausted, show full subtree
     ;; ((or children-skipped
     ;;      (and (eq last-command this-command)
     ;;           (eq org-cycle-subtree-status 'descendants)))
     ;;  (message "DIDN'T FIND DESCENDANTS")
     ;;  ;; We just showed the children, or no children are there,
     ;;  ;; now show everything.
     ;;  (unless (org-before-first-heading-p)
     ;;    (run-hook-with-args 'org-pre-cycle-hook 'subtree))
     ;;  (outline-flag-region eoh eos nil)
     ;;  (org-unlogged-message
     ;;   (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
     ;;  (setq org-cycle-subtree-status 'subtree)
     ;;  (unless (org-before-first-heading-p)
     ;;    (run-hook-with-args 'org-cycle-hook 'subtree)))
     ;; ((and (eq last-command this-command)
     ;;       (eq org-cycle-subtree-status 'descendants))
     ;;  ;; We just showed the children, or no children are there,
     ;;  ;; now show everything.
     ;;  (unless (org-before-first-heading-p)
     ;;    (run-hook-with-args 'org-pre-cycle-hook 'subtree))
     ;;  (outline-flag-region eoh eos nil)
     ;;  ;; (show-branches)
     ;;  (org-unlogged-message
     ;;   (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
     ;;  (setq org-cycle-subtree-status 'subtree)
     ;;  (unless (org-before-first-heading-p)
     ;;    (run-hook-with-args 'org-cycle-hook 'subtree)))
     ;; (t
     ;;  (message "no conditions met")
     ;;  ;; Default action: hide the subtree.
     ;;  (run-hook-with-args 'org-pre-cycle-hook 'folded)
     ;;  (outline-flag-region eoh eos t)
     ;;  (org-unlogged-message "FOLDED")
     ;;  (setq org-cycle-subtree-status 'folded)
     ;;  (unless (org-before-first-heading-p)
     ;;    (run-hook-with-args 'org-cycle-hook 'folded))))))

;; ** Part 5: Hacked indirect buffer functions

;; hacked org-tree-to-indirect-buffer
;; removes several options, imposing the window structure defined earlier
;; on the user to create a "writer's room" hopefully free of distractions

(defun org-writers-room-tree-to-indirect-buffer (&optional  newname)
  "Create first indirect buffer, ibuf, and narrow it to current subtree.  Then create a second indirect guffer, metabuf.  Display these two buffers windows named 'main' and 'metadata'.
Org options to go up and down levels are not available, nor are options to display in a new frame etc. etc.  "
  (interactive "P")
  ;; this is the work of figuring out the right window & buffer
  (let ((cbuf (current-buffer))
        (cwin (selected-window))
        (pos (point))
        beg end level heading ibuf)
    ;; select the apropriate heading
    (save-excursion
      (org-back-to-heading t)
      (setq beg (point)
            heading (org-get-heading))
      (org-end-of-subtree t t)
      (if (org-at-heading-p) (backward-char 1))
      (setq end (point)))
    ;; kill indirect buffer if it still exists
    ;; problem for the metadata window creation!!
    (if (buffer-live-p org-last-indirect-buffer)
        (kill-buffer org-last-indirect-buffer))
    (if (buffer-live-p org-wr-last-meta-buffer)
        (kill-buffer  org-wr-last-meta-buffer))

    ;; create and display the indirect buffers
    (setq ibuf (org-writers-room-get-indirect-buffer cbuf heading)
          org-last-indirect-buffer ibuf)
    (setq metabuf (org-writers-room-get-indirect-buffer cbuf (concat heading "-meta") )
          org-wr-last-meta-buffer metabuf)
    (display-buffer-in-named-window metabuf "metadata")
    (display-buffer-in-named-window ibuf "main")
    ;; set modes and narrow for ibuf & metabuf
    (pop-to-buffer ibuf)
    (narrow-to-region beg end)
    (org-mode)
    (org-wr-main)
    (show-all)
    (goto-char pos)
    (run-hook-with-args 'org-cycle-hook 'all)
    (pop-to-buffer metabuf)
    (narrow-to-region beg end)
    (org-mode)
    (org-wr-meta)
    (show-all)
    (and (window-live-p cwin) (select-window cwin))))


;; * Part 6: templating for new project

;; stolen from org2blog

(defcustom owr-buffer-template "
  #+DATE: 
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: 
#+TAGS:
#+DESCRIPTION:
#+TITLE: 

* Ch 1
:PROPERTIES:
:Synopsis: summary
:Role in Book: hint
:Characters: char
:END:

* Ch 2
:PROPERTIES:
:Synopsis: summary
:Role in Book: hint
:Characters: char
:END:

* Ch 3
:PROPERTIES:
:Synopsis: summary
:Role in Book: hint
:Characters: char
:END:

* Ch 4
:PROPERTIES:
:Synopsis: summary
:Role in Book: hint
:Characters: char
:END:

* Working Notes

** Character Studies

** Places

** Doodles/Cruft

\n"
  "The default template to be inserted in a new post buffer."
  :group 'org-writers-room
  :type 'string)

(defcustom owr-buffer-template-prefix nil
  "A prefix to the default template used for a new post buffer."
  :group 'org-writers-room
  :type 'string)

(defcustom owr-buffer-format-function 'owr-format-buffer
  "Function formatting a buffer according to `owr-buffer-template'."
  :group 'org-writers-room
  :type 'function)



(defun owr-new-entry ()
  "Creates a new buffer for a blog entry."
  (interactive)

  ;; Generate new buffer
  (let ((owr-buffer (generate-new-buffer "new-project")
                    ;;(format owr-buffer-name owr-blog-name)
                    ))
    (switch-to-buffer owr-buffer)
    ;; (add-hook 'kill-buffer-hook 'owr-kill-buffer-hook nil 'local)
    ;; (remove-hook 'kill-buffer-hook 'owr-kill-buffer-hook nil 'local)
    (org-mode)
    ;; Insert the project template
    (insert owr-buffer-template

     ;; insert
     ;; (or owr-buffer-template-prefix "")
     ;; (funcall owr-buffer-format-function
     ;;          owr-buffer-template)
     )
    (search-backward "#+TITLE: ")
    (search-forward " ")
    ;; (org-writers-mode)
    ))


(defun owr-format-buffer (buffer-template)
  "Default buffer formatting function."
  (buffer-template)
  ;; (format buffer-template
  ;;         (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
  ;;         (mapconcat
  ;;          (lambda (cat) cat)
  ;;          (or (plist-get (cdr owr-blog) :default-categories)
  ;;              owr-default-categories)
  ;;          ", ")
  ;;         (or (plist-get (cdr owr-blog) :default-title)
  ;;             owr-default-title))

)

(defun owr-beginning-of-line (&optional arg)
  "Go to the beginning of the current line.  If that is invisible, continue
to a visible line beginning.  This makes the function of C-a more intuitive.
If this is a headline, and `org-special-ctrl-a/e' is set, ignore tags on the
first attempt, and only move to after the tags when the cursor is already
beyond the end of the headline."
  (interactive "P")
  (let ((pos (point))
        (special (if (consp org-special-ctrl-a/e)
                     (car org-special-ctrl-a/e)
                   org-special-ctrl-a/e))
        deactivate-mark	refpos)
    (if (org-bound-and-true-p visual-line-mode)
        (beginning-of-visual-line 1)
      (beginning-of-line 1))
    (if (and arg (fboundp 'move-beginning-of-line))
        (call-interactively 'move-beginning-of-line)
      (if (bobp)
          nil
        (backward-char 1)
        (if (org-truely-invisible-p)
            (while (and (not (bobp)) (org-truely-invisible-p))
              (backward-char 1)
              (beginning-of-line 1))
          (forward-char 1))))
    (when special
      (message "special is set")
      (print (org-element-at-point))
      ;; (message "org-at-property-p reports ")
      ;; (print (org-at-property-p))
      (cond
       ((and (looking-at org-complex-heading-regexp)
             (= (char-after (match-end 1)) ?\ ))
        (setq refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
                          (point-at-eol)))
        (goto-char
         (if (eq special t)
             (cond ((> pos refpos) refpos)
                   ((= pos (point)) refpos)
                   (t (point)))
           (cond ((> pos (point)) (point))
                 ((not (eq last-command this-command)) (point))
                 (t refpos)))))
       ((org-at-property-p)
        (message "is a property")
        (when (looking-at org-property-re)
          (message "looking at property re")
          (let ((after-keyword (+ 1  (match-end 1))))
            (print after-keyword)
            (print (point))
            (goto-char after-keyword)
            (if (eq special t)
                (goto-char after-keyword)
              (if (or (= (point) pos) (> (+ 1 (point)) after-keyword))
                  ;; (and (= (point) pos) (eq last-command this-command))
                  (goto-char after-keyword)
                (goto-char pos))))
          ;; (search-forward ":")
          ;; (search-forward ":")
          ;; (forward-char 1))
          ))
       ((org-at-item-p)
        ;; Being at an item and not looking at an the item means point
        ;; was previously moved to beginning of a visual line, which
        ;; doesn't contain the item.  Therefore, do nothing special,
        ;; just stay here.
        (when (looking-at org-list-full-item-re)
          ;; Set special position at first white space character after
          ;; bullet, and check-box, if any.
          (let ((after-bullet
                 (let ((box (match-end 3)))
                   (if (not box) (match-end 1)
                     (let ((after (char-after box)))
                       (if (and after (= after ? )) (1+ box) box))))))
            ;; Special case: Move point to special position when
            ;; currently after it or at beginning of line.
            (if (eq special t)
                (when (or (> pos after-bullet) (= (point) pos))
                  (goto-char after-bullet))
              ;; Reversed case: Move point to special position when
              ;; point was already at beginning of line and command is
              ;; repeated.
              (when (and (= (point) pos) (eq last-command this-command))
                (goto-char after-bullet))))))))
    (org-no-warnings
     (and (featurep 'xemacs) (setq zmacs-region-stays t))))
  (setq disable-point-adjustment
        (or (not (invisible-p (point)))
            (not (invisible-p (max (point-min) (1- (point))))))))

(defun org-end-of-line (&optional arg)
  "Go to the end of the line.
If this is a headline, and `org-special-ctrl-a/e' is set, ignore
tags on the first attempt, and only move to after the tags when
the cursor is already beyond the end of the headline."
  (interactive "P")
  (let ((special (if (consp org-special-ctrl-a/e) (cdr org-special-ctrl-a/e)
                   org-special-ctrl-a/e))
        (move-fun (cond ((org-bound-and-true-p visual-line-mode)
                         'end-of-visual-line)
                        ((fboundp 'move-end-of-line) 'move-end-of-line)
                        (t 'end-of-line)))
        deactivate-mark)
    (if (or (not special) arg) (call-interactively move-fun)
      (let* ((element (save-excursion (beginning-of-line)
                                      (org-element-at-point)))
             (type (org-element-type element)))
        (cond
         ((memq type '(headline inlinetask))
          (let ((pos (point)))
            (beginning-of-line 1)
            (if (looking-at (org-re ".*?\\(?:\\([ \t]*\\)\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*\\)?$"))
                (if (eq special t)
                    (if (or (< pos (match-beginning 1)) (= pos (match-end 0)))
                        (goto-char (match-beginning 1))
                      (goto-char (match-end 0)))
                  (if (or (< pos (match-end 0))
                          (not (eq this-command last-command)))
                      (goto-char (match-end 0))
                    (goto-char (match-beginning 1))))
              (call-interactively move-fun))))
         ((outline-invisible-p (line-end-position))
          ;; If element is hidden, `move-end-of-line' would put point
          ;; after it.  Use `end-of-line' to stay on current line.
          (call-interactively 'end-of-line))
         (t (call-interactively move-fun)))))
    (org-no-warnings (and (featurep 'xemacs) (setq zmacs-region-stays t))))
  (setq disable-point-adjustment
        (or (not (invisible-p (point)))
            (not (invisible-p (max (point-min) (1- (point))))))))

(provide 'owr-helper)
