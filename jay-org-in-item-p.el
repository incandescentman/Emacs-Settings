(defun org-in-item-p ()
 "Return item beginning position when in a plain list, nil otherwise."
 (save-excursion
  (beginning-of-line)
  (let* ((case-fold-search t)
	  (context (org-list-context))
	  (lim-up (car context))
	  (inlinetask-re (and (featurep 'org-inlinetask)
			    (org-inlinetask-outline-regexp)))
	  (item-re (org-item-re))
	  ;; Indentation isn't meaningful when point starts at an empty
	  ;; line or an inline task.
	  (ind-ref (if (or (looking-at "^[ \t]*$")
			  (and inlinetask-re (looking-at inlinetask-re)))
			10000
		   (current-indentation))))
   (cond
    ((eq (nth 2 context) 'invalid) nil)
    ((looking-at item-re) (point))
    (t
     ;; Detect if cursor in amidst `org-list-end-re'. First, count
     ;; number HL of hard lines it takes, then call `org-in-regexp'
     ;; to compute its boundaries END-BOUNDS. When point is
     ;; in-between AND number HL of hard lines is below 2, then
     ;; move cursor before regexp beginning.

	(let ((hl 0) (i -1) end-bounds)
	 (when (and (progn
		    (while (setq i (string-match
				    "[\r\n]" org-list-end-re (1+ i)))
          (setq hl (1+ hl)))
		    (setq end-bounds (org-in-regexp org-list-end-re hl)))
		   (>= (point) (car end-bounds))
		   (< (point) (cdr end-bounds))
       (< hl 2))
	  (goto-char (car end-bounds))
	  (forward-line -1)))
	;; Look for an item, less indented that reference line.
	(catch 'exit
	 (while t
	  (let ((ind (current-indentation)))
	   (cond
	    ;; This is exactly what we want.
	    ((and (looking-at item-re) (< ind ind-ref))
		(throw 'exit (point)))
	    ;; At upper bound of search or looking at the end of a
	    ;; previous list: search is over.
	    ((<= (point) lim-up) (throw 'exit nil))
	    ((looking-at org-list-end-re) (throw 'exit nil))
	    ;; Skip blocks, drawers, inline-tasks, blank lines
	    ((and (looking-at "^[ \t]*#\\+end_")
		   (re-search-backward "^[ \t]*#\\+begin_" lim-up t)))
	    ((and (looking-at "^[ \t]*:END:")
		   (re-search-backward org-drawer-regexp lim-up t))
		(beginning-of-line))
	    ((and inlinetask-re (looking-at inlinetask-re))
		(org-inlinetask-goto-beginning)
		(forward-line -1))
	    ((looking-at "^[ \t]*$") (forward-line -1))
	    ;; Text at column 0 cannot belong to a list: stop.
	    ((zerop ind) (throw 'exit nil))
	    ;; Normal text less indented than reference line, take
	    ;; it as new reference.
	    ((< ind ind-ref)
		(setq ind-ref ind)
		(forward-line -1))
	    (t (forward-line -1)))))))))))

