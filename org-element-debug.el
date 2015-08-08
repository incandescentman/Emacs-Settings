;;; element-debug-mode by Nicolas, see:
;;
;; (notmuch-show "id:87d21dsvox.fsf@nicolasgoaziou.fr")
;; <http://mid.gmane.org/87d21dsvox.fsf@nicolasgoaziou.fr>
;;
;; (notmuch-show "id:87ioaqsaz2.fsf@nicolasgoaziou.fr")
;; <http://mid.gmane.org/87ioaqsaz2.fsf@nicolasgoaziou.fr>

(defun element-check-cache (&rest ignore)
  (when (org-element--cache-active-p)
    (save-match-data
      (let ((cache (copy-tree org-element--cache t))
	    (requests (copy-tree org-element--cache-sync-requests t))
	    (buffer-contents (org-with-wide-buffer (buffer-string)))
	    (translations (make-hash-table :test #'eq))
	    (structures (make-hash-table :test #'eq))
	    (keys (make-hash-table :test #'eq)))
	;; Fix parents.
	(loop for key in (avl-tree-flatten org-element--cache)
	      for value in (avl-tree-flatten cache)
	      do (let ((struct (and (memq (org-element-type key)
					  '(plain-list item))
				    (gethash (org-element-property :structure key)
					     structures 'missing))))
		   (progn
		     (puthash key value translations)
		     (let ((k (gethash key org-element--cache-sync-keys)))
		       (when k (puthash value k keys)))
		     (puthash
		      key
		      (org-element-put-property
		       value :parent
		       (gethash (org-element-property :parent key)
				translations))
		      translations)
		     (when (eq struct 'missing)
		       (setq struct
			     (puthash (org-element-property :structure key)
				      (org-element-property :structure value)
				      structures)))
		     (when struct
		       (puthash
			key
			(org-element-put-property value :structure struct)
			translations)))))
	;; Fix requests.
	(loop for original in org-element--cache-sync-requests
	      for copy in requests
	      do (aset copy 4 (gethash (aref original 4) translations)))
	(with-temp-buffer
	  (let ((org-element-use-cache nil)) (insert buffer-contents))
	  (let ((org-inhibit-startup t)) (org-mode))
	  (setq org-element--cache cache
		org-element--cache-sync-requests requests
		org-element--cache-sync-keys keys)
	  (org-element--cache-sync (current-buffer) (point-max))
	  (let ((seen '()))
	    (avl-tree-mapc
	     (lambda (element)
	       (let ((beg (org-element-property :begin element))
		     (type (org-element-type element)))
		 (let ((real (let (org-element-use-cache)
			       (goto-char
				(if (memq type '(item table-row)) (1+ beg)
				  beg))
			       (org-element-at-point))))
		   (cond
		    ((member real seen)
		     (message
		      "======\nWARNING. Two entries for the same element\n\n %s"
		      element))
		    ((not (equal real element))
		     (message
		      "======\nWARNING. Corrupted element (%s) at %d\n\nReal:\
	%s\n\nCached: %s\n\nLast request: %s"
		      (org-element-type element) beg real element (car requests)))
		    (t (push real seen))))))
	     org-element--cache)))))))

(define-minor-mode element-debug-mode
  "Minor mode to debug Org Element cache."
  nil " OrgCacheD" nil
  (if element-debug-mode
      (progn (setq org-element-cache-sync-idle-time 3600)
	     (add-hook 'after-change-functions 'element-check-cache t t))
    (setq org-element-cache-sync-idle-time 0.6)
    (remove-hook 'after-change-functions 'element-check-cache t)))

(provide 'org-element-debug)
