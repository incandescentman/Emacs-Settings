;; Partially successful attempt to refactor `kill-clause', although it
;; still fails several tests.  Some requirements clarification might
;; be needed.  In case of doubt, please don't hesitate to e-mail me at
;; rudi.araujo@gmail.com.  Good luck!

(defvar clause-separator-regexp
  "[[:space:]]*\\(^\\|---?\\|[][,;:?!…\"”()}]+\\|\\.+\\([[:space:]]\\|\\'\\|\\`\\|$\\)\\|$\\)[[:space:]]*")

(defun clause-info ()
  (save-excursion
    (save-match-data
      (let ((old-point (point)))
        (re-search-forward clause-separator-regexp nil t)
        (let ((next-clause-sep-beginning (match-beginning 0))
              (next-clause-sep-end (match-end 0)))
          (goto-char next-clause-sep-beginning)
          (re-search-backward clause-separator-regexp nil t)
          (let ((other-clause-sep-beginning (match-beginning 0))
                (other-clause-sep-end (match-end 0)))
            (when (= other-clause-sep-beginning next-clause-sep-beginning)
              (ignore-errors (forward-char 1))
              (re-search-forward clause-separator-regexp nil t)
              (setf other-clause-sep-beginning (match-beginning 0)
                    other-clause-sep-end (match-end 0)))
            (if (< other-clause-sep-beginning next-clause-sep-beginning)
                (list other-clause-sep-beginning
                      other-clause-sep-end
                      next-clause-sep-beginning
                      next-clause-sep-end)
              (list next-clause-sep-beginning
                    next-clause-sep-end
                    other-clause-sep-beginning
                    other-clause-sep-end))))))))

(defun kill-clause ()
  (interactive)
  (smart-expand)
  (destructuring-bind (prev-sep-beg prev-sep-end next-sep-beg next-sep-end)
      (clause-info)
    (kill-region (if (<= prev-sep-beg (point) prev-sep-end)
                     prev-sep-beg
                   (point))
                 (if (or (<= prev-sep-beg (point) prev-sep-end)
                         (<= next-sep-beg (point) next-sep-end))
                     (1- next-sep-end)
                   next-sep-beg)))
  (my/fix-space)
  (save-excursion
    (when (my/beginning-of-sentence-p)
      (capitalize-unless-org-heading))))

