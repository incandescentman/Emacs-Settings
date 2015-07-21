(defun smart-punctuation (new-punct &optional not-so-smart)
  (expand-abbrev)
  (let ((debug-buffer (generate-new-buffer
                       (generate-new-buffer-name
                        "smart-punctutation-debug"))))
    (cl-flet ((print-debug-info (s)
                (with-current-buffer debug-buffer
                  (insert s))))
      (save-restriction
        (when (and (eql major-mode 'org-mode)
                   (org-at-heading-p))
          (print-debug-info "at heading\n")
          (save-excursion
            (org-beginning-of-line)
            (let ((heading-text (fifth (org-heading-components))))
              (print-debug-info (format "text: %s\n" heading-text))
              (when heading-text
                (search-forward heading-text)
                (print-debug-info (format "narrowing to: %s %s\n"
                                          (match-beginning 0) (match-end 0)))
                (narrow-to-region (match-beginning 0) (match-end 0))))))
        (cl-flet ((go-back (regexp)
                    (re-search-backward regexp nil t)
                    (ignore-errors ; might signal `end-of-buffer'
                      (forward-char (length (match-string 0))))))
          (if not-so-smart
              (let ((old-point (point)))
                (go-back "[^ \t]")
                (insert new-punct)
                (goto-char old-point)
                (forward-char (length new-punct)))
            (let ((old-point (point)))
              (print-debug-info (format "point: %s\n" old-point))
              (go-back (format "[^ \t%s]\\|\\`" *smart-punctuation-marks*))
              (print-debug-info (format "before spaces: %s\n" (point)))
              (re-search-forward (format "\\([ \t]*\\)\\([%s]*\\)"
                                         *smart-punctuation-marks*)
                                 nil t)
              (print-debug-info (format "whole match: %S\n" (match-string 0)))
              (print-debug-info (format "spaces before: %S\n" (match-string 1)))
              (print-debug-info (format "pre-existing punct: %S\n" (match-string 2)))
              (let* ((old-punct (match-string 2))
                     (was-after-punct (>= old-point (point))))
                (replace-match "" nil t nil 1)
                (replace-match (or (when (and was-after-punct
                                              (not (string= old-punct "")))
                                     (let ((potential-new-punct (concat old-punct new-punct)))
                                       (find-if (lambda (exception)
                                                  (search potential-new-punct exception))
                                                *smart-punctuation-exceptions*)))
                                   new-punct)
                               nil t nil 2)
                (when (looking-at "[ \t]*\\<")
                  (if was-after-punct
                      (my/fix-space)
                    (save-excursion (my/fix-space)))))))))))
  (when (and (eql major-mode 'org-mode)
             (org-at-heading-p))
    (org-align-tags-here org-tags-column)))
