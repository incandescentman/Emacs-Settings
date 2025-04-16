;;; whittle.el --- Strip filler & duplicates  -*- lexical-binding: t; -*-

(require 'subr-x)   ;; for `string-trim'
(require 'rx)

;;; Customisation -------------------------------------------------------------

(defvar whittle/filler-word-replacements
  ;; (PATTERN‑STRING . REPLACEMENT)
  (let ((base '(("kind of like")
                ("you know")
                ("i mean")
                ("\\(um+\\|uh\\)")   ; escaped once
                ("ok" . "OK")
                ("right[.?!]" . ".")
                (",[[:space:]]*like[[:space:]]*," . " ")   ; <‑‑ NEW
                ))) ; already a char‑class; fine as string
    (mapcar
     (pcase-lambda (`(,pat . ,rep))
       ;; prepend/append word‑boundaries + optional space/punct
       (cons (concat "\\b" pat "\\b[[:space:][:punct:]]*")
             (or rep "")))
     base))
  "Alist mapping filler phrases (as `rx' patterns) to replacements.")

(defun whittle--strip-comma-like (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward ",[[:space:]]*like[[:space:]]*," end t)
      (replace-match " "))))

(defconst whittle/duplicate-word-exclusions
  '("that" "so" "very" "really" "yes" "no" "ha" "wow" "well" "yeah")
  "Words that *can* appear twice legitimately (case‑insensitive).")

;;; Helpers -------------------------------------------------------------------

(defun whittle--region-bounds (beg end)
  "Return (START . END) covering either the active region or the buffer."
  (if (use-region-p)
      (cons beg end)
    (cons (point-min) (point-max))))

;;; Main functions ------------------------------------------------------------

(defun whittle/remove-filler-words (beg end)
  "Remove filler phrases between BEG and END (region if active)."
  (interactive "r")
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (case-fold-search t)
               (counts (make-hash-table :test #'equal)))
    (save-excursion
      (dolist (pair whittle/filler-word-replacements)
        (goto-char start)
        (let ((count 0))
          (while (re-search-forward (car pair) limit t)
            (replace-match (cdr pair) t t)
            (setq count (1+ count)))
          (when (> count 0)
            (puthash (string-trim (car pair) "\\\\b" "\\\\b") count counts)))))
    ;; doubled punctuation / spaces
    (whittle--cleanup-punctuation start end)
    (whittle--strip-comma-like start limit)
    (whittle--report counts "filler phrase")))

(defun whittle/remove-duplicated-words (beg end)
  "Collapse accidental duplicated words in the region or buffer."
  (interactive "r")
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (case-fold-search t)
               (rx-dup (rx word-start (group (+ word)) word-end
                           (+ (any blank "\n" "–" "—"))
                           (backref 1) word-end))
               (removed (make-hash-table :test #'equal)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward rx-dup limit t)
        (let ((w (downcase (match-string 1))))
          (unless (member w whittle/duplicate-word-exclusions)
            (puthash w (1+ (gethash w removed 0)) removed)
            (replace-match (match-string 1))))))
    (whittle--report removed "duplicated word")))

(defun whittle/whittle (&optional beg end)
  "Run both filler‑word and duplicate‑word cleaners on region or buffer."
  (interactive "r")
  (whittle/remove-filler-words beg end)
  (whittle/remove-duplicated-words beg end))

;;; Internals -----------------------------------------------------------------

(defun whittle--cleanup-punctuation (beg end)
  "Collapse doubled punctuation and spaces between BEG and END."
  (let ((case-fold-search nil)
        (rx-doubles (rx (group (any ",.")) (group (backref 1))))
        (rx-spaces  (rx (>= 2 blank))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward rx-doubles end t)
        (replace-match (match-string 1) t t))
      (goto-char beg)
      (while (re-search-forward rx-spaces end t)
        (replace-match " " t t)))))

(defun whittle--report (table what)
  "Display a message summarising TABLE about WHAT was removed."
  (if (= (hash-table-count table) 0)
      (message "No %ss removed." what)
    (let ((parts '()))
      (maphash (lambda (k v) (push (format "\"%s\" × %d" k v) parts)) table)
      (message "Removed %s: %s"
               what
               (string-join (nreverse parts) ", ")))))

(provide 'whittle)
;;; whittle.el ends here
