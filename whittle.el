;;; whittle.el --- Clean filler and transcript artifacts -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)   ;; for `string-join'
(require 'rx)

;;; Customization -------------------------------------------------------------

(defvar whittle/conservative-filler-rules
  '(("kind of like" "\\<kind of like\\>[[:space:][:punct:]]*" "")
    ("i mean" "\\<i mean\\>[[:space:][:punct:]]*" "")
    ("um/uh" "\\<\\(um+\\|uh\\)\\>[[:space:][:punct:]]*" "")
    (", like," ",[[:space:]]*like[[:space:]]*," " "))
  "Conservative filler cleanup rules as (LABEL REGEXP REPLACEMENT).")

(defvar whittle/transcript-edge-filler-rules
  '(("edge filler"
     "^[[:space:]]*\\(?:so\\|well\\|ok\\(?:ay\\)?\\|right\\|you know\\|like\\)[,[:space:]-]*"
     "")
    ("edge filler"
     "\\([.!?][[:space:]\n]+\\)\\(?:so\\|well\\|ok\\(?:ay\\)?\\|right\\|you know\\|like\\)[,[:space:]-]*"
     "\\1")
    ("edge filler"
     "[[:space:]]*,?[[:space:]]*\\(?:right\\|you know\\|ok\\(?:ay\\)?\\)\\([.?!]?\\)[[:space:]]*$"
     "\\1"))
  "Aggressive transcript rules for sentence-edge fillers.")

(defconst whittle/transcript-filler-chain-regexp
  "\\(?:\\<\\(?:um+\\|uh\\|erm\\|ah\\|like\\|you know\\|i mean\\|so\\|well\\|ok\\(?:ay\\)?\\|right\\)\\>[[:space:][:punct:]]*\\)\\{2,\\}"
  "Regexp matching transcript-style chains of filler phrases.")

(defconst whittle/duplicate-word-exclusions
  '("that" "so" "very" "really" "yes" "no" "ha" "wow" "well" "yeah"
    "go" "bye" "oh")
  "Words that can appear twice legitimately (case-insensitive).")

(defconst whittle/false-start-prefixes
  '("a" "an" "he" "her" "his" "i" "it" "my" "our" "she" "the" "their"
    "these" "they" "this" "those" "we" "you" "your")
  "Words that commonly begin transcript false starts.")

;;; Helpers -------------------------------------------------------------------

(defun whittle--region-bounds (beg end)
  "Return (START . END) covering either the active region or the buffer."
  (if (use-region-p)
      (cons beg end)
    (cons (point-min) (point-max))))

(defun whittle--interactive-bounds ()
  "Return interactive bounds for the active region, or nils for the buffer."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list nil nil)))

(defun whittle--increment (table key &optional amount)
  "Increment TABLE entry KEY by AMOUNT, defaulting to 1."
  (puthash key (+ (gethash key table 0) (or amount 1)) table))

(defun whittle--apply-rules (beg end rules)
  "Apply RULES between BEG and END and return a hash table of match counts."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (limit-marker (copy-marker limit))
               (case-fold-search t)
               (counts (make-hash-table :test #'equal)))
    (unwind-protect
        (save-excursion
          (dolist (rule rules)
            (pcase-let ((`(,label ,regexp ,replacement) rule))
              (goto-char start)
              (while (re-search-forward regexp limit-marker t)
                (replace-match replacement t nil)
                (whittle--increment counts label)))))
      (set-marker limit-marker nil))
    counts))

(defun whittle--format-table-summary (table label)
  "Return a summary string for TABLE under LABEL, or nil if TABLE is empty."
  (unless (= (hash-table-count table) 0)
    (let (parts)
      (maphash
       (lambda (key value)
         (push (format "\"%s\" x %d" key value) parts))
       table)
      (format "%s %s" label (string-join (nreverse parts) ", ")))))

(defun whittle--format-count-summary (count label)
  "Return a summary string for COUNT and LABEL, or nil if COUNT is zero."
  (when (> count 0)
    (format "%s %d" label count)))

(defun whittle--report-summaries (prefix summaries)
  "Display PREFIX followed by joined non-nil SUMMARIES."
  (let ((parts (delq nil summaries)))
    (if parts
        (message "%s: %s" prefix (string-join parts "; "))
      (message "%s: no changes" prefix))))

;;; Core passes ----------------------------------------------------------------

(defun whittle--remove-filler-words (beg end rules)
  "Apply filler RULES between BEG and END and return a counts table."
  (whittle--apply-rules beg end rules))

(defun whittle--remove-filler-chains (beg end)
  "Remove chains of repeated transcript fillers between BEG and END."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (limit-marker (copy-marker limit))
               (case-fold-search t)
               (count 0))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (re-search-forward whittle/transcript-filler-chain-regexp limit-marker t)
            (replace-match "" t t)
            (setq count (1+ count))))
      (set-marker limit-marker nil))
    count))

(defun whittle--remove-duplicated-words (beg end)
  "Collapse accidental duplicated words between BEG and END."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (limit-marker (copy-marker limit))
               (case-fold-search t)
               ;; Use explicit Emacs word boundaries (\</\>) for readability.
               (dup-regexp "\\<\\([[:alpha:]']+\\)\\>\\(?:[[:space:]\n–—]+\\1\\>\\)+")
               (removed (make-hash-table :test #'equal)))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (re-search-forward dup-regexp limit-marker t)
            (let ((word (downcase (match-string 1))))
              (unless (member word whittle/duplicate-word-exclusions)
                (let ((match-beg (match-beginning 0))
                      (replacement (match-string 1)))
                  (whittle--increment removed word)
                  (replace-match replacement t t)
                  (goto-char match-beg))))))
      (set-marker limit-marker nil))
    removed))

(defun whittle--remove-false-starts (beg end)
  "Collapse repeated transcript phrases like \"I was I was\" between BEG and END."
  (let* ((prefixes (regexp-opt whittle/false-start-prefixes t))
         (regexp (concat "\\<\\(" prefixes
                         "\\(?:[[:space:]\n]+[[:alpha:]']+\\)\\{0,2\\}\\)"
                         "\\(?:[[:space:]\n]+\\)\\1\\>")))
    (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
                 (limit-marker (copy-marker limit))
                 (case-fold-search t)
                 (removed (make-hash-table :test #'equal)))
      (unwind-protect
          (save-excursion
            (goto-char start)
            (while (re-search-forward regexp limit-marker t)
              (let ((phrase (downcase (match-string 1)))
                    (match-beg (match-beginning 0))
                    (replacement (match-string 1)))
                (whittle--increment removed phrase)
                (replace-match replacement t t)
                (goto-char match-beg))))
        (set-marker limit-marker nil))
      removed)))

(defun whittle--join-transcript-lines (beg end)
  "Join mid-sentence line breaks between BEG and END."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (limit-marker (copy-marker limit))
               (count 0))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (re-search-forward
                  "\\([[:alnum:])\"']\\)\n\\([[:space:]]*[[:lower:][:digit:]\"'([]\\)"
                  limit-marker t)
            (replace-match "\\1 \\2" t)
            (setq count (1+ count))))
      (set-marker limit-marker nil))
    count))

(defun whittle--replace-punctuation-cluster ()
  "Return a normalized replacement for the current punctuation cluster."
  (let* ((cluster (match-string 0))
         (dots (cl-loop for char across cluster count (eq char ?.)))
         (commas (cl-loop for char across cluster count (eq char ?,))))
    (cond
     ((>= dots 3) "...")
     ((> dots 0) ".")
     ((> commas 0) ",")
     (t cluster))))

(defun whittle--cleanup-punctuation (beg end)
  "Normalize punctuation and spacing between BEG and END."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (limit-marker (copy-marker limit))
               (count 0))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (re-search-forward "^[[:space:]]*,+[[:space:]]*" limit-marker t)
            (replace-match "" t t)
            (setq count (1+ count)))
          (goto-char start)
          (while (re-search-forward "\\([.!?\n]\\)[[:space:]]*,+[[:space:]]*" limit-marker t)
            (replace-match
             (if (string= (match-string 1) "\n")
                 "\n"
               (concat (match-string 1) " "))
             t t)
            (setq count (1+ count)))
          (goto-char start)
          (while (re-search-forward "[[:space:]]+\\([,.;:?!]\\)" limit-marker t)
            (replace-match "\\1" t)
            (setq count (1+ count)))
          (goto-char start)
          (while (re-search-forward "\\.\\(?:[[:space:]]*\\.\\)\\{2,\\}" limit-marker t)
            (replace-match "..." t t)
            (setq count (1+ count)))
          (goto-char start)
          (while (re-search-forward "[,.]\\(?:[[:space:]]*[,.]\\)+" limit-marker t)
            (replace-match (whittle--replace-punctuation-cluster) t t)
            (setq count (1+ count)))
          (goto-char start)
          (while (re-search-forward "\\([?!]\\)\\(?:[[:space:]]*\\1\\)+" limit-marker t)
            (replace-match "\\1" t)
            (setq count (1+ count)))
          (goto-char start)
          (while (re-search-forward "\\([[:space:]]\\)\\{2,\\}" limit-marker t)
            (replace-match " " t t)
            (setq count (1+ count))))
      (set-marker limit-marker nil))
    count))

(defun whittle--normalize-case (beg end)
  "Capitalize lone i and sentence starts between BEG and END."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (limit-marker (copy-marker limit))
               (case-fold-search nil)
               (count 0))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (re-search-forward "\\_<i\\_>" limit-marker t)
            (replace-match "I" t t)
            (setq count (1+ count)))
          (goto-char start)
          (skip-chars-forward " \t\n\"'([{" limit-marker)
          (when (looking-at "[[:lower:]]")
            (replace-match (upcase (match-string 0)) t t)
            (setq count (1+ count)))
          (goto-char start)
          (while (re-search-forward "[.?!][[:space:]\n]+" limit-marker t)
            (let ((punct-pos (match-beginning 0)))
              (unless (and (eq (char-after punct-pos) ?.)
                           (eq (char-before punct-pos) ?.))
                (save-excursion
                  (skip-chars-forward " \t\n\"'([{" limit-marker)
                  (when (looking-at "[[:lower:]]")
                    (replace-match (upcase (match-string 0)) t t)
                    (setq count (1+ count))))))))
      (set-marker limit-marker nil))
    count))

;;; Interactive commands ------------------------------------------------------

(defun whittle/remove-filler-words (beg end)
  "Remove conservative filler phrases between BEG and END."
  (interactive (whittle--interactive-bounds))
  (let* ((counts (whittle--remove-filler-words beg end whittle/conservative-filler-rules))
         (punctuation (whittle--cleanup-punctuation beg end)))
    (whittle--report-summaries
     "Whittle filler cleanup"
     (list (whittle--format-table-summary counts "removed")
           (whittle--format-count-summary punctuation "punctuation fixes")))
    counts))

(defun whittle/remove-duplicated-words (beg end)
  "Collapse accidental duplicated words between BEG and END."
  (interactive (whittle--interactive-bounds))
  (let* ((counts (whittle--remove-duplicated-words beg end))
         (punctuation (whittle--cleanup-punctuation beg end)))
    (whittle--report-summaries
     "Whittle duplicate cleanup"
     (list (whittle--format-table-summary counts "removed")
           (whittle--format-count-summary punctuation "punctuation fixes")))
    counts))

(defun whittle (&optional beg end)
  "Run conservative cleanup on the region or buffer."
  (interactive (whittle--interactive-bounds))
  (let* ((filler-counts (whittle--remove-filler-words beg end whittle/conservative-filler-rules))
         (duplicate-counts (whittle--remove-duplicated-words beg end))
         (punctuation (whittle--cleanup-punctuation beg end)))
    (whittle--report-summaries
     "Whittle"
     (list (whittle--format-table-summary filler-counts "fillers")
           (whittle--format-table-summary duplicate-counts "duplicates")
           (whittle--format-count-summary punctuation "punctuation fixes")))))

(defun whittle-transcript (&optional beg end)
  "Run aggressive transcript cleanup on the region or buffer."
  (interactive (whittle--interactive-bounds))
  (let* ((line-joins (whittle--join-transcript-lines beg end))
         (filler-chains (whittle--remove-filler-chains beg end))
         (conservative-fillers
          (whittle--remove-filler-words beg end whittle/conservative-filler-rules))
         (edge-fillers
          (whittle--remove-filler-words beg end whittle/transcript-edge-filler-rules))
         (false-starts (whittle--remove-false-starts beg end))
         (duplicate-counts (whittle--remove-duplicated-words beg end))
         (punctuation (whittle--cleanup-punctuation beg end))
         (case-fixes (whittle--normalize-case beg end)))
    (whittle--report-summaries
     "Whittle transcript"
     (list (whittle--format-count-summary line-joins "joined lines")
           (whittle--format-count-summary filler-chains "filler chains")
           (whittle--format-table-summary conservative-fillers "fillers")
           (whittle--format-table-summary edge-fillers "edge fillers")
           (whittle--format-table-summary false-starts "false starts")
           (whittle--format-table-summary duplicate-counts "duplicates")
           (whittle--format-count-summary punctuation "punctuation fixes")
           (whittle--format-count-summary case-fixes "case fixes")))))

(provide 'whittle)
;;; whittle.el ends here
