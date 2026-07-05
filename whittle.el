;;; whittle.el --- Clean filler and transcript artifacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Two entry points:
;;
;;   `whittle' — conservative pass for any prose. Removes comma-bounded
;;     interjections (", um, ", ", I mean, ", ", like, ", ", kind of like, "),
;;     collapses accidental duplicated words, normalizes punctuation and case.
;;
;;   `whittle-transcript' — aggressive pass for speech-to-text output.
;;     Adds line-joining for mid-sentence wraps, sentence-edge filler removal
;;     ("Like, ..." / ". So, ..."), filler-chain collapse, and false-start
;;     detection on top of the conservative pass.
;;
;; Both accept an optional region. Empty buffer or no region falls through to
;; the whole buffer. Reports per-rule counts in the echo area on completion.

;;; Code:

(require 'cl-lib)
(require 'subr-x)   ;; for `string-join'
(require 'rx)
(require 'seq)

;;; Customization -------------------------------------------------------------

(defvar whittle/conservative-filler-rules
  ;; Two patterns per filler: the comma-bounded interjection form (",
  ;; um, ") is removed wholesale, including both surrounding commas, so
  ;; we don't leave a stranded leading comma like "very, hierarchical".
  ;; The bare form ("um ") is removed afterward for any leftovers.
  '(("kind of like" ",[[:blank:]]*\\<kind of like\\>[[:blank:]]*,?[[:blank:]]*" " ")
    ("kind of like" "\\<kind of like\\>[[:space:][:punct:]]*" "")
    ("i mean" ",[[:blank:]]*\\<i mean\\>[[:blank:]]*,?[[:blank:]]*" " ")
    ("i mean" "\\<i mean\\>[[:space:][:punct:]]*" "")
    ("um/uh" ",[[:blank:]]*\\<\\(?:um+\\|uh\\)\\>[[:blank:]]*,?[[:blank:]]*" " ")
    ("um/uh" "\\<\\(?:um+\\|uh\\)\\>[[:space:][:punct:]]*" "")
    (", like," ",[[:blank:]]*\\<like\\>[[:blank:]]*,[[:blank:]]*" " "))
  "Conservative filler cleanup rules as (LABEL REGEXP REPLACEMENT).")

(defvar whittle/transcript-edge-filler-rules
  ;; Use [:blank:] (space/tab) rather than [:space:] (which includes
  ;; newlines) for leading/trailing slop. [:space:] would let `^...*`
  ;; anchor at a blank line and greedily eat the trailing newline,
  ;; collapsing the paragraph break before the next paragraph's filler.
  '(("edge filler"
     "^[[:blank:]]*\\(?:\\<so\\>\\|\\<well\\>\\|\\<ok\\(?:ay\\)?\\>\\|\\<right\\>\\|\\<you know\\>\\|\\<like\\>\\)[,[:blank:]-]*"
     "")
    ("edge filler"
     "\\([.!?][[:blank:]\n]+\\)\\(?:\\<so\\>\\|\\<well\\>\\|\\<ok\\(?:ay\\)?\\>\\|\\<right\\>\\|\\<you know\\>\\|\\<like\\>\\)[,[:blank:]-]*"
     "\\1")
    ("edge filler"
     "[[:blank:]]*,?[[:blank:]]*\\(?:\\<right\\>\\|\\<you know\\>\\)\\([.?!]?\\)[[:blank:]]*$"
     "\\1"))
  "Aggressive transcript rules for sentence-edge fillers.")

(defconst whittle/transcript-filler-chain-regexp
  "\\(?:\\<\\(?:um+\\|uh\\|erm\\|ah\\|like\\|you know\\|i mean\\|so\\|well\\|ok\\(?:ay\\)?\\|right\\)\\>[[:space:][:punct:]]*\\)\\{2,\\}"
  "Regexp matching transcript-style chains of filler phrases.")

(defconst whittle/duplicate-word-exclusions
  '("that" "so" "very" "really" "yes" "no" "ha" "wow" "well" "yeah"
    "go" "bye" "oh" "had" "have")
  "Words that can appear twice legitimately (case-insensitive).")

(defconst whittle/duplicate-word-comma-exclusions
  '("am" "are" "be" "been" "being" "can" "could" "did" "do" "does"
    "had" "has" "have" "he" "her" "hers" "him" "his" "i" "is" "it"
    "its" "me" "might" "must" "my" "our" "ours" "she" "should" "that"
    "their" "theirs" "them" "they" "us" "was" "we" "were" "will"
    "would" "you" "your" "yours")
  "Function words that should not collapse when repeated across a comma.")

(defvar whittle/transcript-report-directory "/tmp"
  "Directory where `whittle-transcript-report' writes review reports.")

(defconst whittle/false-start-prefixes
  '("a" "an" "he" "her" "his" "i" "it" "my" "our" "she" "the" "their"
    "these" "they" "this" "those" "we" "you" "your"
    ;; Contractions — the word-boundary in the regex won't reach across
    ;; the apostrophe, so "it" alone won't match "it's a, it's a".
    "he's" "i'm" "it's" "she's" "that's" "there's" "they're" "we're"
    "what's" "you're")
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
               ;; Comma is a separator too — transcripts often punctuate
               ;; stammered repeats as "your, your" or "I, I".
               (dup-regexp "\\<\\([[:alpha:]']+\\)\\>\\(?:[[:space:]\n,–—]+\\1\\>\\)+")
               (removed (make-hash-table :test #'equal)))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (re-search-forward dup-regexp limit-marker t)
            (let ((word (downcase (match-string 1)))
                  (match-text (match-string 0))
                  (preceding (char-before (match-beginning 0))))
              ;; Skip if the first occurrence is part of a hyphenated
              ;; compound (e.g. "goings-on on the ship" must stay), or
              ;; if the word is in the exclusion list. Comma-separated
              ;; function-word repeats like "is, is" are usually grammar,
              ;; not stammers.
              (unless (or (eq preceding ?-)
                          (member word whittle/duplicate-word-exclusions)
                          (and (string-match-p "," match-text)
                               (member word whittle/duplicate-word-comma-exclusions)))
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
                         "\\(?:[[:space:]\n,]+\\)\\1\\>")))
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

(defun whittle--collapse-em-dash-false-starts (beg end)
  "Drop transcript false-start fragments ending in `—' between BEG and END.
A candidate fragment is the text between the previous sentence boundary
and an em-dash followed by whitespace. It is removed only if it contains
at least one comma (a strong false-start signal — \"I, it's— I think\")
and is 10 words or fewer. Leading-comma-less em-dash uses like \"The cat
— a tabby — sat\" are left alone."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (limit-marker (copy-marker limit))
               (count 0))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (re-search-forward "—[[:space:]]+" limit-marker t)
            (let* ((em-start (match-beginning 0))
                   (em-end (match-end 0))
                   (frag-start
                    (save-excursion
                      (goto-char em-start)
                      (if (re-search-backward "[.!?\n]" start t)
                          (progn (forward-char 1)
                                 (skip-chars-forward " \t" em-start)
                                 (point))
                        start)))
                   (fragment (buffer-substring-no-properties frag-start em-start)))
              (when (and (string-match-p "," fragment)
                         (<= (length (split-string fragment "[[:space:]]+" t)) 10))
                (delete-region frag-start em-end)
                (setq count (1+ count))))))
      (set-marker limit-marker nil))
    count))

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
          (while (re-search-forward "[[:blank:]]+\\([,.;:?!]\\)" limit-marker t)
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
          (while (re-search-forward "[[:blank:]]\\{2,\\}" limit-marker t)
            (replace-match " " t t)
            (setq count (1+ count))))
      (set-marker limit-marker nil))
    count))

(defun whittle--normalize-case (beg end &optional skip-initial)
  "Capitalize lone i and sentence starts between BEG and END.
When SKIP-INITIAL is non-nil, do not capitalize the first
character in the region. Report mode uses this when processing
paragraph units in temp buffers, where the first character of the
unit is not necessarily the start of the source buffer."
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
          (unless skip-initial
            (goto-char start)
            (skip-chars-forward " \t\n\"'([{" limit-marker)
            (when (looking-at "[[:lower:]]")
              (replace-match (upcase (match-string 0)) t t)
              (setq count (1+ count))))
          (goto-char start)
          (while (re-search-forward "[.?!][[:space:]\n]+" limit-marker t)
            (let ((punct-pos (match-beginning 0)))
              (unless (and (eq (char-after punct-pos) ?.)
                           (eq (char-before punct-pos) ?.))
                (save-excursion
                  (skip-chars-forward " \t\n\"'([{" limit-marker)
                  (when (looking-at "[[:lower:]]")
                    (replace-match (upcase (match-string 0)) t t)
                    (setq count (1+ count)))))))
          ;; Capitalize after org headings (e.g. *** Speaker:\ntext)
          (goto-char start)
          (while (re-search-forward "^\\*+[^\n]*\n" limit-marker t)
            (skip-chars-forward " \t\n\"'([{" limit-marker)
            (when (looking-at "[[:lower:]]")
              (replace-match (upcase (match-string 0)) t t)
              (setq count (1+ count)))))
      (set-marker limit-marker nil))
    count))

;;; Transcript review reports --------------------------------------------------

(defconst whittle--transcript-report-risk-order '("high" "medium" "low")
  "Display order for transcript report risk sections.")

(defconst whittle--transcript-report-medium-risk-passes
  '("sentence-edge filler removal"
    "em-dash false-start detection"
    "false-start collapse"
    "duplicate-word collapse"
    "punctuation cleanup"
    "case normalization")
  "Passes that merit medium review risk unless promoted to high.")

(defun whittle--transcript-report-pass-specs ()
  "Return transcript dry-run pass specs as (LABEL . FUNCTION)."
  (list
   (cons "line-joining" #'whittle--join-transcript-lines)
   (cons "filler-chain collapse" #'whittle--remove-filler-chains)
   (cons "conservative filler removal"
         (lambda (beg end)
           (whittle--remove-filler-words beg end whittle/conservative-filler-rules)))
   (cons "sentence-edge filler removal"
         (lambda (beg end)
           (whittle--remove-filler-words beg end whittle/transcript-edge-filler-rules)))
   ;; Keep this order aligned with `whittle-transcript'.
   (cons "em-dash false-start detection" #'whittle--collapse-em-dash-false-starts)
   (cons "false-start collapse" #'whittle--remove-false-starts)
   (cons "duplicate-word collapse" #'whittle--remove-duplicated-words)
   (cons "punctuation cleanup" #'whittle--cleanup-punctuation)
   (cons "case normalization"
         (lambda (beg end)
           (whittle--normalize-case beg end t)))))

(defun whittle--transcript-report-source-name ()
  "Return the current source path or buffer name for a report."
  (or (and buffer-file-name (file-truename buffer-file-name))
      (format "buffer:%s" (buffer-name))))

(defun whittle--transcript-report-temp-file ()
  "Return a new temp-file path for a transcript report."
  (expand-file-name
   (format "whittle-transcript-review-%s.org"
           (format-time-string "%Y%m%d-%H%M%S"))
   whittle/transcript-report-directory))

(defun whittle--transcript-report-blank-line-p ()
  "Return non-nil when point is on a blank line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun whittle--transcript-report-heading-line-p ()
  "Return non-nil when point is on an org heading line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^\\*+ ")))

(defun whittle--transcript-report-trim-unit (text)
  "Remove trailing newlines from paragraph unit TEXT."
  (replace-regexp-in-string "\n+\\'" "" text))

(defun whittle--transcript-report-units (beg end)
  "Return stable paragraph units between BEG and END.
Units are split on blank lines. Org heading lines are boundaries
and are excluded from cleanup."
  (pcase-let* ((`(,start . ,limit) (whittle--region-bounds beg end))
               (units nil)
               (index 0)
               (unit-start nil))
    (cl-labels
        ((finish-unit
          (unit-end)
          (when (and unit-start (< unit-start unit-end))
            (let ((text (whittle--transcript-report-trim-unit
                         (buffer-substring-no-properties unit-start unit-end))))
              (unless (string-blank-p text)
                (setq index (1+ index))
                (push (list :index index
                            :line (line-number-at-pos unit-start)
                            :text text)
                      units))))
          (setq unit-start nil)))
      (save-excursion
        (goto-char start)
        (while (< (point) limit)
          (let ((line-start (point)))
            (if (or (whittle--transcript-report-blank-line-p)
                    (whittle--transcript-report-heading-line-p))
                (progn
                  (finish-unit line-start)
                  (forward-line 1))
              (unless unit-start
                (setq unit-start line-start))
              (forward-line 1))))
        (finish-unit limit)))
    (nreverse units)))

(defun whittle--transcript-report-process-unit (text)
  "Run the transcript cleanup pipeline on TEXT and return result plist."
  (with-temp-buffer
    (insert text)
    (let (passes)
      (dolist (spec (whittle--transcript-report-pass-specs))
        (let ((before-pass (buffer-string)))
          (funcall (cdr spec) (point-min) (point-max))
          (unless (string= before-pass (buffer-string))
            (push (car spec) passes))))
      (list :after (buffer-string)
            :passes (nreverse passes)))))

(defun whittle--transcript-report-comma-function-repeat-p (text)
  "Return non-nil if TEXT has a comma-separated repeated function word."
  (let ((case-fold-search t)
        (start 0)
        found)
    (while (and (not found)
                (string-match
                 "\\<\\([[:alpha:]']+\\)\\>[[:blank:]]*,[[:blank:]]*\\1\\>"
                 text start))
      (when (member (downcase (match-string 1 text))
                    whittle/duplicate-word-comma-exclusions)
        (setq found t))
      (setq start (match-end 0)))
    found))

(defun whittle--transcript-report-risk (before passes)
  "Return risk label for BEFORE text changed by PASSES."
  (let ((case-fold-search t))
    (cond
     ((or (string-match-p "\\<\\(like\\|i mean\\|kind of like\\)\\>" before)
          (and (member "duplicate-word collapse" passes)
               (whittle--transcript-report-comma-function-repeat-p before))
          (and (member "case normalization" passes)
               (> (length passes) 1)))
      "high")
     ((cl-some (lambda (pass)
                 (member pass whittle--transcript-report-medium-risk-passes))
               passes)
      "medium")
     (t "low"))))

(defun whittle--transcript-report-build-entries (units)
  "Return changed report entries for paragraph UNITS."
  (let (entries)
    (dolist (unit units)
      (let* ((before (plist-get unit :text))
             (result (whittle--transcript-report-process-unit before))
             (after (plist-get result :after))
             (passes (plist-get result :passes)))
        (when (and passes (not (string= before after)))
          (push (list :unit (plist-get unit :index)
                      :line (plist-get unit :line)
                      :risk (whittle--transcript-report-risk before passes)
                      :passes passes
                      :before before
                      :after after)
                entries))))
    (cl-loop for entry in (nreverse entries)
             for number from 1
             collect (append (list :number number) entry))))

(defun whittle--transcript-report-count-by (entries key values)
  "Return an alist counting ENTRIES by plist KEY in VALUES order."
  (mapcar (lambda (value)
            (cons value
                  (cl-count-if (lambda (entry)
                                 (equal (plist-get entry key) value))
                               entries)))
          values))

(defun whittle--transcript-report-pass-counts (entries)
  "Return an alist of pass labels and counts for ENTRIES."
  (let ((pass-labels (mapcar #'car (whittle--transcript-report-pass-specs))))
    (mapcar (lambda (label)
              (cons label
                    (cl-count-if (lambda (entry)
                                   (member label (plist-get entry :passes)))
                                 entries)))
            pass-labels)))

(defun whittle--transcript-report-count-line (counts)
  "Return one org summary line for alist COUNTS."
  (if (cl-some (lambda (item) (> (cdr item) 0)) counts)
      (string-join
       (mapcar (lambda (item)
                 (format "%s %d" (car item) (cdr item)))
               counts)
       "; ")
    "none"))

(defun whittle--transcript-report-format-entry (entry)
  "Return org text for a single report ENTRY."
  (format (concat "*** Change %d\n"
                  "- Approx line :: %d\n"
                  "- Unit :: %d\n"
                  "- Risk :: %s\n"
                  "- Passes :: %s\n\n"
                  "Before:\n"
                  "#+begin_quote\n%s\n#+end_quote\n\n"
                  "After:\n"
                  "#+begin_quote\n%s\n#+end_quote\n\n")
          (plist-get entry :number)
          (plist-get entry :line)
          (plist-get entry :unit)
          (plist-get entry :risk)
          (string-join (plist-get entry :passes) ", ")
          (plist-get entry :before)
          (plist-get entry :after)))

(defun whittle--transcript-report-risk-title (risk)
  "Return section title for RISK."
  (pcase risk
    ("high" "High-Risk Changes")
    ("medium" "Medium-Risk Changes")
    ("low" "Low-Risk Changes")
    (_ (format "%s Changes" (capitalize risk)))))

(defun whittle--transcript-report-format-risk-section (risk entries)
  "Return org report section for RISK from ENTRIES."
  (let ((matching (seq-filter (lambda (entry)
                                (equal (plist-get entry :risk) risk))
                              entries)))
    (concat
     (format "** %s\n\n" (whittle--transcript-report-risk-title risk))
     (if matching
         (mapconcat #'whittle--transcript-report-format-entry matching "")
       "No changes.\n\n"))))

(defun whittle--transcript-report-format (source temp-file entries)
  "Return the full transcript review report for SOURCE and ENTRIES."
  (let* ((risk-counts
          (whittle--transcript-report-count-by
           entries :risk whittle--transcript-report-risk-order))
         (pass-counts (whittle--transcript-report-pass-counts entries)))
    (concat
     "* Whittle Transcript Review Report\n"
     (format "- Source :: %s\n" source)
     (format "- Generated :: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]"))
     "- Command :: whittle-transcript-report\n"
     "- Mode :: Dry-run report only; the source buffer is unchanged.\n"
     "- Unit boundary :: Blank-line-delimited paragraph units; org headings are boundaries and are excluded from cleanup.\n"
     "- Source edit target :: Codex should edit the source file above directly; this report is not a replacement transcript.\n"
     "- Boundary caveat :: Unit-level dry runs intentionally prevent line-joining across paragraph or heading boundaries, so boundary-sensitive output may differ from a whole-buffer edit.\n"
     (format "- Temp file :: %s\n\n" temp-file)
     "** Instructions for Codex\n\n"
     "Review each before/after pair, apply only the safe changes directly to the source file, and repair any broken changes instead of copying the report wholesale.\n\n"
     "** Summary\n\n"
     (format "- Changed units :: %d\n" (length entries))
     (format "- Risk counts :: %s\n"
             (whittle--transcript-report-count-line risk-counts))
     (format "- Pass counts :: %s\n\n"
             (whittle--transcript-report-count-line pass-counts))
     (mapconcat (lambda (risk)
                  (whittle--transcript-report-format-risk-section risk entries))
                whittle--transcript-report-risk-order
                ""))))

(defun whittle--transcript-report-generate (beg end &optional temp-file)
  "Generate a transcript review report for BEG to END.
Return a plist with :report, :temp-file, and :entries."
  (let* ((target-temp-file (or temp-file (whittle--transcript-report-temp-file)))
         (units (whittle--transcript-report-units beg end))
         (entries (whittle--transcript-report-build-entries units))
         (source (whittle--transcript-report-source-name))
         (report (whittle--transcript-report-format source target-temp-file entries)))
    (list :report report
          :temp-file target-temp-file
          :entries entries)))

(defun whittle--write-string-to-file (text file)
  "Write TEXT to FILE, creating the parent directory if needed."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert text)))

(defun whittle--copy-string-to-pbcopy (text)
  "Copy TEXT to the macOS clipboard using pbcopy."
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "pbcopy" nil nil nil)))

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
         (punctuation (whittle--cleanup-punctuation beg end))
         (case-fixes (whittle--normalize-case beg end)))
    (whittle--report-summaries
     "Whittle"
     (list (whittle--format-table-summary filler-counts "fillers")
           (whittle--format-table-summary duplicate-counts "duplicates")
           (whittle--format-count-summary punctuation "punctuation fixes")
           (whittle--format-count-summary case-fixes "case fixes")))))

(defun whittle-transcript (&optional beg end)
  "Run aggressive transcript cleanup on the region or buffer."
  (interactive (whittle--interactive-bounds))
  (let* ((line-joins (whittle--join-transcript-lines beg end))
         (filler-chains (whittle--remove-filler-chains beg end))
         (conservative-fillers
          (whittle--remove-filler-words beg end whittle/conservative-filler-rules))
         (edge-fillers
          (whittle--remove-filler-words beg end whittle/transcript-edge-filler-rules))
         ;; Em-dash collapse must run before false-starts/dup-words, since
         ;; those would otherwise strip the comma that signals the fragment
         ;; is a stammer rather than a parenthetical.
         (em-dash-false-starts (whittle--collapse-em-dash-false-starts beg end))
         (false-starts (whittle--remove-false-starts beg end))
         (duplicate-counts (whittle--remove-duplicated-words beg end))
         (punctuation (whittle--cleanup-punctuation beg end))
         (case-fixes (whittle--normalize-case beg end)))
    (whittle--report-summaries
     "Whittle transcript"
     (list (whittle--format-count-summary line-joins "joined lines")
           (whittle--format-count-summary filler-chains "filler chains")
           (whittle--format-count-summary em-dash-false-starts "em-dash false starts")
           (whittle--format-table-summary conservative-fillers "fillers")
           (whittle--format-table-summary edge-fillers "edge fillers")
           (whittle--format-table-summary false-starts "false starts")
           (whittle--format-table-summary duplicate-counts "duplicates")
           (whittle--format-count-summary punctuation "punctuation fixes")
           (whittle--format-count-summary case-fixes "case fixes")))))

(defun whittle-transcript-report (&optional beg end)
  "Create a dry-run review report for transcript cleanup.
The source buffer is not edited. The report is copied to the
clipboard and saved under `whittle/transcript-report-directory'."
  (interactive (whittle--interactive-bounds))
  (let* ((result (whittle--transcript-report-generate beg end))
         (report (plist-get result :report))
         (temp-file (plist-get result :temp-file)))
    (whittle--write-string-to-file report temp-file)
    (whittle--copy-string-to-pbcopy report)
    (message "Whittle transcript report copied to clipboard and saved to %s" temp-file)
    result))

(provide 'whittle)
;;; whittle.el ends here
