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
;;     detection on top of the conservative pass. Also copies an org before/after
;;     report to the clipboard for Codex review.
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
     "^[[:blank:]]*\\(?:\\<so\\>\\|\\<well\\>\\|\\<ok\\(?:ay\\)?\\>\\|\\<you know\\>\\)[,[:blank:]-]*"
     "")
    ("edge filler"
     "\\([.!?][[:blank:]\n]+\\)\\(?:\\<so\\>\\|\\<well\\>\\|\\<ok\\(?:ay\\)?\\>\\|\\<you know\\>\\)[,[:blank:]-]*"
     "\\1")
    ("edge filler"
     "[[:blank:]]*,?[[:blank:]]*\\<you know\\>\\([.?!]?\\)[[:blank:]]*$"
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

(defvar whittle/report-clipboard-detail-risks '("high" "medium")
  "Risk levels to include as full before/after entries in clipboard reports.")

(defvar whittle/report-clipboard-max-detailed-entries 25
  "Maximum number of full before/after entries in clipboard reports.")

(defvar whittle/report-clipboard-context-chars 32
  "Characters of context to keep around the changed span in clipboard reports.")

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
                    (match-text (match-string 0))
                    (match-beg (match-beginning 0))
                    (replacement (match-string 1)))
                ;; A comma-separated repeat of a single function word
                ;; (e.g. "you, you have to stop") is usually grammar, not
                ;; a restart. The comma-exclusion list is all single
                ;; words, so multi-word restarts like "I was, I was"
                ;; still collapse.
                (unless (and (string-match-p "," match-text)
                             (member phrase whittle/duplicate-word-comma-exclusions))
                  (whittle--increment removed phrase)
                  (replace-match replacement t t)
                  (goto-char match-beg)))))
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

;;; Cleanup review reports -----------------------------------------------------

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

(defconst whittle--transcript-report-hazard-order
  '("orphan-leading-punctuation"
    "orphan-ellipsis"
    "punctuation-cluster"
    "semantic-question-flip"
    "sentence-initial-i-mean"
    "quote-marker-deletion"
    "like-clause-fusion"
    "right-so-chain"
    "lowercase-start-after-opener"
    "case-clause-fusion"
    "deleted-comma-before-pronoun"
    "imperative-fusion"
    "protected-span-overlap")
  "Display order for transcript cleanup hazard tags.")

(defun whittle--transcript-report-pass-specs (&optional profile)
  "Return cleanup pass specs for PROFILE as (LABEL . FUNCTION).
PROFILE is either `conservative' or `transcript', defaulting to
`transcript' for compatibility with older report helpers."
  (let ((conservative-fillers
         (mapcar
          (lambda (rule)
            (let ((single-rule rule))
              (cons "conservative filler removal"
                    (lambda (beg end)
                      (whittle--remove-filler-words beg end (list single-rule))))))
          whittle/conservative-filler-rules))
        (duplicate (cons "duplicate-word collapse" #'whittle--remove-duplicated-words))
        (punctuation (cons "punctuation cleanup" #'whittle--cleanup-punctuation))
        (case-normalization
         (cons "case normalization"
               (lambda (beg end)
                 (whittle--normalize-case beg end t)))))
    (if (eq profile 'conservative)
        (append conservative-fillers
                (list duplicate punctuation case-normalization))
      (append
       (list
        (cons "line-joining" #'whittle--join-transcript-lines)
        (cons "filler-chain collapse" #'whittle--remove-filler-chains))
       conservative-fillers
       (list
        (cons "sentence-edge filler removal"
              (lambda (beg end)
                (whittle--remove-filler-words
                 beg end whittle/transcript-edge-filler-rules)))
        ;; Em-dash collapse must run before false-starts/dup-words, since
        ;; those would otherwise strip the comma that signals the fragment
        ;; is a stammer rather than a parenthetical.
        (cons "em-dash false-start detection" #'whittle--collapse-em-dash-false-starts)
        (cons "false-start collapse" #'whittle--remove-false-starts)
        duplicate
        punctuation
        case-normalization)))))

(defun whittle--transcript-report-pass-labels (&optional profile)
  "Return unique cleanup pass labels for PROFILE in display order."
  (seq-uniq
   (mapcar #'car (whittle--transcript-report-pass-specs profile))
   #'string=))

(defun whittle--transcript-report-source-name ()
  "Return the current source path or buffer name for a report."
  (or (and buffer-file-name (file-truename buffer-file-name))
      (format "buffer:%s" (buffer-name))))

(defun whittle--transcript-report-temp-file (&optional command-name)
  "Return a new temp-file path for a cleanup report from COMMAND-NAME."
  (expand-file-name
   (format "%s-review-%s.org"
           (or command-name "whittle-transcript")
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
            (let* ((text-end
                    (save-excursion
                      (goto-char unit-end)
                      (skip-chars-backward "\n" unit-start)
                      (point)))
                   (text (whittle--transcript-report-trim-unit
                          (buffer-substring-no-properties unit-start text-end))))
              (unless (string-blank-p text)
                (setq index (1+ index))
                (push (list :index index
                            :line (line-number-at-pos unit-start)
                            :start unit-start
                            :end text-end
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

(defun whittle--transcript-report-new-hazards (before-tags after-tags)
  "Return hazard tags in AFTER-TAGS that are not present in BEFORE-TAGS."
  (seq-remove (lambda (hazard)
                (member hazard before-tags))
              after-tags))

(defun whittle--transcript-report-change-span (before after)
  "Return the changed source span in BEFORE as a 1-based (START . END) pair."
  (let* ((prefix-length (whittle--common-prefix-length before after))
         (suffix-length (whittle--common-suffix-length before after prefix-length))
         (before-change-end (max prefix-length (- (length before) suffix-length))))
    (cons (1+ prefix-length) (1+ before-change-end))))

(defun whittle--transcript-report-protect-char-p (char)
  "Return non-nil when CHAR should be protected next to a held-back span."
  (and char
       (or (memq char '(?\s ?\t ?, ?. ?? ?! ?\; ?: ?… ?\" ?\' ?\( ?\)
                            ?\[ ?\] ?{ ?} ?- ?– ?—))
           (eq char ?\n))))

(defun whittle--transcript-report-expand-protected-span (text span)
  "Expand SPAN in TEXT to include adjacent punctuation and spacing."
  (let ((start (car span))
        (end (cdr span))
        (length (length text)))
    (while (and (> start 1)
                (whittle--transcript-report-protect-char-p
                 (aref text (- start 2))))
      (setq start (1- start)))
    (while (and (<= end length)
                (whittle--transcript-report-protect-char-p
                 (aref text (1- end))))
      (setq end (1+ end)))
    (cons start end)))

(defun whittle--transcript-report-span-overlap-p (left right)
  "Return non-nil when LEFT and RIGHT spans overlap."
  (and (< (car left) (cdr right))
       (< (car right) (cdr left))))

(defun whittle--transcript-report-overlaps-protected-p (span protected-spans)
  "Return non-nil if SPAN overlaps any span in PROTECTED-SPANS."
  (cl-some (lambda (protected)
             (whittle--transcript-report-span-overlap-p span protected))
           protected-spans))

(defun whittle--transcript-report-shift-protected-spans
    (protected-spans change-span before after)
  "Shift PROTECTED-SPANS after accepting CHANGE-SPAN from BEFORE to AFTER."
  (let ((delta (- (length after) (length before)))
        (change-end (cdr change-span)))
    (mapcar (lambda (protected)
              (if (<= change-end (car protected))
                  (cons (+ (car protected) delta)
                        (+ (cdr protected) delta))
                protected))
            protected-spans)))

(defun whittle--transcript-report-run-unit (text profile revert-hazards)
  "Run PROFILE cleanup on TEXT.
When REVERT-HAZARDS is non-nil, revert a pass that introduces a
new hazard and record the held-back candidate edit."
  (with-temp-buffer
    (insert text)
    (let ((original text)
          passes
          held-back
          protected-spans)
      (dolist (spec (whittle--transcript-report-pass-specs profile))
        (let* ((label (car spec))
               (before-pass (buffer-string))
               (before-hazards
                (whittle--transcript-report-hazards
                 original before-pass (nreverse (copy-sequence passes)) profile)))
          (funcall (cdr spec) (point-min) (point-max))
          (let ((after-pass (buffer-string)))
            (unless (string= before-pass after-pass)
              (let* ((candidate-passes
                      (append (nreverse (copy-sequence passes)) (list label)))
                     (change-span
                      (whittle--transcript-report-change-span
                       before-pass after-pass))
                     (protected-change-span
                      (whittle--transcript-report-expand-protected-span
                       before-pass change-span))
                     (touches-protected
                      (whittle--transcript-report-overlaps-protected-p
                       protected-change-span protected-spans))
                     (after-hazards
                      (whittle--transcript-report-hazards
                       original after-pass candidate-passes profile))
                     (new-hazards
                      (whittle--transcript-report-new-hazards
                       before-hazards after-hazards)))
                (cond
                 ((and revert-hazards touches-protected)
                  (push (list :pass label
                              :hazards '("protected-span-overlap")
                              :status 'protected-overlap
                              :before before-pass
                              :after after-pass
                              :protected-span protected-change-span)
                        held-back)
                  (erase-buffer)
                  (insert before-pass))
                 ((and revert-hazards new-hazards)
                   (push (list :pass label
                               :hazards new-hazards
                               :status 'hazard
                               :before before-pass
                               :after after-pass
                               :protected-span protected-change-span)
                         held-back)
                   (push protected-change-span protected-spans)
                   (erase-buffer)
                   (insert before-pass))
                  (t
                   (setq protected-spans
                         (whittle--transcript-report-shift-protected-spans
                          protected-spans change-span before-pass after-pass))
                   (unless (member label passes)
                     (push label passes)))))))))
      (list :after (buffer-string)
            :passes (nreverse passes)
            :hazards (whittle--transcript-report-hazards
                      original
                      (buffer-string)
                      (nreverse (copy-sequence passes))
                      profile)
            :held-back (nreverse held-back)))))

(defun whittle--transcript-report-process-unit (text &optional profile)
  "Run the PROFILE cleanup pipeline on TEXT and return result plist."
  (let* ((safe-result
          (whittle--transcript-report-run-unit text profile t))
         (full-result
          (whittle--transcript-report-run-unit text profile nil))
         (held-back (plist-get safe-result :held-back))
         (held-back-hazards
          (seq-uniq
           (apply #'append (mapcar (lambda (record)
                                     (plist-get record :hazards))
                                   held-back))
           #'string=)))
    (list :after (plist-get safe-result :after)
          :passes (plist-get safe-result :passes)
          :hazards held-back-hazards
          :held-back held-back
          :full-after (plist-get full-result :after)
          :full-passes (plist-get full-result :passes)
          :full-hazards (plist-get full-result :hazards))))

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

(defun whittle--transcript-profile-p (profile)
  "Return non-nil when PROFILE should use transcript safety gates."
  (not (eq profile 'conservative)))

(defun whittle--string-match-case-p (regexp string)
  "Return non-nil if REGEXP matches STRING case-sensitively."
  (let ((case-fold-search nil))
    (string-match-p regexp string)))

(defun whittle--transcript-report-like-clause-fusion-tags (before after)
  "Return hazard tags for risky comma-bounded `like' deletion."
  (let ((case-fold-search t)
        (start 0)
        tags)
    (when (and (string-match-p ",[[:blank:]]*like[[:blank:]]*," before)
               (not (string-match-p ",[[:blank:]]*like[[:blank:]]*," after)))
      (while (string-match
              "\\<\\([[:alpha:]']+\\)\\>[[:blank:]]*,[[:blank:]]*like[[:blank:]]*,[[:blank:]]*\\<\\([[:alpha:]']+\\)\\>"
              before start)
        (let ((next (downcase (match-string 2 before))))
          (when (member next
                        '("i" "i'm" "you" "you're" "we" "we're" "they"
                          "they're" "he" "she" "it"))
            (push "like-clause-fusion" tags)
            (push "deleted-comma-before-pronoun" tags))
          (when (member next
                        '("ask" "outline" "tell" "think" "forget" "let"
                          "how" "what" "where" "when" "why" "who"))
            (push "like-clause-fusion" tags)
            (push "imperative-fusion" tags)))
        (setq start (match-end 0))))
    (seq-uniq tags #'string=)))

(defun whittle--transcript-report-hazards (before after _passes &optional profile)
  "Return mechanical hazard tags for cleanup from BEFORE to AFTER.
PASSES is the list of cleanup passes that have run so far."
  (let ((case-fold-search t)
        (transcript-profile (whittle--transcript-profile-p profile))
        hazards)
    (when (string-match-p "\\`[[:space:]]*\\(?:…\\|\\.\\.\\.\\)" after)
      (push "orphan-ellipsis" hazards))
    (when (string-match-p "\\`[[:space:]]*[?.]" after)
      (push "orphan-leading-punctuation" hazards))
    (when (string-match-p "\\.\\?" after)
      (push "punctuation-cluster" hazards))
    (when (and (string-match-p
                "[[:blank:]]*,[[:blank:]]*\\<right\\>[[:blank:]]*\\?[[:blank:]]*\\'"
                before)
               (string-match-p "\\?[[:blank:]]*\\'" after)
               (not (string-match-p "\\<right\\>" after)))
      (push "semantic-question-flip" hazards))
    (when (and transcript-profile
               (string-match-p
                "\\(?:\\`\\|[.?!…][[:space:]]+\\)\\<i mean\\>[[:blank:]]*,[[:blank:]]*[[:alpha:]]"
                before)
               (not (string-match-p
                     "\\(?:\\`\\|[.?!…][[:space:]]+\\)\\<i mean\\>[[:blank:]]*,"
                     after)))
      (push "sentence-initial-i-mean" hazards))
    (when (and transcript-profile
               (string-match-p
                ",[[:blank:]]*\\<i mean\\>[[:blank:]]*,[[:blank:]]*\\(?:\\<i\\>\\|\\<you\\>\\|\\<we\\>\\|\\<they\\>\\|\\<he\\>\\|\\<she\\>\\|\\<it\\>\\|\\<don't\\>\\|\\<do\\>\\|\\<first\\>\\)"
                before)
               (not (string-match-p
                     ",[[:blank:]]*\\<i mean\\>[[:blank:]]*,"
                     after)))
      (push "sentence-initial-i-mean" hazards)
      (push "deleted-comma-before-pronoun" hazards))
    (when (and (string-match-p "\\`[[:blank:]]*\\<like\\>" before)
               (not (string-match-p "\\`[[:blank:]]*\\<like\\>" after)))
      (push "quote-marker-deletion" hazards))
    (setq hazards
          (append (whittle--transcript-report-like-clause-fusion-tags before after)
                  hazards))
    (when (and transcript-profile
               (string-match-p
                "\\<right\\>[[:blank:]]*\\?[[:blank:]]+\\<so\\>[[:blank:]]*\\(?:…\\|\\.\\.\\.\\)"
                before)
               (not (string-match-p
                     "\\<right\\>[[:blank:]]*\\?[[:blank:]]+\\<so\\>[[:blank:]]*\\(?:…\\|\\.\\.\\.\\)"
                     after)))
      (push "right-so-chain" hazards))
    (when (and transcript-profile
               (string-match-p
                "\\`[[:blank:]]*\\(?:\\<so\\>\\|\\<well\\>\\|\\<ok\\(?:ay\\)?\\>\\)[,[:blank:]-]+"
                before)
               (whittle--string-match-case-p "\\`[[:blank:]]*[[:lower:]]" after))
      (push "lowercase-start-after-opener" hazards))
    (when (and transcript-profile
               (string-match-p
                "\\<right\\>[[:blank:]]*\\?[[:blank:]]+\\<so\\>[[:blank:]]*\\(?:…\\|\\.\\.\\.\\)"
                before)
               (whittle--string-match-case-p ",[[:blank:]]+[[:upper:]][[:lower:]]" after))
      (push "case-clause-fusion" hazards))
    (when (and transcript-profile
               (whittle--string-match-case-p
                "\\.[[:blank:]\n]+\\(?:that\\|which\\|who\\|whose\\|whom\\|is\\|are\\|was\\|were\\)\\>"
                before)
               (whittle--string-match-case-p
                "\\.[[:blank:]\n]+\\(?:That\\|Which\\|Who\\|Whose\\|Whom\\|Is\\|Are\\|Was\\|Were\\)\\>"
                after))
      (push "case-clause-fusion" hazards))
    (seq-filter
     (lambda (hazard)
       (member hazard hazards))
     whittle--transcript-report-hazard-order)))

(defun whittle--transcript-report-hazard-p (entry)
  "Return non-nil if ENTRY has mechanical hazard tags."
  (not (null (plist-get entry :hazards))))

(defun whittle--transcript-report-risk (before _after passes hazards)
  "Return risk label for BEFORE to AFTER changed by PASSES and HAZARDS."
  (let ((case-fold-search t))
    (cond
     ((or hazards
          (string-match-p "\\<\\(like\\|i mean\\|kind of like\\)\\>" before)
          (and (or (member "duplicate-word collapse" passes)
                   (member "false-start collapse" passes))
               (whittle--transcript-report-comma-function-repeat-p before))
          (and (member "case normalization" passes)
               (> (length passes) 1)))
      "high")
     ((cl-some (lambda (pass)
                 (member pass whittle--transcript-report-medium-risk-passes))
               passes)
      "medium")
     (t "low"))))

(defun whittle--transcript-report-build-entries (units &optional profile)
  "Return changed report entries for paragraph UNITS using PROFILE."
  (let (entries)
    (dolist (unit units)
      (let* ((before (plist-get unit :text))
             (result (whittle--transcript-report-process-unit before profile))
             (after (plist-get result :after))
             (passes (plist-get result :passes))
             (hazards (plist-get result :hazards))
             (held-back (plist-get result :held-back)))
        (when (or (and passes (not (string= before after)))
                  held-back)
          (push (list :unit (plist-get unit :index)
                      :line (plist-get unit :line)
                      :start (plist-get unit :start)
                      :end (plist-get unit :end)
                      :risk (whittle--transcript-report-risk
                             before after passes nil)
                      :passes passes
                      :hazards hazards
                      :held-back held-back
                      :before before
                      :after after
                      :full-after (plist-get result :full-after)
                      :full-passes (plist-get result :full-passes)
                      :full-hazards (plist-get result :full-hazards))
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

(defun whittle--transcript-report-pass-counts (entries &optional profile)
  "Return an alist of pass labels and counts for ENTRIES."
  (let ((pass-labels (whittle--transcript-report-pass-labels profile)))
    (mapcar (lambda (label)
              (cons label
                    (cl-count-if (lambda (entry)
                                   (member label (plist-get entry :passes)))
                                 entries)))
            pass-labels)))

(defun whittle--transcript-report-applied-entry-p (entry)
  "Return non-nil when ENTRY changes the source buffer."
  (and (plist-get entry :passes)
       (not (string= (plist-get entry :before)
                     (plist-get entry :after)))))

(defun whittle--transcript-report-applied-entries (entries)
  "Return ENTRIES whose safe cleanup is applied to the source."
  (seq-filter #'whittle--transcript-report-applied-entry-p entries))

(defun whittle--transcript-report-held-back-records (entries)
  "Return held-back hazardous edit records from ENTRIES with context."
  (let (records)
    (dolist (entry entries)
      (let ((entry-applied
             (whittle--transcript-report-applied-entry-p entry)))
        (dolist (record (plist-get entry :held-back))
          (push (append (list :entry-number (plist-get entry :number)
                              :line (plist-get entry :line)
                              :unit (plist-get entry :unit)
                              :entry-applied entry-applied)
                        record)
                records))))
    (cl-loop for record in (nreverse records)
             for number from 1
             collect (append (list :number number) record))))

(defun whittle--transcript-report-record-outcome (record)
  "Return the user-facing protection outcome for held-back RECORD."
  (cond
   ((eq (plist-get record :status) 'protected-overlap)
    "candidate skipped because protected-span replay could not be made offset-safe")
   ((plist-get record :entry-applied)
    "safe partial cleanup applied; hazardous span protected")
   (t
    "held back and untouched")))

(defun whittle--transcript-report-span-text (text span)
  "Return protected substring from TEXT using 1-based SPAN."
  (when span
    (let ((start (max 0 (1- (car span))))
          (end (min (length text) (1- (cdr span)))))
      (when (<= start end)
        (substring text start end)))))

(defun whittle--transcript-report-count-line (counts)
  "Return one org summary line for alist COUNTS."
  (if (cl-some (lambda (item) (> (cdr item) 0)) counts)
      (string-join
       (mapcar (lambda (item)
                 (format "%s %d" (car item) (cdr item)))
               counts)
       "; ")
    "none"))

(defun whittle--transcript-report-hazard-summary (entries)
  "Return one org summary line for hazard tags in ENTRIES."
  (let* ((records (whittle--transcript-report-held-back-records entries))
         (counts
          (mapcar (lambda (hazard)
                    (cons hazard
                          (cl-count-if
                           (lambda (record)
                             (member hazard (plist-get record :hazards)))
                           records)))
                  whittle--transcript-report-hazard-order)))
    (whittle--transcript-report-count-line counts)))

(defun whittle--transcript-report-hazard-line (entry)
  "Return an org hazard line for ENTRY, or nil."
  (when-let ((hazards (plist-get entry :hazards)))
    (format "- Held-back hazards :: %s\n" (string-join hazards ", "))))

(defun whittle--transcript-report-format-held-back (record &optional compact)
  "Return org text for a held-back hazardous edit RECORD.
When COMPACT is non-nil, quote only local changed-span excerpts."
  (let* ((before (plist-get record :before))
         (after (plist-get record :after))
         (excerpts (when compact (whittle--change-excerpts before after)))
         (before-text (if compact (car excerpts) before))
         (after-text (if compact (cdr excerpts) after))
         (protected-text
          (whittle--transcript-report-span-text
           before (plist-get record :protected-span))))
    (concat
     (format (concat "*** Held Back %d\n"
                     "- Approx line :: %d\n"
                     "- Unit :: %d\n"
                     "- Pass :: %s\n"
                     "- Hazards :: %s\n"
                     "- Outcome :: %s\n"
                     "- Safety gate :: this candidate edit was not applied; safe edits in the same unit may have landed\n")
             (plist-get record :number)
             (plist-get record :line)
             (plist-get record :unit)
             (plist-get record :pass)
             (string-join (plist-get record :hazards) ", ")
             (whittle--transcript-report-record-outcome record))
     (when protected-text
       (format "- Protected span :: %s\n" (string-trim protected-text)))
     (format (concat "\nCandidate before%s:\n"
                     "#+begin_quote\n%s\n#+end_quote\n\n"
                     "Candidate after%s:\n"
                     "#+begin_quote\n%s\n#+end_quote\n\n")
             (if compact " excerpt" "")
             before-text
             (if compact " excerpt" "")
             after-text))))

(defun whittle--transcript-report-format-entry (entry)
  "Return org text for a single report ENTRY."
  (concat
   (format (concat "*** Change %d\n"
                   "- Approx line :: %d\n"
                   "- Unit :: %d\n"
                   "- Risk :: %s\n"
                   "- Passes :: %s\n")
           (plist-get entry :number)
           (plist-get entry :line)
           (plist-get entry :unit)
           (plist-get entry :risk)
           (string-join (plist-get entry :passes) ", "))
   (or (whittle--transcript-report-hazard-line entry) "")
   (when (whittle--transcript-report-hazard-p entry)
     "- Safety gate :: hazardous candidate edit(s) held back; applied after below is the safe partial result\n")
   (format (concat "\nBefore:\n"
                   "#+begin_quote\n%s\n#+end_quote\n\n"
                   "After:\n"
                   "#+begin_quote\n%s\n#+end_quote\n\n")
           (plist-get entry :before)
           (plist-get entry :after))
   (when (and (whittle--transcript-report-hazard-p entry)
              (not (string= (plist-get entry :after)
                            (plist-get entry :full-after))))
     (format (concat "Full aggressive after, not applied:\n"
                     "#+begin_quote\n%s\n#+end_quote\n\n")
             (plist-get entry :full-after)))))

(defun whittle--common-prefix-length (left right)
  "Return the common prefix length of LEFT and RIGHT."
  (let ((limit (min (length left) (length right)))
        (index 0))
    (while (and (< index limit)
                (eq (aref left index) (aref right index)))
      (setq index (1+ index)))
    index))

(defun whittle--common-suffix-length (left right prefix-length)
  "Return common suffix length of LEFT and RIGHT after PREFIX-LENGTH."
  (let ((left-index (1- (length left)))
        (right-index (1- (length right)))
        (count 0))
    (while (and (>= left-index prefix-length)
                (>= right-index prefix-length)
                (eq (aref left left-index) (aref right right-index)))
      (setq count (1+ count)
            left-index (1- left-index)
            right-index (1- right-index)))
    count))

(defun whittle--snippet-around-change (text change-start change-end)
  "Return a compact TEXT snippet around CHANGE-START to CHANGE-END."
  (let* ((context whittle/report-clipboard-context-chars)
         (start (max 0 (- change-start context)))
         (end (min (length text) (+ change-end context)))
         (prefix (if (> start 0) "... " ""))
         (suffix (if (< end (length text)) " ..." ""))
         (snippet (substring text start end)))
    (string-trim (concat prefix snippet suffix))))

(defun whittle--change-excerpts (before after)
  "Return compact before/after excerpts around the changed span."
  (let* ((prefix-length (whittle--common-prefix-length before after))
         (suffix-length (whittle--common-suffix-length before after prefix-length))
         (before-change-end (max prefix-length (- (length before) suffix-length)))
         (after-change-end (max prefix-length (- (length after) suffix-length))))
    (cons (whittle--snippet-around-change before prefix-length before-change-end)
          (whittle--snippet-around-change after prefix-length after-change-end))))

(defun whittle--transcript-report-format-compact-entry (entry)
  "Return compact org text for a detailed clipboard ENTRY."
  (let* ((excerpts
          (whittle--change-excerpts
           (plist-get entry :before)
           (plist-get entry :after)))
         (before-excerpt (car excerpts))
         (after-excerpt (cdr excerpts)))
    (concat
     (format (concat "*** Change %d\n"
                     "- Approx line :: %d\n"
                     "- Risk :: %s\n"
                     "- Passes :: %s\n")
             (plist-get entry :number)
             (plist-get entry :line)
             (plist-get entry :risk)
             (string-join (plist-get entry :passes) ", "))
     (or (whittle--transcript-report-hazard-line entry) "")
     (when (whittle--transcript-report-hazard-p entry)
       "- Safety gate :: hazardous candidate edit(s) held back; excerpt below is the safe partial result\n")
     (format (concat "\nBefore excerpt:\n"
                     "#+begin_quote\n%s\n#+end_quote\n\n"
                     "After excerpt:\n"
                     "#+begin_quote\n%s\n#+end_quote\n\n")
             before-excerpt
             after-excerpt))))

(defun whittle--transcript-report-format-one-line-entry (entry)
  "Return one compact summary line for ENTRY."
  (format "- line %d, %s :: %s\n"
          (plist-get entry :line)
          (plist-get entry :risk)
          (string-join (plist-get entry :passes) ", ")))

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

(defun whittle--transcript-report-entries-with-risks (entries risks)
  "Return ENTRIES whose :risk is a member of RISKS."
  (seq-filter (lambda (entry)
                (member (plist-get entry :risk) risks))
              entries))

(defun whittle--transcript-report-format-compact
    (source full-report-file entries command-name profile applied)
  "Return a compact clipboard report for SOURCE and ENTRIES.
FULL-REPORT-FILE is the path to the complete before/after report."
  (let* ((applied-entries (whittle--transcript-report-applied-entries entries))
         (held-back-records (whittle--transcript-report-held-back-records entries))
         (risk-counts
          (whittle--transcript-report-count-by
           applied-entries :risk whittle--transcript-report-risk-order))
         (pass-counts (whittle--transcript-report-pass-counts applied-entries profile))
         (hazard-count (length held-back-records))
         (applied-count (length applied-entries))
         (title (if (eq profile 'conservative)
                    "Whittle Codex Review"
                  "Whittle Transcript Codex Review"))
         (shown-held-back-records
          (seq-take held-back-records whittle/report-clipboard-max-detailed-entries))
         (omitted-hazard-count
          (- hazard-count (length shown-held-back-records)))
         (detail-entries
          (whittle--transcript-report-entries-with-risks
           applied-entries whittle/report-clipboard-detail-risks))
         (shown-entries
          (seq-take detail-entries whittle/report-clipboard-max-detailed-entries))
         (omitted-detail-count (- (length detail-entries) (length shown-entries)))
         (low-entries
          (whittle--transcript-report-entries-with-risks applied-entries '("low"))))
    (concat
     (format "* %s\n" title)
     (format "- Source :: %s\n" source)
     (format "- Command :: %s\n" command-name)
     (format "- Mode :: %s\n"
             (if applied
                 "cleanup already applied; verify and repair"
               "dry run; source unchanged"))
     (format "- Changed units :: %d\n" applied-count)
     (when applied
       (format "- Applied units :: %d\n" applied-count))
     (when (> hazard-count 0)
       (format "- Held back :: %d hazardous edit%s for review.\n"
               hazard-count
               (if (= hazard-count 1) "" "s")))
     (format "- Risk counts :: %s\n"
             (whittle--transcript-report-count-line risk-counts))
     (format "- Pass counts :: %s\n" (whittle--transcript-report-count-line pass-counts))
     (format "- Hazard counts :: %s\n"
             (whittle--transcript-report-hazard-summary entries))
     (format "- Full report :: %s\n" full-report-file)
     (format "- Clipboard detail :: before/after excerpts for %s-risk changes, capped at %d.\n\n"
             (string-join whittle/report-clipboard-detail-risks ", ")
             whittle/report-clipboard-max-detailed-entries)
     "** Codex Instructions\n\n"
     (if applied
         "The source file has already been changed with safe partial cleanup. Review high/medium excerpts below. Hazardous candidate edits were held back and need manual review. If an applied change is wrong, edit the source file directly and repair only that mistake. Do not rewrite for style. Use the full report if needed.\n\n"
       "Source unchanged. Review high/medium excerpts below, then edit the source file directly if needed. Use the full report if needed.\n\n")
     "** Hazard-Tagged / Not Applied\n\n"
     (when (> omitted-hazard-count 0)
       (format "- Held-back edits omitted from clipboard :: %d; see full report.\n\n"
               omitted-hazard-count))
     (if shown-held-back-records
         (mapconcat (lambda (record)
                      (whittle--transcript-report-format-held-back record t))
                    shown-held-back-records "")
       "No held-back hazardous edits.\n\n")
     "** Applied High/Medium-Risk Changes\n\n"
     (when (> omitted-detail-count 0)
       (format "- Detailed entries omitted from clipboard :: %d; see full report.\n\n"
               omitted-detail-count))
     (if shown-entries
         (mapconcat #'whittle--transcript-report-format-compact-entry shown-entries "")
       "No applied high- or medium-risk changes. See summary/full report if needed.\n\n")
     "** Low-Risk Summary\n\n"
     (if low-entries
         (mapconcat #'whittle--transcript-report-format-one-line-entry low-entries "")
       "No low-risk changes.\n"))))

(defun whittle--transcript-report-format
    (source temp-file entries command-name profile applied)
  "Return the full cleanup review report for SOURCE and ENTRIES."
  (let* ((applied-entries (whittle--transcript-report-applied-entries entries))
         (held-back-records (whittle--transcript-report-held-back-records entries))
         (risk-counts
          (whittle--transcript-report-count-by
           applied-entries :risk whittle--transcript-report-risk-order))
         (pass-counts (whittle--transcript-report-pass-counts applied-entries profile))
         (hazard-count (length held-back-records))
         (applied-count (length applied-entries))
         (title (if (eq profile 'conservative)
                    "Whittle Review Report"
                  "Whittle Transcript Review Report")))
    (concat
     (format "* %s\n" title)
     (format "- Source :: %s\n" source)
     (format "- Generated :: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]"))
     (format "- Command :: %s\n" command-name)
     (if applied
         "- Mode :: Cleanup has already been applied to the source buffer; this report is for Codex verification and repair.\n"
       "- Mode :: Dry-run report only; the source buffer is unchanged.\n")
     "- Unit boundary :: Blank-line-delimited paragraph units; org headings are boundaries and are excluded from cleanup.\n"
     "- Source edit target :: Codex should edit the source file above directly; this report is not a replacement transcript.\n"
     "- Boundary caveat :: Unit-level dry runs intentionally prevent line-joining across paragraph or heading boundaries, so boundary-sensitive output may differ from a whole-buffer edit.\n"
     (format "- Temp file :: %s\n\n" temp-file)
     "** Instructions for Codex\n\n"
     (if applied
         "The source file has already been changed by the command above. Review each before/after pair. If an after version is wrong, edit the source file directly to repair that specific mistake. Do not copy this report wholesale into the transcript.\n\n"
       "Review each before/after pair, apply only the safe changes directly to the source file, and repair any broken changes instead of copying the report wholesale.\n\n")
     "** Summary\n\n"
     (format "- Changed units :: %d\n" applied-count)
     (when applied
       (format "- Applied units :: %d\n" applied-count))
     (when (> hazard-count 0)
       (format "- Held back :: %d hazardous edit%s for review.\n"
               hazard-count
               (if (= hazard-count 1) "" "s")))
     (format "- Risk counts :: %s\n"
             (whittle--transcript-report-count-line risk-counts))
     (format "- Pass counts :: %s\n"
             (whittle--transcript-report-count-line pass-counts))
     (format "- Hazard counts :: %s\n\n"
             (whittle--transcript-report-hazard-summary entries))
     "** Hazard-Tagged / Not Applied\n\n"
     (if held-back-records
         (mapconcat #'whittle--transcript-report-format-held-back held-back-records "")
       "No held-back hazardous edits.\n\n")
     (mapconcat (lambda (risk)
                  (whittle--transcript-report-format-risk-section
                   risk applied-entries))
                whittle--transcript-report-risk-order
                ""))))

(defun whittle--transcript-report-generate
    (beg end &optional temp-file profile command-name applied)
  "Generate a transcript review report for BEG to END.
Return a plist with :report, :temp-file, and :entries."
  (let* ((target-command (or command-name "whittle-transcript-report"))
         (target-temp-file
          (or temp-file (whittle--transcript-report-temp-file target-command)))
         (units (whittle--transcript-report-units beg end))
         (entries (whittle--transcript-report-build-entries units profile))
         (source (whittle--transcript-report-source-name))
         (report
          (whittle--transcript-report-format
           source target-temp-file entries target-command profile applied)))
    (list :report report
          :temp-file target-temp-file
          :source source
          :entries entries)))

(defun whittle--transcript-report-apply-entries (entries)
  "Apply report ENTRIES to the current buffer."
  (save-excursion
    (dolist (entry (sort (copy-sequence entries)
                         (lambda (a b)
                           (> (plist-get a :start) (plist-get b :start)))))
      (let ((start (plist-get entry :start))
            (end (plist-get entry :end))
            (after (plist-get entry :after)))
        (goto-char start)
        (delete-region start end)
        (insert after)))))

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

(defun whittle-apply (&optional beg end)
  "Apply conservative cleanup to the region or buffer without copying a report."
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

(defun whittle-transcript-apply (&optional beg end)
  "Apply aggressive transcript cleanup to the region or buffer."
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

(defun whittle--run-cleanup-with-report (beg end profile command-name)
  "Apply PROFILE cleanup from BEG to END and copy COMMAND-NAME report."
  (let* ((result
          (whittle--transcript-report-generate
           beg end nil profile command-name t))
         (entries (plist-get result :entries))
         (hazard-count
          (length (whittle--transcript-report-held-back-records entries)))
         (applied-count
          (length (whittle--transcript-report-applied-entries entries)))
         (full-report (plist-get result :report))
         (temp-file (plist-get result :temp-file))
         (source (plist-get result :source))
         (clipboard-report
          (whittle--transcript-report-format-compact
           source temp-file entries command-name profile t)))
    (whittle--transcript-report-apply-entries entries)
    (whittle--write-string-to-file full-report temp-file)
    (whittle--copy-string-to-pbcopy clipboard-report)
    (message
     "%s changed %d units; held back %d hazardous edit%s for review; compact Codex report copied to clipboard; full report saved to %s"
     command-name
     applied-count
     hazard-count
     (if (= hazard-count 1) "" "s")
     temp-file)
    (append result (list :clipboard-report clipboard-report))))

(defun whittle (&optional beg end)
  "Apply conservative cleanup and copy a Codex review report.
The buffer is changed first. The clipboard receives an org report
with the source path, Codex instructions, and before/after pairs."
  (interactive (whittle--interactive-bounds))
  (whittle--run-cleanup-with-report beg end 'conservative "whittle"))

(defun whittle--transcript-report-command (beg end &optional profile command-name)
  "Create, save, and copy a dry-run review report from BEG to END.
The source buffer is not edited. The report is copied to the
clipboard and saved under `whittle/transcript-report-directory'."
  (let* ((target-profile (or profile 'transcript))
         (target-command (or command-name "whittle-transcript-report"))
         (result
          (whittle--transcript-report-generate
           beg end nil target-profile target-command nil))
         (full-report (plist-get result :report))
         (temp-file (plist-get result :temp-file))
         (source (plist-get result :source))
         (clipboard-report
          (whittle--transcript-report-format-compact
           source temp-file (plist-get result :entries)
           target-command target-profile nil)))
    (whittle--write-string-to-file full-report temp-file)
    (whittle--copy-string-to-pbcopy clipboard-report)
    (message "Compact Whittle report copied to clipboard; full report saved to %s" temp-file)
    (append result (list :clipboard-report clipboard-report))))

(defun whittle-transcript (&optional beg end)
  "Apply transcript cleanup and copy a Codex review report.
The buffer is changed first. The clipboard receives an org report
with the source path, Codex instructions, and before/after pairs."
  (interactive (whittle--interactive-bounds))
  (whittle--run-cleanup-with-report beg end 'transcript "whittle-transcript"))

(defun whittle-transcript-report (&optional beg end)
  "Create a dry-run transcript cleanup report and copy it to the clipboard.
This does not edit the source buffer. Use `whittle-transcript' for
the normal apply-plus-report workflow."
  (interactive (whittle--interactive-bounds))
  (whittle--transcript-report-command beg end 'transcript "whittle-transcript-report"))

(provide 'whittle)
;;; whittle.el ends here
