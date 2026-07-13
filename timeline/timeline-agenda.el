;;; timeline-agenda.el --- Upcoming-events view for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; A read-only "what's next" agenda over the Markdown timeline: lists the
;; upcoming dated entries on or after today, sorted chronologically, and lets
;; you jump from any line back to that date in the Calendar.

;;; Code:

(require 'seq)
(require 'timeline-core)
(require 'timeline-diary)

(defvar my-timeline-upcoming-count 15
  "Default number of upcoming diary entries shown by `my-timeline-upcoming'.")

(defvar-local my-timeline-upcoming--current-count nil
  "Count used to build the current upcoming-events buffer.")

(defconst my-timeline--last-day-regexp
  "\\`Last day of[[:space:]]+\\(.+\\)\\'"
  "Regexp matching the final boundary marker for a multi-day event.")

(defun my-timeline--collect-entries ()
  "Scan `diary-file' and return every dated entry.
Each element is a plist with :abs (absolute day number), :date
\(list MONTH DAY YEAR), :string (\"M/D/YYYY\") and :bullets (list of
non-empty bullet strings)."
  (let ((buffer (find-file-noselect diary-file))
        entries)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" nil t)
          (let* ((month (string-to-number (match-string 1)))
                 (day   (string-to-number (match-string 2)))
                 (year  (string-to-number (match-string 3)))
                 (date  (list month day year))
                 (abs   (calendar-absolute-from-gregorian date))
                 (bullets nil))
            (forward-line 1)
            (while (and (not (eobp))
                        (not (looking-at "^[0-9]+/[0-9]+/[0-9]+$"))
                        (not (looking-at "^#")))
              (when (looking-at "^  - ?\\(.*\\)$")
                (let ((text (string-trim (match-string 1))))
                  (unless (string-empty-p text)
                    (push text bullets))))
              (forward-line 1))
            (push (list :abs abs :date date
                        :string (my-calendar--diary-format-date month day year)
                        :bullets (nreverse bullets))
                  entries)))))
    (nreverse entries)))

(defun my-timeline--upcoming (today-abs &optional count)
  "Return entries on or after TODAY-ABS, sorted ascending.
When COUNT is non-nil, return at most that many."
  (let ((future (seq-filter (lambda (e)
                              (and (>= (plist-get e :abs) today-abs)
                                   (plist-get e :bullets)))
                            (my-timeline--collect-entries))))
    (setq future (sort future (lambda (a b) (< (plist-get a :abs) (plist-get b :abs)))))
    (if count (seq-take future count) future)))

(defun my-timeline--relative-day-string (delta)
  "Return a human phrase for DELTA days from today."
  (cond ((= delta 0) "today")
        ((= delta 1) "tomorrow")
        (t (format "in %d days" delta))))

(defun my-timeline--last-day-title (bullet)
  "Return the event title from a final-boundary BULLET, or nil."
  (let ((case-fold-search t))
    (when (string-match my-timeline--last-day-regexp bullet)
      (string-trim (match-string 1 bullet)))))

(defun my-timeline--find-range-start (entries end-abs title used-starts)
  "Find TITLE's nearest unused start before END-ABS in ENTRIES.
USED-STARTS contains boundary keys already paired with an end marker."
  (let ((case-fold-search t)
        found)
    (dolist (entry entries)
      (when (< (plist-get entry :abs) end-abs)
        (dolist (bullet (plist-get entry :bullets))
          (let ((key (list (plist-get entry :abs) bullet)))
            (when (and (not (my-timeline--last-day-title bullet))
                       (string-match-p (regexp-quote title) bullet)
                       (not (member key used-starts)))
              ;; ENTRIES are chronological, so the last match is the nearest.
              (setq found (list :entry entry :bullet bullet :key key)))))))
    found))

(defun my-timeline--collect-ranges (entries)
  "Pair first- and last-day boundary bullets found in ENTRIES."
  (let ((sorted (sort (copy-sequence entries)
                      (lambda (a b)
                        (< (plist-get a :abs) (plist-get b :abs)))))
        used-starts
        ranges)
    (dolist (end-entry sorted)
      (dolist (end-bullet (plist-get end-entry :bullets))
        (when-let* ((title (my-timeline--last-day-title end-bullet))
                    (start (my-timeline--find-range-start
                            sorted (plist-get end-entry :abs) title used-starts)))
          (push (plist-get start :key) used-starts)
          (push (list :title title
                      :start-entry (plist-get start :entry)
                      :start-bullet (plist-get start :bullet)
                      :end-entry end-entry
                      :end-bullet end-bullet)
                ranges))))
    (nreverse ranges)))

(defun my-timeline--range-boundary-p (entry bullet ranges)
  "Return non-nil when BULLET in ENTRY is a boundary paired in RANGES."
  (let ((abs (plist-get entry :abs)))
    (seq-some
     (lambda (range)
       (or (and (= abs (plist-get (plist-get range :start-entry) :abs))
                (equal bullet (plist-get range :start-bullet)))
           (and (= abs (plist-get (plist-get range :end-entry) :abs))
                (equal bullet (plist-get range :end-bullet)))))
     ranges)))

(defun my-timeline--range-agenda-item (range today-abs)
  "Convert RANGE to an agenda item relative to TODAY-ABS.
Return nil after the range's final day."
  (let* ((start-entry (plist-get range :start-entry))
         (end-entry (plist-get range :end-entry))
         (start-abs (plist-get start-entry :abs))
         (end-abs (plist-get end-entry :abs)))
    (when (>= end-abs today-abs)
      (list :kind 'range
            :sort-abs (max today-abs start-abs)
            :date (plist-get start-entry :date)
            :title (plist-get range :title)
            :description (plist-get range :start-bullet)
            :start-abs start-abs
            :end-abs end-abs
            :start-date (plist-get start-entry :date)
            :end-date (plist-get end-entry :date)))))

(defun my-timeline--agenda-items (today-abs &optional count)
  "Return range-aware agenda items on or after TODAY-ABS.
Paired first/last boundaries become one item.  Active ranges sort under
today, while unpaired boundary markers remain ordinary entries.  When
COUNT is non-nil, return at most that many items."
  (let* ((entries (my-timeline--collect-entries))
         (ranges (my-timeline--collect-ranges entries))
         items)
    (dolist (range ranges)
      (when-let ((item (my-timeline--range-agenda-item range today-abs)))
        (push item items)))
    (dolist (entry entries)
      (when (>= (plist-get entry :abs) today-abs)
        (let ((bullets
               (seq-remove
                (lambda (bullet)
                  (my-timeline--range-boundary-p entry bullet ranges))
                (plist-get entry :bullets))))
          (when bullets
            (push (list :kind 'entry
                        :sort-abs (plist-get entry :abs)
                        :date (plist-get entry :date)
                        :bullets bullets)
                  items)))))
    (setq items
          (sort items
                (lambda (a b)
                  (let ((a-abs (plist-get a :sort-abs))
                        (b-abs (plist-get b :sort-abs)))
                    (if (= a-abs b-abs)
                        (and (eq (plist-get a :kind) 'range)
                             (not (eq (plist-get b :kind) 'range)))
                      (< a-abs b-abs))))))
    (if count (seq-take items count) items)))

(defun my-timeline--range-status (item today-abs)
  "Return a human status line for range ITEM relative to TODAY-ABS."
  (let ((start-delta (- (plist-get item :start-abs) today-abs))
        (end-delta (- (plist-get item :end-abs) today-abs)))
    (cond
     ((= end-delta 0) "Last day today")
     ((< start-delta 0)
      (format "Ongoing · ends %s"
              (my-timeline--relative-day-string end-delta)))
     ((= start-delta 0)
      (format "Starts today · ends %s"
              (my-timeline--relative-day-string end-delta)))
     (t
      (format "Starts %s · ends %s"
              (my-timeline--relative-day-string start-delta)
              (my-timeline--relative-day-string end-delta))))))

(defun my-timeline--week-end-absolute (today-abs)
  "Return the absolute date of Sunday in TODAY-ABS's Monday-first week."
  (let* ((today (calendar-gregorian-from-absolute today-abs))
         (weekday (calendar-day-of-week today)))
    (+ today-abs (if (= weekday 0) 0 (- 7 weekday)))))

(defun my-timeline--agenda-section (item today-abs week-end-abs)
  "Return ITEM's section symbol relative to TODAY-ABS and WEEK-END-ABS."
  (let ((abs (plist-get item :sort-abs)))
    (cond ((= abs today-abs) 'today)
          ((<= abs week-end-abs) 'this-week)
          (t 'later))))

(defun my-timeline--insert-agenda-item (item today-abs)
  "Insert ITEM into the current agenda buffer relative to TODAY-ABS."
  (let ((start (point)))
    (if (eq (plist-get item :kind) 'range)
        (progn
          (insert (format "%s\n" (plist-get item :title)))
          (insert (format "    %s\n"
                          (my-timeline--range-status item today-abs)))
          (insert (format "    • %s\n\n" (plist-get item :description))))
      (let* ((date (plist-get item :date))
             (delta (- (plist-get item :sort-abs) today-abs)))
        (insert (format "%s %s  (%s)\n"
                        (calendar-day-name date)
                        (my-calendar--describe-date date)
                        (my-timeline--relative-day-string delta)))
        (dolist (bullet (plist-get item :bullets))
          (insert (format "    • %s\n" bullet)))
        (insert "\n")))
    (put-text-property start (point) 'my-timeline-date
                       (plist-get item :date))))

(defun my-timeline--insert-agenda-section (title section items today-abs week-end-abs)
  "Insert TITLE and ITEMS belonging to SECTION into the current buffer."
  (let ((section-items
         (seq-filter
          (lambda (item)
            (eq (my-timeline--agenda-section item today-abs week-end-abs)
                section))
          items)))
    (when section-items
      (insert title "\n" (make-string (length title) ?─) "\n\n")
      (dolist (item section-items)
        (my-timeline--insert-agenda-item item today-abs)))))

(defvar my-timeline-upcoming-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my-timeline-upcoming-visit)
    (define-key map (kbd "o")   #'my-timeline-upcoming-visit)
    (define-key map (kbd "g")   #'my-timeline-upcoming-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `my-timeline-upcoming-mode'.")

(define-derived-mode my-timeline-upcoming-mode special-mode "Timeline-Upcoming"
  "Major mode for the upcoming-events listing.")

(defun my-timeline-upcoming (&optional count)
  "Show upcoming timeline entries in the *Timeline Upcoming* buffer.
COUNT defaults to `my-timeline-upcoming-count'.  A numeric prefix
sets the count; a plain \\[universal-argument] shows all future entries."
  (interactive
   (list (cond ((consp current-prefix-arg) nil)
               (current-prefix-arg (prefix-numeric-value current-prefix-arg))
               (t my-timeline-upcoming-count))))
  (let* ((today (calendar-current-date))
         (today-abs (calendar-absolute-from-gregorian today))
         (week-end-abs (my-timeline--week-end-absolute today-abs))
         (items (my-timeline--agenda-items today-abs count))
         (buf (get-buffer-create "*Timeline Upcoming*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Upcoming timeline events — as of %s\n"
                        (my-calendar--describe-date today)))
        (insert "RET/o jump to entry or range start · g refresh · q quit\n\n")
        (if (null items)
            (insert "No upcoming entries found.\n")
          (my-timeline--insert-agenda-section
           "Today" 'today items today-abs week-end-abs)
          (my-timeline--insert-agenda-section
           "This week" 'this-week items today-abs week-end-abs)
          (my-timeline--insert-agenda-section
           "Later" 'later items today-abs week-end-abs))
        (goto-char (point-min)))
      (my-timeline-upcoming-mode)
      (setq-local my-timeline-upcoming--current-count count))
    (pop-to-buffer buf)))

(defun my-timeline-upcoming-refresh ()
  "Rebuild the upcoming-events listing."
  (interactive)
  (my-timeline-upcoming my-timeline-upcoming--current-count))

(defun my-timeline-upcoming-visit ()
  "Open the entry or range start on the current line and edit it."
  (interactive)
  (let ((date (get-text-property (point) 'my-timeline-date)))
    (unless date
      (user-error "No timeline entry on this line"))
    (calendar)
    (calendar-goto-date date)
    (when (fboundp 'my-calendar-edit-diary-entry)
      (my-calendar-edit-diary-entry))))

(provide 'timeline-agenda)

;;; timeline-agenda.el ends here
