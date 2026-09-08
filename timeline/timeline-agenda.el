;;; timeline-agenda.el --- Upcoming-events view for timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; A read-only "what's next" agenda over the Markdown timeline: lists the
;; upcoming dated entries on or after today, sorted chronologically, and lets
;; you jump from any line back to that date in the Calendar.

;;; Code:

(require 'seq)
(require 'org)
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
      (save-restriction
        (widen)
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
                (let ((text (string-trim (match-string-no-properties 1))))
                  (unless (string-empty-p text)
                    (push text bullets))))
              (forward-line 1))
            (push (list :abs abs :date date
                        :string (my-calendar--diary-format-date month day year)
                        :bullets (nreverse bullets))
                  entries))))))
    (nreverse entries)))

(defvar-local my-timeline--data-cache nil
  "Parsed diary text, keyed by this source buffer's character modification tick.")

(defun my-timeline--data ()
  "Return cached entries and paired ranges from the live, possibly unsaved diary."
  (with-current-buffer (find-file-noselect diary-file)
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick (car my-timeline--data-cache))
        (let ((entries (my-timeline--collect-entries)))
          (setq my-timeline--data-cache
                (list tick entries (my-timeline--collect-ranges entries)))))
      (cdr my-timeline--data-cache))))

(defun my-timeline--view-buffer (name mode)
  "Get NAME, initializing its read-only Org MODE only once.
Keep the writing layout's automatic Olivetti toggle out of generated views."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (eq major-mode mode) (funcall mode))
      (setq-local disable-olivetti-auto-toggle t)
      (when (bound-and-true-p olivetti-mode) (olivetti-mode -1))
      (setq-local left-margin-width 0)
      (setq-local right-margin-width 0)
      (setq-local buffer-read-only t)
      ;; The near-global key minor mode can shadow Org view keys.  Override
      ;; only this view's explicit commands, leaving its other shortcuts alone.
      (when (boundp 'key-minor-mode-map)
        (let ((override (make-sparse-keymap)))
          (set-keymap-parent override key-minor-mode-map)
          (dolist (key '("RET" "e" "i" "o" "g" "q" "<" ">"
                         "C-v" "M-v" "<next>" "<prior>"))
            (let ((command (lookup-key (current-local-map) (kbd key))))
              (when (or (eq command 'quit-window)
                        (and (symbolp command)
                             (string-prefix-p "my-timeline-" (symbol-name command))))
                (define-key override (kbd key) command))))
          (setq-local minor-mode-overriding-map-alist
                      (cons (cons 'key-minor-mode override)
                            (assq-delete-all 'key-minor-mode
                                             (copy-alist minor-mode-overriding-map-alist))))))
      (buffer-disable-undo))
    buffer))

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
  (let* ((data (my-timeline--data))
         (entries (car data))
         (ranges (cadr data))
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
          (insert (format "** %s\n" (plist-get item :title)))
          (insert (format "%s\n\n"
                          (my-timeline--range-status item today-abs)))
          (insert (format "- %s\n\n" (plist-get item :description))))
      (let* ((date (plist-get item :date))
             (delta (- (plist-get item :sort-abs) today-abs)))
        (insert (format "** %s %s  (%s)\n\n"
                        (calendar-day-name date)
                        (my-calendar--describe-date date)
                        (my-timeline--relative-day-string delta)))
        (dolist (bullet (plist-get item :bullets))
          (insert (format "- %s\n" bullet)))
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
      (insert "* " title "\n\n")
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

;; Set explicitly so reloading also replaces the former special-mode parent.
(set-keymap-parent my-timeline-upcoming-mode-map org-mode-map)

(define-derived-mode my-timeline-upcoming-mode org-mode "Timeline-Upcoming"
  "Read-only Org mode for the upcoming-events listing."
  (org-show-all)
  (setq buffer-read-only t))

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
         (buf (my-timeline--view-buffer "*Timeline Upcoming*"
                                        #'my-timeline-upcoming-mode)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Upcoming timeline events — as of %s\n\n"
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
      (org-show-all)
      (set-buffer-modified-p nil)
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
    (my-timeline-open)
    (my-timeline-goto-date date)
    (when (fboundp 'my-calendar-edit-diary-entry)
      (my-calendar-edit-diary-entry))))

(defvar-local my-timeline-preview--date nil)
(defvar-local my-timeline-preview--before 7)
(defvar-local my-timeline-preview--after 21)
(defvar-local my-timeline-preview--render-key nil)
(defvar-local my-timeline-preview--selected-position nil)

(defun my-timeline--date-label (date)
  "Describe DATE with its weekday and absolute calendar date."
  (format "%s, %s" (calendar-day-name date) (my-calendar--describe-date date)))

(defun my-timeline--preview-items (selected before after)
  "Return items BEFORE through AFTER days around SELECTED's absolute day.
Ranges active on SELECTED move into its section, so ongoing events count
as selected-day content even when the diary has no entry for that date."
  (let (items)
    (dolist (item (my-timeline--agenda-items (- selected before)))
      (when (<= (plist-get item :sort-abs) (+ selected after))
        (when (and (eq (plist-get item :kind) 'range)
                   (<= (plist-get item :start-abs) selected)
                   (>= (plist-get item :end-abs) selected))
          (setq item (plist-put (copy-sequence item) :sort-abs selected)))
        (push item items)))
    (sort (nreverse items)
          (lambda (a b) (< (plist-get a :sort-abs) (plist-get b :sort-abs))))))

(defun my-timeline--insert-preview-item (item selected &optional omit-date-heading)
  "Insert ITEM with absolute dates and a status relative to SELECTED.
OMIT-DATE-HEADING avoids repeating the selected day's section heading."
  (let ((start (point)))
    (if (eq (plist-get item :kind) 'range)
        (progn
          (insert "** " (plist-get item :title) "\n")
          (insert (my-timeline--date-label (plist-get item :start-date))
                  " — " (my-timeline--date-label (plist-get item :end-date))
                  (cond ((< (plist-get item :end-abs) selected) " · ended")
                        ((< (plist-get item :start-abs) selected)
                         (if (= (plist-get item :end-abs) selected)
                             " · last day on selected date" " · ongoing on selected date"))
                        ((= (plist-get item :start-abs) selected) " · starts on selected date")
                        (t ""))
                  "\n\n- " (plist-get item :description) "\n\n"))
      (unless omit-date-heading
        (insert "** " (my-timeline--date-label (plist-get item :date)) "\n\n"))
      (dolist (bullet (plist-get item :bullets)) (insert "- " bullet "\n"))
      (insert "\n"))
    (put-text-property start (point) 'my-timeline-date (plist-get item :date))))

(defun my-timeline--render-preview (date &optional preserve-position)
  "Render DATE in the current itinerary buffer without touching the diary.
PRESERVE-POSITION keeps point and scroll positions when extending the range."
  (let* ((selected (calendar-absolute-from-gregorian date))
         (items (my-timeline--preview-items selected my-timeline-preview--before
                                          my-timeline-preview--after))
         (old-point (point))
         (window-starts (mapcar (lambda (w) (cons w (window-start w)))
                               (get-buffer-window-list (current-buffer)))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "#+TITLE: Timeline itinerary\n\n"
              "RET/e edit · i add · </> extend earlier/later · g refresh · q return to grid\n\n")
      (when (seq-some (lambda (item) (< (plist-get item :sort-abs) selected)) items)
        (insert "* Earlier\n\n"))
      (dolist (item items)
        (when (< (plist-get item :sort-abs) selected)
          (my-timeline--insert-preview-item item selected)))
      (setq my-timeline-preview--selected-position (point))
      (let ((start (point))
            (selected-items (seq-filter (lambda (item)
                                          (= (plist-get item :sort-abs) selected)) items)))
        (insert "* Selected — " (my-timeline--date-label date) "\n\n")
        (put-text-property start (point) 'my-timeline-date date)
        (if selected-items
            (dolist (item selected-items) (my-timeline--insert-preview-item item selected t))
          (insert "No entries. Press i to add one.\n")
          (when-let ((next (car (my-timeline--agenda-items (1+ selected)))))
            (let ((next-start (point)))
              (insert "Next: " (my-timeline--date-label (plist-get next :date)) " — "
                      (if (eq (plist-get next :kind) 'range)
                          (plist-get next :title) (car (plist-get next :bullets))) "\n")
              (put-text-property next-start (point) 'my-timeline-date (plist-get next :date))))
          (insert "\n")))
      (insert "* Later\n\n")
      (dolist (item items)
        (when (> (plist-get item :sort-abs) selected)
          (my-timeline--insert-preview-item item selected)))
      (insert "Press > for later dates or < for earlier dates.\n")
      (org-show-all)
      (set-buffer-modified-p nil))
    (goto-char (if preserve-position (min old-point (point-max))
                 my-timeline-preview--selected-position))
    (dolist (win (get-buffer-window-list (current-buffer)))
      (set-window-point win (point))
      (if preserve-position
          (set-window-start win (or (cdr (assq win window-starts)) (point-min)) t)
        ;; Keep earlier context visible without letting it push the selection
        ;; below the fold when the previous week is unusually busy.
        (set-window-start win (save-excursion (forward-line -8) (point)) t)))))

(define-derived-mode my-timeline-help-mode org-mode "Timeline-Help"
  "Read-only Org guide to Timeline's current keybindings."
  (setq buffer-read-only t))
(define-key my-timeline-help-mode-map (kbd "q") #'quit-window)

(define-derived-mode my-timeline-preview-mode org-mode "Timeline-Itinerary"
  "Read-only Org itinerary around the calendar's selected date."
  (setq buffer-read-only t))

(defun my-timeline-preview-show (&optional date force)
  "Show DATE's itinerary in the session's lower window.
FORCE rebuilds even if neither the selected date nor diary text has changed.
An active diary edit always takes precedence over automatic or manual preview."
  (interactive)
  (let* ((session (my-timeline--session))
         (date (or date (calendar-cursor-to-date t))))
    (cond
     ((plist-get session :editing)
      (when (called-interactively-p 'any)
        (message "Finish the diary view with C-c C-c to resume the itinerary")))
     ((not session)
      (my-timeline-open)
      (calendar-goto-date date)
      (my-timeline-preview-show date force))
     (t
      (let* ((buffer (my-timeline--view-buffer "*Timeline Itinerary*" #'my-timeline-preview-mode))
             (window (plist-get session :content-window))
             (source (find-file-noselect diary-file))
             (key (list date source (with-current-buffer source (buffer-chars-modified-tick)))))
        (unless (window-live-p window)
          (setq window (split-window (plist-get session :calendar-window) 10 'below))
          (my-timeline--session-put :content-window window))
        (set-window-buffer window buffer)
        (set-window-margins window 0 0)
        (with-current-buffer buffer
          (unless (equal date my-timeline-preview--date)
            (setq my-timeline-preview--before 7 my-timeline-preview--after 21))
          (setq my-timeline-preview--date date)
          (when (or force (not (equal key my-timeline-preview--render-key)))
            (my-timeline--render-preview date)
            (setq my-timeline-preview--render-key key))))))))

(defun my-timeline-preview-refresh ()
  "Refresh the current itinerary from the live diary buffer."
  (interactive)
  (my-timeline-preview-show my-timeline-preview--date t))

(defun my-timeline-preview-extend (earlier)
  "Extend the visible itinerary by four weeks, EARLIER or later."
  (if earlier
      (cl-incf my-timeline-preview--before 28)
    (cl-incf my-timeline-preview--after 28))
  ;; Extending the beginning inserts text ahead of point.  Keep the same
  ;; logical date visible, rather than the old numeric buffer position.
  (let* ((date (get-text-property (point) 'my-timeline-date))
         (selected my-timeline-preview--date))
    (my-timeline--render-preview selected (not earlier))
    (when (and earlier date)
      (goto-char (or (text-property-any (point-min) (point-max) 'my-timeline-date date)
                     my-timeline-preview--selected-position)))))

(defun my-timeline-preview-earlier ()
  "Show four more weeks before this itinerary."
  (interactive) (my-timeline-preview-extend t))

(defun my-timeline-preview-later ()
  "Show four more weeks after this itinerary."
  (interactive) (my-timeline-preview-extend nil))

(defun my-timeline-preview-scroll-up (&optional arg)
  "Scroll later, extending the itinerary when the end is reached."
  (interactive "P")
  (when (pos-visible-in-window-p (point-max)) (my-timeline-preview-later))
  (scroll-up-command arg))

(defun my-timeline-preview-scroll-down (&optional arg)
  "Scroll earlier, extending the itinerary when the beginning is reached."
  (interactive "P")
  (when (= (window-start) (point-min)) (my-timeline-preview-earlier))
  (scroll-down-command arg))

(defun my-timeline-preview-return ()
  "Return focus to the calendar grid."
  (interactive) (my-calendar-focus-calendar-window))

(defun my-timeline-preview-visit (&optional insert)
  "Edit the date at point, or add an entry when INSERT is non-nil."
  (interactive)
  (let ((date (or (get-text-property (point) 'my-timeline-date)
                  my-timeline-preview--date)))
    (my-calendar-focus-calendar-window)
    (my-timeline-goto-date date)
    (if insert (my-calendar-insert-diary-entry date)
      (my-calendar-edit-diary-entry))))

(defun my-timeline-preview-insert ()
  "Add an entry to the date at point."
  (interactive) (my-timeline-preview-visit t))

(dolist (binding '(("RET" . my-timeline-preview-visit)
                   ("e" . my-timeline-preview-visit)
                   ("i" . my-timeline-preview-insert)
                   ("g" . my-timeline-preview-refresh)
                   ("q" . my-timeline-preview-return)
                   ("<" . my-timeline-preview-earlier)
                   (">" . my-timeline-preview-later)
                   ("C-v" . my-timeline-preview-scroll-up)
                   ("M-v" . my-timeline-preview-scroll-down)
                   ("<next>" . my-timeline-preview-scroll-up)
                   ("<prior>" . my-timeline-preview-scroll-down)))
  (define-key my-timeline-preview-mode-map (kbd (car binding)) (cdr binding)))

(provide 'timeline-agenda)

;;; timeline-agenda.el ends here
