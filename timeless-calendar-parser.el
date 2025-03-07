;; Timeless Diary Parser for Emacs Calendar
;;
;; Purpose:
;; This Emacs Lisp function extends the default Emacs diary parser to handle
;; more flexible diary entry formats, including entries that start with hyphens,
;; and allows organizing diary entries under monthly or yearly headers.
;;
;; Supported Formats:
;; 1. Standard numeric dates: MM/DD or MM/DD/YYYY
;; 2. Named month dates: March 15 or March 15, 2025
;; 3. Weekday names (e.g., Monday)
;; 4. Wildcard dates: */25 (monthly on the 25th), * 15 (every month on the 15th)
;; 5. Entries starting with hyphens or bullet points (-, •)
;; 6. Headers for organization (e.g., * 2025, ** March 2025)
;;
;; Example diary file:
;;
;; * 2025
;; ** March 2025
;; 03/12/2025
;; - Doctor's appointment at 10am
;; - Lunch with Sarah at noon
;;
;; March 14, 2025
;; - Visit art museum
;; - Dinner with Alex
;;
;; ** April 2025
;; 04/01/2025
;; - Move to New York City
;;
;; Recurring entries:
;; * 15 Payday
;; Fri Weekly meeting with team
;; 12/25 Christmas
;;
;; Usage:
;; Add the following line to your Emacs init file:
;; (add-hook 'diary-list-entries-hook 'timeless-diary-flexible-parser)

(defun timeless-diary-flexible-parser (date)
  "Custom parser to handle flexible diary entry formats."
  (let ((date-pattern (concat "^\\(" (calendar-date-string date) "\\|"
                              (calendar-date-string date t) "\\|"
                              (format-time-string "%B %e, %Y" (encode-time 0 0 0
                                                                           (extract-calendar-day date)
                                                                           (extract-calendar-month date)
                                                                           (extract-calendar-year date)))
                              "\\|"
                              (calendar-day-name date t)
                              "\\)")))
    (goto-char (point-min))
    (while (re-search-forward date-pattern nil t)
      (let ((entry (buffer-substring-no-properties
                    (line-beginning-position 2)
                    (progn (forward-line 1)
                           (while (and (not (eobp))
                                       (looking-at "^[ \t]*[-•]"))
                             (forward-line 1))
                           (point)))))
        (diary-add-to-list date (string-trim entry))))))


(add-hook 'diary-list-entries-hook 'timeless-diary-flexible-parser)
