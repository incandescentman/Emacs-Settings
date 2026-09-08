;;; timeline.el --- Calendar timeline helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Aggregates the modular timeline configuration.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'timeline-core)
(require 'timeline-calendar)
(require 'timeline-diary)
(require 'timeline-cleanup)
(require 'timeline-search)
(require 'timeline-agenda)
(require 'timeline-tests)

(with-eval-after-load 'calendar
  ;; Editing / viewing bindings
  (define-key calendar-mode-map (kbd "RET") #'my-calendar-edit-diary-entry)
  (define-key calendar-mode-map (kbd "e")   #'my-calendar-edit-diary-entry)
  (define-key calendar-mode-map (kbd "v")   #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "o")   #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "SPC") #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "O")   #'my-calendar-show-fancy-diary-listing)

  ;; Natural dates, following, and deliberate-jump history.
  (define-key calendar-mode-map (kbd "j") #'my-timeline-jump)
  (define-key calendar-mode-map (kbd "g d") #'my-timeline-jump)
  (define-key calendar-mode-map (kbd "V") #'my-timeline-toggle-preview)
  (define-key calendar-mode-map (kbd "C-c <left>") #'my-timeline-history-back)
  (define-key calendar-mode-map (kbd "C-c <right>") #'my-timeline-history-forward)

  ;; Insertion + navigation
  (define-key calendar-mode-map (kbd "i") #'my-calendar-insert-diary-entry)
  (define-key calendar-mode-map (kbd "c") #'my-calendar-insert-diary-entry)
  (define-key calendar-mode-map (kbd "C") #'my-calendar-insert-diary-entry-and-autopopulate)
  (define-key calendar-mode-map (kbd "I") #'diary-insert-entry)
  (define-key calendar-mode-map (kbd "t") #'my-timeline-today-edit)
  (define-key calendar-mode-map (kbd "M-t") #'my-calendar-toggle-last-date)
  (define-key calendar-mode-map (kbd "a")   #'my-timeline-upcoming)
  (define-key calendar-mode-map (kbd "?")   #'my-calendar-help)

  ;; Month / year navigation helpers bound alongside standard keys.
  (define-key calendar-mode-map (kbd "n") #'calendar-forward-month)
  (define-key calendar-mode-map (kbd "M-<right>") #'calendar-forward-month)
  (define-key calendar-mode-map (kbd "p") #'calendar-backward-month)
  (define-key calendar-mode-map (kbd "M-<left>") #'calendar-backward-month)
  (define-key calendar-mode-map (kbd "N") #'calendar-forward-year)
  (define-key calendar-mode-map (kbd "P") #'calendar-backward-year))

(with-eval-after-load 'which-key
  (with-eval-after-load 'calendar
    (when (fboundp 'which-key-add-key-based-replacements)
      (which-key-add-key-based-replacements
        "j" "Jump to natural date"
        "g d" "Jump to natural date"
        "V" "Toggle itinerary follow"
        "C-c <left>" "Previous date jump"
        "C-c <right>" "Next date jump"
        "i" "Insert diary entry"
        "c" "Insert diary entry"
        "C" "Insert diary (with default)"
        "I" "Stock diary-insert-entry"
        "RET" "Edit diary entry"
        "e" "Edit diary entry"
        "v" "Show itinerary"
        "o" "Show itinerary"
        "SPC" "Show itinerary"
        "O" "Fancy diary listing"
        "t" "Jump to today + edit entry"
        "M-t" "Toggle today/last date"
        "a" "Upcoming events"
        "n" "Next month"
        "M-<right>" "Next month"
        "p" "Prev month"
        "M-<left>" "Prev month"
        "N" "Next year"
        "P" "Prev year"
        "?" "Show calendar help"))))

(provide 'timeline)

;;; timeline.el ends here
