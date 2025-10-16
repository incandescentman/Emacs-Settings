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
(require 'timeline-tests)

(with-eval-after-load 'calendar
  ;; Editing / viewing bindings
  (define-key calendar-mode-map (kbd "RET") #'my-calendar-edit-diary-entry)
  (define-key calendar-mode-map (kbd "e")   #'my-calendar-edit-diary-entry)
  (define-key calendar-mode-map (kbd "v")   #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "o")   #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "SPC") #'my-calendar-view-diary-entry)
  (define-key calendar-mode-map (kbd "O")   #'my-calendar-show-fancy-diary-listing)

  ;; Insertion + navigation
  (define-key calendar-mode-map (kbd "i") #'my-calendar-insert-diary-entry)
  (define-key calendar-mode-map (kbd "c") #'my-calendar-insert-diary-entry)
  (define-key calendar-mode-map (kbd "C") #'my-calendar-insert-diary-entry-and-autopopulate)
  (define-key calendar-mode-map (kbd "I") #'calendar-insert-diary-entry)
  (define-key calendar-mode-map (kbd "t")
              (lambda ()
                (interactive)
                (calendar-goto-today)
                (my-calendar-jump-to-diary-entry)))
  (define-key calendar-mode-map (kbd "M-t") #'my-calendar-toggle-last-date)
  (define-key calendar-mode-map (kbd "?")   #'my-calendar-help)

  ;; Month / year navigation helpers bound alongside standard keys.
  (define-key calendar-mode-map (kbd "n")
              (lambda ()
                (interactive)
                (calendar-forward-month 1)))
  (define-key calendar-mode-map (kbd "M-<right>")
              (lambda ()
                (interactive)
                (calendar-forward-month 1)))
  (define-key calendar-mode-map (kbd "p")
              (lambda ()
                (interactive)
                (calendar-backward-month 1)))
  (define-key calendar-mode-map (kbd "M-<left>")
              (lambda ()
                (interactive)
                (calendar-backward-month 1)))
  (define-key calendar-mode-map (kbd "N")
              (lambda ()
                (interactive)
                (calendar-forward-year 1)))
  (define-key calendar-mode-map (kbd "P")
              (lambda ()
                (interactive)
                (calendar-backward-year 1))))

(with-eval-after-load 'which-key
  (with-eval-after-load 'calendar
    (when (fboundp 'which-key-add-key-based-replacements)
      (which-key-add-key-based-replacements
        "i" "Insert diary entry"
        "c" "Insert diary entry"
        "C" "Insert diary (with default)"
        "I" "Original insert diary"
        "RET" "Edit diary entry"
        "e" "Edit diary entry"
        "v" "View diary entry"
        "o" "View diary entry"
        "SPC" "View diary entry"
        "O" "Fancy diary listing"
        "t" "Jump to today + view entry"
        "M-t" "Toggle today/last date"
        "n" "Next month"
        "M-<right>" "Next month"
        "p" "Prev month"
        "M-<left>" "Prev month"
        "N" "Next year"
        "P" "Prev year"
        "?" "Show calendar help"))))

(provide 'timeline)

;;; timeline.el ends here
