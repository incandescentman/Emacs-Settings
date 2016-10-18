;; mu4e-send-delay.el -- not part of mu4e

;; Copyright (C) 2016 Benjamin Andresen <benny@in-ulm.de>

;; Author: Benjamin Andresen <benny@in-ulm.de>
;; Maintainer: Benjamin Andresen <benny@in-ulm.de>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Heavily inspired by gnus-delay and mu4e-delay, but made to fit into the mu4e
;; environment better.
;; Thanks to Ben Maughen and Kai Großjohann.

;;; Code:

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl-lib)

(require 'gnus-util)
(autoload 'parse-time-string "parse-time" nil nil)

(require 'mu4e-utils)
(require 'mu4e-compose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed sending of messages / Additional composing
(defgroup mu4e-send-delay nil
  "Customization for delayed sending of messages"
  :group 'mu4e)

(defcustom mu4e-send-delay-flag "delay"
  "Flag to determine if mail is saved as delay."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-header "X-Delay"
  "Header name for storing info about delayed mails."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-strip-header-before-send t
  "Remove `mu4e-send-delay-header' before sending mail."
  :type 'bool
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-include-header-in-draft t
  "Whether to include the delay header when starting to draft a
message; if nil, only do so when sending the message"
  :type 'bool
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-default-delay "3m"
  "Default length of delay."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-default-hour "8"
  "Default length of delay."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-timer 120
  "Number of seconds between checks for delayed mail to send."
  :type 'integer
  :group 'mu4e-delay)

;; copied from mu4e-delay which is from gnus-delay
(defun mu4e-send-delay-add-delay-header (delay)
  "Delay this article by some time.
  DELAY is a string, giving the length of the time.  Possible values are:

  ,* <digits><units> for <units> in minutes (`m'), hours (`h'), days (`d'),
    weeks (`w'), months (`M'), or years (`Y');

  ,* YYYY-MM-DD for a specific date.  The time of day is given by the
    variable `mu4e-send-delay-default-hour', minute and second are zero.

  ,* hh:mm for a specific time.  Use 24h format.  If it is later than this
    time, then the deadline is tomorrow, else today."
  (interactive
   (list (read-string
          "Target date (YYYY-MM-DD), time (hh:mm), or length of delay (units in [mhdwMY]): "
          mu4e-delay-default-delay)))
  ;; Allow spell checking etc.
  (run-hooks 'message-send-hook)
  (let (num unit days year month day hour minute deadline)
    (cond ((string-match
            "\\([0-9][0-9][0-9]?[0-9]?\\)-\\([0-9]+\\)-\\([0-9]+\\)"
            delay)
           (setq year  (string-to-number (match-string 1 delay))
                 month (string-to-number (match-string 2 delay))
                 day   (string-to-number (match-string 3 delay)))
           (setq deadline
                 (message-make-date
                  (encode-time 0 0      ; second and minute
                               mu4e-delay-default-hour
                               day month year))))
          ((string-match "\\([0-9]+\\):\\([0-9]+\\)" delay)
           (setq hour   (string-to-number (match-string 1 delay))
                 minute (string-to-number (match-string 2 delay)))
           ;; Use current time, except...
           (setq deadline (apply 'vector (decode-time (current-time))))
           ;; ... for minute and hour.
           (aset deadline 1 minute)
           (aset deadline 2 hour)
           ;; Convert to seconds.
           (setq deadline (float-time (apply 'encode-time
                                             (append deadline nil))))
           ;; If this time has passed already, add a day.
           (when (< deadline (float-time))
             (setq deadline (+ 86400 deadline))) ; 86400 secs/day
           ;; Convert seconds to date header.
           (setq deadline (message-make-date
                           (seconds-to-time deadline))))
          ((string-match "\\([0-9]+\\)\\s-*\\([mhdwMY]\\)" delay)
           (setq num (match-string 1 delay))
           (setq unit (match-string 2 delay))
           ;; Start from seconds, then multiply into needed units.
           (setq num (string-to-number num))
           (cond ((string= unit "Y")
                  (setq delay (* num 60 60 24 365)))
                 ((string= unit "M")
                  (setq delay (* num 60 60 24 30)))
                 ((string= unit "w")
                  (setq delay (* num 60 60 24 7)))
                 ((string= unit "d")
                  (setq delay (* num 60 60 24)))
                 ((string= unit "h")
                  (setq delay (* num 60 60)))
                 (t
                  (setq delay (* num 60))))
           (setq deadline (message-make-date
                           (seconds-to-time (+ (float-time) delay)))))
          (t (error "Malformed delay `%s'" delay)))
    (message-add-header (format "%s: %s" mu4e-send-delay-header deadline))
    deadline))

;; bind this command
(defun mu4e-send-delay-send-and-exit (&optional instant)
  "Delayed send this mail, unless INSTANT (prefix-argument) is non-nil. "
  (interactive "P")
  (if instant
      (progn
        (message-remove-header mu4e-send-delay-header nil t)
        (message-send-and-exit))
    (mu4e-send-delay-postpone-and-exit)))

(defun mu4e-send-delay-postpone-and-exit ()
  (let ((buffer (current-buffer)) schedule-time)
    (setq schedule-time (mu4e-send-delay-schedule-this-mail))
    (if message-kill-buffer-on-exit
        (kill-buffer buffer))
    (mu4e-message "Mail scheduled for sending at: %s" schedule-time)))

(defun mu4e-send-delay-schedule-this-mail ()
  (condition-case err
      (progn
        (if (not (equal major-mode 'mu4e-compose-mode))
            (error "Not a mu4e:compose buffer."))
        (let (schedule-time current-header-value)
          (setq delay-header-value (message-fetch-field mu4e-send-delay-header))
          (if delay-header-value (message-remove-header mu4e-send-delay-header))
          (setq schedule-time
                (mu4e-send-delay-add-delay-header (or delay-header-value
                                                      mu4e-send-delay-default-delay)))
          (message-dont-send)
          schedule-time))
    (error (princ (format "mu4e-send-delay: %s" err)))))

;; Show up in the main view
(add-to-list 'mu4e-header-info-custom
             '(:send-delay . ( :name "Scheduled"
                                     :shortname "Delay"
                                     :help "Date/Time when mail is scheduled for dispatch"
                                     :function (lambda (msg)
                                                 (mu4e-send-delay-header-value
                                                  (mu4e-message-field msg :path))))))
(add-to-list 'mu4e-view-fields :send-delay t)

(defun mu4e-send-delay-header-value (file-path)
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-quote mu4e-send-delay-header) ":\\s-+"))
      (buffer-substring (point) (point-at-eol)))))

(defun mu4e-send-delay-time-elapsed-p (time-string)
  ;; if it's not a valid time-string, don't encode it
  (let ((parsed-ts (parse-time-string time-string)))
    (when (not (cl-every #'null parsed-ts))
      (let* ((delay-time (apply 'encode-time parsed-ts))
             (time-since (time-since delay-time)))
        (and (>= (nth 0 time-since) 0)
             (>= (nth 1 time-since) 0))))))

(defun mu4e-send-delay-elapsed-p (file-path)
  (let ((header-value (mu4e-send-delay-header-value file-path)))
    (if header-value (mu4e-send-delay-time-elapsed-p header-value))))

(defun mu4e-send-delay-file-buffer-open (mail-file-path)
  (get-file-buffer mail-file-path))

(defun mu4e-send-delay-draft-delete-header ()
  (message-remove-header mu4e-send-delay-header nil t))

(defun mu4e-send-delay-draft-add-header ()
  (message-add-header (format "%s: %s"
                              mu4e-send-delay-header
                              mu4e-send-delay-default-delay)))

(defun mu4e-send-delay-draft-refresh-header (&optional draft-buffer)
  (save-excursion
    (with-current-buffer (or draft-buffer (current-buffer))
      (mu4e-send-delay-draft-delete-header)
      (mu4e-send-delay-draft-add-header))))

(defun mu4e-send-delay-send-if-due (mail-file-path)
  "Send mail when MAIL-FILE-PATH contains scheduled time earlier
than current time and is not currently being edited."
  (when (and (mu4e-send-delay-elapsed-p mail-file-path)
             (not (mu4e-send-delay-file-buffer-open mail-file-path)))
    (condition-case err
        (progn
          (with-temp-buffer
            (insert-file-contents-literally mail-file-path)

            ;; force recode to fix character encoding issue
            (set-buffer-file-coding-system 'utf-8 t)
            (recode-region (point-min) (point-max) 'prefer-utf-8 'utf-8-unix)

            (mu4e~draft-insert-mail-header-separator)
            (mu4e-compose-mode)
            (when mu4e-send-delay-strip-header-before-send
              (message-remove-header mu4e-send-delay-header nil t))
            ;; set modified to nil so buffer can be killed
            (message-send-mail)
            (set-buffer-modified-p nil))
          (mu4e-send-delay-move-to-sent-and-delete-draft mail-file-path)
          t)
      (error "mu4e-send: %s" err))))

(defun mu4e-send-delay-move-to-sent-and-delete-draft (mail-file-path)
  (let (buf file)
    (when mail-file-path
      (setq buf (find-file-noselect mail-file-path))
      (set-buffer (get-buffer-create " *mu4e-send-delay temp*"))
      (erase-buffer)
      (insert-buffer-substring buf)
      (message-narrow-to-head)
      ;; can't use message-add-header as it lacks the mail header
      ;; separator
      (insert (format "Date: %s\n" (message-make-date)))

      ;; XXX: I'm not happy with the duplication of logic
      (setq file (message-fetch-field "fcc"))
      (message-remove-header "fcc" nil)
      (message-remove-header mu4e-send-delay-header nil)

      ;; write mail to Sent-folder
      (write-file file)
      (kill-buffer (current-buffer))

      ;; delete mail from Drafts-folder
      (delete-file mail-file-path)
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (kill-buffer buf)))))

(defmacro mu4e-send-delay-with-mu4e-context (context &rest body)
  "Evaluate BODY, with `mu4e~current-context' set and
`with~mu4e-context-vars'"
  (declare (indent 2))
  `(let* ((mu4e~context-current ,context))
     (with~mu4e-context-vars ,context
         ,@body)))

(defun mu4e-send-delay-get-drafts-folder ()
  (expand-file-name
   (concat mu4e-maildir (mu4e-get-drafts-folder) "/cur")))

(defun mu4e-send-delay-send-queue ()
  "Send all delayed mails that are due now."
  (interactive)
  (let ((dirs (if mu4e-contexts
                  (mapcar (lambda (context)
                            (mu4e-send-delay-with-mu4e-context context
                                (mu4e-send-delay-get-drafts-folder)))
                          mu4e-contexts)
                (list (mu4e-send-delay-get-drafts-folder)))))
    (when (memq t
                (mapcar (lambda (dir)
                          (cl-loop for file in (directory-files dir t "^[^\.]")
                                   collect (mu4e-send-delay-send-if-due file)))
                        dirs))
      ;; only update index if something was done
      (mu4e-update-index))))

(defvar mu4e-send-delay-send-queue-timer nil
  "Timer to run `mu4e-send-delay-send-queue'")

(defun mu4e-send-delay-initialize-send-queue-timer ()
  "Set up `mu4e-send-delay-send-queue' to run on a timer."
  (interactive)
  (unless mu4e-send-delay-send-queue-timer
    (setq mu4e-send-delay-send-queue-timer
          (run-with-timer 0 mu4e-send-delay-timer #'mu4e-send-delay-send-queue))))

(defun mu4e-send-delay-setup ()
  "Sets up `mu4e-compose-mode-hook' and defines modified `mu4e~draft-common-construct' for Drafts
`mu4e-compose-mode-hook' is used to refresh the schedule header upon edit and
`mu4e~draft-common-construct' is used to include the initial mu4e-send-delay header value."
  (interactive)
  (add-hook 'mu4e-compose-mode-hook #'mu4e-send-delay-draft-refresh-header)

  ;; Find a better way to do this
  (defun mu4e~draft-common-construct ()
    "Construct the common headers for each message."
    (concat
     (mu4e~draft-header "User-agent" mu4e-user-agent-string)
     (when mu4e-compose-auto-include-date
       (mu4e~draft-header "Date" (message-make-date)))
     (when mu4e-send-delay-include-header-in-draft
       (mu4e~draft-header mu4e-send-delay-header mu4e-send-delay-default-delay)))))

(provide 'mu4e-send-delay)
