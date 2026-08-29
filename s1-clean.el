;;; s1-clean.el --- Clean the current transcript with local S1-mini -*- lexical-binding: t; -*-

;;; Commentary:

;; Three entry points for the transcription repo's S1-mini cleanup wrapper,
;; /Users/jay/Dropbox/github/transcription/bin/s1-clean:
;;
;;   `s1-clean-file' — clean the transcript in the current buffer. Saves the
;;     buffer first, then runs the wrapper asynchronously into `*s1-clean*'; a
;;     long transcript takes minutes at roughly two seconds per speaker turn, so
;;     Emacs is never blocked. On success the cleaned derivative opens in the
;;     other window and the wrapper's summary appears in the echo area. On
;;     failure — including the wrapper's refusal to overwrite an existing
;;     derivative — the process buffer is shown so the reason is visible.
;;     With a prefix argument, passes --force to replace existing outputs.
;;
;;   `s1-clean-stats' — fast, no model. Reports turn count, word count, and
;;     fillers per 100 words so you can decide whether cleaning is worth it.
;;
;;   `s1-clean-diff' — open the `_s1mini_cleaned.diff' sidecar in `diff-mode'.
;;
;; The wrapper never modifies the file it reads; it writes a separate
;; `<stem>_s1mini_cleaned.org' derivative beside it. The model is local and
;; free, so nothing here spends money. The result is a clean non-verbatim
;; reading copy, never the canonical archival transcript.

;;; Code:

(require 'subr-x)

;;; Customization -------------------------------------------------------------

(defvar s1-clean-program
  "/Users/jay/Dropbox/github/transcription/bin/s1-clean"
  "Absolute path to the repo-owned S1-mini cleanup wrapper.")

(defvar s1-clean-buffer-name "*s1-clean*"
  "Name of the buffer holding the wrapper's output.")

(defconst s1-clean--stem-suffixes '("_diarized" "_transcript")
  "Filename suffixes the wrapper strips before adding its own.
Kept in sync with `output_stem' in the wrapper.")

;;; Pure helpers --------------------------------------------------------------

(defun s1-clean--stem (file)
  "Return FILE's base name with a trailing transcript suffix removed."
  (let ((base (file-name-base file)))
    (or (seq-some (lambda (suffix)
                    (and (string-suffix-p suffix base)
                         (substring base 0 (- (length base) (length suffix)))))
                  s1-clean--stem-suffixes)
        base)))

(defun s1-clean--sidecar (file extension)
  "Return the `_s1mini_cleaned' sidecar for FILE with EXTENSION."
  (expand-file-name (concat (s1-clean--stem file) "_s1mini_cleaned" extension)
                    (file-name-directory (expand-file-name file))))

(defun s1-clean--output-file (file)
  "Return the cleaned Org derivative the wrapper writes for FILE."
  (s1-clean--sidecar file ".org"))

(defun s1-clean--diff-file (file)
  "Return the word-diff sidecar the wrapper writes for FILE."
  (s1-clean--sidecar file ".diff"))

(defun s1-clean--current-file ()
  "Return the current buffer's Org file, or signal an error.
Signals when the buffer visits no file, visits a remote file, or
visits something other than an Org transcript."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (when (file-remote-p file)
      (user-error "s1-clean only supports local files"))
    (unless (string-equal (downcase (or (file-name-extension file) "")) "org")
      (user-error "s1-clean needs an Org transcript, not %s" (file-name-nondirectory file)))
    file))

(defun s1-clean--summary (buffer)
  "Return the wrapper's final summary line from BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^Wrote .*$" nil t)
          (match-string-no-properties 0))))))

;;; Commands ------------------------------------------------------------------

;;;###autoload
(defun s1-clean-file (&optional force)
  "Clean the current buffer's Org transcript with the local S1-mini model.
Saves the buffer first, then runs the wrapper asynchronously in
`s1-clean-buffer-name'.  With prefix argument FORCE, pass --force so
existing outputs are replaced."
  (interactive "P")
  (let ((file (s1-clean--current-file)))
    (unless (file-executable-p s1-clean-program)
      (user-error "s1-clean wrapper is not executable: %s" s1-clean-program))
    (when (buffer-modified-p)
      (save-buffer))
    (let ((buffer (get-buffer-create s1-clean-buffer-name)))
      (when (process-live-p (get-buffer-process buffer))
        (user-error "An s1-clean run is already in progress"))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq default-directory (file-name-directory (expand-file-name file))))
      (let* ((args (append (list file) (when force '("--force"))))
             (output (s1-clean--output-file file))
             (process (apply #'start-process "s1-clean" buffer s1-clean-program args)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (memq (process-status proc) '(exit signal))
             (let ((status (process-exit-status proc))
                   (proc-buffer (process-buffer proc)))
               (if (and (eq (process-status proc) 'exit) (zerop status))
                   (progn
                     (if (file-readable-p output)
                         (find-file-other-window output)
                       (message "s1-clean finished but %s is missing" output))
                     (message "%s" (or (s1-clean--summary proc-buffer)
                                       "s1-clean finished")))
                 (message "s1-clean failed (%s)" (string-trim event))
                 (pop-to-buffer proc-buffer))))))
        (message "s1-clean running on %s; output in %s"
                 (file-name-nondirectory file) s1-clean-buffer-name)))))

;;;###autoload
(defun s1-clean-stats ()
  "Report turns, words, and fillers per 100 words for the current transcript.
Runs the wrapper's --stats mode, which needs no model and is fast."
  (interactive)
  (let ((file (s1-clean--current-file)))
    (unless (file-executable-p s1-clean-program)
      (user-error "s1-clean wrapper is not executable: %s" s1-clean-program))
    (with-temp-buffer
      (let ((status (call-process s1-clean-program nil t nil file "--stats")))
        (let ((output (string-trim (buffer-string))))
          (unless (and (integerp status) (zerop status))
            (user-error "s1-clean --stats failed (%s): %s" status output))
          (message "%s" output))))))

;;;###autoload
(defun s1-clean-diff ()
  "Open the `_s1mini_cleaned.diff' sidecar for the current transcript."
  (interactive)
  (let ((diff (s1-clean--diff-file (s1-clean--current-file))))
    (unless (file-readable-p diff)
      (user-error "No cleanup diff yet: %s" diff))
    (find-file-other-window diff)
    (diff-mode)))

(provide 's1-clean)

;;; s1-clean.el ends here
