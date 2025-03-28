(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)

(defun safe-replace (pattern replacement beg end)
  "Safely replace PATTERN with REPLACEMENT between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((last-point (point)))
      (while (re-search-forward pattern end t)
        (unless (> (point) last-point)  ; Prevent infinite loops
          (error "Infinite loop detected in regex replacement."))
        (setq last-point (point))
        (replace-match replacement t t)))))

(defun safe-pbpaste ()
  "Run pbpaste with a timeout to prevent hanging."
  (let ((default-directory "/tmp/"))  ; Avoid issues with remote TRAMP paths
    (with-temp-buffer
      (if (zerop (call-process "gtimeout" nil t nil "2" "pbpaste"))  ; Requires coreutils for `gtimeout`
          (buffer-string)
        (error "pbpaste timed out")))))

(defun pasteboard-paste-spaces-maybe ()
  "Paste from pasteboard, choosing the method based on the current working mode and (if in code) surrounding characters.

In prose (in `org-mode` without `org-config-files-local-mode` or in a mode derived from `text-mode`), always use `pasteboard-paste-without-smart-quotes`.

In code (any mode other than the above, or in `org-mode` when `org-config-files-local-mode` is enabled),
use the original logic: if the character before or after point is in a predefined set,
use `pasteboard-paste-no-spaces`, otherwise use `pasteboard-paste-without-smart-quotes`."
  (interactive)
  (if (or (and (eq major-mode 'org-mode)
               (not (bound-and-true-p org-config-files-local-mode)))
          (derived-mode-p 'text-mode))
      ;; Prose: always use without-smart-quotes.
      (pasteboard-paste-without-smart-quotes)
    ;; Code: use the original surrounding-character logic.
    (let* ((prev-char (char-before))
           (next-char (char-after))
(char-set '(?: ?' ?\( ?\) ?| ?\[ ?\] ?/ ?\\ ?\" ?= ?< ?> ?{ ?}))
           (use-no-spaces (or (member prev-char char-set)
                              (member next-char char-set))))
      (if use-no-spaces
          (pasteboard-paste-no-spaces)
        (pasteboard-paste-without-smart-quotes)))))

(defun pasteboard-copy ()
  "Copy region to OS X system pasteboard."
  (interactive)
  (let* ((txt (buffer-substring (region-beginning) (region-end))))
    (shell-command-to-string
     (format "echo -n %s | pbcopy" (shell-quote-argument txt)))))

(defun pasteboard-copy-and-replace-em-dashes-in-clipboard ()
  "Copy selected region to macOS system pasteboard.
If we're in `shell-script-mode`, `emacs-lisp-mode`,
`org-config-files-local-mode`, or a mode derived from `prog-mode`,
then copy text verbatim (no replacements).
Otherwise, replace occurrences of `---` or `--` with an em dash."
  (interactive)
  (if (use-region-p)
      (let ((txt (buffer-substring-no-properties
                  (region-beginning) (region-end))))
        (if (or (eq major-mode 'shell-script-mode)
                (eq major-mode 'emacs-lisp-mode)
                (bound-and-true-p org-config-files-local-mode)
                (derived-mode-p 'prog-mode))
            ;; Copy verbatim
            (progn
              ;; Safely copy verbatim, including newlines
              (with-temp-buffer
                (insert txt)
                (shell-command-on-region (point-min) (point-max) "pbcopy"))
              (message "Copied text verbatim."))
          ;; Otherwise, replace --- or -- with em dash
          (setq txt (replace-regexp-in-string
                     "\\(?:---\\|--\\)" "—" txt))
          (with-temp-buffer
            (insert txt)
            (shell-command-on-region (point-min) (point-max) "pbcopy"))
          (message "Text with em dashes copied to macOS pasteboard.")))
    (message "No region selected.")))


(defun pasteboard-cut-and-capitalize-and-replace-em-dashes ()
  "Cut region and put on OS X pasteboard, replacing dash sequences with em dashes.
Then delete the region, fix spacing, and, if at the beginning of a sentence,
capitalize the text (unless it's an Org heading)."
  (interactive)
  ;; Use the copy-and-replace function instead of pasteboard-copy.
  (pasteboard-copy-and-replace-em-dashes-in-clipboard)
  (delete-region (region-beginning) (region-end))
  (my/fix-space)
  (save-excursion
    (when (my/beginning-of-sentence-p)
      (capitalize-unless-org-heading))))

(defun pasteboard-cut-and-capitalize-and-replace-em-dashes-maybe ()
  "Cut region and put it on the OS X pasteboard using a command selected by the current mode.

When working with prose (in `org-mode` without `org-config-files-local-mode`
or in a mode derived from `text-mode`), replace dash sequences with em dashes.
When working with code (any mode other than `org-mode` or in `org-mode` when
`org-config-files-local-mode` is active), cut region and copy verbatim by calling
`pasteboard-cut-and-capitalize`."
  (interactive)
  (if (or (and (eq major-mode 'org-mode)
               (not (bound-and-true-p org-config-files-local-mode)))
          (derived-mode-p 'text-mode))
      (pasteboard-cut-and-capitalize-and-replace-em-dashes)
    (pasteboard-cut-and-capitalize)))
