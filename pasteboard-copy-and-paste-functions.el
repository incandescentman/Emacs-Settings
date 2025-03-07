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

(defun pasteboard-copy-and-replace-em-dashes-in-clipboard-maybe ()
  "Copy to the macOS pasteboard using a command selected by the current mode.

When working with prose (in `org-mode` without `org-config-files-local-mode`
or in a mode derived from `text-mode`), call
`pasteboard-copy-and-replace-em-dashes-in-clipboard` to replace dash sequences
with em dashes.

When working with code (any mode other than `org-mode` or in `org-mode` when
`org-config-files-local-mode` is active), call `pasteboard-copy` to copy verbatim."
  (interactive)
  (if (or (and (eq major-mode 'org-mode)
               (not (bound-and-true-p org-config-files-local-mode)))
          (derived-mode-p 'text-mode))
      (pasteboard-copy-and-replace-em-dashes-in-clipboard)
    (pasteboard-copy)))

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
           (char-set '(?: ?' ?( ?) ?| ?[ ?] ?/ ?\\ ?\" ?= ?< ?> ?{ ?}))
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
If we're in `shell-script-mode`, `emacs-lisp-mode`, `org-config-files-local-mode`,
or a mode derived from `prog-mode`, copy text verbatim (no replacements).
Otherwise, replace occurrences of `---` and `--` with em dashes in the clipboard text."
  (interactive)
  (if (use-region-p)
      (if (or (eq major-mode 'shell-script-mode)
              (eq major-mode 'emacs-lisp-mode)
              (bound-and-true-p org-config-files-local-mode)
              (derived-mode-p 'prog-mode))
          ;; Just copy verbatim
          (let ((txt (buffer-substring-no-properties (region-beginning) (region-end))))
            (shell-command-to-string
             (format "echo -n %s | pbcopy" (shell-quote-argument txt)))
            (message "Copied text verbatim."))
        ;; Otherwise do the dash replacements
        (let* ((txt (buffer-substring-no-properties (region-beginning) (region-end)))
               (txt (with-temp-buffer
                      (insert txt)
(let ((text (buffer-substring-no-properties (point-min) (point-max))))
  (replace-regexp-in-string "---" "—" text))
                      (buffer-string)))
               (txt (with-temp-buffer
                      (insert txt)
(let ((text (buffer-substring-no-properties (point-min) (point-max))))
  (replace-regexp-in-string "---" "—" text))
                      (buffer-string))))
          (with-temp-buffer
            (insert txt)
            (shell-command-on-region (point-min) (point-max) "pbcopy"))
          (message "Text with em dashes copied to macOS pasteboard.")))
    (message "No region selected")))

(defun pasteboard-copy-to-end-of-buffer ()
  "Copy text from point to the end of the buffer to OS X system pasteboard."
  (interactive)
  (let* ((txt (buffer-substring (point) (point-max))))
    (shell-command-to-string
     (format "echo -n %s | pbcopy" (shell-quote-argument txt)))))

(defun pasteboard-copy-and-convert-to-markdown-link ()
  "Copy region to OS X system pasteboard, converting Org-style links to Markdown format."
  (interactive)
  (if (use-region-p)
      (let* ((txt (buffer-substring (region-beginning) (region-end)))
             (txt-updated-links
              (replace-regexp-in-string
               "\\[\\[\\([^]]*\\)\\]\\(\\[\\([^]]*\\)\\]\\)?\\]"
               (lambda (m)
                 ;; The match data is set up so match-string works
                 (concat "[" (or (match-string 3 m)
                                 (match-string 1 m))
                         "](" (match-string 1 m) ")"))
               txt)))
        (shell-command-to-string
         (format "echo -n %s | pbcopy" (shell-quote-argument txt-updated-links)))
        (message "Copied and converted Org links to Markdown."))
    (message "No region selected")))

(defun convert-markdown-links-to-org-mode (beg end)
  "Convert Markdown links to Org-mode links in the specified region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\[\\([^][]+\\)\\](\\([^)]+\\))" end t)
      (replace-match "[[\\2][\\1]]" t))))



(defun pasteboard-paste-and-convert-markdown-links-to-org-mode ()
 "Paste from OS X system pasteboard and convert Markdown links to Org-mode format."
 (interactive)
 (let* ((clipboard-content (shell-command-to-string "pbpaste"))
     (clean-content (string-trim clipboard-content))
     (start (point))
     (end (if mark-active (mark) (point))))
  (if (string-empty-p clean-content)
    (message "Clipboard is empty.")
   (let ((converted-content
       (replace-regexp-in-string
       "\\[\\([^][]+\\)\\](\\([^)]+\\))"
       "[[\\2][\\1]]"
       clean-content)))
    (delete-region start end)
    (insert converted-content)
    (message "Content pasted and converted successfully.")))))

(defun pasteboard-paste ()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (let ((start (point))
        (end (if mark-active
                 (mark)
               (point)))
        (ins-text
         (shell-command-to-string "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'")))
    (delete-region start end)
    (insert ins-text)
    (my/fix-space)
    (save-excursion
      (goto-char start)
      (my/fix-space)))
                                        ; (reflash-indentation)
  )

(defun pasteboard-paste-without-smart-quotes ()
  "Paste from the system clipboard, replace smart quotes, and convert Markdown links to Org-mode format."
  (interactive)
  (let ((beg (point)))
    (pasteboard-paste) ; Paste the content from the clipboard.
    (replace-smart-quotes beg (point)) ; Replace smart quotes in the pasted content.
    (convert-markdown-links-to-org-mode beg (point)) ; Convert Markdown links to Org-mode.
    ;; If you have other cleanup functions, call them here.
    ))

(defun pasteboard-paste-no-spaces ()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (let ((start (point))
	(end (if mark-active
		 (mark)
	       (point))))
    (shell-command-on-region start end
			     "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'"
			     nil t)
    (save-excursion
      )))

(defun pasteboard-paste-adjusted-subtrees ()
  "Paste text from the system pasteboard, adjusting Org headings to be subheadings.
This function ensures that all Org-mode headings in the pasted text
are adjusted so they become subheadings under the current Org heading."
  (interactive)
  (let* ((text (shell-command-to-string "pbpaste"))
         ;; Ensure we have the correct current heading level
         (current-level (save-excursion
                          (if (org-before-first-heading-p)
                              0
                            (or (org-current-level)
                                (progn
                                  (org-back-to-heading t)
                                  (org-current-level))
                                0)))))
    ;; Clean up the text by removing carriage returns
    (setq text (replace-regexp-in-string "\r" "" text))
    ;; Adjust the heading levels in the pasted text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((min-level nil))
        ;; Find the minimum heading level in the pasted text
        (while (re-search-forward "^\\(\\*+\\) " nil t)
          (let ((level (length (match-string 1))))
            (when (or (not min-level) (< level min-level))
              (setq min-level level))))
        (when min-level
          ;; Calculate the shift needed to adjust heading levels
          (let ((shift (- (+ current-level 1) min-level)))
            (goto-char (point-min))
            ;; Adjust each heading in the pasted text
            (while (re-search-forward "^\\(\\*+\\)" nil t)
              (let* ((stars (match-string 1))
                     (level (length stars))
                     (new-level (max 1 (+ level shift))))
                (replace-match (make-string new-level ?*) t t)))))
        ;; Retrieve the adjusted text
        (setq text (buffer-string))))
    ;; Insert the adjusted text at point
    (insert text)))

(defun pasteboard-paste-adjusted-subtrees-spaces-maybe ()
  "Paste Org text from pasteboard, adjust heading levels to be subheadings,
and handle spacing based on surrounding punctuation."
  (interactive)
  (let* ((text (shell-command-to-string "pbpaste"))
         (current-level (save-excursion
                          (if (org-before-first-heading-p)
                              0
                            (or (org-current-level)
                                (progn
                                  (org-back-to-heading t)
                                  (org-current-level))
                                0))))
         (prev-char (char-before))
         (next-char (char-after))
         (char-set '(?: ?' ?( ?) ?| ?[ ?] ?/ ?\\ ?\" ?= ?< ?> ?{ ?})))

    ;; Clean up the text by removing carriage returns
    (setq text (replace-regexp-in-string "\r" "" text))

    ;; Adjust the heading levels in the pasted text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((min-level nil))
        (while (re-search-forward "^\\(\\*+\\) " nil t)
          (let ((level (length (match-string 1))))
            (when (or (null min-level) (< level min-level))
              (setq min-level level))))
        (when min-level
          (let ((shift (- (+ current-level 1) min-level)))
            (goto-char (point-min))
            (while (re-search-forward "^\\(\\*+\\)" nil t)
              (let* ((stars (match-string 1))
                     (level (length stars))
                     (new-level (max 1 (+ level shift))))
                (replace-match (make-string new-level ?*) t t))))))
      (setq text (buffer-string)))

    ;; Insert the text at point and perform quote replacements if appropriate
    (let ((start (point)))
      (insert text)
      (let ((end-pos (point)))
        ;; If we're NOT next to punctuation, do quote replacements
        (unless (or (member prev-char char-set)
                    (member next-char char-set))
          (save-excursion
            (goto-char start)
            ;; Replace various types of apostrophes with a straight '
            (ignore-errors
              (while (re-search-forward "['']" end-pos t)
                (replace-match "'" t t)))
            (goto-char start)
            ;; Replace straight or curly double quotes with a straight "
            (ignore-errors
              (while (re-search-forward "[\"""]" end-pos t)
                (replace-match "\"" t t)))))))))

(defun pasteboard-cut ()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end))
  (my/fix-space)
  )

(defun pasteboard-cut-and-capitalize ()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end))
  (my/fix-space)
  (save-excursion
    (when (my/beginning-of-sentence-p)
      (capitalize-unless-org-heading))))

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

(defvar-local failed-search nil)

(defun wrapped-search-forward (str)
  (interactive "sWrappedSearch:")
  (if (and
       failed-search
       (>= (car failed-search) (point))
       (string-equal (cdr failed-search) str))
      (let ((p (save-excursion
                 (goto-char 0)
                 (search-forward str nil t))))
        (if p
            (progn
              (goto-char p)
              (setq-local failed-search nil))
          (message "WrappedSearch: Not found.")))
    (let ((p (search-forward str nil t)))
      (unless p
        (setq-local failed-search (cons (point) str))
        (message "Search: Not found.")))))

(defun pasteboard-search-for-clipboard-contents ()
  (interactive)
  (let ((search-term
         (with-temp-buffer
           (pasteboard-paste-no-spaces)
           (buffer-string))))
    (wrapped-search-forward search-term)))

(setq x-select-enable-clipboard t)

(defun push-kill-ring-pasteboard-to-MacOS-clipboard ()
  (interactive)
  (x-select-text (current-kill 0)))

(defun push-MacOS-clipboard-to-kill-ring ()
 "Push the content of the MacOS clipboard to the Emacs kill ring."
 (interactive)
 (let ((clipboard-content (shell-command-to-string "pbpaste")))
  (when (and clipboard-content (not (string= clipboard-content "")))
   (kill-new clipboard-content)
   (message "Pushed clipboard content to kill ring: %s" clipboard-content))))

(defun gist-buffer-to-pasteboard ()
  (interactive)
  (gist-buffer)
  (push-kill-ring-pasteboard-to-MacOS-clipboard)
  )
