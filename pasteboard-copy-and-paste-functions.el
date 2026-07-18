;; -*- lexical-binding: t; -*-
;; NOTE: This .el file is the source of truth. Do not recreate or tangle from a .org version.
(require 'subr-x)
(require 'url-parse)

(defgroup pasteboard nil
  "Adaptive pasteboard options."
  :group 'convenience
  :prefix "pasteboard-")

(defgroup smart-quotes nil
  "Customization group for smart quote replacements."
  :group 'convenience)

(defcustom pasteboard-convert-markdown-inline-emphasis nil
  "When non-nil, convert Markdown *italic* and **bold** to Org /italic/ and *bold* during adaptive pastes."
  :type 'boolean
  :group 'pasteboard)

(defcustom smart-quote-regexp-replacements
  '(("\\(\\w\\)\\(  [-—] \\|—\\)" . "\\1---")
    ("\\_<okay\\_>" . "OK")
    ("  SCHEDULED" . " SCHEDULED")
    ("  DEADLINE" . " DEADLINE"))
  "Contextual regexp replacements applied after literal character normalization."
  :type '(alist :key-type regexp :value-type string)
  :group 'smart-quotes)


(defun replace-smart-quotes-regexp (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (mapcar
   (lambda (r)
     (save-excursion
       (goto-char beg)
       (let ((end-marker (copy-marker (min end (point-max)))))
         (while (re-search-forward (car r) end-marker t)
           (replace-match (cdr r) t nil))
         (set-marker end-marker nil))))
   smart-quote-regexp-replacements))

(defun replace-non-heading-double-asterisks (beg end)
  "Convert markdown style **bold** to Org *bold* unless the line is an Org heading."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\*\\*" end t)
      (let ((match-beg (match-beginning 0)))
        (when match-beg
          (goto-char match-beg)
          (if (looking-at "^\\*+ ")
              (forward-char 2)
              (replace-match "*" t t)))))))

(defcustom smart-quotes-replacement-pairs
  '(("“" . "\"")
    ("”" . "\"")
    ("‘" . "'")
    ("’" . "'")
    ("‚" . "'")
    ("ﬃ" . "ffi")
    ("‛" . "'")
    ("„" . "\"")
    ("‟" . "\"")
    ("‹" . "'")
    ("›" . "'")
    ("«" . "\"")
    ("»" . "\"")
    ("–" . "-")
    ("" . "")
    ("" . "")
    ("—" . "---")
    ("…" . "...")
    ("• " . "- ")
    ("•" . "-")
    (" " . " ")
    ("ﬀ" . "ff")
    ("·" . "-")
    ("‧" . "-")
    ("⁃" . "-")
    ("‐" . "-")
    ("‑" . "-")
    ("‒" . "-")
    ("‾" . "-")
    (" " . " ")       ; Non-breaking space
    ("\u200B" . "")   ; Zero-width space
    ("\u200C" . "")   ; Zero-width non-joiner
    ("\u200D" . "")   ; Zero-width joiner
    ("\f" . " ")      ; Form feed/page break from OCR/PDF text
    ("\t" . " ")      ; Tab character replaced with a space
    ("\u02BC" . "'")  ; Modifier letter apostrophe
    ("\uFF07" . "'")  ; Fullwidth apostrophe
    ("\u00AD" . "-")) ; Soft hyphen
  "Literal character replacements applied by `replace-smart-quotes`."
  :type '(alist :key-type string :value-type string)
  :group 'smart-quotes)

(require 'cl-lib)  ; Ensure cl-lib is loaded for cl-every


(defun replace-smart-quotes (beg end)
  "Replace smart quotes and format text appropriately in the specified region.

The replacements are defined in the `smart-quotes-replacement-pairs` variable."
  (interactive "r")
  ;; Ensure beg <= end
  (when (> beg end)
    (let ((temp beg))
      (setq beg end)
      (setq end temp)))
  ;; Error checking for smart-quotes-replacement-pairs
  (unless (and (boundp 'smart-quotes-replacement-pairs)
               (listp smart-quotes-replacement-pairs)
               (cl-every (lambda (pair)
                           (and (consp pair)
                                (stringp (car pair))
                                (stringp (cdr pair))))
                         smart-quotes-replacement-pairs))
    (error "Invalid `smart-quotes-replacement-pairs` format; must be a list of string pairs"))
  ;; Convert end into a marker that adjusts with buffer changes
  (let ((end-marker (copy-marker end)))
    (save-excursion
      ;; Replace markdown bold **text** with org bold *text*
      ;; Skip pairs that belong to the leading stars of an Org heading.
      ;; When `pasteboard-convert-markdown-inline-emphasis` is enabled, leave the double
      ;; asterisks intact so the dedicated emphasis pass can rewrite them safely later.
      (unless pasteboard-convert-markdown-inline-emphasis
        (goto-char beg)
        (while (re-search-forward "\\*\\*" end-marker t)
          (let* ((match-pos (match-beginning 0))
                 (match-end (match-end 0))
                 (line-start (line-beginning-position))
                 (heading-bound
                  (save-excursion
                    (goto-char line-start)
                    (let ((has-stars nil))
                      (while (eq (char-after) ?*)
                        (setq has-stars t)
                        (forward-char 1))
                      (when (and has-stars (looking-at "\\s-"))
                        (point))))))
            (unless (and heading-bound (< match-pos heading-bound))
              (goto-char match-pos)
              (delete-region match-pos match-end)
              (insert "*")))))
      ;; Remove lines that contain only '---' (possibly with surrounding spaces)
      (goto-char beg)
      (while (re-search-forward "^\\s-*---\\s-*$" end-marker t)
        (replace-match "" t t))
      ;; Remove lines that contain only three or more asterisks (common hrules)
      (goto-char beg)
      (while (re-search-forward "^\\s-*\\*\\{3,\\}\\s-*$" end-marker t)
        (replace-match "" t t))
      ;; Remove a single stray leading space before "-" at BOL without altering nested list indentation.
      (goto-char beg)
      (while (re-search-forward "^ [-]" end-marker t)
        (replace-match "-" t t))
      ;; Perform replacements using the external `smart-quotes-replacement-pairs`
      (dolist (pair smart-quotes-replacement-pairs)
        (goto-char beg)
        (while (re-search-forward (regexp-quote (car pair)) end-marker t)
          (replace-match (cdr pair) t t))))))

(provide 'smart-quotes-replacements)

(defun replace-weird-spaces (beg end)
  "Replace NBSP, narrow NBSP, thin, hair, and zero‑width space with ASCII space.
Preserves ZWJ (U+200D) and ZWNJ (U+200C) for proper emoji rendering."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "[\u00A0\u202F\u2009\u200A\u200B]" end t)
      (replace-match " " t t))))

(defun ensure-heading-emoji-space (beg end)
  "Turn \"***🔁\" into \"*** 🔁\" in Org headings.

Looks at the first non‑space char right after the stars; if it’s
non‑ASCII (code‑point > 127) and there’s no space already, insert one."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(\\*+\\)\\([^[:space:]]\\)" end t)
      (let* ((stars  (match-string 1))
             (char   (match-string 2))
             (cp     (string-to-char char)))
        (when (and (> cp 127)
                   ;; only if we *haven’t* already got a space
                   (not (string-match-p " " (buffer-substring (match-beginning 0)
                                                              (match-end 0)))))
          (replace-match (concat stars " " char) t t))))))

(defun convert-markdown-headings-to-org (beg end)
  "Turn #, ##, ### etc. at bol into *, **, *** etc. in the region."
  (save-excursion
    (goto-char beg)
    (let ((limit (copy-marker end))
          (in-fence nil))
      (while (< (point) limit)
        (cond
         ((looking-at "^\\s-*```")
          (setq in-fence (not in-fence)))
         ((and (not in-fence)
               (looking-at "^\\s-*\\(#+\\)\\s-+"))
          (replace-match (concat (make-string (length (match-string 1)) ?*) " ")
                         t t)))
        (let ((before (point)))
          (forward-line 1)
          (when (<= (point) before)
            (goto-char limit)))))))

(defun convert-markdown-blockquotes-to-org (beg end)
  "Convert Markdown/email blockquote markers between BEG and END to Org quote lines.

Handles consecutive lines that start with optional indentation and \">\" (or
just \">\" alone).  Text lines receive an Org \": \" prefix; bare markers become
bare \":\" lines so paragraph breaks inside a quote remain visible."
  (save-excursion
    (goto-char beg)
    (let ((limit (copy-marker end)))
      (while (< (point) limit)
        (if (looking-at "^[ \t]*>\\(?:[ \t]\\|$\\)")
            (let ((block-start (point)))
              (while (and (< (point) limit)
                          (looking-at "^[ \t]*>\\(?:[ \t]\\|$\\)"))
                (forward-line 1))
              (let ((block-end (copy-marker (point))))
                (save-excursion
                  (goto-char block-start)
                  (while (< (point) block-end)
                    (when (looking-at "^[ \t]*>\\(?:[ \t]\\)?\\(.*\\)$")
                      (let ((content (match-string 1)))
                        (replace-match (if (string-empty-p content)
                                           ":"
                                         (concat ": " content))
                                       t t)))
                    (forward-line 1)))
                (set-marker block-end nil))))
        (forward-line 1))
      (set-marker limit nil))))

(defun pasteboard--strip-cite-markers (beg end)
  "Remove cite markers like [cite_start] or [cite: …] between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((limit (copy-marker end)))
      (while (re-search-forward "\\[cite[_:][^]]*\\]" limit t)
        (replace-match "" t t))
      ;; Collapse doubled spaces created by removals when they're between non-space chars.
      (goto-char beg)
      (while (re-search-forward "\\([^ \t\n]\\) \\{2,\\}\\([^ \t\n]\\)" limit t)
        (replace-match "\\1 \\2" t nil))
      (set-marker limit nil))))

(defun strip-cite-markers (beg end)
  "Interactively remove cite markers in the region or whole buffer.

If a region is active, operate on that region; otherwise, process the entire
buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
  (pasteboard--strip-cite-markers beg end)
  (message "Removed cite markers"))

(defun pasteboard--strip-trailing-whitespace (beg end)
  "Remove trailing whitespace from all lines between BEG and END.
Text copied from terminal UIs (e.g., Claude Code transcript panels) often has
lines padded with spaces to a fixed width.  This creates awkward formatting
when pasted into Org files.  Stripping trailing whitespace normalises the text."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "[ \t]+$" end t)
      (replace-match "" t t))))

(defun pasteboard--ensure-blank-line-before-headings (beg end)
  "Ensure there is a blank line before any Org heading between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\*+ " end t)
      (let ((pos (match-beginning 0)))
        (save-excursion
          (goto-char pos)
          (let* ((prev-is-heading (save-excursion
                                    (and (not (bobp))
                                         (forward-line -1)
                                         (looking-at-p "^\\*+ "))))
                 (prev-is-blank (save-excursion
                                  (and (not (bobp))
                                       (forward-line -1)
                                       (looking-at-p "^\\s-*$")))))
            (unless (or (bobp) prev-is-heading prev-is-blank)
              (goto-char pos)
              (insert "\n")
              (setq end (1+ end)))))))))

(defun pasteboard--remove-blank-line-after-headings (beg end)
  "Remove blank lines between headings and body text between BEG and END.
Preserves blank lines before headings and # directives (code blocks, properties)."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(\\*+ .+\\)\n\n+\\([^*\n#]\\)" end t)
      (replace-match "\\1\n\\2" t))))

(defun pasteboard--remove-redundant-heading-asterisks (beg end)
  "Remove redundant trailing asterisks from org headings between BEG and END.
Converts '** Heading **' to '** Heading', but preserves embedded emphasis."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(\\*+\\) \\(.*?\\)[ \\t]+\\*+[ \\t]*$" end t)
      (replace-match (concat (match-string 1) " " (match-string 2)) t t))))

(defun pasteboard--convert-markdown-inline-emphasis (beg end)
  "Convert inline Markdown emphasis between BEG and END to Org markup.

Single-asterisk italics become `/italic/` and double-asterisk bold becomes `*bold*`."
  (save-excursion
    (let ((case-fold-search nil))
      (goto-char beg)
      ;; Convert Markdown italics that are not part of bold markers.
      (while (re-search-forward "\\(^\\|[^*]\\)\\*\\(\\S-\\(?:[^*\n]*?\\S-\\)?\\)\\*\\([^*]\\|$\\)" end t)
        (let* ((prefix (match-string 1))
               (content (match-string 2))
               (suffix (match-string 3)))
          (unless (eq (aref content 0) ?*)
            (replace-match (concat prefix "/" content "/" suffix) nil t))))
      ;; Convert Markdown bold markers.
      (goto-char beg)
      (while (re-search-forward "\\(^\\|[^*]\\)\\*\\*\\(\\S-\\(?:[^*\n]*?\\S-\\)?\\)\\*\\*\\([^*]\\|$\\)" end t)
        (let* ((prefix (match-string 1))
               (content (match-string 2))
               (suffix (match-string 3)))
          (unless (eq (aref content 0) ?*)
            (replace-match (concat prefix "*" content "*" suffix) nil t)))))))

(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)

(defun safe-replace (pattern replacement beg end)
  "Safely replace PATTERN with REPLACEMENT between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((last-point beg))
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

(defun pasteboard-copy-adaptive ()
  "Smart copy to macOS pasteboard: choose verbatim vs. cleaned text.
With prefix argument (C-u), force verbatim copy."
  (interactive)
  (if current-prefix-arg
      (progn
        (call-interactively #'pasteboard-copy-verbatim)
        (message "Copied text verbatim (forced)"))
    (let* ((result
            (cond
             ;; ------------------------------------------ verbatim buckets ------------------------------------------
             ;; 1) Messages buffer - ALWAYS verbatim
             ((string= (buffer-name) "*Messages*")
              (cons "verbatim (Messages buffer)" #'pasteboard-copy-verbatim))
             ;; 2) Shell / Elisp / Web / Markdown / Backtrace
             ((or (eq major-mode 'sh-mode)
                  (eq major-mode 'emacs-lisp-mode)
                  (eq major-mode 'web-mode)
                  (eq major-mode 'markdown-mode)
                  (eq major-mode 'gfm-mode)
                  (derived-mode-p 'markdown-mode)
                  (derived-mode-p 'backtrace-mode))
              (cons "verbatim (mode match)" #'pasteboard-copy-verbatim))
             ;; 3) Org buffer **with** org-config-files-local-mode enabled
             ((and (eq major-mode 'org-mode)
                   (bound-and-true-p org-config-files-local-mode))
              (cons "verbatim (org-local)" #'pasteboard-copy-verbatim))
             ;; 4) Any file ending in .mdx
             ((and buffer-file-name
                   (string-match-p "\\.mdx\\'" buffer-file-name))
              (cons "verbatim (.mdx)" #'pasteboard-copy-verbatim))
             ;; 5) Any programming mode
             ((derived-mode-p 'prog-mode)
              (cons "verbatim (prog)" #'pasteboard-copy-verbatim))
             ;; ------------------------------------------ clean buckets ------------------------------------------
             ;; 6) Org or generic text (when org-config-files-local-mode is off)
             ((or (eq major-mode 'text-mode)
                  (and (eq major-mode 'org-mode)
                       (not (bound-and-true-p org-config-files-local-mode))))
              (cons "clean" #'pasteboard-copy-and-replace-em-dashes-in-clipboard))
             ;; ---------------------------------------- heuristic fallback --------------------------------------
             ((and (use-region-p)
                   (save-excursion
                     (goto-char (region-beginning))
                     (looking-at-p "\\s-*\\([({[]\\|[#;]\\|https?://\\)")))
              (cons "verbatim (heuristic)" #'pasteboard-copy-verbatim))
             (t
              (cons "clean (default)" #'pasteboard-copy-and-replace-em-dashes-in-clipboard))))
           (choice (car result))
           (handler (cdr result)))
      (when handler
        (call-interactively handler))
      (message "Copied text %s" choice))))

(defun pasteboard-copy ()
  "Copy region to OS X system pasteboard."
  (interactive)
  (let* ((txt (buffer-substring (region-beginning) (region-end))))
    (shell-command-to-string
     (format "echo -n %s | pbcopy" (shell-quote-argument txt)))))

(defun pasteboard-copy-verbatim (beg end)
  "Copy region between BEG and END to the macOS pasteboard verbatim.

Unlike the old echo→pbcopy helper, this uses Emacs' built-in
`gui-select-text`, so it follows the identical encoding path that
`kill-region` uses when `select-enable-clipboard` is non-nil."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  ;; Grab the bytes exactly as they live in the buffer.
  (let ((txt (buffer-substring-no-properties beg end)))
    ;; Same function `kill-region` calls under the hood.
    (gui-select-text txt))
  (message "Copied %d characters verbatim." (- end beg)))

(defun pasteboard-copy-and-replace-em-dashes-in-clipboard (&optional arg)
  "Copy region to macOS pasteboard.

No ARG → behave contextually (verbatim in code, replacement in text).
ARG positive or plain C-u → force verbatim.
ARG zero or negative       → force replacement."
  (interactive "P")
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((txt (buffer-substring-no-properties (region-beginning) (region-end)))
         ;; Decide which behaviour to use.
         (verbatim
          (cond
           ;; Forced by user
           (arg
            (> (prefix-numeric-value arg) 0))
           ;; Heuristic (original behaviour)
           (t (or (derived-mode-p 'prog-mode)
                  (eq major-mode 'shell-script-mode)
                  (eq major-mode 'emacs-lisp-mode)
                  (eq major-mode 'web-mode)
                  (bound-and-true-p org-config-files-local-mode))))))
    (with-temp-buffer
      (insert (if verbatim
                  txt
                  (replace-regexp-in-string "\\(---\\|--\\)" "—" txt)))
      (shell-command-on-region (point-min) (point-max) "pbcopy"))
    (message (if verbatim
                 "Copied text verbatim."
                 "Copied text with em dashes."))))

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

(setq select-enable-clipboard t)
(setq select-enable-primary t)

(defun org-insert-link-from-clipboard (beg end &optional url)
  "Replace text in region with an Org bracket link using the macOS clipboard URL."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected."))

  (let* ((url (string-trim (or url (pasteboard--clipboard-string))))
         (region-text (buffer-substring-no-properties beg end))
         (bracket-link (format "[[%s][%s]]" url region-text))
         (insert-beg (copy-marker (min beg end)))
         insert-end)
    ;;  (message "DEBUG: In `org-insert-link-from-clipboard`. region-text='%s', url='%s'" region-text url)
    (delete-region beg end)
    (goto-char insert-beg)
    (insert bracket-link)
    (setq insert-end (copy-marker (point) t))
    (cons insert-beg insert-end)))

(defun pasteboard--normalise-single-http-url (text)
  "Return one validated HTTP(S) URL from TEXT, or nil.
Whitespace and surrounding prose are rejected.  A leading `www.' is
deliberately supported and normalized to an explicit HTTPS URL."
  (when (and (stringp text)
             (not (string-empty-p text))
             (not (string-match-p "[ \t\n\r]" text)))
    (let* ((case-fold-search t)
           (candidate
            (cond
             ((string-match-p "\\`https?://" text) text)
             ((string-match-p "\\`www\\." text) (concat "https://" text))))
           (parsed (and candidate (url-generic-parse-url candidate)))
           (scheme (and parsed (url-type parsed)))
           (host (and parsed (url-host parsed))))
      (when (and (member (downcase (or scheme "")) '("http" "https"))
                 (stringp host)
                 (string-match-p
                  "^\\(?:\\[[0-9A-Fa-f:.]+\\]\\|[[:alnum:]][[:alnum:].-]*\\)$"
                  host))
        candidate))))

(defun is-org-roam-buffer-p ()
  "Check if the current buffer is an org-roam buffer by looking for ID property at the beginning."
  (save-excursion
    (goto-char (point-min))
    (and (eq major-mode 'org-mode)
         (looking-at-p "^:PROPERTIES:\n:ID:\\s-+[^\n]+\n:END:"))))

(defun demote-org-headings-in-region (beg end)
  "Demote all Org headings in the region between BEG and END by one level."
  (save-excursion
    (let ((end-marker (copy-marker end)))
      (goto-char beg)
      ;; An Org heading requires a space after its leading stars.  Without
      ;; that delimiter, bold metadata such as "*User:* Anonymous" was
      ;; mistaken for a heading and changed to "**User:* Anonymous".
      (while (re-search-forward "^\\(\\*+\\) " end-marker t)
        (let ((stars (match-string 1)))
          (replace-match (concat "*" stars " ") t t)))
      (set-marker end-marker nil))))

(defun demote-org-headings-adaptive ()
  "Demote all Org headings by one level.
If a region is active, demote only headings within the region.
Otherwise, demote from point to the end of the buffer."
  (interactive)
  (let ((beg (if (use-region-p) (region-beginning) (point)))
        (end (if (use-region-p) (region-end) (point-max))))
    (demote-org-headings-in-region beg end)
    (message "Demoted all headings in %s"
             (if (use-region-p) "region" "buffer from point to end"))))

(defun pasteboard--clipboard-string ()
  "Return the current macOS clipboard as a normalised string."
  (shell-command-to-string "pbpaste | perl -p -e 's/\\r$//' | tr '\\r' '\\n'"))

(defun convert-markdown-links-to-org-mode (beg end)
  "Convert [label](url) style links in region to Org [[url][label]] links."
  (interactive "r")
  (save-excursion
    (let ((end-marker (copy-marker end)))
      (goto-char beg)
      (while (re-search-forward "\\[\\([^][]+\\)\\](\\([^()]+\\))" end-marker t)
        (replace-match "[[\\2][\\1]]" t))
      (set-marker end-marker nil))))

(defun pasteboard--next-fence-placeholder (text counter)
  "Return a deterministic fence placeholder absent from TEXT, starting at COUNTER."
  (let (placeholder)
    (while (or (null placeholder) (string-match-p (regexp-quote placeholder) text))
      (setq placeholder (format "PASTEBOARDFENCEPROTECTED%06d" counter)
            counter (1+ counter)))
    (cons placeholder counter)))

(defun pasteboard--protect-org-source-blocks (text)
  "Replace Org source blocks in TEXT with inert placeholders.
Both closed and unclosed blocks are restored byte-for-byte after prose cleanup."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (counter 0)
          replacements)
      (while (re-search-forward "^[ 	]*#\\+begin_src\\b.*$" nil t)
        (let* ((block-beg (match-beginning 0))
               (_ (forward-line 1))
               (closed (re-search-forward "^[ 	]*#\\+end_src[ 	]*$" nil t))
               (block-end
                (if closed
                    (progn (forward-line 1) (point))
                  (point-max)))
               (original (buffer-substring-no-properties block-beg block-end))
               (ends-in-newline (string-suffix-p "\n" original))
               (placeholder-data
                (pasteboard--next-fence-placeholder (buffer-string) counter))
               (placeholder (car placeholder-data))
               (final-text (if ends-in-newline
                               (substring original 0 -1)
                             original)))
          (setq counter (cdr placeholder-data))
          (push (cons placeholder final-text) replacements)
          (delete-region block-beg block-end)
          (goto-char block-beg)
          (insert placeholder (if ends-in-newline "\n" ""))))
      (list :text (buffer-string)
            :replacements (nreverse replacements)))))

(defun pasteboard--protect-markdown-fences (text)
  "Replace Markdown fenced blocks in TEXT with inert placeholders.

Return a plist containing the protected text and a placeholder-to-final-text
alist.  Closed backtick and tilde fences are converted to Org source-block
delimiters in their final text; their contents are preserved byte-for-byte.
An unclosed fence is protected during cleaning but restored entirely unchanged."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((counter 0)
          replacements)
      (while (re-search-forward
              "^\\([ \t]*\\)\\(`\\{3,\\}\\|~\\{3,\\}\\)\\(.*\\)$" nil t)
        (let* ((block-beg (match-beginning 0))
               (open-indent (match-string-no-properties 1))
               (fence (match-string-no-properties 2))
               (fence-char (aref fence 0))
               (fence-length (length fence))
               (info (string-trim (match-string-no-properties 3))))
          ;; A backtick info string containing a backtick is not a CommonMark
          ;; opening fence.  Continue on the next line instead of protecting it.
          (if (and (= fence-char ?`) (string-match-p "`" info))
              (forward-line 1)
            (let* ((open-line-end (line-end-position))
                   (_ (forward-line 1))
                   (content-beg (point))
                   (closing-regexp
                    (format "^\\([ \t]*\\)%s\\{%d,\\}[ \t]*$"
                            (regexp-quote (char-to-string fence-char))
                            fence-length))
                   (closed (re-search-forward closing-regexp nil t))
                   (close-beg (and closed (match-beginning 0)))
                   (close-indent (and closed (match-string-no-properties 1)))
                   (close-line-end (and closed (line-end-position)))
                   (block-end
                    (if closed
                        (progn (forward-line 1) (point))
                      (point-max)))
                   (original
                    (buffer-substring-no-properties block-beg block-end))
                   (language (car (split-string info "[ \t]+" t)))
                   (converted
                    (if closed
                        (concat open-indent "#+begin_src"
                                (if language (concat " " language) "")
                                (buffer-substring-no-properties open-line-end content-beg)
                                (buffer-substring-no-properties content-beg close-beg)
                                close-indent "#+end_src"
                                (buffer-substring-no-properties close-line-end block-end))
                      original))
                   (ends-in-newline (string-suffix-p "\n" original))
                   (placeholder-data
                    (pasteboard--next-fence-placeholder (buffer-string) counter))
                   (placeholder (car placeholder-data))
                   (final-text (if ends-in-newline
                                   (substring converted 0 -1)
                                 converted)))
              (setq counter (cdr placeholder-data))
              (push (cons placeholder final-text) replacements)
              (delete-region block-beg block-end)
              (goto-char block-beg)
              (insert placeholder (if ends-in-newline "\n" ""))))))
      (list :text (buffer-string)
            :replacements (nreverse replacements)))))

(defun pasteboard--restore-markdown-fences (replacements)
  "Restore protected Markdown fence REPLACEMENTS in the current buffer."
  (dolist (replacement replacements)
    (goto-char (point-min))
    (unless (search-forward (car replacement) nil t)
      (error "Missing protected fence placeholder: %s" (car replacement)))
    (replace-match (cdr replacement) t t)))

(defun convert-markdown-to-org-code-blocks-simple ()
  "Convert closed Markdown fences to Org source blocks without changing code.
Both backtick and tilde fences are supported.  Unclosed fences remain unchanged."
  (interactive)
  (let* ((protected (pasteboard--protect-markdown-fences
                     (buffer-substring-no-properties (point-min) (point-max))))
         (protected-text (plist-get protected :text))
         (replacements (plist-get protected :replacements)))
    (erase-buffer)
    (insert protected-text)
    (pasteboard--restore-markdown-fences replacements)))

(defun pasteboard--analyse-clipboard-text (text)
  "Return a plist describing TEXT, detecting whether it looks like Markdown or Org."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((line-number 1)
          (heading-lines '()))
      (while (not (eobp))
        (when (looking-at "^[ \t]*#[ \t]+")
          (push line-number heading-lines))
        (forward-line 1)
        (cl-incf line-number))
      (let ((md-score 0)
            (org-score 0)
            (asterisk-bullets 0)
            (case-fold-search nil))
        (dolist (spec
                 '((md-score "^[ \t]*#\\{1,6\\} " 3)
                   (md-score "^[ \t]*[-+] \\S-" 1)
                   (md-score "^[ \t]*\\d+\\. \\S-" 1)
                   (md-score "^[ \t]*> " 1)
                   (md-score "^[ \t]*`\\{3\\}" 2)
                   (md-score "\\[[^][]+\\](https?://[^)]+)" 1)
                   (md-score "\\*\\*[^*\n]+\\*\\*" 3)  ; inline **bold**
                   (org-score "^[ \t]*#\\+" 3)
                   (org-score "^[ \t]*\\*+ \\(TODO\\|DONE\\|NEXT\\|WAIT\\|HOLD\\|CANCELLED\\|NOTE\\|IDEA\\|FIXME\\|PROJ\\)\\b" 3)
                   (org-score "^[ \t]*\\*+ .*:[[:alnum:]_@#%:]+:[ \t]*$" 2)
                   (org-score "^[ \t]*:PROPERTIES:[ \t]*$" 3)
                   (org-score "^[ \t]*:END:[ \t]*$" 1)
                   (org-score "^[ \t]*SCHEDULED:" 2)
                   (org-score "^[ \t]*DEADLINE:" 2)
                   (org-score "^[ \t]*\\*\\{2,\\} \\S-" 2)))
          (pcase-let ((`(,target ,regex ,weight) spec))
            (goto-char (point-min))
            (while (re-search-forward regex nil t)
              (pcase target
                ('md-score  (cl-incf md-score weight))
                ('org-score (cl-incf org-score weight))))))
        ;; Classify single-asterisk lines by neighbor context:
        ;; indented or list-neighbored → bullet; surrounded by paragraph
        ;; text → likely an Org heading, score as Org.
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]*\\)\\* \\S-" nil t)
          (let* ((pos (match-beginning 0))
                 (indent (length (match-string 1)))
                 (prev (pasteboard--neighbor-nonblank-kind pos 'prev))
                 (next (pasteboard--neighbor-nonblank-kind pos 'next))
                 (listish '(asterisk-bullet dash-bullet ordered-bullet)))
            (if (or (> indent 0)
                    (memq prev listish)
                    (memq next listish))
                (cl-incf asterisk-bullets)
                (cl-incf org-score 1))))
        (let ((style
               (cond
                ((>= md-score (+ org-score 2)) 'markdown)
                ((>= org-score (+ md-score 2)) 'org)
                ((and (>= md-score 3) (= org-score 0)) 'markdown)
                ((and (= md-score 0) (> org-score 0)) 'org)
                ((and (>= asterisk-bullets 3) (= org-score 0)) 'markdown)
                (t nil))))
          (list
           :style style
           :markdown-heading-lines (nreverse heading-lines)
           :md-score md-score
           :org-score org-score
           :asterisk-bullets asterisk-bullets))))))

(defun pasteboard--line-kind-at-pos (pos)
  "Return the syntactic 'kind' of line at POS."
  (save-excursion
    (goto-char pos)
    (cond
     ((looking-at "^\\s-*$") 'blank)
     ((looking-at "^[ \t]*\\*\\{2,\\} \\S-") 'org-heading)
     ((looking-at "^[ \t]*\\* \\S-") 'asterisk-bullet)
     ((looking-at "^[ \t]*[-+] \\S-") 'dash-bullet)
     ((looking-at "^[ \t]*\\d+\\. \\S-") 'ordered-bullet)
     ((looking-at "^[ \t]*:") 'definition)
     (t 'text))))

(defun pasteboard--neighbor-nonblank-kind (pos direction)
  "Return the first non-blank line kind from POS in DIRECTION (`prev or `next)."
  (save-excursion
    (goto-char pos)
    (let ((step (pcase direction
                  ('next 1)
                  ('prev -1)
                  (_ (error "Unknown direction %S" direction)))))
      (catch 'result
        (while t
          (let ((status (forward-line step)))
            (when (/= status 0)
              (throw 'result 'none))
            (forward-line 0)
            (unless (looking-at "^\\s*$")
              (throw 'result (pasteboard--line-kind-at-pos (point))))))))))

(defun pasteboard--should-convert-asterisk-line (pos indent skip-lines)
  "Heuristic to decide whether the single-asterisk line at POS should become a dash bullet.
INDENT is the number of leading spaces.  SKIP-LINES is a list of line numbers preserved as headings."
  (let ((line-no (line-number-at-pos pos)))
    (unless (and skip-lines (memq line-no skip-lines))
      (if (> indent 0)
          t
          (let* ((prev-kind (pasteboard--neighbor-nonblank-kind pos 'prev))
                 (next-kind (pasteboard--neighbor-nonblank-kind pos 'next))
                 (listish '(asterisk-bullet dash-bullet ordered-bullet definition)))
            (or (memq prev-kind listish)
                (memq next-kind listish)))))))

(defun pasteboard--convert-asterisk-bullets-to-dashes (beg end &optional skip-lines)
  "Convert Markdown-style leading '*' bullets to '-' between BEG and END.
SKIP-LINES is a list of 1-based line numbers that should remain untouched."
  (let (targets)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\([ \t]*\\)\\* \\(.*\\)$" end t)
        (let* ((line-beg (match-beginning 0))
               (line-end (match-end 0))
               (indent-str (match-string 1))
               (body (match-string 2))
               (indent (length indent-str)))
          (when (pasteboard--should-convert-asterisk-line line-beg indent skip-lines)
            (push (list line-beg line-end indent-str body) targets)))))
    (save-excursion
      (dolist (target targets)
        (pcase-let ((`(,line-beg ,line-end ,indent-str ,body) target))
          (goto-char line-beg)
          (delete-region line-beg line-end)
          (insert indent-str "- " body))))))

(defun pasteboard--tighten-markdown-table-separators (beg end)
  "Tighten Markdown table separator rows between BEG and END for Org tables.
Transforms lines like \"| --- | --- |\" into \"|---|---|\" while leaving data rows alone."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let* ((line-beg (line-beginning-position))
             (line-end (line-end-position))
             (line (buffer-substring-no-properties line-beg line-end)))
        (when (and (string-match-p "\\`\\s-*|.*|\\s-*\\'" line)
                   (string-match-p "-" line))
          (let* ((indent (progn (string-match "^\\s-*" line)
                                (match-string 0 line)))
                 (content (substring line (length indent)))
                 (cells (split-string content "|" t))
                 (trimmed-cells (mapcar #'string-trim cells)))
            (when (and trimmed-cells
                       (cl-every (lambda (cell)
                                   (and (not (string-empty-p cell))
                                        (string-match-p "\\`[: -]+\\'" cell)
                                        (string-match-p "-" cell)))
                                 trimmed-cells))
              (let ((tight (concat "|" (mapconcat #'identity trimmed-cells "|") "|")))
                (unless (string= content tight)
                  (goto-char line-beg)
                  (delete-region line-beg line-end)
                  (insert indent tight)
                  (setq line-end (line-end-position)))))))
        (goto-char line-end)
        (forward-line 1)
        (setq end (point-max))))))

(defun pasteboard--clean-string (text &optional style-override)
  "Return cleaned TEXT for Org/text pastes.
This function is pure text transformation and does not insert into buffers.
When STYLE-OVERRIDE is `org' or `markdown', use that syntax path instead of
the clipboard analyser's result."
  (let* ((protected-org-blocks (pasteboard--protect-org-source-blocks text))
         (protected-fences
          (pasteboard--protect-markdown-fences
           (plist-get protected-org-blocks :text)))
         (protected-text (plist-get protected-fences :text))
         (fence-replacements
          (append (plist-get protected-org-blocks :replacements)
                  (plist-get protected-fences :replacements)))
         (analysis (pasteboard--analyse-clipboard-text protected-text))
         (style (or style-override (plist-get analysis :style)))
         (heading-line-numbers (plist-get analysis :markdown-heading-lines)))
    (with-temp-buffer
      (insert protected-text)
      (let* ((beg (point-min))
             (end (point-max))
             (heading-markers
              (when heading-line-numbers
                (save-excursion
                  (goto-char beg)
                  (let ((current-line 1)
                        (markers nil))
                    (dolist (target heading-line-numbers)
                      (forward-line (- target current-line))
                      (push (copy-marker (line-beginning-position)) markers)
                      (setq current-line target))
                    (nreverse markers))))))
        (unwind-protect
            (progn
              ;; Convert Markdown headings first, but only when the style
              ;; analyser resolved the payload as Markdown.  In Org input,
              ;; a line such as "# #+LATEX_HEADER: ..." is a comment.
              (when (eq style 'markdown)
                (convert-markdown-headings-to-org beg end))
              (replace-smart-quotes beg end)
              (replace-smart-quotes-regexp beg end)
              (replace-weird-spaces beg end)
              (pasteboard--strip-cite-markers beg end)
              (convert-markdown-blockquotes-to-org beg end)
              (convert-markdown-links-to-org-mode beg end)
              ;; Buffer size may have changed; refresh region bounds before further narrowing.
              (setq beg (point-min)
                    end (point-max))
              (when (eq style 'markdown)
                (save-restriction
                  (narrow-to-region beg end)
                  (let* ((region-beg (point-min))
                         (region-end (point-max))
                         (skip-lines (when heading-markers
                                       (mapcar #'line-number-at-pos heading-markers))))
                    (when pasteboard-convert-markdown-inline-emphasis
                      (pasteboard--convert-markdown-inline-emphasis region-beg region-end))
                    (pasteboard--convert-asterisk-bullets-to-dashes region-beg region-end skip-lines)
                    ;; Fallback: if any single-asterisk lines slipped through,
                    ;; coerce them now — but only when the neighbor heuristic
                    ;; agrees they are bullets, not Org headings.
                    (goto-char region-beg)
                    (while (re-search-forward "^\\([ \t]*\\)\\* \\(.*\\)$" region-end t)
                      (let ((match-beg (match-beginning 0))
                            (indent (length (match-string 1))))
                        (when (pasteboard--should-convert-asterisk-line match-beg indent skip-lines)
                          (replace-match "\\1- \\2" t))))
                    (pasteboard--tighten-markdown-table-separators region-beg region-end)
                    (when (fboundp 'normalize-dashes)
                      (normalize-dashes)))))
              ;; Strip trailing whitespace last (after all content transforms) to
              ;; clean up fixed-width padding from terminal UI copies.
              (pasteboard--strip-trailing-whitespace (point-min) (point-max))
              (pasteboard--ensure-blank-line-before-headings (point-min) (point-max))
              ;; Remove blank lines between headings and body text
              (pasteboard--remove-blank-line-after-headings (point-min) (point-max))
              ;; Remove redundant asterisks from headings (e.g., "** Heading **" -> "** Heading")
              (pasteboard--remove-redundant-heading-asterisks (point-min) (point-max))
              ;; Restore fenced contents only after every prose transformation.
              (pasteboard--restore-markdown-fences fence-replacements)
              (buffer-string))
          (when heading-markers
            (mapc (lambda (marker) (set-marker marker nil)) heading-markers)))))))

(defun pasteboard--demote-headings-in-string (text)
  "Demote all Org headings found in TEXT by one level."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\) " nil t)
      (let ((stars (match-string 1)))
        (replace-match (concat "*" stars " ") t t)))
    (buffer-string)))

(defun pasteboard-paste-adaptive ()
  "Paste from the macOS pasteboard, choosing behaviour based on context while keeping Org's cache stable.
With prefix argument (C-u), force verbatim paste.
All non-verbatim cleaned paths delegate to `pasteboard-paste-clean' so there
is one obvious clean pipeline."
  (interactive)
  ;; If prefix arg, force verbatim
  (if current-prefix-arg
      (progn
        (prog1 (pasteboard-paste-verbatim (pasteboard--clipboard-string))
          (message "Pasted: verbatim (forced)")))
      ;; Otherwise, smart paste
      (let* ((clipboard-raw (pasteboard--clipboard-string))
             (trimmed (string-trim clipboard-raw))
             (exact-url (pasteboard--normalise-single-http-url trimmed))
             choice
             inserted-range)
        (cond
         ;; Programming and verbatim modes route before link insertion.  An
         ;; exact URL over a selection must never inject Org syntax into code.
         ((or (eq major-mode 'sh-mode)
              (eq major-mode 'python-mode)
              (eq major-mode 'emacs-lisp-mode)
              (eq major-mode 'markdown-mode)
              (eq major-mode 'gfm-mode)
              (derived-mode-p 'markdown-mode)
              (derived-mode-p 'prog-mode)
              (eq major-mode 'web-mode)
              (eq major-mode 'fundamental-mode))
          (setq choice "verbatim")
          (setq inserted-range (pasteboard-paste-verbatim clipboard-raw)))
         ((and (use-region-p)
               (eq major-mode 'org-mode)
               (not (bound-and-true-p org-config-files-local-mode))
               exact-url)
          (setq choice "bracket-link")
          (setq inserted-range
                (org-insert-link-from-clipboard
                 (region-beginning) (region-end) exact-url)))
         ((or (and (eq major-mode 'org-mode)
                   (not (bound-and-true-p org-config-files-local-mode)))
              (derived-mode-p 'text-mode))
          (setq choice "clean")
          ;; One obvious clean path: adaptive clean inserts go through pasteboard-paste-clean.
          (setq inserted-range (pasteboard-paste-clean nil clipboard-raw)))
         (t
          (let* ((prev-char (char-before))
                 (next-char (char-after))
                 (char-set '(?: ?' ?\( ?\) ?| ?\[ ?\] ?/ ?\\ ?\" ?= ?< ?> ?{ ?}))
                 (use-no-spaces (or (member prev-char char-set)
                                    (member next-char char-set))))
            (if use-no-spaces
                (progn
                  (setq choice "paste-raw")
                  (setq inserted-range (pasteboard-paste-verbatim clipboard-raw)))
              (setq choice "paste-clean")
              ;; Keep fallback clean behavior aligned with pasteboard-paste-clean.
              (setq inserted-range (pasteboard-paste-clean nil clipboard-raw))))))
        (when choice
          (message "Pasted: %s" choice))
        inserted-range)))

(defun pasteboard-paste (&optional text)
  "Paste TEXT (or the current clipboard) at point, normalising whitespace."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (region-end) (point)))
         (ins-text (or text (pasteboard--clipboard-string)))
         (insert-beg (copy-marker start))
         insert-end)
    (combine-after-change-calls
      (atomic-change-group
        (delete-region start end)
        (goto-char insert-beg)
        (insert ins-text)
        (setq insert-end (copy-marker (point) t))
        (let ((paste-end insert-end))
          (my/fix-space)
          (save-excursion
            (goto-char insert-beg)
            (my/fix-space))
          (goto-char paste-end))))
    (cons insert-beg insert-end)))

(defun pasteboard-paste-clean (&optional raw text style-override)
  "Canonical clean paste path for clipboard text.
When RAW is non-nil, bypass cleaning.  Otherwise run the clean-string pipeline,
then insert in a single edit.  STYLE-OVERRIDE is passed to the clean pipeline."
  (interactive "P")
  (let* ((source (or text (pasteboard--clipboard-string)))
         (insert-text (if raw source
                        (pasteboard--clean-string source style-override))))
    (pasteboard-paste insert-text)))

(defun pasteboard-paste-verbatim (&optional text)
  "Paste verbatim text at point, bypassing smart cleanup."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (region-end) (point)))
         (ins-text (or text (pasteboard--clipboard-string)))
         (insert-beg (copy-marker start))
         insert-end)
    (combine-after-change-calls
      (atomic-change-group
        (delete-region start end)
        (goto-char insert-beg)
        (insert ins-text)
        (setq insert-end (copy-marker (point) t))))
    (cons insert-beg insert-end)))

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

(defun pasteboard-paste-adjusted-subtrees-adaptive ()
  "Clean and paste an Org subtree, then adjust only its inserted headings.
The explicit Org syntax path preserves consecutive single-star sibling headings
instead of routing them through the ambiguous Markdown bullet heuristic."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Adjusted subtree paste requires an Org buffer"))
  (let* ((insertion-point (if (use-region-p) (region-beginning) (point)))
         (current-level
          (save-excursion
            (goto-char insertion-point)
            (if (org-before-first-heading-p)
                0
              (or (org-current-level)
                  (progn
                    (org-back-to-heading t)
                    (org-current-level))
                  0))))
         ;; This command explicitly expects Org subtree syntax.  It still gets
         ;; punctuation, link, quote, and whitespace cleanup, but never the
         ;; Markdown single-asterisk bullet conversion.
         (inserted-range
          (pasteboard-paste-clean nil (pasteboard--clipboard-string) 'org))
         (paste-beg (car inserted-range))
         (paste-end (cdr inserted-range)))
    (save-excursion
      (goto-char paste-beg)
      (let (min-level)
        (while (re-search-forward "^\\(\\*+\\) " paste-end t)
          (let ((level (length (match-string 1))))
            (when (or (null min-level) (< level min-level))
              (setq min-level level))))
        (when min-level
          (let ((shift (- (1+ current-level) min-level)))
            (unless (zerop shift)
              (goto-char paste-beg)
              (while (re-search-forward "^\\(\\*+\\) " paste-end t)
                (let* ((level (length (match-string 1)))
                       (new-level (max 1 (+ level shift))))
                  (replace-match (concat (make-string new-level ?*) " ")
                                 t t))))))))
    (message "Pasted with adjusted heading levels")
    inserted-range))

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

(defun pasteboard-cut-adaptive ()
  "Smart cut to macOS pasteboard: uses pasteboard-copy-adaptive logic, then deletes region.
With prefix argument (C-u), force verbatim cut."
  (interactive)
  ;; If prefix arg, force verbatim
  (if current-prefix-arg
      (progn
        (call-interactively #'pasteboard-copy-verbatim)
        (delete-region (region-beginning) (region-end))
        (my/fix-space)
        (message "Cut text verbatim (forced)"))
      ;; Otherwise, use smart copy then delete
      (let ((was-text-mode (or (and (eq major-mode 'org-mode)
                                    (not (bound-and-true-p org-config-files-local-mode)))
                               (derived-mode-p 'text-mode))))
        ;; Use the adaptive copy which already has all the smart logic
        (call-interactively #'pasteboard-copy-adaptive)
        ;; Delete the region
        (delete-region (region-beginning) (region-end))
        ;; Fix spacing
        (my/fix-space)
        ;; Capitalize at beginning of sentence (text modes only)
        (when was-text-mode
          (save-excursion
            (when (my/beginning-of-sentence-p)
              (capitalize-unless-org-heading)))))))

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
           (pasteboard-paste-verbatim)
           (buffer-string))))
    (wrapped-search-forward search-term)))

(setq select-enable-clipboard t)

(defun push-kill-ring-pasteboard-to-MacOS-clipboard ()
  (interactive)
  (gui-select-text (current-kill 0)))

(defun push-MacOS-clipboard-to-kill-ring ()
  "Push the content of the MacOS clipboard to the Emacs kill ring."
  (interactive)
  (let ((clipboard-content (shell-command-to-string "pbpaste")))
    (when (and clipboard-content (not (string= clipboard-content "")))
      (kill-new clipboard-content)
      (message "Pushed clipboard content to kill ring: %s" clipboard-content))))

(defun clipboard-to-kill-ring ()
  "Copy clipboard contents directly to the kill ring."
  (interactive)
  (let ((clipboard-content (shell-command-to-string "pbpaste")))
    (unless (string-equal clipboard-content "")
      (kill-new clipboard-content))))

(defun clipboard-to-kill-ring-and-strip-trailing-newlines ()
  "Strip trailing newline from clipboard content before copying to kill ring."
  (interactive)
  (let ((clipboard-content (shell-command-to-string "pbpaste")))
    (when clipboard-content
      (setq clipboard-content
            (if (string-match "\n\\'" clipboard-content)
                (replace-match "" t t clipboard-content)
              clipboard-content))
      (unless (string-equal clipboard-content "")
        (kill-new clipboard-content)))))

(defun copy-region-as-kill-and-push-to-clipboard (beg end)
  "Copy region BEG..END as kill and push it to the macOS clipboard."
  (interactive "r")
  (copy-region-as-kill beg end)
  (push-kill-ring-pasteboard-to-MacOS-clipboard))

(defun yas/pasteboard-raw ()
  "Return content of OS X system pasteboard via `pbpaste'."
  (shell-command-to-string "pbpaste | perl -p -e 's/\r$//' | tr '\r' '\n'"))

(defun pasteboard-copy-large-file ()
  "Copy the current region to the pasteboard using a temporary file."
  (interactive)
  (let* ((txt (buffer-substring (region-beginning) (region-end)))
         (temp-file (make-temp-file "emacs-pasteboard")))
    (with-temp-file temp-file
      (insert txt))
    (shell-command-to-string
     (format "cat %s | pbcopy" (shell-quote-argument temp-file)))
    (delete-file temp-file)))

(defun gist-buffer-to-pasteboard ()
  (interactive)
  (gist-buffer)
  (push-kill-ring-pasteboard-to-MacOS-clipboard)
  )
