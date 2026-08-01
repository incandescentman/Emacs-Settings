;;; capture-cleanup-tests.el --- ERT regression tests for capture cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for `jay/org-web-tools-cleanup-string` so heading/timestamp
;; and spacing cleanup regressions are caught quickly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defconst jay-capture-cleanup-test--shared-functions-file
  (expand-file-name
   "shared-functions.el"
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to `shared-functions.el` in this repo.")

(defconst jay-capture-cleanup-test--pasteboard-file
  (expand-file-name
   "pasteboard-copy-and-paste-functions.el"
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to the pasteboard whitespace cleaner in this repo.")

(load jay-capture-cleanup-test--pasteboard-file nil 'nomessage)

(defun jay-capture-cleanup-test--load-definitions (file symbols)
  "Load defuns/defvars named in SYMBOLS from FILE without loading the full config."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (condition-case nil
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (memq (car form) '(defun defvar defcustom))
                         (memq (nth 1 form) symbols))
                (eval form t)))
          (end-of-file
           (setq done t)))))))

(defun jay-capture-cleanup-test--ensure-target-loaded ()
  "Ensure the capture cleanup function under test is available."
  (unless (fboundp 'jay/org-web-tools-cleanup-string)
    (jay-capture-cleanup-test--load-definitions
     jay-capture-cleanup-test--shared-functions-file
     '(jay/org-web-tools-include-timestamp
       jay/org-web-tools-request-headers
       jay/org-web-tools-cleanup-string
       jay/org-web-tools--plz-error-object
       jay/org-web-tools--plz-error-status
       jay/org-web-tools--capture-error-entry
       jay/org-web-tools--url-as-readable-org))))

(jay-capture-cleanup-test--ensure-target-loaded)

(ert-deftest jay-capture-cleanup-removes-timestamp-by-default ()
  "Default cleanup should strip capture timestamps."
  (let ((jay/org-web-tools-include-timestamp nil))
    (should
     (equal
      (jay/org-web-tools-cleanup-string
       "[2026-02-10 Tue 15:14]\n*** Title\nBody\n")
      "*** Title\nBody\n"))))

(ert-deftest jay-capture-cleanup-preserves-timestamp-when-enabled ()
  "Timestamp should remain when the opt-in variable is non-nil."
  (let ((jay/org-web-tools-include-timestamp t))
    (should
     (string-prefix-p
      "[2026-02-10 Tue 15:14]\n"
      (jay/org-web-tools-cleanup-string
       "[2026-02-10 Tue 15:14]\n*** Title\nBody\n")))))

(ert-deftest jay-capture-cleanup-strips-social-share-links ()
  "Cleanup should remove known social-share and mailto links."
  (let* ((input
          (mapconcat
           #'identity
           '("[[https://twitter.com/intent/tweet?url=abc][Share on X]]"
             "[[https://www.facebook.com/sharer/sharer.php?u=abc][Share on Facebook]]"
             "[[mailto:?subject=Check%20this][Email]]"
             "[[https://example.com/post][Keep me]]")
           "\n"))
         (output (jay/org-web-tools-cleanup-string input)))
    (should (string-match-p (regexp-quote "[[https://example.com/post][Keep me]]") output))
    (should-not (string-match-p "twitter\\.com/intent" output))
    (should-not (string-match-p "facebook\\.com/sharer" output))
    (should-not (string-match-p "\\[\\[mailto:\\?subject=" output))))

(ert-deftest jay-capture-cleanup-strips-view-count-and-relative-time ()
  "Cleanup should strip view-count and relative-time lines."
  (let ((output
         (jay/org-web-tools-cleanup-string
          (mapconcat
           #'identity
           '("//6,728"
             "2 years ago"
             "an hour ago"
             "Main body")
           "\n"))))
    (should-not (string-match-p "^//[0-9,]+" output))
    (should-not (string-match-p "years? ago" output))
    (should-not (string-match-p "hours? ago" output))
    (should (string-match-p "Main body" output))))

(ert-deftest jay-capture-cleanup-fixes-heading-body-and-whitespace-spacing ()
  "Cleanup should remove heading-body blank gaps and whitespace-only lines."
  (should
    (equal
     (jay/org-web-tools-cleanup-string
      "*** Heading\n\nBody line\n   \n\t\n*** Next\n\n#+begin_src emacs-lisp\n(message \"x\")\n#+end_src\n")
     "*** Heading\nBody line\n*** Next\n\n#+begin_src emacs-lisp\n(message \"x\")\n#+end_src\n")))

(ert-deftest jay-capture-cleanup-normalizes-weird-spaces ()
  "Web captures should share the clean-paste Unicode-space policy."
  (should
   (equal
    (jay/org-web-tools-cleanup-string
     "A\u00A0group\u202Fof\u2009children\u200Aand\u200Btext")
    "A group of children and text")))

(ert-deftest jay-org-web-tools-readable-fetch-sends-browser-headers ()
  "Jay's org-web-tools wrapper should send browser-like request headers."
  (let (seen-headers)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest args)
                 (setq seen-headers (plist-get args :headers))
                 'fake-dom))
              ((symbol-function 'org-web-tools--eww-readable)
               (lambda (_dom)
                 (cons "Example Page" "<main>Article body</main>")))
              ((symbol-function 'org-web-tools--cleanup-title)
               #'identity)
              ((symbol-function 'org-web-tools--html-to-org-with-pandoc)
               (lambda (_html)
                 "Article body\n"))
              ((symbol-function 'org-web-tools--demote-headings-below)
               (lambda (&rest _args)
                 nil)))
      (let ((output (jay/org-web-tools--url-as-readable-org "https://example.com/post")))
        (should (string-match-p "Mozilla/5.0" (cdr (assoc "User-Agent" seen-headers))))
        (should (string-match-p "\\* \\[\\[https://example.com/post\\]\\[Example Page\\]\\]" output))
        (should (string-match-p "\\*\\* Article" output))))))

(ert-deftest jay-org-web-tools-readable-fetch-falls-back-on-plz-error ()
  "A blocked HTTP fetch should produce a minimal Org entry, not a debugger."
  (unless (get 'plz-error 'error-conditions)
    (define-error 'plz-error "plz error" 'error))
  (cl-letf (((symbol-function 'plz)
             (lambda (&rest _args)
               (signal 'plz-error '("HTTP error")))))
    (let ((output (jay/org-web-tools--url-as-readable-org "https://example.com/blocked")))
      (should (string-match-p "Capture failed" output))
      (should (string-match-p "\\[\\[https://example.com/blocked\\]" output)))))

(provide 'capture-cleanup-tests)

;;; capture-cleanup-tests.el ends here
