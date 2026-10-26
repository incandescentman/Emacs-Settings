;;; capture-cleanup-tests.el --- ERT regression tests for capture cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for `jay/org-web-tools-cleanup-string` so heading/timestamp
;; and spacing cleanup regressions are caught quickly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'plz)

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
       jay/org-web-tools--repair-linked-images
       jay/org-web-tools--normalize-paragraph-breaks
       jay/org-web-tools-extractor
       jay/org-web-tools-extractor-script
       jay/org-web-tools--defuddle-html
       jay/org-web-tools--extract-readable
       jay/org-web-tools-cleanup-string
       jay/org-web-tools--plz-error-object
       jay/org-web-tools--plz-error-status
       jay/org-web-tools--capture-error-message
       jay/org-web-tools--url-as-readable-org
       jay/get-url-from-clipboard-or-chrome
       jay/org-web-tools-insert-web-page-as-entry-clean))))

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
              ((symbol-function 'jay/org-web-tools--extract-readable)
               (lambda (_html _url)
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

(ert-deftest jay-org-web-tools-chatgpt-failure-stops-without-clipboard-fallback ()
  "The reported ChatGPT challenge must stop capture without changing the note."
  (let* ((url "https://chatgpt.com/c/6a7df428-2e28-83ea-9e32-abd9100d68cd")
         (failure (make-plz-error
                   :response (make-plz-response
                              :status 403
                              :headers '((cf-mitigated . "challenge")))))
         requests)
    (with-temp-buffer
      (insert-file-contents
       "/Users/jay/Dropbox/roam/notes/20261026181221-strobist_complete.org")
      (org-mode)
      (let ((before (buffer-string))
            (before-point (point))
            (before-modified (buffer-modified-p)))
        (cl-letf (((symbol-function 'jay/get-url-from-chrome) (lambda () url))
                  ((symbol-function 'gui-get-selection)
                   (lambda (&rest _) (ert-fail "Clipboard must not be consulted")))
                  ((symbol-function 'org-web-tools--get-first-url)
                   (lambda () (ert-fail "Kill ring must not be consulted")))
                  ((symbol-function 'plz)
                   (lambda (_method requested-url &rest _args)
                     (push requested-url requests)
                     (signal 'plz-http-error (list "HTTP error" failure)))))
          (let* ((err (should-error
                       (call-interactively #'jay/org-web-tools-insert-web-page-as-entry-clean)
                       :type 'user-error))
                 (message (error-message-string err)))
            (should (string-match-p (regexp-quote url) message))
            (should (string-match-p "HTTP 403" message))
            (should (string-match-p "JavaScript and cookies" message))
            (should (string-match-p "html2org-clipboard" message))))
        (should (equal requests (list url)))
        (should (equal before (buffer-string)))
        (should (= before-point (point)))
        (should (eq before-modified (buffer-modified-p)))))))

(ert-deftest jay-org-web-tools-http-failure-does-not-invent-a-challenge ()
  "HTTP status alone must not be reported as a Cloudflare challenge."
  (let* ((url "https://strobist.blogspot.com/2006/03/lighting-101.html")
         (failure (make-plz-error :response (make-plz-response :status 403)))
         (message (jay/org-web-tools--capture-error-message
                   url (list 'plz-http-error "HTTP error" failure))))
    (should (string-match-p (regexp-quote url) message))
    (should (string-match-p "HTTP 403" message))
    (should-not (string-match-p "JavaScript\\|ChatGPT" message))))

(ert-deftest jay-org-web-tools-network-failure-includes-reason ()
  "Network failures should retain the actionable curl error."
  (let* ((failure (make-plz-error :curl-error '(6 . "Could not resolve host")))
         (message (jay/org-web-tools--capture-error-message
                   "https://strobist.blogspot.com/2006/03/lighting-101.html"
                   (list 'plz-curl-error "Curl error" failure))))
    (should (string-match-p "Could not resolve host" message))))

(ert-deftest jay-org-web-tools-conversion-errors-keep-original-diagnostics ()
  "Readability, title, and Pandoc bugs must not receive fetch-failure advice."
  (dolist (case '((jay/org-web-tools--extract-readable
                   . (void-function missing-readability-function))
                  (org-web-tools--cleanup-title
                   . (invalid-regexp "Unmatched [ or [^"))
                  (org-web-tools--html-to-org-with-pandoc
                   . (error "Pandoc failed: conversion error"))))
    (let ((original-error (cdr case)))
      (cl-letf (((symbol-function 'plz) (lambda (&rest _) 'fake-dom))
                ((symbol-function 'jay/org-web-tools--extract-readable)
                 (lambda (_html _url) (cons "Strobist: Lighting 101" "<p>Article</p>")))
                ((symbol-function 'org-web-tools--cleanup-title) #'identity)
                ((symbol-function 'org-web-tools--html-to-org-with-pandoc)
                 (lambda (_) "Article\n")))
        (cl-letf (((symbol-function (car case))
                   (lambda (&rest _) (signal (car original-error) (cdr original-error)))))
          (should
           (equal
            (should-error
             (jay/org-web-tools--url-as-readable-org
              "https://strobist.blogspot.com/2006/03/lighting-101.html"))
            original-error)))))))

(ert-deftest jay-capture-cleanup-repairs-strobist-linked-images ()
  "Preserve both URLs in each malformed linked image from the reported note."
  (dolist (pair
    '(
      ("[[https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhwKHyNRi5FCeGU7RJRrHJYgR7zTWGI2ec6TiotwG4p7aQhZykbQ0R4MoAoWC2ctoiK_W8AKHRI991-xgX7tRl69HRmg03F5U67zbFi5xMpN2VnJhCZ-81f6nVtWop6dRZ-faTb/s1600/David_Hobby_Headshot_240.jpg][[[https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhwKHyNRi5FCeGU7RJRrHJYgR7zTWGI2ec6TiotwG4p7aQhZykbQ0R4MoAoWC2ctoiK_W8AKHRI991-xgX7tRl69HRmg03F5U67zbFi5xMpN2VnJhCZ-81f6nVtWop6dRZ-faTb/s200/David_Hobby_Headshot_240.jpg]  ]]]" "[[https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhwKHyNRi5FCeGU7RJRrHJYgR7zTWGI2ec6TiotwG4p7aQhZykbQ0R4MoAoWC2ctoiK_W8AKHRI991-xgX7tRl69HRmg03F5U67zbFi5xMpN2VnJhCZ-81f6nVtWop6dRZ-faTb/s1600/David_Hobby_Headshot_240.jpg][https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhwKHyNRi5FCeGU7RJRrHJYgR7zTWGI2ec6TiotwG4p7aQhZykbQ0R4MoAoWC2ctoiK_W8AKHRI991-xgX7tRl69HRmg03F5U67zbFi5xMpN2VnJhCZ-81f6nVtWop6dRZ-faTb/s200/David_Hobby_Headshot_240.jpg]]")
      ("[[http://strobist.blogspot.com/2006/03/on-assignment-wind-tunnel.html][[[https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgupfzhaVm0JjLhrQeGcHCnJI-3_rXo-H7MZTTVYkyym3Oru_7B55YXTxW8okUzzGkgkNQpxWharG9n57dKRUXhH6lgjJw2AWVWtu-OhbEVx4ElawKn1m9_D2Uiup5eLlnySOYrRg/s640/WindTunnel.jpg]  ]]]" "[[http://strobist.blogspot.com/2006/03/on-assignment-wind-tunnel.html][https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgupfzhaVm0JjLhrQeGcHCnJI-3_rXo-H7MZTTVYkyym3Oru_7B55YXTxW8okUzzGkgkNQpxWharG9n57dKRUXhH6lgjJw2AWVWtu-OhbEVx4ElawKn1m9_D2Uiup5eLlnySOYrRg/s640/WindTunnel.jpg]]")
      ("[[http://strobist.blogspot.com/2009/06/on-assignment-hcac-dancer.html][[[https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEg5GMXwizpMY3ewqRmYonGo7AVK2HulhkTdePGLaXBDrpkL1iNxFu7wuopcjRZGJqaMNjjK1GRlxCRUzeJiq-DBLhhpoQ_V2YWP60I6h4oQXdZ8_VN6ZMTiK8SolouCvfx-3YPw/s1600/Kassi.jpg]  ]]]" "[[http://strobist.blogspot.com/2009/06/on-assignment-hcac-dancer.html][https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEg5GMXwizpMY3ewqRmYonGo7AVK2HulhkTdePGLaXBDrpkL1iNxFu7wuopcjRZGJqaMNjjK1GRlxCRUzeJiq-DBLhhpoQ_V2YWP60I6h4oQXdZ8_VN6ZMTiK8SolouCvfx-3YPw/s1600/Kassi.jpg]]")
      ("[[http://strobist.blogspot.com/2008/08/on-assignment-50-years.html][[[https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEi6UyUwJUvcHc8uNz9etX0ueEfkMjlUpaPtusS5u-E4wCbFW_DAR6Tn6R8CR6Sc_vwqS6689FkiDQDDels0Sl7O_462K9ayZ-Wp6Pof15GgGDtTldb4TS2f6SQ-FrQVG5xRptPu/s1600/MomDad.jpg]  ]]]" "[[http://strobist.blogspot.com/2008/08/on-assignment-50-years.html][https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEi6UyUwJUvcHc8uNz9etX0ueEfkMjlUpaPtusS5u-E4wCbFW_DAR6Tn6R8CR6Sc_vwqS6689FkiDQDDels0Sl7O_462K9ayZ-Wp6Pof15GgGDtTldb4TS2f6SQ-FrQVG5xRptPu/s1600/MomDad.jpg]]")
      ("[[https://www.blogger.com/email-post/23951026/114221855544128764][[[https://resources.blogblog.com/img/icon18_email.gif]  ]]]" "[[https://www.blogger.com/email-post/23951026/114221855544128764][https://resources.blogblog.com/img/icon18_email.gif]]")
      ("[[https://www.blogger.com/post-edit.g?blogID=23951026&postID=114221855544128764&from=pencil][[[https://resources.blogblog.com/img/icon18_edit_allbkg.gif]  ]]]" "[[https://www.blogger.com/post-edit.g?blogID=23951026&postID=114221855544128764&from=pencil][https://resources.blogblog.com/img/icon18_edit_allbkg.gif]]")))
    (let ((fixed (jay/org-web-tools--repair-linked-images (car pair))))
      (should (equal fixed (cadr pair)))
      (should (equal fixed (jay/org-web-tools--repair-linked-images fixed)))
      (should (equal fixed (jay/org-web-tools--repair-linked-images
                            (replace-regexp-in-string "  " "\u200b\u200b" (car pair) t t)))))
    (with-temp-buffer
      (org-mode)
      (insert (cadr pair))
      (goto-char (point-min))
      (should (eq (org-element-type (org-element-context)) 'link)))))

(ert-deftest jay-capture-url-prefix-overrides-chrome-without-clipboard ()
  "An explicit URL override must work even with a Chrome tab present."
  (cl-letf (((symbol-function 'jay/get-url-from-chrome)
             (lambda () "https://chatgpt.com/"))
            ((symbol-function 'read-string)
             (lambda (&rest _) "https://strobist.blogspot.com/2006/03/lighting-101.html"))
            ((symbol-function 'gui-get-selection)
             (lambda (&rest _) (ert-fail "Override must not read clipboard"))))
    (should (equal (jay/get-url-from-clipboard-or-chrome '(4))
                   "https://strobist.blogspot.com/2006/03/lighting-101.html"))))

(ert-deftest jay-capture-defuddle-unavailable-falls-back-to-eww ()
  "Missing Node must use EWW on the same HTML, without another fetch."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
            ((symbol-function 'org-web-tools--sanitized-dom) (lambda () 'parsed-dom))
            ((symbol-function 'org-web-tools--eww-readable)
             (lambda (dom) (should (eq dom 'parsed-dom)) '("Title" . "<p>Article</p>"))))
    (should (equal (jay/org-web-tools--extract-readable "<p>Article</p>" "https://strobist.blogspot.com/")
                   '("Title" . "<p>Article</p>")))))

(provide 'capture-cleanup-tests)

;;; capture-cleanup-tests.el ends here
