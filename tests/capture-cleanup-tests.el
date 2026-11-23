;;; capture-cleanup-tests.el --- ERT regression tests for capture cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for `jay/org-web-tools-cleanup-string` so heading/timestamp
;; and spacing cleanup regressions are caught quickly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'plz)

(defconst jay-capture-cleanup-test--module-file
  (expand-file-name
   "web-capture.el"
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to the production web-capture module in this repo.")

(defconst jay-capture-test--fixtures-directory
  (expand-file-name "tests/fixtures/"
                    (file-name-directory jay-capture-cleanup-test--module-file))
  "Stable test documents, independent of editable working notes.")

;; Load the production module, including its real library dependencies.
;; The pasteboard Unicode-space cleaner is loaded through the module's autoload.
(add-to-list 'load-path (file-name-directory jay-capture-cleanup-test--module-file))
(load jay-capture-cleanup-test--module-file nil 'nomessage)

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
        (should (string-match-p ":WEB_CAPTURE_URL: https://example.com/post" output))
      (should-not (string-match-p "^\\*+ Article$" output))))))

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
       (expand-file-name "strobist-welcome-capture.org"
                         jay-capture-test--fixtures-directory))
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

(ert-deftest jay-capture-title-keeps-text-after-inline-markup ()
  "Blogger titles with <br> or <i> inside <title> must not truncate."
  (dolist (case '(("<html><head><title>Strobist: Lighting 101:<br>Balancing Flash With Ambient, Pt 2</title></head><body></body></html>"
                   . "Strobist: Lighting 101:Balancing Flash With Ambient, Pt 2")
                  ("<html><head><title>Strobist: SLC-0L-03: <i>Joker</i> Cinematographer <br>On Light &amp; Color</title></head><body></body></html>"
                   . "Strobist: SLC-0L-03: Joker Cinematographer On Light & Color")
                  ("<html><head><title>Plain Title</title></head><body></body></html>"
                   . "Plain Title")))
    (let ((dom (with-temp-buffer
                 (insert (car case))
                 (libxml-parse-html-region (point-min) (point-max)))))
      (should (equal (jay/org-web-tools--dom-title dom) (cdr case))))))

(ert-deftest jay-capture-defuddle-unavailable-falls-back-to-eww ()
  "Missing Node must use EWW on the same HTML, without another fetch."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
            ((symbol-function 'org-web-tools--sanitized-dom) (lambda () 'parsed-dom))
            ((symbol-function 'org-web-tools--eww-readable)
             (lambda (dom) (should (eq dom 'parsed-dom)) '("Title" . "<p>Article</p>"))))
    (should (equal (jay/org-web-tools--extract-readable "<p>Article</p>" "https://strobist.blogspot.com/")
                   '("Title" . "<p>Article</p>")))))

(ert-deftest jay-capture-cleanup-removes-leading-break-after-strobist-photo ()
  "Remove the reported standalone break, preserving an inline caption break."
  (let ((input "[[http://strobist.blogspot.com/2006/03/on-assignment-wind-tunnel.html][https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgupfzhaVm0JjLhrQeGcHCnJI-3_rXo-H7MZTTVYkyym3Oru_7B55YXTxW8okUzzGkgkNQpxWharG9n57dKRUXhH6lgjJw2AWVWtu-OhbEVx4ElawKn1m9_D2Uiup5eLlnySOYrRg/s640/WindTunnel.jpg]]\n\n\\\\\nAnd that's how this site started.\n")
        (expected "[[http://strobist.blogspot.com/2006/03/on-assignment-wind-tunnel.html][https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgupfzhaVm0JjLhrQeGcHCnJI-3_rXo-H7MZTTVYkyym3Oru_7B55YXTxW8okUzzGkgkNQpxWharG9n57dKRUXhH6lgjJw2AWVWtu-OhbEVx4ElawKn1m9_D2Uiup5eLlnySOYrRg/s640/WindTunnel.jpg]]\n\nAnd that's how this site started.\n")
        (caption "[[http://strobist.blogspot.com/2006/03/on-assignment-wind-tunnel.html][https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgupfzhaVm0JjLhrQeGcHCnJI-3_rXo-H7MZTTVYkyym3Oru_7B55YXTxW8okUzzGkgkNQpxWharG9n57dKRUXhH6lgjJw2AWVWtu-OhbEVx4ElawKn1m9_D2Uiup5eLlnySOYrRg/s640/WindTunnel.jpg]]\\\\\nAnd that's how this site started.\n"))
    (should (equal (jay/org-web-tools--normalize-paragraph-breaks input) expected))
    (should (equal (jay/org-web-tools--normalize-paragraph-breaks expected) expected))
    (should (equal (jay/org-web-tools--normalize-paragraph-breaks caption) caption))
    (should (equal (jay/org-web-tools--normalize-paragraph-breaks
                    (replace-regexp-in-string "\n\n" "\n" input))
                   expected))))

(defun jay-capture-test--real-page ()
  "Read the saved real Strobist welcome capture, independent of the live note."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "strobist-welcome-capture.org"
                       jay-capture-test--fixtures-directory))
    (buffer-string)))

(ert-deftest jay-capture-at-cursor-preserves-surrounding-text ()
  "Insert at the chosen line, preserving surrounding text and page levels."
  (let ((page (jay-capture-test--real-page)))
    (dolist (location '(blank middle end section))
      (with-temp-buffer
        (let ((org-mode-hook nil)) (org-mode))
        (insert-file-contents
         (expand-file-name "capture-insertion-context.org"
                           jay-capture-test--fixtures-directory))
        (goto-char (point-min))
        (pcase location
          ('section (re-search-forward (rx line-start "* Course Research")))
          (_ (re-search-forward "^xxx$")
             (pcase location
               ('blank (forward-line 1))
               ('middle (backward-char 2)))))
        ;; Compute expectations independently of the production context helper.
        (let* ((position (save-excursion
                           (unless (eq location 'blank) (end-of-line))
                           (point)))
               (prefix (buffer-substring-no-properties (point-min) position))
               (suffix (buffer-substring-no-properties position (point-max)))
               (newline (if (eq location 'blank) "" "\n"))
               (original (buffer-string)))
          (cl-letf (((symbol-function 'jay/org-web-tools--url-as-readable-org)
                     (lambda (_url)
                       ;; Fetching must not determine the destination.
                       (goto-char (point-max))
                       page)))
            (dotimes (index 3)
              (jay/org-web-tools-insert-web-page-as-entry-clean
               "https://strobist.blogspot.com/2006/02/welcome-to-strobist.html")
              (should (equal (buffer-substring-no-properties (point) (point-max))
                             (concat (make-string (1+ index) ?\n) suffix)))))
          (should (equal prefix (buffer-substring-no-properties (point-min) position)))
          (goto-char position)
          (should (looking-at-p (regexp-quote (concat newline "** [[https://strobist"))))
          (let ((count 0))
            (while (re-search-forward (rx line-start (group (+ "*")) " [[https://strobist.blogspot.com/2006/02/welcome-to-strobist.html") nil t)
              (should (= (length (match-string 1)) 2))
              (setq count (1+ count)))
            (should (= count 3)))
          ;; Removing only the inserted interval restores the source exactly.
          (delete-region position (+ position (- (buffer-size) (length original))))
          (should (equal original (buffer-string))))))))

(ert-deftest jay-capture-repeated-real-pages-retain-level ()
  "Repeat captures at several depths, including before an existing heading."
  (let ((page (jay-capture-test--real-page)))
    (dolist (level '(1 2 3))
      (dolist (following '(nil t))
        (with-temp-buffer
          (let ((org-mode-hook nil)) (org-mode))
          (org-paste-subtree level page t)
          (when following
            (save-excursion
              (insert "\n")
              (insert-file-contents
               (expand-file-name "capture-insertion-context.org"
                           jay-capture-test--fixtures-directory))))
          (let ((suffix (buffer-substring-no-properties (point) (point-max))))
            (cl-letf (((symbol-function 'jay/org-web-tools--url-as-readable-org)
                       (lambda (_url) page)))
              (dotimes (index 3)
                (jay/org-web-tools-insert-web-page-as-entry-clean
                 "https://strobist.blogspot.com/2006/02/welcome-to-strobist.html")
                (should (equal (buffer-substring-no-properties (point) (point-max))
                               (concat (make-string (1+ index) ?\n) suffix))))))
          (goto-char (point-min))
          (let ((count 0))
            (while (re-search-forward
                    (rx line-start (group (+ "*")) " [[https://strobist.blogspot.com/2006/02/welcome-to-strobist.html") nil t)
              (should (= (length (match-string 1)) level))
              (setq count (1+ count)))
            (should (= count 4))))))))

(ert-deftest jay-capture-marker-is-local-and-legacy-still-works ()
  "Plain linked sections are not captures; metadata never inherits."
  (let ((page (jay-capture-test--real-page)) (org-use-property-inheritance t))
    (with-temp-buffer
      (org-mode) (insert page) (goto-char (point-min))
      (should (jay/org-web-tools--capture-entry-p))
      (org-end-of-meta-data t) (insert "** Section\n") (forward-line -1)
      (should-not (jay/org-web-tools--capture-entry-p)))
    (with-temp-buffer
      (org-mode) (insert page) (goto-char (point-min))
      (org-entry-delete nil "WEB_CAPTURE_URL")
      (should-not (jay/org-web-tools--capture-entry-p))
      (org-end-of-meta-data t) (insert "** Article\n")
      (goto-char (point-min))
      (should (jay/org-web-tools--capture-entry-p)))))

(ert-deftest jay-capture-separates-real-photo-from-prose ()
  "A standalone photo must not share the first line of the next paragraph."
  (with-temp-buffer
    (insert (jay-capture-test--real-page))
    (goto-char (point-min))
    (re-search-forward "^\\[\\[https://blogger[^\n]+$")
    (let* ((photo (match-string 0))
           (input (concat photo "\nAt its heart, this website is about one thing.\n"))
           (expected (concat photo "\n\nAt its heart, this website is about one thing.\n")))
      (should (equal expected (jay/org-web-tools--separate-image-paragraphs input)))
      (should (equal expected (jay/org-web-tools--separate-image-paragraphs expected)))
      (dolist (block '("src org" "verse"))
        (let ((literal (concat "#+begin_" block "\n" input "#+end_" (car (split-string block)) "\n")))
          (should (equal literal (jay/org-web-tools--separate-image-paragraphs literal)))))
      (should (equal (concat "See " input)
                     (jay/org-web-tools--separate-image-paragraphs (concat "See " input)))))))

(ert-deftest jay-capture-new-entry-has-property-and-direct-body ()
  "New captures omit Article and place genuine sections below the page title."
  (let ((url "https://strobist.blogspot.com/2016/07/lighting-102-controlling-specular.html"))
    (cl-letf (((symbol-function 'plz) (lambda (&rest _) "source"))
              ((symbol-function 'jay/org-web-tools--extract-readable)
               (lambda (&rest _) '("Controlling Specular Highlights" . "source")))
              ((symbol-function 'org-web-tools--cleanup-title) #'identity)
              ((symbol-function 'org-web-tools--html-to-org-with-pandoc)
               (lambda (_) "* Product Shot, Same Principle\nHere's a shot of my (then) two-speedlight kit, circa 2016.\n")))
      (let ((text (jay/org-web-tools-cleanup-string (jay/org-web-tools--url-as-readable-org url))))
        (should (string-match-p (regexp-quote (concat ":WEB_CAPTURE_URL: " url)) text))
        (should (string-match-p "^\\*\\* Product Shot, Same Principle$" text))
        (should-not (string-match-p "^\\*+ Article$" text))))))

(ert-deftest jay-capture-retains-strobist-styled-section-title ()
  "The actual legacy font-size title survives extraction as a heading."
  ;; Source: https://strobist.blogspot.com/2016/07/lighting-102-controlling-specular.html
  (let ((html "<html><body><article><br><font size=4>Product Shot, Same Principle</font><br><br>Here's a shot of my (then) two-speedlight kit, circa 2016. That's eleven different items, all black, on white paper.<br><br>You can't just overexpose your way to surface detail on this, because you'll blow away your whites. And to make matters worse, many of the objects have bright shiny parts, too.</article></body></html>"))
    (should (string-match-p "<h2[^>]*>Product Shot, Same Principle</h2>"
                           (jay/org-web-tools--defuddle-html html "https://strobist.blogspot.com/2016/07/lighting-102-controlling-specular.html")))))

(provide 'capture-cleanup-tests)

;;; capture-cleanup-tests.el ends here
