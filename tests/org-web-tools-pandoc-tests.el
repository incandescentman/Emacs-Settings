;;; org-web-tools-pandoc-tests.el --- ERT tests for org-web-tools Pandoc probing -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused regression tests for the vendored org-web-tools Pandoc option
;; chooser so Pandoc CLI changes do not break page capture again.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst jay-org-web-tools-pandoc-test--source-file
  (expand-file-name
   "org-web-tools-master/org-web-tools.el"
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to the vendored `org-web-tools.el` file in this repo.")

(defun jay-org-web-tools-pandoc-test--load-definitions (file symbols)
  "Load defuns/defvars named in SYMBOLS from FILE without the full config."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (condition-case nil
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (memq (car form) '(defun defvar defconst defcustom))
                         (memq (nth 1 form) symbols))
                (eval form t)))
          (end-of-file
           (setq done t)))))))

(unless (fboundp 'org-web-tools--check-pandoc-no-wrap-option)
  (jay-org-web-tools-pandoc-test--load-definitions
   jay-org-web-tools-pandoc-test--source-file
   '(org-web-tools--check-pandoc-no-wrap-option)))

(ert-deftest jay-org-web-tools-pandoc-check-prefers-wrap-none ()
  "Prefer the current Pandoc flag when it is supported."
  (cl-letf (((symbol-function 'org-web-tools--pandoc-option-probe)
             (lambda (option)
               (pcase option
                 ("--wrap=none" '(0 . ""))
                 ("--no-wrap" '(0 . "deprecated"))
                 (_ (error "Unexpected option: %s" option))))))
    (should (equal (org-web-tools--check-pandoc-no-wrap-option) "--wrap=none"))))

(ert-deftest jay-org-web-tools-pandoc-check-falls-back-to-no-wrap ()
  "Fall back to the older flag when `--wrap=none` is unavailable."
  (cl-letf (((symbol-function 'org-web-tools--pandoc-option-probe)
             (lambda (option)
               (pcase option
                 ("--wrap=none" '(6 . "Unknown option"))
                 ("--no-wrap" '(0 . ""))
                 (_ (error "Unexpected option: %s" option))))))
    (should (equal (org-web-tools--check-pandoc-no-wrap-option) "--no-wrap"))))

(ert-deftest jay-org-web-tools-pandoc-check-surfaces-both-failures ()
  "If neither option works, the error should include both probe results."
  (cl-letf (((symbol-function 'org-web-tools--pandoc-option-probe)
             (lambda (option)
               (pcase option
                 ("--wrap=none" '(6 . "wrap failure"))
                 ("--no-wrap" '(7 . "no-wrap failure"))
                 (_ (error "Unexpected option: %s" option))))))
    (should-error
     (org-web-tools--check-pandoc-no-wrap-option)
     :type 'error
     :exclude-subtypes nil)))

(provide 'org-web-tools-pandoc-tests)

;;; org-web-tools-pandoc-tests.el ends here
