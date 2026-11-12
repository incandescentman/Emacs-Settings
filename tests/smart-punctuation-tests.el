;;; smart-punctuation-tests.el --- Prose punctuation regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl)
(require 'org)
(load (expand-file-name "../spacecraft-mode.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun jay/test-smart-period (text)
  "Return resulting text and point after typing a period after TEXT."
  (with-temp-buffer
    (setq major-mode 'text-mode)
    (insert text)
    (let ((post-self-insert-hook nil)
          (mark-active nil))
      (smart-period))
    (list (buffer-string) (1- (point)))))

(ert-deftest jay/smart-period-does-not-match-ordinary-words ()
  (dolist (text '("I like ice" "pass the egg" "my ire"))
    (should (equal (jay/test-smart-period text)
                   (list (concat text ".") (1+ (length text)))))))

(ert-deftest jay/smart-period-recognizes-literal-abbreviations ()
  (should (equal (jay/test-smart-period "vs ") '("vs. " 4)))
  (dolist (text '("vs. " "i.e. " "e.g. "))
    (should (equal (jay/test-smart-period text) (list text (length text))))))

(ert-deftest jay/smart-period-adds-one-dot ()
  (dolist (dots '("." ".." "..." "...." "....."))
    (let ((text (concat "wait" dots " ")))
      (should (equal (jay/test-smart-period text)
                     (list (concat "wait" dots ". ") (1+ (length text))))))))

(ert-deftest jay/smart-period-moves-before-space ()
  (should (equal (jay/test-smart-period "hello ") '("hello. " 7)))
  (should (equal (jay/test-smart-period "hello, ") '("hello. " 7))))

(provide 'smart-punctuation-tests)
