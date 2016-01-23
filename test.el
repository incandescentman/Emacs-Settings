
(defun test (fn file)
  "Tests FN against the test cases described in FILE, which
should have the following format:

| Input: <custom string>
| Output: <custom string>
| 
| Input: <another custom string>
| Output: <another custom string>
| 
| ...

FN is applied in a buffer containing each of the described
inputs, and checks if the result is the same as the corresponding
expected output.

The caret character (^) in the custom strings has a special
meaning: it represents the position of POINT.  If it occurs in
the input string, it is the initial position, and in the output
string, it is the expected final position.  If no caret is found,
the beginning of the string is considered.

The results of the tests are written in a new buffer.

If FN signals an error, it is muffled and its type printed on the
output buffer.  

NB: don't expect much \"bullet-proofness\" in the file parser, so
don't slack on the format of the test file.  Pay special
attention to whitespaces at the end of input or output lines, as
they will be considered as part of the test case."
  (let ((results-buffer (generate-new-buffer (generate-new-buffer-name "tests-result"))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (loop while (re-search-forward "^Input: \\(.*\\)\nOutput: \\(.*\\)$"
                                     nil t)
            for input-maybe-with-caret = (match-string 1)
            for desired-output-maybe-with-caret = (match-string 2)
            for input-point-idx = (position ?^ input-maybe-with-caret)
            for desired-output-point-idx = (position ?^ desired-output-maybe-with-caret)
            for input = (remove ?^ input-maybe-with-caret)
            for desired-output = (remove ?^ desired-output-maybe-with-caret)
            do (with-temp-buffer
                 (org-mode)
                 (insert input)
                 (goto-char (if input-point-idx
                                (1+ input-point-idx)
                              (point-min)))
                 (let (error-symbol data)
                   (condition-case condition (call-interactively fn)
                     ((error quit)
                      (setf error-symbol (car condition)
                            data (cdr condition))))
                   (let* ((output-point-idx (1- (point)))
                          (actual-output (buffer-string))
                          (actual-output-with-caret
                           (concat (substring actual-output 0 output-point-idx)
                                   "^"
                                   (substring actual-output output-point-idx))))
                     (with-current-buffer results-buffer
                       (insert "Input: " input-maybe-with-caret "\n"
                               "Desired output: " desired-output-maybe-with-caret "\n"
                               "Actual output:  " actual-output-with-caret "\n"
                               (if (string= desired-output actual-output)
                                   "OK"
                                 "FAIL"))
                       (when desired-output-point-idx
                         (insert " / "
                                 (if (eql (1+ desired-output-point-idx)
                                          (1+ output-point-idx))
                                     "OK"
                                   "FAIL")))
                       (when error-symbol
                         (insert " (" (symbol-name error-symbol))
                         (when data
                           (insert " - " (format "%s" data)))
                         (insert ")"))
                       (insert "\n\n")))))))
    (switch-to-buffer results-buffer)))
