;; -*- coding: utf-8; -*- 

(require 'dictem)

(defun dictem-lingvo-dsl-highlight ()
  ; trn/ex/com/*
  (goto-char (point-min))
  (while (search-forward-regexp "\\[/?trn\\]\\|\\[/?p\\]\\|\\[/?c\\]\\|\\[/?com\\]\\|\\[/?[*]\\]\\|\\[/?b\\]\\|\\[/?i\\]\\|\\[/?m[0-9]?\\]\\|\\[/?ex\\]" nil t)
    (replace-match "" t t))

  ; [ex] [/ex]
;  (goto-char (point-min))
;  (while (search-forward-regexp "\\[ex\\]\\([][]*\\)\\[/ex\\]" nil t)
;    (add-text-properties (match-beginning 0) (match-end 0)
;			 '(face dictem-lingvo-dsl-example-face))
;    (let* ((beg (match-beginning 1))
;	   (end (match-end 1))
;	   (repl (buffer-substring beg end)))
;      (replace-match repl 1 1)))

  ; <<>>
  (goto-char (point-min))
  (while (search-forward-regexp "\\(<<\\|\\[ref\\]\\)\\([^\n]*\\)\\(>>\\|\\[/ref\\]\\)" nil t)
    (let* ((beg (match-beginning 2))
	   (end (match-end 2))
	   (repl (buffer-substring beg end)))
      (replace-match (concat "{" repl "}") t t)))

  ; hyperlinks
  (dictem-postprocess-definition-hyperlinks)
  )

(progn
  (set-buffer "*dsl-buffer*")
  (dictem-lingvo-dsl-highlight))

(defface dictem-lingvo-dsl-italic-face
  '((((background light)) (:italic true))
    (((background dark))  (:italic true)))
  "Face for italic"
  )

(defface dictem-lingvo-dsl-color-face
  '((((background light)) (:italic true))
    (((background dark))  (:italic true)))
  "Face for color"
  )

(defface dictem-lingvo-dsl-example-face
  '((((background light)) (:italic true))
    (((background dark))  (:italic true)))
  "Face for color"
  )

(defface dictem-lingvo-dsl-bold-face
  '((((background light)) (:bold true))
    (((background dark))  (:bold true)))
  "Face for bold"
  )

(defface dictem-lingvo-dsl-trn-face
  '((((background light)) (:bold true :italic true))
    (((background dark))  (:bold true :italic true)))
  "Face for trn"
  )

(provide 'dictem-lingvo-dsl)
