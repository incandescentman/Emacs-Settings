;;; xah-replace-pairs.el --- emacs lisp functions for multi-pair find/replace.  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2010-2021 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.5.20210906030129
;; Created: 17 Aug 2010
;; Package-Requires: ((emacs "24.1"))
;; Keywords: lisp, tools, find replace
;; License: GPL v3
;; URL: http://ergoemacs.org/emacs/elisp_replace_string_region.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides elisp functions that do find/replace with multiple pairs of strings. and guarantees that earlier find/replace pair does not effect later find/replace pairs.

;; The functions are:

;; xah-replace-pairs-region
;; xah-replace-pairs-in-string
;; xah-replace-regexp-pairs-region
;; xah-replace-regexp-pairs-in-string
;; xah-replace-pairs-region-recursive
;; xah-replace-pairs-in-string-recursive

;; Call `describe-function' on them for detail.

;; Or, see home page at
;; http://ergoemacs.org/emacs/elisp_replace_string_region.html

;; If you like it, please support by Buy Xah Emacs Tutorial
;; http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html
;; Thanks.

;;; History:

;; 2015-04-28 major rewrite. This package was xfrp_find_replace_pairs
;; version 1.0, 2010-08-17. First version.

;; HHH___________________________________________________________________
;;; Code:

(defun xah-replace-pairs-region (Begin End Pairs &optional Report-p Hilight-p)
  "Replace multiple Pairs of find/replace strings in region Begin End.

Pairs is a sequence of pairs [[f1 r1] [f2 r2] …] each element or entire argument can be list or vector. f are find string, r are replace string.

Find strings case sensitivity depends on `case-fold-search'. The replacement are literal and case sensitive.

Once a subsring in the buffer is replaced, that part will not change again.  For example, if the buffer content is “abcd”, and the Pairs are a → c and c → d, then, result is “cbdd”, not “dbdd”.

Report-p is t or nil. If t, it prints each replaced pairs, one pair per line.

Returns a list, each element is a vector [position findStr replaceStr].

Version 2020-12-18"
  ;; 2021-09-06 the non intentional replacement happens when:
  ;; a replace string is a superset or subset of subsequent find string.

  ;; Example:
  ;; text is: sa

  ;; subset situation:
  ;; a → p
  ;; sp → b

  ;; superset situation:
  ;; a → op
  ;; p → b

  ;; for efficiency, to fix it and avoid intermediate find/replace. If any superset or subset occure, try to move the pair to the bottom. If still happens, then, those pair with the replacemen string super/sub issue needs a intermediate replacement
  (let (($tempMapPoints nil) ($changeLog nil))
    ;; set $tempMapPoints, to unicode private use area chars
    (dotimes (i (length Pairs)) (push (char-to-string (+ #xe000 i)) $tempMapPoints))
    ;; (message "%s" Pairs)
    ;; (message "%s" $tempMapPoints)
    (save-excursion
      (save-restriction
        (narrow-to-region Begin End)
        (dotimes (i (length Pairs))
          (goto-char (point-min))
          (while (search-forward (elt (elt Pairs i) 0) nil t)
            (replace-match (elt $tempMapPoints i) t t)))
        (dotimes (i (length Pairs))
          (goto-char (point-min))
          (while (search-forward (elt $tempMapPoints i) nil t)
            (push (vector (point)
                          (elt (elt Pairs i) 0)
                          (elt (elt Pairs i) 1)) $changeLog)
            (replace-match (elt (elt Pairs i) 1) t t)
            (when Hilight-p
              (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight))))))
    (when (and Report-p (> (length $changeLog) 0))
      (mapc
       (lambda ($x)
         (princ $x)
         (terpri))
       (reverse $changeLog)))
    $changeLog
    ))

(defun xah-replace-pairs-in-string (Str Pairs)
  "Replace string Str by find/replace Pairs sequence.
Returns the new string.
This function is a wrapper of `xah-replace-pairs-region'. See there for detail."
  (with-temp-buffer
    (insert Str)
    (xah-replace-pairs-region (point-min) (point-max) Pairs)
    (buffer-string)))

(defun xah-replace-regexp-pairs-region (Begin End Pairs &optional Fixedcase-p Literal-p Hilight-p)
  "Replace regex string find/replace Pairs in region.

Begin End are the region boundaries.

Pairs is
 [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.

The optional arguments Fixedcase-p and Literal-p is the same as in `replace-match'.
If Hilight-p is true, highlight the changed region.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)
Version 2017-02-21 2021-08-14"
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (mapc
       (lambda ($x)
         (goto-char (point-min))
         (while (re-search-forward (elt $x 0) (point-max) t)
           (replace-match (elt $x 1) Fixedcase-p Literal-p)
           (when Hilight-p
             (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight))))
       Pairs))))

(defun xah-replace-regexp-pairs-in-string (Str Pairs &optional Fixedcase-p Literal-p)
  "Replace string Str recursively by regex find/replace pairs Pairs sequence.

This function is a wrapper of `xah-replace-regexp-pairs-region'. See there for detail.

See also `xah-replace-pairs-in-string'."
  (with-temp-buffer
    (insert Str)
    (goto-char (point-min))
    (xah-replace-regexp-pairs-region (point-min) (point-max) Pairs Fixedcase-p Literal-p)
    (buffer-string)))

(defun xah-replace-pairs-region-recursive (Begin End Pairs)
  "Replace multiple Pairs of find/replace strings in region Begin End.

This function is similar to `xah-replace-pairs-region', except that the replacement is done recursively after each find/replace pair.  Earlier replaced value may be replaced again.

For example, if the input string is “abcd”, and the pairs are a → c and c → d, then, the result is “dbdd”, not “cbdd”.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)

The replacement are literal and case sensitive.
Version 2017-02-21"
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (mapc
       (lambda (x)
         (goto-char (point-min))
         (while (search-forward (elt x 0) (point-max) t)
           (replace-match (elt x 1) t t)))
       Pairs))))

(defun xah-replace-pairs-in-string-recursive (Str Pairs)
  "Replace string Str recursively by find/replace pairs Pairs sequence.

This function is is a wrapper of `xah-replace-pairs-region-recursive'. See there for detail."
  (with-temp-buffer
    (insert Str)
    (goto-char (point-min))
    (xah-replace-pairs-region-recursive (point-min) (point-max) Pairs)
    (buffer-string)))

(provide 'xah-replace-pairs)

;;; xah-replace-pairs.el ends here
