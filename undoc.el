;;; undoc.el --- strip MS Word-style formatting to leave a readable ASCII file

;; Copyright (C) 2002  Joshua Guttman

;; Author: Joshua D. Guttman <guttman@mitre.org>
;; Maintainer: Joshua D. Guttman <guttman@mitre.org>
;; Version: 1.01
;; Keywords: Word, word processors 
;; 

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; We first substitute reasonable ASCII equivalents for some MS Word 
;; special characters, after which we extract the ASCII substrings and
;; reformat via fill-paragraph.

;; The text of the Word input document is generally readable as a result,
;; although graphics are irretrievable.  Tables are handled crudely, but
;; are sometimes readable.  

;; Put these autoloads in ~/.emacs:

;;; (autoload 'undoc "undoc" "" t)
;;; (autoload 'undoc-current-buffer "undoc" "" t)
;;; (autoload 'undoc-region-after-mime-decode "undoc" "" t)

;; Alternatively, use

;;; (require 'undoc)

(defconst emacs-strings-contiguous-ascii-length 4 
  "Minimum length for contiguous ASCII to count as a string")
(defconst emacs-strings-plb 32
  "Inclusive lower bound for the ASCII printable characters.")
(defconst emacs-strings-pgb 126
  "Inclusive upper bound for the ASCII printable characters.")	;
								; inclusive!

(defconst emacs-string-whitespace '(9 10) 
  "Tab and linefeed are also printable, 
though not in the range [emacs-strings-plb,emacs-strings-pgb]. ")

(defun just-whitespace (str) 
  "Return true if STR is nothing but TAB, LINEFEED, FORMFEED, CR, and SPACE." 
  (let ((ptr 0)
	(ln (length str)))
    (while (and (< ptr ln)
		(memq (aref str ptr) '(9 10 12 13 32)))
      (setq ptr (1+ ptr)))
    (= ptr ln)))

(defun emacs-strings (s) 
  "Return a string containing all of the purely ASCII substrings of S."
  (let ((ln (length s))
	(anchor 0)
	(ptr 0)
	string-list)
    (while (< anchor ln)
      (if (<= anchor ptr) 
	  (progn 
	    (while (and (< ptr ln)
			(let ((next-char (aref s ptr)))
			  (or (and (<= emacs-strings-plb next-char)
				   (<= next-char emacs-strings-pgb))
			      (memq next-char emacs-string-whitespace))))
	      (setq ptr (1+ ptr)))
	    (if (<= emacs-strings-contiguous-ascii-length (- ptr anchor))
		(setq string-list (cons (substring s anchor ptr) string-list)))
	    (setq anchor (1+ ptr)))
	(let ((next-char (aref s anchor)))
	  (if (or (and (<= emacs-strings-plb next-char)
		       (<= next-char emacs-strings-pgb))
		  (memq next-char emacs-string-whitespace))
	      (setq ptr (1+ anchor))
	    (setq anchor (1+ anchor))))))
    (mapconcat
     (lambda (s) s)
     (reverse string-list)
     "")))







(defconst undoc-replacement-table 
  '((7 . "!&!&!&!")			; bell 
    (148 . "''")			; close quotes 
    (147 . "``")			; open quotes
    (138 . "'")				; single smartquotes
    (146 . "'")				; single smartquotes
    (133 . "...")			; ellipses?  
    (150 . "--"))			; hyphen 

;     ((? . "!&!&!&!")
;       (?\224 . "''")
;       (?\223 . "``")
;       (?\212 . "'")
;       (?’ . "'")				; 146
;       (?\205 . "...")
;       (?\226 . "--"))
  "List of integer.string pairs.
Characters matching a car will be replaced by the cdr when undocing.")

(defun undoc-replace () 
  "Replace Word wierdo characters with ASCII substitutes 
according to `undoc-replacement-table'. "
  (goto-char (point-min))
  (let ((targets 
	 (mapconcat
	  (lambda (p) (format "%c" (car p)))
	  undoc-replacement-table "\\|")))
    (while (re-search-forward targets nil t)
      (forward-char -1)
      (let ((c (char-after)))
	(delete-char 1)
	(insert (or (cdr (assoc c undoc-replacement-table))
		    (error "Weird char %c" c)))))))

(defun undoc-paragraph-contains-tag (tag here) 
  "Return true if TAG occurs in the paragraph containing or after HERE."
  (goto-char here) 
  (let ((end (save-excursion (forward-paragraph) (point))))
    (search-forward tag end t)))

(defun undoc-current-buffer () 
  "Replace contents of current buffer with undoc-ed ASCII equivalent."
  (goto-char (point-min))
  (replace-string "\r" "\n")
  (goto-char (point-min))
  (replace-string "\007\007" "\n")	; ^G^G: line separator in tables. 
  (let ((new-text (emacs-strings (buffer-string))))
    (delete-region (point-min) (point-max))
    (insert new-text))
  (goto-char (point-min))
  ;;  (shell-command-on-region (point-min) (point-max) "strings" t t)
  (replace-regexp 			; separate paragraphs 
   "\\([^\n]\\)\n\\([^\n]\\)"		; if not yet separated 
   "\\1\n\n\\2")
  (goto-char (point-min))
  (while (not (eobp))
    (if (undoc-paragraph-contains-tag "!&!&!&!" (point))
	(progn (forward-paragraph)
	       (insert "\n"))
      (forward-word 1)
      (fill-paragraph nil)
      (forward-paragraph)))
  (goto-char (point-min))
  (while (search-forward "!&!&!&!" nil t)
    (replace-match "\t"))
  (goto-char (point-min)))

(defun undoc-find-read-only (filename)
  "Find a .doc file and display readable version read-only."
  (interactive "fDoc file: ")
  (find-file filename)
  (toggle-read-only 0)
  (undoc-current-buffer)
  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)  
  (text-mode))


(defun undoc (filename) 
  "Construct new file with undoc-ed contents of FILENAME.
New file is called basename.txt, where basename 
is the non-extension part of FILENAME. "
  (interactive "fDoc file to salvage: ") 
  (let ((buff
	 (find-file-noselect filename nil 'rawfile))
	(write-file-hooks nil))
    (set-buffer buff)
    (write-file 
     (concat 
      (file-name-sans-extension filename)
      ".txt"))
    (undoc-current-buffer)
    (save-buffer)
    (switch-to-buffer buff)))

(defun undoc-region (start end &optional decode-function) 
  "Write region from START to END to temporary file and call undoc on that.
Optional arg DECODE-FUNCTION is applied to the contents of the region first."
  (let ((mm-tmp-directory
	 (cond ((boundp 'mm-tmp-directory) mm-tmp-directory)
	       ((fboundp 'temp-directory) (temp-directory))
	       ((boundp 'temporary-file-directory) temporary-file-directory)
	       ("/tmp/"))))
    (let ((fn (concat 
	       (make-temp-name (expand-file-name "undoc." mm-tmp-directory))
	       ".doc")))
      (write-region 
       (funcall 
	(or decode-function 'identity)
	(buffer-substring start end))
       nil 
       fn)
      (undoc fn)
      (delete-file fn))))

(defun undoc-string (string &optional decode-function) 
  "Write region from STRING to temporary file and call undoc on that.
Optional arg DECODE-FUNCTION is applied to the contents of the string first."
  (let ((mm-tmp-directory
	 (cond ((boundp 'mm-tmp-directory) mm-tmp-directory)
	       ((fboundp 'temp-directory) (temp-directory))
	       ((boundp 'temporary-file-directory) temporary-file-directory)
	       ("/tmp/"))))
    (let ((fn (concat 
	       (make-temp-name (expand-file-name "undoc." mm-tmp-directory))
	       ".doc")))
      (write-region 
       (funcall 
	(or decode-function 'identity)
	string)
       nil 
       fn)
      (undoc fn)
      (delete-file fn))))

(defun undoc-region-after-mime-decode (start end) 
  "After Mime-decoding the region from START to END, undoc that region. 
See undoc-region."
  (interactive "r")
  (let ((buffer-read-only nil))
    (goto-char end)
    (shell-command-on-region start end "mimencode -u" nil t)
    (undoc-region (min (point) (mark)) (max (point) (mark)))))

(provide 'undoc)
