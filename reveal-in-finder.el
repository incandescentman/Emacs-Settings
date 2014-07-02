;;; reveal-in-finder.el --- Reveal the file associated with the buffer in the OS X Finder

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA
;; Keywords: OS X, Finder
;; URL: https://github.com/kaz-yos/elisp
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage:
;;
;; If M-x reveal-in-finder is invoked in a file-associated buffer,
;; it will open the folder enclosing the file in the OS X Finder.
;; It will also highlight the file the buffer is associated with within the folder.
;;
;; If M-x reveal-in-finder is invoked in a buffer not associated with a file,
;; it will open the folder defined in the default-directory variable.
;; In a dired buffer, this should open the current folder in the OS X Finder.
;;
;;
;; Special thanks:
;;
;; This is a modified version of the open-finder found at the URL below.
;; Thank you elemakil and lawlist for introducing this nice piece of code,
;; http://stackoverflow.com/questions/20510333/in-emacs-how-to-show-current-file-in-finder
;; and Peter Salazar for pointing out a useful link about AppleScript (below),
;; http://stackoverflow.com/questions/11222501/finding-a-file-selecting-it-in-finder-issue
;; and mikeypostman and purcell for auditing the code for MELPA approval.


;;; Code:

;;;###autoload
(defun reveal-in-finder ()
  "Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current directory."
  (interactive)
  (let* ((path (buffer-file-name))
	dir file)		   ; let* definition ends here.
    (if path
	;; If path has been successfully obtained, set these variables.
	(progn (setq dir (file-name-directory path))
	       (setq file (file-name-nondirectory path)))
      ;; If path is empty, there is no file name. Use the default-directory variable.
      ;; This should work in a dired buffer.
      (setq dir (expand-file-name default-directory)))	
    (reveal-in-finder-as dir file) ; Global variables are required to pass them to the helper.
    ))

;; AppleScript helper function. Thanks milkeypostman for suggestions.
;; Use let* to reuse revealpath in defining script.
(defun reveal-in-finder-as (dir file)
  "A helper function for reveal-in-finder.
This function runs the actual AppleScript."
  (let* ((revealpath (if file		   ; Define revealpath local variable.
			 (concat dir file) ; dir/file if file name available.
		       dir))		   ; dir only if not.
	 (script			   ; Define script variable using revealpath and text.
	  (concat
	   "set thePath to POSIX file \"" revealpath "\"\n"
	   "tell application \"Finder\"\n"
	   " set frontmost to true\n"
	   " reveal thePath \n"
	   "end tell\n")))		   ; let* definitions end here.
    ;; (message script)			   ; Check the text output.
    (start-process "osascript-getinfo" nil "osascript" "-e" script) ; Run AppleScript.
    ))

(provide 'reveal-in-finder)
;;; reveal-in-finder.el ends here
