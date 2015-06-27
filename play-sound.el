;;; play-sound.el --- Play sound files on OSX

;; Copyright (C) 2011, 2012, 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: comm, tools, convenience

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

;; Provide a compatibility layer for play-sound on OSX since
;; play-sound-internal is not implemented.

;;; To install:

;; (unless (and (fboundp 'play-sound-internal)
;;              (subrp (symbol-function 'play-sound-internal)))
;;   (require 'play-sound))

;;; Code:

(eval-when-compile (require 'cl))

(defun play-sound-internal (sound)
  "Internal function for `play-sound' (which see)."
  (or (eq (car-safe sound) 'sound)
      (signal 'wrong-type-argument (list sound)))

  (destructuring-bind (&key file data volume device)
      (cdr sound)

    (and (or data device)
	 (error "DATA and DEVICE arg not supported"))

    (unless (file-name-absolute-p file)
      (setq file (expand-file-name file data-directory)))

    (apply #'start-process "afplay" nil
	   "afplay" (append (and volume (list "-v" volume))
			    (list file)))))

(provide 'play-sound)
;;; play-sound.el ends here
