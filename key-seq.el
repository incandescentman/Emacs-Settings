;;; key-seq.el --- map pairs of sequentially pressed keys to commands

;; Copyright (C) 2015 Vyacheslav Levit
;; Copyright (C) 2003,2005,2008,2012 David Andersson

;; Author: Vyacheslav Levit <dev@vlevit.org>
;; Maintainer: Vyacheslav Levit <dev@vlevit.org>
;; Version: 1.0.0
;; Created: 15 June 2015
;; Package-Requires: ((key-chord "0.6"))
;; Keywords: convenience keyboard keybindings
;; URL: http://github.com/vlevit/key-seq.el

;; This file is NOT part of Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This package provides functions to map pairs of sequentially but
;; quickly pressed keys to commands:
;;
;;  - `key-seq-define-global' defines a pair in the global key-map,
;;  - `key-seq-define' defines a pair in a specific key-map.
;;
;; The package depends on key-chord.el and it requires active
;; key-chord-mode to work. Add this line to your configuration:
;;
;;    (key-chord-mode 1)
;;
;; The only difference between key-chord-* functions and key-seq-*
;; functions is that the latter executes commands only if the order of
;; pressed keys matches the order of defined bindings. For example,
;; with the following binding
;;
;;    (key-seq-define-global "qd" 'dired)
;;
;; dired shall be run if you press `q' and `d' only in that order
;; while if you define the binding with `key-chord-define-global' both
;; `qd' and `dq' shall run dired.
;;
;; To unset key sequence use either `key-seq-unset-global' or
;; `key-seq-unset-local'.
;;
;; For more information and various customizations see key-chord.el
;; documentation.
;;
;; The code in key-seq.el is 99% copy/paste from key-chord.el.

;;; Code:

;;;###autoload
(defun key-seq-define-global (keys command)
  "Define a key sequence of the two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.
\nNote that KEYS defined locally in the current buffer will have precedence."
  (interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-seq-define (current-global-map) keys command))

;;;###autoload
(defun key-seq-define-local (keys command)
  "Locally define a key sequence of the two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.
\nThe binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  (interactive "sSet key chord locally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-chord-define (current-local-map) keys command))

;;;###autoload
(defun key-seq-define (keymap keys command)
  "Define in KEYMAP, a key sequence of the two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed."
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (define-key keymap (vector 'key-chord key1 key2) command)
      (define-key keymap (vector 'key-chord key1 key2) command))))

(defun key-seq-unset-global (keys)
  "Remove global key sequence of the two keys in KEYS."
  (interactive "sUnset key sequence globally (2 keys): ")
  (key-seq-define (current-global-map) keys nil))

(defun key-seq-unset-local (keys)
  "Remove local key sequence of the two keys in KEYS."
  (interactive "sUnset key sequence locally (2 keys): ")
  (key-seq-define (current-local-map) keys nil))

(provide 'key-seq)

;;; key-seq.el ends here
