;;; auto-capitalize.el --- Automatically capitalize (or upcase) words -*- lexical-binding: t; -*-

;; Copyright   1998,2001,2002,2005 Kevin Rodgers

;; This project was copied from emacswiki page
;; (https://www.emacswiki.org/emacs/auto-capitalize.el) and I changed
;; some details. Big difference is this package requires Emacs 24.3 or
;; higher version.

;; Original Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; (Please don’t contact original author if you found a bug in this
;; package)
;; Maintainer: Yuta Yamada <cokesboy at gmail.com>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))

;; Created: 20 May 1998
;; Version: $Revision: 2.20 $
;; Package-Version: 2.20
;; Keywords: text, wp, convenience
;; RCS $Id: auto-capitalize.el,v 2.20 2005/05/25 18:47:22 kevinr Exp $
;; URL: https://github.com/yuutayamada/auto-capitalize-el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; In `auto-capitalize' minor mode, the first word at the beginning of
;; a paragraph or sentence (i.e. at `left-margin' on a line following
;; `paragraph-separate', after `paragraph-start' at `left-margin', or
;; after `sentence-end') is automatically capitalized when a following
;; whitespace or punctuation character is inserted.
;;
;; The `auto-capitalize-words' variable can be customized so that
;; commonly used proper nouns and acronyms are capitalized or upcased,
;; respectively.
;;
;; The `auto-capitalize-yank' option controls whether words in yanked
;; text should by capitalized in the same way.
;;
;; To install auto-capitalize.el, copy it to a `load-path' directory,
;; `M-x byte-compile-file' it, and add this to your
;; site-lisp/default.el or ~/.emacs file:
;; (autoload 'auto-capitalize-mode "auto-capitalize"
;;   "Toggle `auto-capitalize' minor mode in this buffer." t)
;; (autoload 'turn-on-auto-capitalize-mode "auto-capitalize"
;;   "Turn on `auto-capitalize' minor mode in this buffer." t)
;; (autoload 'enable-auto-capitalize-mode "auto-capitalize"
;;   "Enable `auto-capitalize' minor mode in this buffer." t)
;;
;; To turn on (unconditional) capitalization in all Text modes, add
;; this to your site-lisp/default.el or ~/.emacs file:
;; (add-hook 'text-mode-hook 'turn-on-auto-capitalize-mode)
;; To enable (interactive) capitalization in all Text modes, add this
;; to your site-lisp/default.el or ~/.emacs file:
;; (add-hook 'text-mode-hook 'enable-auto-capitalize-mode)
;;
;; To prevent a word from ever being capitalized or upcased
;; (e.g. "http"), simply add it (in lowercase) to the
;; `auto-capitalize-words' list.
;;
;; To prevent a word in the `auto-capitalize-words' list from being
;; capitalized or upcased in a particular context (e.g.
;; "GNU.emacs.sources"), insert the following whitespace or
;; punctuation character with `M-x quoted-insert' (e.g. `gnu C-q .').
;;
;; To enable contractions based on a word in the
;; `auto-capitalize-words' list to be capitalized or upcased
;; (e.g. "I'm") in the middle of a sentence in Text mode, define the
;; apostrophe as a punctuation character or as a symbol that joins two
;; words:
;; ;; Use "_" instead of "." to define apostrophe as a symbol:
;; (modify-syntax-entry ?' ".   " text-mode-syntax-table) ; was "w   "

;;; Some minor changes made by me (after I copied from emacswiki):
;;
;; 1 Apply Emacs 24.3 (due to ‘last-command-char’ -> ‘last-command-event’)
;; 2 Add default predicate function.  It does:
;;   * Only allow auto capitalization after specific character you
;;     typed.  (see ‘auto-capitalize-allowed-chars’)
;;   * Configurable on-and-off in specific buffers
;;     (see ‘auto-capitalize-inhibit-buffers’)
;;   * Work with prog-mode based major-mode.  Only turned on if the
;;     cursor is inside comment or string.
;;   * Added some package specific predicates.
;; 3 fixed some warnings.
;; 4 use of lexical-biding.
;; 5 use capitalized words of aspell’s dictionary
;;   (see ‘auto-capitalize-aspell-file’)
;;
;; Note that I only used this package in Ubuntu and only Emacs (not
;; XEmacs). So I might be wrongly changed something because original
;; version had some XEmacs specific conditions.  (Pull Requests are
;; welcome)
;;
;;; Code:

;; Rationale:
;;
;; The implementation of auto-capitalize via an after-change-function is
;; somewhat complicated, but two simpler designs don't work due to
;; quirks in Emacs' implementation itself:
;;
;; One idea is to advise `self-insert-command' to `upcase'
;; `last-command-event' before it is run, but command_loop_1 optimizes
;; out the call to the Lisp binding with its C binding
;; (Fself_insert_command), which prevents any advice from being run.
;;
;; Another idea is to use a before-change-function to `upcase'
;; `last-command-event', but the change functions are called by
;; internal_self_insert, which has already had `last-command-event'
;; passed to it as a C function parameter by command_loop_1.

;; Package interface:

(require 'cl-lib) ; cl-find, cl-minusp
(require 'rx)

(defgroup auto-capitalize nil "auto-capitalize customization group"
  :group 'convenience)

;; User options:

(defcustom auto-capitalize-ask nil
  "*If non-nil, ask before capitalize."
  :group 'auto-capitalize
  :type 'boolean)

(defcustom auto-capitalize-yank nil
  "*If non-nil, the first word of yanked sentences are automatically capitalized."
  :group 'auto-capitalize
  :type 'boolean)

;; User variables:

(defcustom auto-capitalize-words '("I");  "Stallman" "GNU" "http"
  "If non-nil, a list of proper nouns or acronyms.
If `auto-capitalize' mode is on, these words will be automatically
capitalized or upcased as listed (mixed case is allowable as well), even
in the middle of a sentence.  A lowercase word will not have its case
modified."
  :group 'auto-capitalize
  :type '(repeat (string :tag "Word list")))

(defcustom auto-capitalize-predicate 'auto-capitalize-default-predicate-function
  "If non-nil, a function that determines whether to enable capitalization.
In auto-capitalize mode, it is called with no arguments and should return a
non-nil value if the current word is within \"normal\" text."
  :group 'auto-capitalize
  :type '(choise (function :tag "Predicate function")
                 (const nil)))

(defcustom auto-capitalize-allowed-chars '(?\  ?, ?. ?? ?' ?’ ?: ?\; ?- ?!)
  "Whether auto capitalize after you typed those characters.
If you set nil, then don't restrict by this variable."
  :group 'auto-capitalize
  :type '(choise (repeat (character :tag "Characters to start"))
                 (const nil)))

(defcustom auto-capitalize-inhibit-buffers '("*scratch*")
  "Inhibit auto capitalize mode in those buffer."
  :group 'auto-capitalize
  :type '(repeat (string :tag "Word list")))

(defcustom auto-capitalize-predicate-functions nil
  "This hook is used to call predicate functions.
The function should return t if the predicate is ok or
return nil if it's failure."
  :group 'auto-capitalize
  :type '(choise
          (repeat (function :tag "Predicate functions"))
          (const nil)))

(defcustom auto-capitalize-aspell-file nil
  "You can set a file path of aspell to use capitalized words of aspell.
The file name would be something like .aspell.en.pws."
  :group 'auto-capitalize
  :type '(choise
          (const nil)
          (file)))

(defvar auto-capitalize-avoid-words-regex
  (rx (not (syntax word)) (or "e.g." "i.e." "vs.") (0+ " "))
  "Regex to avoid words.")

;; Internal variables:

(defconst auto-capitalize-version "$Revision: 2.20 $"
  "This version of auto-capitalize.el.")

(defvar-local auto-capitalize-state nil
  "If non-nil, the first word of a sentence is automatically capitalized.
If non-nil but not t, query the user before capitalizing a word.
This variable automatically becomes buffer-local when set in any fashion\;
see `\\[auto-capitalize-mode]', `\\[turn-on-capitalize-mode]', or
`\\[enable-auto-capitalize-mode]'.")

(defvar auto-capitalize--match-data nil)

;; Maybe this regex has to be changed in XEmacs
(defvar auto-capitalize-regex-lower "[[:lower:]]+")
(defvar auto-capitalize-regex-verify
  "\\<\\([[:upper:]]?[[:lower:]]+\\.\\)+\\=")

;; Commands:

(defun auto-capitalize-default-predicate-function ()
  "Return t if condition is ok."
  (and (not buffer-read-only)
       (not (minibufferp))
       ;; activate if prog-mode and cursor is in string or comment.
       (if (derived-mode-p 'prog-mode)
           (and (derived-mode-p 'prog-mode)
                (save-excursion (nth 8 (syntax-ppss))))
         t)
       ;; don’t capitalize if previous string is something like [a-z].[a-z].
       ;; (it’s mainly to prevent capitalize after i.e. or e.g.)
       (not (and (eq last-command-event ?.)
                 (memq (char-before (max (point-min) (- (point) 2)))
                       '(?\  ?\( ?.))))
       ;; activate after only specific characters you type
       (or (null auto-capitalize-allowed-chars)
           (member last-command-event auto-capitalize-allowed-chars))
       ;; don't turn on like inferior-XXX-mode
       (not (derived-mode-p 'comint-mode))
       ;; For user hook
       (run-hook-with-args-until-failure auto-capitalize-predicate-functions)
       ;; For specific major-mode
       (let ((fname (intern (format "auto-capitalize-predicate-%s" major-mode))))
         (if (fboundp fname)
             (funcall fname)
           t))))

;;;###autoload
(easy-mmode-define-minor-mode auto-capitalize-mode
  "Toggle `auto-capitalize' minor mode in this buffer.
With optional prefix ARG, turn `auto-capitalize' mode on iff ARG is positive.
This sets `auto-capitalize' to t or nil (for this buffer) and ensures that
`auto-capitalize' is installed in `after-change-functions' (for all buffers)."
  nil " ACap" nil
  (cond
   ;; Turn off
   ((or (not auto-capitalize-mode) buffer-read-only
        (member (buffer-name) auto-capitalize-inhibit-buffers))
    (setq-local auto-capitalize-state nil)
    (remove-hook 'after-change-functions 'auto-capitalize-capitalize t))
   ;; Turn on
   (t
    (setq-local auto-capitalize-state t)
    (add-hook 'after-change-functions 'auto-capitalize-capitalize nil t))))

;;;###autoload
(defun turn-on-auto-capitalize-mode ()
  "Turn on `auto-capitalize' mode in this buffer.
This sets `auto-capitalize' to t."
  (interactive)
  (auto-capitalize-mode 1))

;;;###autoload
(defun turn-off-auto-capitalize-mode ()
  "Turn off `auto-capitalize' mode in this buffer.
This sets `auto-capitalize' to nil."
  (interactive)
  (auto-capitalize-mode -1))

;;;###autoload
(defun enable-auto-capitalize-mode ()
  "Enable `auto-capitalize' mode in this buffer.
This sets `auto-capitalize-state' to t."
  (interactive)
  (setq auto-capitalize-ask t))

;; Internal functions:

(defun auto-capitalize-sentence-end()
  "portability function. emacs 22.0.50 introduced sentence-end
function, not available on other emacsen.
Fix known to work on 23.0.90 and later"
  (if (fboundp 'sentence-end)
      (sentence-end)
    sentence-end))

(defun auto-capitalize-condition (beg end length)
  "Check condition."
  (condition-case error
      (or (and (or (eq this-command 'self-insert-command)
                   ;; LaTeX mode binds "." to TeX-insert-punctuation,
                   ;; and "\"" to TeX-insert-quote:
                   (let ((key (this-command-keys)))
                     ;; XEmacs `lookup-key' signals "unable to bind
                     ;; this type of event" for commands invoked via
                     ;; the mouse:
                     (and (not (and (vectorp key)
                                    (> (length key) 0)
                                    (fboundp 'misc-user-event-p)
                                    (misc-user-event-p (aref key 0))))
                          (eq (lookup-key global-map key t)
                              'self-insert-command)
                          ;; single character insertion?
                          (= length 0)
                          (= (- end beg) 1))))
               (let ((self-insert-char
                      (cond ((fboundp 'event-to-character) ; XEmacs
                             (event-to-character last-command-event
                                                 nil nil t))
                            (t last-command-event)))) ; GNU Emacs
                 (not (equal (char-syntax self-insert-char) ?w))))
          (memq this-command '(newline newline-and-indent)))
    (error error)))

(defun auto-capitalize-capitalize (beg end length)
  "If `auto-capitalize' mode is on, then capitalize the previous word.
The previous word is capitalized (or upcased) if it is a member of the
`auto-capitalize-words' list; or if it begins a paragraph or sentence.

Capitalization occurs only if the current command was invoked via a
self-inserting non-word character (e.g. whitespace or punctuation)\; but
if the `auto-capitalize-yank' option is set, then the first word of
yanked sentences will be capitalized as well.

Capitalization can be disabled in specific contexts via the
`auto-capitalize-predicate' variable.

This should be installed as an `after-change-function'."
  (condition-case error
      (when (and auto-capitalize-state
                 (or (null auto-capitalize-predicate)
                     (funcall auto-capitalize-predicate)))
        (cond ((auto-capitalize-condition beg end length)
               ;; self-inserting, non-word character
               (when (and (> beg (point-min))
                          (equal (char-syntax (char-after (1- beg))) ?w))
                 (auto-capitalize-capitalize-preceded-word)))
              ((and auto-capitalize-yank
                    ;; `yank' sets `this-command' to t, and the
                    ;; after-change-functions are run before it has been
                    ;; reset:
                    (or (eq this-command 'yank)
                        (and (= length 0) ; insertion?
                             (eq this-command 't))))
               (save-excursion
                 (goto-char beg)
                 (save-match-data
                   (while (re-search-forward "\\Sw" end t)
                     (setq auto-capitalize--match-data (match-data))
                     ;; recursion!
                     (let* ((this-command 'self-insert-command)
                            (non-word-char (char-after (match-beginning 0)))
                            (last-command-event
                             (cond ((fboundp 'character-to-event) ; XEmacs
                                    (character-to-event non-word-char))
                                   (t non-word-char)))) ; GNU Emacs
                       (set-match-data auto-capitalize--match-data)
                       (auto-capitalize-capitalize (match-beginning 0)
                                        (match-end 0)
                                        0))))))))
    (error error)))

(defun auto-capitalize-user-specified (lowercase-word m-beg m-end)
  "Find LOWERCASE-WORD and capitalize it.
The M-BEG and M-END are used to substring LOWERCASE-WORD."
  (when (not (member (setq lowercase-word
                           (buffer-substring m-beg m-end))
                     auto-capitalize-words))
    ;; not preserving lower case
    ;; capitalize!
    (undo-boundary)
    (replace-match (cl-find lowercase-word
                            auto-capitalize-words
                            :key 'downcase
                            :test 'string-equal)
                   t t)))

(defun auto-capitalize-capitalizable-p (text-start word-start)
  ""
  (goto-char text-start)
  (and (or (equal text-start (point-min)) ; (bobp)
           ;; beginning of paragraph?
           (and (= (current-column) left-margin)
                (or (save-excursion
                      (and (zerop (forward-line -1))
                           (looking-at paragraph-separate)))
                    (save-excursion
                      (and (re-search-backward paragraph-start
                                               nil t)
                           (= (match-end 0) text-start)
                           (= (current-column) left-margin)))))
           ;; beginning of sentence?
           (save-excursion
             (save-restriction
               (narrow-to-region (point-min) word-start)
               (and (re-search-backward (auto-capitalize-sentence-end)
                                        nil t)
                    (= (match-end 0) text-start)
                    ;; verify: preceded by whitespace?
                    (let ((previous-char (char-before text-start)))
                      ;; In some modes, newline (^J, aka LFD) is comment-end,
                      ;; not whitespace:
                      (or (eq ?\n previous-char)
                          (eq ?\  (char-syntax previous-char))))
                    ;; verify: not preceded by an abbreviation?
                    (let ((case-fold-search nil)
                          (abbrev-regexp auto-capitalize-regex-verify))
                      (goto-char
                       (1+ (match-beginning 0)))
                      (or (not
                           (re-search-backward abbrev-regexp nil t))
                          (not
                           (member (match-string 0) auto-capitalize-words))))))))
       ;; inserting lowercase text?
       (let ((case-fold-search nil))
         (goto-char word-start)
         (looking-at auto-capitalize-regex-lower))
       (and (eq auto-capitalize-state t)
            (if (not auto-capitalize-ask)
                t
              (auto-capitalize--ask)))))

(defun auto-capitalize--ask ()
  (prog1 (y-or-n-p
          (format "Capitalize \"%s\"? "
                  (buffer-substring (match-beginning 0) (match-end 0))))
    (message "")))

(defun auto-capitalize--avoid-word-p ()
  "Return non-nil if previous word is matched ‘auto-capitalize-avoid-words’."
  (if auto-capitalize-avoid-words-regex
      (looking-back auto-capitalize-avoid-words-regex nil)
    nil))

(defun auto-capitalize-capitalize-preceded-word ()
  "Capitalize preceded by a word character."
  (save-excursion
    (forward-word -1)
    (unless (auto-capitalize--avoid-word-p)
      (save-match-data
        (let* ((word-start (point))
               (text-start (auto-capitalize--backward))
               lowercase-word)
          (cond ((and auto-capitalize-words
                      (let ((case-fold-search nil))
                        (goto-char word-start)
                        (looking-at
                         (concat "\\("
                                 (mapconcat 'downcase
                                            auto-capitalize-words
                                            "\\|")
                                 "\\)\\>"))))
                 (auto-capitalize-user-specified
                  lowercase-word (match-beginning 1) (match-end 1)))
                ((auto-capitalize-capitalizable-p
                  text-start word-start)
                 ;; capitalize!
                 (undo-boundary)
                 (goto-char word-start)
                 (capitalize-word 1))))))))

(defun auto-capitalize--backward ()
  "Return point of text start."
  (while (or (cl-minusp (skip-chars-backward "\""))
             (cl-minusp (skip-syntax-backward "\"(")))
    t)
  (point))

(defun auto-capitalize--get-buffer-string (file)
  "Get buffer string from FILE."
  (let* ((current        (current-buffer))
         (aspell-buffer  (find-file-noselect file))
         words)
    (switch-to-buffer aspell-buffer)
    (setq words (buffer-substring-no-properties (point-min) (point-max)))
    (switch-to-buffer current)
    words))

(defun auto-capitalize--get-aspell-capital-words (file)
  "Return list of words from FILE."
  (if (file-exists-p file)
      (cl-loop with personal-dict = (auto-capitalize--get-buffer-string file)
               with words = (split-string personal-dict "\n")
               with case-fold-search = nil
               for word in words
               if (string-match "[A-Z]" word)
               collect word)
    (error (format "The file %s doesn't exist" file))))

(defun auto-capitalize-merge-aspell-words (&optional file)
  "Extract words from FILE and merge ti to ‘auto-capitalize-words’."
  (let ((f (or auto-capitalize-aspell-file file)))
    (when (file-exists-p f)
      (setq auto-capitalize-words
            (append auto-capitalize-words
                    (auto-capitalize--get-aspell-capital-words f))))))

;;;###autoload
(defun auto-capitalize-setup ()
  "Setup auto-capitalize."
  (auto-capitalize-merge-aspell-words)
  (add-hook 'after-change-major-mode-hook 'auto-capitalize-mode))

;; Org mode
(with-eval-after-load "org"
  (defun auto-capitalize-predicate-org-mode ()
    (if (not (eq major-mode 'org-mode))
        t
      (not (and (fboundp 'org-in-src-block-p) (org-in-src-block-p))))))

;; SKK (ddskk)
(with-eval-after-load "skk"
  (add-to-list
   'auto-capitalize-predicate-functions
   (lambda ()
     (or (not (bound-and-true-p skk-mode))
         (and (bound-and-true-p skk-mode)
              (fboundp 'skk-current-input-mode)
              (eq 'latin (skk-current-input-mode)))))))

;; 1 Jun 2009: It does not work with Aquamacs 1.7/GNUEmacs 22. Only the first word in the buffer
;; (or the first word typed after mode activation) is capitalized.
;; Maybe the code is too old (1998). -- Rikal

;; 29 Aug 2009: Added auto-capitalize-sentence-end which should probably work on older and current day emacsen
;; tested on 23.0.90, please test on your emacs
;; -- dtaht

;; 30 Nov 2010: @Rikal: Are you ending sentences as required (e.g.: with two spaces)? Check "C-h f sentence-end RET".
;; -- elena

;; 6 Sep 2013: Apply SKK package and split functions
;; -- Yuta

(provide 'auto-capitalize)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; auto-capitalize.el ends here
