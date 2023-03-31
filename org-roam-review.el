;;; org-roam-review.el --- Extends org-roam with spaced-repetition review of nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (ts "0.3-pre") (org-roam "2.2.2") (org-drill "2.7.0"))

;; Homepage: https://github.com/chrisbarrett/nursery

;; Version: 0.0.1-pre

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides commands to categorise and review org-roam nodes for Evergreen
;; note-taking. Notes are surfaced using the spaced-repetition algorithm from
;; org-drill.

;; The main entrypoint is `M-x org-roam-review', which shows your nodes due for
;; review and refinement. With a prefix argument, that command will list all
;; your nodes by category, which is useful for getting a quick overview of your
;; Evergreens.

;; Example configuration:
;;
;;     (use-package org-roam-review
;;       :commands (org-roam-review
;;                  org-roam-review-list-by-maturity
;;                  org-roam-review-list-recently-added)
;;       ;; Optional - tag all newly-created notes as seedlings
;;       :hook (org-roam-capture-new-node . org-roam-review-set-seedling)
;;       :general
;;       ;; Optional - bindings for evil-mode compatability.
;;       (:states '(normal) :keymaps 'org-roam-review-mode-map
;;        "TAB" 'magit-section-cycle
;;        "g r" 'org-roam-review-refresh)
;;       (:keymaps 'org-mode-map
;;        "C-c r r" '(org-roam-review-accept :wk "accept")
;;        "C-c r f" '(org-roam-review-forgot :wk "forgot")
;;        "C-c r u" '(org-roam-review-bury :wk "bury")
;;        "C-c r m" '(org-roam-review-set-memorise :wk "set memorise")
;;        "C-c r x" '(org-roam-review-set-excluded :wk "set excluded")
;;        "C-c r b" '(org-roam-review-set-budding :wk "set budding")
;;        "C-c r s" '(org-roam-review-set-seedling :wk "set seedling")
;;        "C-c r e" '(org-roam-review-set-evergreen :wk "set evergreen")))

;;; Code:

(require 'dash)
;; (require 'org-tags-filter)
(require 'org-drill)
(require 'org-roam-node)
(require 'org-roam-dailies)
;; (require 'plisty)
(require 'ts)

(defgroup org-roam-review nil
  "Extends org-roam with spaced-repetition review of nodes."
  :group 'productivity
  :prefix "org-roam-review-")

(defcustom org-roam-review-tags-ignored-for-review-buffer '("dailies")
  "A list of tags that disqualify a node from review."
  :group 'org-roam-review
  :type '(list string))

(defcustom org-roam-review-show-instructions-p t
  "Whether to show instructions in review buffers."
  :group 'org-roam-review
  :type 'boolean)

(defcustom org-roam-review-ignored-tags '("dailies")
  "A list of tags for nodes that should never be reviewable."
  :group 'org-roam-review
  :type '(list string))

(defcustom org-roam-review-kill-reviewed-buffers-p t
  "Whether to kill a buffer after it has been accepted in a review."
  :group 'org-roam-review
  :type 'boolean)

(defcustom org-roam-review-title-formatter #'org-roam-review-default-title-formatter
  "Function that renders node title in review buffers.

It must take a node and return a (possibly propertized) string."
  :group 'org-roam-review
  :type 'function)

(defface org-roam-review-instructions
  '((t
     (:inherit font-lock-comment-face)))
  "Face for instructional information in a review buffer."
  :group 'org-roam-review)

(defface org-roam-review-heading
  '((t (:inherit org-level-2 :bold t)))
  "Face for headings in review buffers."
  :group 'org-roam-review)

(defface org-roam-review-tags-filter-keyword
  '((t (:bold t)))
  "Face for the tags heading in review buffers."
  :group 'org-roam-review)

(defface org-roam-review-tags-filter
  '((t (:italic t)))
  "Face for tag filter text in review buffers."
  :group 'org-roam-review)

(defconst org-roam-review-maturity-values '("seedling" "evergreen" "budding"))

(defconst org-roam-review-maturity-emoji-alist
  '(("seedling" . "🌱")
    ("budding" . "🪴")
    ("evergreen" . "🌲")))

(defconst org-roam-review-properties
  '("LAST_REVIEW"
    "NEXT_REVIEW"
    "MATURITY"
    "DRILL_LAST_INTERVAL"
    "DRILL_REPEATS_SINCE_FAIL"
    "DRILL_TOTAL_REPEATS"
    "DRILL_FAILURE_COUNT"
    "DRILL_AVERAGE_QUALITY"
    "DRILL_EASE")
  "List of properties managed by `org-roam-review'.")

(defvar org-roam-review-node-accepted-hook nil
  "Hook run after marking a node as successfully reviewed.

The hook is run within `org-roam-review-accept', with that node
as the current buffer.")

(defvar org-roam-review-node-forgotten-hook nil
  "Hook run after marking a node as forgotten.

The hook is run within `org-roam-review-forgot', with that node
as the current buffer.")

(defvar org-roam-review-node-buried-hook nil
  "Hook run after marking a node as successfully reviewed.

The hook is run within `org-roam-review-bury', with that node as
the current buffer.")

(defvar org-roam-review-node-processed-hook '(org-roam-review--update-workspace-for-completed-review)
  "Hook run whenever a node is buried or accepted in a review.

The hook is run with the node as the current buffer.")

(defvar org-roam-review-next-node-selected-hook '(org-roam-review-open-node-if-in-review-session)
  "A hook executed when point advances to the next node for review.

Running `org-roam-review-accept' or `org-roam-review-bury' causes
point to advance to the next section in the review buffer, when
open. The hook is runs with the review buffer as the current
buffer, and with point at the section corresponding to the next
node for review.

The default value for this hook means the next node for
review is automatically opened, where available.")


;;; SRS property management & parsing

;; We parse & store a number of properties on nodes to track review state.

(defvar org-roam-review--maturity-score-revisit 1)
(defvar org-roam-review--maturity-score-ok 4)
(defvar org-roam-review--maturity-score-bury 5)

(defun org-roam-review--update-next-review (quality)
  "Adapted from org-drill.

QUALITY is a number 0-5 inclusive.

- only use sm5 algorithm for simplicity
- use properties instead of SCHEDULED.
- remove support for 'weighting' a node."
  (-let* ((ofmatrix org-drill-sm5-optimal-factor-matrix)
          ((last-interval repetitions failures total-repeats meanq ease) (org-drill-get-item-data))
          ((next-interval repetitions ease failures meanq total-repeats new-ofmatrix)
           (org-drill-determine-next-interval-sm5 last-interval repetitions
                                                  ease quality failures
                                                  meanq total-repeats ofmatrix))
          (next-interval (round (if (cl-minusp next-interval)
                                    next-interval
                                  (max 1.0 (+ last-interval (- next-interval last-interval))))))
          (new-time (ts-adjust 'day next-interval (ts-now))))
    (setq org-drill-sm5-optimal-factor-matrix new-ofmatrix)
    (org-drill-store-item-data next-interval repetitions failures total-repeats meanq ease)

    (let ((next-review (ts-format "[%Y-%m-%d %a]" new-time)))
      (org-set-property "NEXT_REVIEW" next-review)
      next-review)))

(defun org-roam-review--update-node-srs-properties (score &optional maturity)
  "Set the MATURITY and updated SCORE for a node.

A higher score means that the node will appear less frequently."
  (cl-assert (or (null maturity) (member maturity org-roam-review-maturity-values)))
  (cl-assert (derived-mode-p 'org-mode))
  (when (and maturity (org-roam-review--daily-file-p (buffer-file-name)))
    (user-error "Cannot set maturity on daily file"))
  (let ((id (org-entry-get (point-min) "ID")))
    (unless id
      (error "Not visiting an org-roam node--no ID property found"))
    (org-with-point-at (org-find-property "ID" id)
      (atomic-change-group
        (let ((next-review (org-roam-review--update-next-review score)))
          (ignore-errors
            (org-roam-tag-remove org-roam-review-maturity-values))

          (when maturity
            (org-roam-tag-add (list maturity))
            (org-set-property "MATURITY" maturity))

          (org-set-property "LAST_REVIEW" (org-format-time-string "[%Y-%m-%d %a]"))

          (save-buffer)

          (if maturity
              (message "Maturity set to '%s'. Review scheduled for %s" maturity next-review)
            (message "Review scheduled for %s" next-review)))))))

(defun org-roam-review-node-ignored-p (node &optional filter-plist)
  (let* ((filter-plist (or filter-plist org-tags-filter-last-value))
         (tags (org-roam-node-tags node))
         (forbidden-tags (org-tags-filter-forbidden filter-plist))
         (required-tags (org-tags-filter-required filter-plist)))
    (or (seq-intersection tags forbidden-tags)
        (seq-difference required-tags tags))))

(defun org-roam-review-node-created-at (node)
  (-when-let* (((&alist "CREATED" created) (org-roam-node-properties node)))
    (ts-parse-org created)))

(defun org-roam-review-node-next-review (node)
  (-when-let* (((&alist "NEXT_REVIEW" next-review) (org-roam-node-properties node)))
    (ts-parse-org next-review)))

(defun org-roam-review-node-maturity (node)
  (-when-let* (((&alist "MATURITY" maturity) (org-roam-node-properties node)))
    (intern maturity)))

(defun org-roam-review-node-list ()
  "Return all org-roam-nodes that are not explicitly ignored from reviews."
  (let ((table (ht-create)))
    (dolist (node (org-roam-node-list))
      (unless (org-roam-review-node-ignored-p node)
        (ht-set table (org-roam-node-id node) node)))
    (ht-values table)))


;;; Review buffers

(defmacro org-roam-review--with-current-review-buffer (&rest body)
  (declare (indent 0))
  `(if-let* ((buf (car (org-roam-review-buffers))))
       (with-current-buffer buf
         ,@body)
     (error "No review buffer")))

(defun org-roam-review--daily-file-p (&optional file)
  "Test whether FILE is a daily node.

If FILE is not given, checks the current buffer.

This is a wrapper that makes sure org-roam-directory is well-formed.

See:
https://github.com/org-roam/org-roam/issues/2032"
  (cl-assert (or file (buffer-file-name)))
  (let ((org-roam-directory (string-remove-suffix org-roam-dailies-directory org-roam-directory)))
    (org-roam-dailies--daily-note-p file)))

(defun org-roam-review--tags-at-pt (&optional local)
  (seq-map #'substring-no-properties
           (if (org-before-first-heading-p)
               org-file-tags
             (org-get-tags nil local))))

(defvar-local org-roam-review-buffer-refresh-command nil)

(defun org-roam-review-buffers ()
  (seq-filter (lambda (buf)
                (and (buffer-live-p buf)
                     (with-current-buffer buf
                       (derived-mode-p 'org-roam-review-mode))))
              (buffer-list)))

(defun org-roam-review-refresh (&optional interactive-p)
  "Rebuild the review buffer.

INTERACTIVE-P indicates that the function was called
interactively. Extra messages will be logged."
  (interactive "P")
  (dolist (buf (org-roam-review-buffers))
    (with-current-buffer buf
      (unless org-roam-review-buffer-refresh-command
        (error "Refresh command not defined"))
      (funcall org-roam-review-buffer-refresh-command)))
  (when interactive-p
    (message "Buffer refreshed")))

(defun org-roam-review-modify-tags (tags-filter &optional no-refresh)
  "Read tags filter interactively.

TAGS-FILTER is plist of type `org-tags-filter'.

NO-REFRESH means don't update open org-roam-review buffers.

When called with a `C-u' prefix arg, clear the current filter."
  (interactive (list
                (unless current-prefix-arg
                  (org-tags-filter-read))))
  (setq org-tags-filter-last-value tags-filter)
  (unless no-refresh
    (org-roam-review-refresh t)))

(defvar org-roam-review-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "/") #'org-roam-review-modify-tags)
    (define-key keymap (kbd "TAB") #'magit-section-cycle)
    (define-key keymap (kbd "g") #'org-roam-review-refresh)
    (define-key keymap (kbd "a") #'org-roam-review-accept)
    (define-key keymap (kbd "u") #'org-roam-review-bury)
    (define-key keymap (kbd "x") #'org-roam-review-set-excluded)
    (define-key keymap [remap org-roam-buffer-refresh] #'org-roam-review-refresh)
    keymap))

(defun org-roam-review--refresh-buffer-override (fn &rest args)
  (if (equal (buffer-name) org-roam-buffer)
      (apply fn args)
    (call-interactively 'org-roam-review-refresh)))

(define-derived-mode org-roam-review-mode org-roam-mode "Org-roam-review"
  "Major mode for displaying relevant information about Org-roam nodes for review."
  :group 'org-roam-review
  ;; HACK: avoid all calls to org-roam-buffer-review if we're in a review
  ;; buffer, since it will error.
  (advice-add 'org-roam-buffer-refresh :around #'org-roam-review--refresh-buffer-override))

(defvar org-roam-review-indent-width 2)

(defun org-roam-review-indent-string (str depth)
  (replace-regexp-in-string (rx bol) (make-string (* depth org-roam-review-indent-width) 32)
                            str))

(cl-defun org-roam-review-insert-preview (node &key point (depth 0))
  (magit-insert-section section (org-roam-preview-section)
    (let* ((start (or point (org-roam-node-point node)))
           (file (org-roam-node-file node))
           (preview (org-roam-fontify-like-in-org-mode (org-roam-preview-get-contents file start)))
           (post-formatters
            (append org-roam-preview-postprocess-functions
                    (list (lambda (content)
                            (org-roam-review-indent-string (if (string-blank-p (string-trim-left content))
                                                               (propertize "(Empty)" 'font-lock-face 'font-lock-comment-face)
                                                             content)
                                                           depth)))))
           (formatted-preview (seq-reduce (lambda (str fn)
                                            (save-match-data
                                              (save-excursion
                                                (funcall fn str))))
                                          post-formatters
                                          preview)))
      (oset section file file)
      (oset section point start)
      (insert formatted-preview)
      (insert "\n\n"))))

(defun org-roam-review-default-title-formatter (node)
  (propertize (org-roam-node-title node)
              'font-lock-face 'magit-section-secondary-heading))

(defun org-roam-review--insert-node (node)
  (atomic-change-group
    (magit-insert-section section (org-roam-node-section (org-roam-node-id node) t)
      (magit-insert-heading (funcall org-roam-review-title-formatter node))
      (oset section node node)
      ;; KLUDGE: Mofified macro-expansion of `magit-insert-section-body' that
      ;; avoids unsetting the parent section's keymap.
      (oset section washer
            (lambda ()
              (org-roam-review-insert-preview node)
              (magit-section-maybe-remove-visibility-indicator section))))))

(defvar org-roam-review-default-placeholder
  (propertize "(None)" 'face 'font-lock-comment-face))

(defun org-roam-review--insert-nodes (nodes placeholder)
  (if nodes
      (--each (nreverse nodes)
        (org-roam-review--insert-node it))
    (insert (or placeholder org-roam-review-default-placeholder))
    (newline)))

(plisty-define org-roam-review-render-args
  :optional (:group-on :nodes :placeholder :sort)
  :required (:root-section))

(defclass org-roam-review-grouping-section (magit-section) ())

(defalias 'org-roam-review--render
  (-lambda ((&plist :group-on :nodes :placeholder :sort :root-section))
    (let ((sort (or sort (-const t))))
      (cond
       ((null nodes)
        (insert (or placeholder org-roam-review-default-placeholder))
        (newline))
       (group-on
        (let ((grouped (->> (seq-group-by group-on nodes)
                            (-sort (-on #'<= (-lambda ((key . _))
                                               (if (stringp key) key (or (cdr key) 0))))))))
          (pcase-dolist (`(,key . ,group) grouped)
            (when (and key group)
              (let ((header (format "%s (%s)"
                                    (if (stringp key) key (car key))
                                    (length group))))
                (magit-insert-section section (org-roam-review-grouping-section header)
                  (oset section parent root-section)
                  (magit-insert-heading (propertize header 'font-lock-face 'magit-section-heading))
                  (org-roam-review--insert-nodes (-sort sort group) placeholder)
                  (insert "\n")))))))
       (t
        (org-roam-review--insert-nodes (-sort sort nodes) placeholder))))))

(cl-defun org-roam-review--re-render (&key render title instructions group-on placeholder sort nodes)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-review-mode)
    (org-roam-buffer-set-header-line-format title)
    (magit-insert-section root-section (root)
      (when (and org-roam-review-show-instructions-p instructions nodes)
        (let ((start (point)))
          (insert (propertize instructions 'font-lock-face 'org-roam-review-instructions))
          (fill-region start (point)))
        (newline 2))

      (let ((forbidden-tags (seq-map (lambda (it) (format "-%s" it)) (org-tags-filter-forbidden org-tags-filter-last-value)))
            (required-tags (seq-map (lambda (it) (format "+%s" it)) (org-tags-filter-required org-tags-filter-last-value))))
        (when (or forbidden-tags required-tags)
          (insert (concat (propertize "Filters:" 'face 'org-roam-review-tags-filter-keyword)
                          " "
                          (propertize (string-join (append forbidden-tags required-tags) " ") 'face 'org-roam-review-tags-filter)))
          (newline 2)))

      (let ((start-of-content (point)))
        (funcall render
                 (org-roam-review-render-args-create :nodes nodes
                                                     :group-on group-on
                                                     :sort sort
                                                     :root-section root-section
                                                     :placeholder placeholder))
        (goto-char start-of-content)))))

(cl-defun org-roam-review-create-buffer
    (&key title instructions group-on placeholder sort
          (nodes #'org-roam-review-node-list)
          (buffer-name "*org-roam-review*")
          (render 'org-roam-review--render))
  "Create a buffer for displaying nodes via `magit-section'.

The following keyword arguments are required:

- TITLE is the header line for the buffer.

- INSTRUCTIONS is a paragraph inserted below the title. It is
  automatically paragraph-filled.

The following keyword arguments are optional:

- NODES is a function returning a list of nodes to display (which
  is possibly empty). It defaults to all non-ignored nodes.

- PLACEHOLDER is a string to be shown if there are no nodes to
  display.

- BUFFER-NAME is the name to use for the created buffer.

- RENDER is a function taking a single argument, a plist of type
  `org-roam-review-render-args', that populates the buffer using
  the magit-section API. It can be used to override the default
  rendering behaviour.

- GROUP-ON is a projection function that is passed a node and
  should return one of:

    - nil, meaning the node should be omitted

    - a string to use for grouping the node

    - a cons of `(GROUP-NAME . GROUP-PRIORITY)', where:

        - GROUP-NAME is the string for grouping the node

        - GROUP-PRIORITY is a number used to order group in the
          buffer.

- SORT is a projection function that is passed two nodes within a
  group and returns non-nil if the first element should sort
  before the second."
  (cl-assert title)
  (cl-assert (functionp nodes))
  (let (re-render)
    (setq re-render
          (lambda (updated-nodes)
            (with-current-buffer (get-buffer-create buffer-name)
              (org-roam-review--re-render :title title
                                          :instructions instructions
                                          :nodes updated-nodes
                                          :group-on group-on
                                          :placeholder placeholder
                                          :sort sort
                                          :render render)
              (setq-local org-roam-review-buffer-refresh-command (lambda () (funcall re-render (funcall nodes))))
              (current-buffer))))
    (funcall re-render (funcall nodes))))

;;;###autoload
(defun org-roam-review (&optional all)
  "List nodes that are due for review.

With optional prefix arg ALL, list all evergreen nodes
categorised by their maturity."
  (interactive "P")
  (if all
      (org-roam-review-list-by-maturity)
    (org-roam-review-list-due)))

(defun org-roam-review--maturity-header (node)
  (if (member "memo" (org-roam-node-tags node))
      (cons "Memorise 💭" 0)
    (pcase (org-roam-review-node-maturity node)
      ('seedling (cons "Seedling 🌱" 3))
      ('budding (cons "Budding 🪴" 2))
      ('evergreen (cons "Evergreen 🌲" 1))
      (value value))))

(defun org-roam-review-node-due-p (node)
  (when-let* ((next-review (org-roam-review-node-next-review node)))
    (ts<= next-review (ts-now))))

;;;###autoload
(defun org-roam-review-list-due ()
  "List nodes that are due for review."
  (interactive)
  (display-buffer
   (org-roam-review-create-buffer
    :title "Due Notes"
    :instructions "The nodes below are due for review.
Read each node and add new thoughts and connections, then mark
them as reviewed with `org-roam-review-accept',
`org-roam-review-bury' or by updating their maturity."
    :placeholder (concat (propertize "You're up-to-date!" 'face 'font-lock-comment-face) " 😸")
    :group-on #'org-roam-review--maturity-header
    :sort (-on #'ts< #'org-roam-review-node-next-review)
    :nodes
    (lambda ()
      (seq-filter (lambda (node)
                    (and (zerop (org-roam-node-level node))
                         (null (seq-intersection (org-roam-node-tags node)
                                                 org-roam-review-tags-ignored-for-review-buffer))
                         (org-roam-review-node-due-p node)))
                  (org-roam-review-node-list))))))

(defalias 'org-roam-review-sort-by-title-case-insensitive
  (-on #'string-greaterp (-compose  #'downcase #'org-roam-node-title)))

;;;###autoload
(defun org-roam-review-list-by-maturity ()
  "List all evergreen nodes categorised by maturity."
  (interactive)
  (display-buffer
   (org-roam-review-create-buffer
    :title "Evergreen Notes"
    :instructions "The nodes below are categorised by maturity."
    :group-on #'org-roam-review--maturity-header
    :sort #'org-roam-review-sort-by-title-case-insensitive)))

(defun org-roam-review--node-added-group (node)
  (when-let* ((created (org-roam-review-node-created-at node))
              (recently (ts-adjust 'hour -24 (ts-now))))
    (cond
     ((ts<= recently created)
      (cons "Recent" 1))
     ((ts<= (ts-adjust 'day -3 recently) created)
      (cons "Last 3 days" 2))
     ((ts<= (ts-adjust 'day -7 recently) created)
      (cons "Last week" 3)))))

;;;###autoload
(defun org-roam-review-list-recently-added ()
  "List nodes that were created recently, grouped by time."
  (interactive)
  (display-buffer
   (org-roam-review-create-buffer
    :title "Recently Created Notes"
    :instructions "The nodes below are sorted by when they were created."
    :group-on #'org-roam-review--node-added-group
    :sort #'org-roam-review-sort-by-title-case-insensitive)))


;;; Commands for manipulating node review state.

(defun org-roam-review--update-workspace-for-completed-review ()
  (when-let* ((buf (get-buffer "*org-roam-review*")))
    (display-buffer buf)))

(defmacro org-roam-review--visiting-node-at-point (&rest body)
  (declare (indent 0))
  `(let* ((node (org-roam-node-at-point t))
          (file (org-roam-node-file node)))
     (cond
      (file
       (with-current-buffer (find-file-noselect file)
         (save-excursion
           (goto-char (org-roam-node-point node))
           ,@body)))
      ((derived-mode-p 'org-mode)
       (org-with-wide-buffer
        (point-min)
        ,@body))
      (t
       (error "Invalid context for visiting node")))
     node))

(defun org-roam-review--in-multiwindow-session-p ()
  (and (< 1 (length (window-list)))
       (seq-find (lambda (it) (equal (get-buffer "*org-roam-review*") (window-buffer it)))
                 (window-list))))

(defun org-roam-review-open-node-if-in-review-session ()
  (when (org-roam-review--in-multiwindow-session-p)
    (when-let* ((node (org-roam-node-at-point)))
      (org-roam-node-visit node))))

(defun org-roam-review--forward-to-uncommented-sibling ()
  (ignore-errors
    (let ((section (magit-current-section))
          (stop)
          (found))
      (while (not stop)
        (magit-section-forward-sibling)
        ;; Skip over group headlines
        (magit-section-case
          (org-roam-review-grouping-section
           (magit-section-forward)))

        (setq found (not (equal 'font-lock-comment-face (get-text-property (point) 'face))))
        (let ((unchanged (equal (magit-current-section) section)))
          (setq stop (or found unchanged))))
      found)))

(defun org-roam-review--update-review-buffer-entry (node)
  ;; Nothing to do if there are no review buffers.
  (when (org-roam-review-buffers)
    (org-roam-review--with-current-review-buffer
      (let ((continue t)
            (found-pos nil)
            (id (org-roam-node-id node)))
        (save-excursion
          (goto-char (point-min))
          (while (and (not found-pos) continue)
            (if (equal id (ignore-errors (org-roam-node-id (org-roam-node-at-point))))
                (setq continue nil
                      found-pos (point))
              (or (ignore-errors (magit-section-forward) t)
                  (setq continue nil)))))

        (when found-pos
          (goto-char found-pos)
          (when-let* ((section (magit-current-section)))
            (when (oref section node)
              (let ((inhibit-read-only t))
                (put-text-property (line-beginning-position) (line-end-position) 'face 'font-lock-comment-face))
              (magit-section-hide section))))))))

(defmacro org-roam-review--transform-selected-sections (&rest body)
  "Execute BODY, possibly over multiple sections.

Return the affected sections."
  (declare (indent 0))
  `(progn
     (if-let* ((sections (-list (or (magit-region-sections) (magit-section-at)))))
         (dolist (section sections)
           (goto-char (oref section start))
           (let ((buf (current-buffer))
                 (result (progn ,@body)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (deactivate-mark)
                 (magit-section-deactivate-mark)
                 (magit-section-update-highlight)))
             result))
       ,@body)

     (when (org-roam-review-buffers)
       (org-roam-review--with-current-review-buffer
         (when (org-roam-review--forward-to-uncommented-sibling)
           (run-hooks 'org-roam-review-next-node-selected-hook))))))

(defun org-roam-review--maybe-kill-reviewed-buffer (buf)
  (when (and org-roam-review-kill-reviewed-buffers-p (buffer-live-p buf))
    (with-current-buffer buf
      (save-buffer)
      (kill-buffer))))

;;;###autoload
(defun org-roam-review-accept ()
  "Confirm review of the current node."
  (interactive)
  (let ((count 0))
    (org-roam-review--transform-selected-sections
      (cl-incf count)
      (let ((node (org-roam-review--visiting-node-at-point
                    (org-roam-review--update-node-srs-properties org-roam-review--maturity-score-ok
                                                                 (org-entry-get-with-inheritance "MATURITY"))
                    (let ((buf (current-buffer)))
                      (run-hooks 'org-roam-review-node-accepted-hook)
                      (run-hooks 'org-roam-review-node-processed-hook)
                      (org-roam-review--maybe-kill-reviewed-buffer buf)))))
        (org-roam-review--update-review-buffer-entry node)))
    (message "Node%s scheduled for future review" (if (= 1 count) "" "s"))))

;;;###autoload
(defun org-roam-review-forgot ()
  "Mark the current node as requiring review again soon."
  (interactive)
  (let ((count 0))
    (org-roam-review--transform-selected-sections
      (cl-incf count)
      (let ((node (org-roam-review--visiting-node-at-point
                    (org-roam-review--update-node-srs-properties org-roam-review--maturity-score-revisit
                                                                 (org-entry-get-with-inheritance "MATURITY"))
                    (let ((buf (current-buffer)))
                      (run-hooks 'org-roam-review-node-forgotten-hook)
                      (run-hooks 'org-roam-review-node-processed-hook)
                      (org-roam-review--maybe-kill-reviewed-buffer buf)))))
        (org-roam-review--update-review-buffer-entry node)))
    (message "Node%s scheduled for review again soon" (if (= 1 count) "" "s"))))

;;;###autoload
(defun org-roam-review-bury ()
  "Confirm review of the current node and bury it."
  (interactive)
  (let ((count 0))
    (org-roam-review--transform-selected-sections
      (cl-incf count)
      (let ((node (org-roam-review--visiting-node-at-point
                    (org-roam-review--update-node-srs-properties org-roam-review--maturity-score-bury
                                                                 (org-entry-get-with-inheritance "MATURITY"))
                    (let ((buf (current-buffer)))
                      (run-hooks 'org-roam-review-node-buried-hook)
                      (run-hooks 'org-roam-review-node-processed-hook)
                      (org-roam-review--maybe-kill-reviewed-buffer buf)))))
        (org-roam-review--update-review-buffer-entry node)))
    (message "Node%s buried" (if (= 1 count) "" "s"))))

(defun org-roam-review--skip-node-for-maturity-assignment-p ()
  (org-with-wide-buffer
   (or (org-roam-review--daily-file-p (buffer-file-name))
       (seq-intersection org-roam-review-ignored-tags (org-roam-review--tags-at-pt)))))

;;;###autoload
(defun org-roam-review-set-memorise ()
  "Set the current node as a 'memorise' node.

It will show up in a dedicated section of the review buffer when it's due."
  (interactive)
  (org-roam-review--transform-selected-sections
    (let ((node (org-roam-review--visiting-node-at-point
                  (ignore-errors
                    (org-roam-tag-remove org-roam-review-maturity-values))
                  (org-roam-tag-add (list "memo"))
                  (org-delete-property "MATURITY")
                  (org-roam-review--update-node-srs-properties org-roam-review--maturity-score-revisit))))
      (org-roam-review--update-review-buffer-entry node))))

;;;###autoload
(defun org-roam-review-set-budding (&optional bury)
  "Set the current node as a 'budding' node and confirm it's been reviewed.

With prefix arg BURY, the node is less likely to be surfaced in
the future."
  (interactive "P")
  (org-roam-review--transform-selected-sections
    (let* ((score (if bury
                      org-roam-review--maturity-score-bury
                    org-roam-review--maturity-score-ok))
           (node (org-roam-review--visiting-node-at-point
                   (unless (org-roam-review--skip-node-for-maturity-assignment-p)
                     (org-roam-review--update-node-srs-properties score "budding")))))
      (org-roam-review--update-review-buffer-entry node))))

;;;###autoload
(defun org-roam-review-set-seedling (&optional bury)
  "Set the current node as a 'seedling' node and confirm it's been reviewed.

With prefix arg BURY, the node is less likely to be surfaced in
the future."
  (interactive "P")
  (org-roam-review--transform-selected-sections
    (let* ((score (if bury
                      org-roam-review--maturity-score-bury
                    org-roam-review--maturity-score-revisit))
           (node (org-roam-review--visiting-node-at-point
                   (unless (org-roam-review--skip-node-for-maturity-assignment-p)
                     (org-roam-review--update-node-srs-properties score "seedling")))))
      (org-roam-review--update-review-buffer-entry node))))

;;;###autoload
(defun org-roam-review-set-evergreen (&optional bury)
  "Set the current node as an 'evergreen' node and confirm it's been reviewed.

With prefix arg BURY, the node is less likely to be surfaced in
the future."
  (interactive "P")
  (org-roam-review--transform-selected-sections
    (let* ((score (if bury
                      org-roam-review--maturity-score-bury
                    org-roam-review--maturity-score-ok))
           (node (org-roam-review--visiting-node-at-point
                   (unless (org-roam-review--skip-node-for-maturity-assignment-p)
                     (org-roam-review--update-node-srs-properties score "evergreen")))))
      (org-roam-review--update-review-buffer-entry node))))

(defun org-roam-review--delete-tags-and-properties (node-id)
  (let ((message-log-max))
    (org-with-point-at (org-find-property "ID" node-id)
      (atomic-change-group
        (ignore-errors
          (org-roam-tag-remove org-roam-review-maturity-values))
        (dolist (name org-roam-review-properties)
          (org-delete-property name))))))

;;;###autoload
(defun org-roam-review-set-excluded ()
  "Exclude this node from reviews.

This deletes all the properties and tags managed by this
package."
  (interactive)
  (let ((titles))
    (org-roam-review--transform-selected-sections
      (let ((node (org-roam-review--visiting-node-at-point
                    (let ((id (org-entry-get (point-min) "ID")))
                      (unless id
                        (error "No ID in buffer"))
                      (org-with-point-at (org-find-property "ID" id)
                        (org-roam-review--delete-tags-and-properties id)
                        (save-buffer))
                      (let ((title (org-roam-node-title (org-roam-node-from-id id))))
                        (push title titles))))))

        (org-roam-review--update-review-buffer-entry node)))

    (if (equal 1 (length titles))
        (message "Excluded node `%s' from reviews" (car titles))
      (message "Excluded %s nodes from reviews" (length titles)))))

(provide 'org-roam-review)

;;; org-roam-review.el ends here
