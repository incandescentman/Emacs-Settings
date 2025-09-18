;;; smart-return.el --- Enhanced Return Key Functionality for Org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides a context-aware Return key in Org-mode, handling:
;; - Empty list items (escape the list),
;; - Image URLs,
;; - Org links,
;; - Active regions,
;; - Checklist items,
;; - General lists,
;; - Or else falling back to standard Org return or a simple newline.
;;
;; The goal is to allow "double-Enter" to exit a list item (like pressing Enter
;; on an empty bullet or empty checklist), plus create new items when needed,
;; and handle image or link behavior as desired.

;; =============================================================================
;; SMART-RETURN LOGIC TREE
;;
;; The `smart-return' function implements context-aware behavior for the Return
;; key in Org-mode. Below is a detailed breakdown of its decision tree:
;;
;; 1. EMPTY LIST ITEM / ESCAPE FROM LIST
;;    -------------------------------------
;;    Condition: (org-in-empty-item-p)
;;      - Detects if the current list (or checklist) item is empty.
;;      - An empty item contains only a bullet (or numbered marker) and,
;;        optionally, a checkbox ([ ] or [X]) with no further text.
;;
;;    Action:
;;      a. Move point to the beginning of the item.
;;      b. Delete the marker (bullet/number + optional checkbox).
;;      c. Insert a newline to "escape" the list.
;;      d. If Org-mode auto-inserts a new bullet or number on the fresh line,
;;         remove it.
;;      e. Delete any leftover indentation or horizontal space.
;;
;;    Example:
;;      Before: "-^"  (a bullet with point, no text)
;;      After:  an empty line (the list is escaped)
;;
;; 2. IMAGE HANDLING
;;    -----------------
;;    Condition: (org-url-at-point-is-image-p)
;;      - Checks if the URL at point ends with a recognized image extension.
;;
;;    Action:
;;      - Call `display-online-image-in-new-buffer' to fetch and display
;;        the image in Emacs.
;;
;; 3. LINK HANDLING
;;    ----------------
;;    Condition: (org-link-at-point-p) AND (org-return-follows-link;;      - Determines if the point is on an Org-mode link and if link
;;        following is enabled.
;;
;;    Action:
;;      - Use `org-open-at-point' to open the link.
;;
;; 4. ACTIVE REGION HANDLING
;;    -------------------------
;;    Condition: (use-region-p)
;;      - Checks if there is an active (selected) region.
;;
;;    Action:
;;      a. Delete the active region.
;;      b. Insert a newline with proper indentation using
;;         `org-return-indent'.
;;
;; 5. CHECKLIST ITEM HANDLING
;;    --------------------------
;;    Condition: (org-at-item-p) AND (smart-return--at-checklist-p)
;;      - Detects if the current list item is a checklist item.
;;
;;    Action:
;;      - Insert a new checklist item on a new line using
;;        `smart-return--insert-checklist-item'.
;;      * The new checklist item is always created as unchecked ("- [ ]"),
;;        regardless of the current item's state.
;;
;;    Example:
;;      Before: "- [X] Request access to Ironclad ^"
;;      After:  "- [X] Request access to Ironclad ^"
;;              "- [ ] ^"
;;
;; 6. GENERAL LIST ITEM HANDLING
;;    -----------------------------
;;    Condition: (org-at-item-p)
;;      - Applies if the point is in any list item (that is not empty or a
;;        checklist item).
;;
;;    Action:
;;      - Insert a new list item using `org-insert-item'.
;;
;; 7. DEFAULT ORG RETURN
;;    ----------------------
;;    Condition: (derived-mode-p 'org-mode)
;;      - Applies if the current major mode is Org-mode and none of the above
;;        conditions have been met.
;;
;;    Action:
;;      - Execute the standard `org-return' behavior.
;;
;; 8. FALLBACK
;;    ---------
;;    Condition: (None of the above conditions apply)
;;
;;    Action:
;;      - Simply insert a newline.
;;
;; =============================================================================

;;; Code:

(require 'org)
(require 'url)
(require 'cl-lib)





;;------------------------------------------------------------------------------
;; 1) EMPTY LIST ITEM DETECTION (including checklists)
;;------------------------------------------------------------------------------

(defun org-in-empty-item-p ()
  "Return t if the point is in an empty Org list item.
An empty item is one that only has a bullet (or a numbered marker)
and, optionally, a checkbox ([ ] or [X]) with no further text. For example:
-
- [ ]
1) [X]
etc."
  (when (org-at-item-p)
    (save-excursion
      (beginning-of-line)
      ;; Match bullets (- + *) or numbered items (1. 1) plus optional checkbox.
      (looking-at-p
       "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[[ Xx]\\][ \t]*\\)?$"))))


;;------------------------------------------------------------------------------
;; 2) IMAGE HANDLING
;;------------------------------------------------------------------------------

(defun org-url-at-point-is-image-p ()
  "Return t if the URL at point points to an image file (based on file extension)."
  (let* ((url (thing-at-point 'url))
         (image-extensions
          '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico" "heic" "avif"))
         (regexp (concat "\\.\\("
                         (mapconcat #'identity image-extensions "\\|")
                         "\\)\\(?:\\?[^[:space:]]*\\)?\\(?:#[^[:space:]]*\\)?$")))
    (when url
      (string-match-p regexp url))))

(defun display-online-image-in-new-buffer (url)
  "Fetch and display an image from URL in a new buffer using `url-retrieve'."
  (interactive "sEnter image URL: ")
  (if (and url (string-match-p "^https?://" url))
      (progn
        (message "Fetching image from %s..." url)
        (url-retrieve
         url
         (lambda (status)
           (if (plist-get status :error)
               (message "Error retrieving image: %s"
                        (plist-get status :error))
               (goto-char (point-min))
               (re-search-forward "\r?\n\r?\n" nil 'move)
               (let ((image-data (buffer-substring-no-properties
                                  (point) (point-max))))
                 (with-current-buffer (get-buffer-create "*Online Image*")
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (kill-all-local-variables)
                     (insert-image (create-image image-data nil t))
                     (set-buffer-modified-p nil)
                     (image-mode))
                   (display-buffer (current-buffer))))
               (kill-buffer (current-buffer))
               (message "Image fetched and displayed successfully."))))
        ;; If all goes well, return t
        t)
      (message "Invalid URL provided.")
      nil))


;;------------------------------------------------------------------------------
;; 3) LINK HANDLING
;;------------------------------------------------------------------------------

(defun org-link-at-point-p ()
  "Return non-nil when point is directly on an Org link.
Treat point at the first character *after* the link as not on the link so
that pressing Return there falls back to normal newline behavior."
  (let ((pos (point)))
    (save-excursion
      (when (org-in-regexp org-link-bracket-re 1)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (and (<= beg pos)
               (< pos end)))))))


;;------------------------------------------------------------------------------
;; 4) CHECKLIST HANDLING
;;------------------------------------------------------------------------------

(defun smart-return--at-checklist-p ()
  "Return non-nil if the current item is a checklist item.
A checklist item is assumed to start with a bullet (e.g. '- ' or '+ ') followed
by a checkbox marker ([ ] or [X])."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*[-+*][ \t]+\\[[ Xx]\\][ \t]+")))

(defun smart-return--insert-checklist-item ()
  "Insert a new checklist item on a new line, preserving indentation.
The new line will have the same bullet, but a blank checkbox [ ]."
  (let ((indent (current-indentation))
        bullet)
    ;; Capture the bullet character, but always insert "[ ]".
    (save-excursion
      (beginning-of-line)
      (when (looking-at
             "^[ \t]*\\([-+*]\\)[ \t]+\\(\\[[ Xx]\\]\\)[ \t]+")
        ;; Instead of preserving the current checkbox state, force an unchecked box.
        (setq bullet (concat (match-string 1) " [ ] "))))
    (unless bullet
      (setq bullet "- [ ] "))
    (end-of-line)
    (newline)
    (insert (make-string indent ?\s) bullet)))


;;------------------------------------------------------------------------------
;; 5) MAIN SMART-RETURN FUNCTION
;;------------------------------------------------------------------------------

(defun org-element-cache-pause ()
  (setq org-element-use-cache nil))

(defun org-element-cache-resume ()
  (setq org-element-use-cache t))



;;------------------------------------------------------------------------------
;; MAIN SMART-RETURN FUNCTION - Complete replacement
;;------------------------------------------------------------------------------

(defun smart-return ()
  "Perform a context-aware Return in Org-mode.

Decision Tree:
1) Escape an empty list item, if `org-in-empty-item-p' is t.
2) If the URL at point is an image, display it in Emacs.
3) If on an Org link and `org-return-follows-link' is set, open it.
4) If a region is active, delete it and call `org-return-indent'.
5) If on a checklist item, insert a new checklist item.
6) If on any other list item, insert a new bullet/item.
7) If in Org-mode (and none of the above conditions match), do `org-return'.
8) Otherwise, just insert a newline.

This function aims to make \"Enter\" in Org-mode more intuitive
for lists and checklists while respecting typical Org conventions."
  (interactive)
  (cond
   ;; 1) If in an empty list item (or empty checklist item), "escape" the list.
   ((org-in-empty-item-p)
    ;; Only pause the cache for this specific operation
    (org-element-cache-pause)
    (unwind-protect
        (progn
          ;; Move to beginning of item and remove bullet/checkbox.
          (org-beginning-of-item)
          (delete-region (point) (line-end-position))
          ;; Insert a newline to end the list context.
          (newline)
          ;; If Org auto-inserted a new list bullet or number on the new line, remove it.
          (when (looking-at "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+")
            (delete-region (point) (line-end-position)))
          ;; Remove any leftover indentation, etc.
          (delete-horizontal-space))
      (org-element-cache-resume)))

   ;; 2) If the URL at point is an image, display it in Emacs.
   ((org-url-at-point-is-image-p)
    (display-online-image-in-new-buffer (thing-at-point 'url)))

   ;; 3) If point is on an Org link and link-following is enabled, open it.
   ((and (org-link-at-point-p) org-return-follows-link)
    (org-open-at-point))

   ;; 4) If a region is active, delete it and indent.
   ((use-region-p)
    (delete-region (region-beginning) (region-end))
    (org-return-indent))

   ;; 5) If in a list and the item is a checklist, insert a new `[ ]`.
   ((and (org-at-item-p) (smart-return--at-checklist-p))
    (smart-return--insert-checklist-item))

   ;; 6) If in a list but not a checklist, insert a new bullet.
   ((org-at-item-p)
    (org-insert-item))

   ;; 7) Otherwise, if in Org-mode, do the normal `org-return'.
   ((derived-mode-p 'org-mode)
    (org-return))

   ;; 8) Fallback to a simple newline.
   (t
    (newline))))


;;------------------------------------------------------------------------------
;; 6) KEY BINDINGS
;;------------------------------------------------------------------------------

;; Bind `smart-return' to RET in Org-mode buffers.
(define-key org-mode-map (kbd "RET") #'smart-return)

(provide 'smart-return)

;;; smart-return.el ends here
