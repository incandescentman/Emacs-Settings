;;; smart-return.el --- Enhanced Return Key Functionality for Org-mode -*- lexical-binding: t; -*-

;; =============================================================================
;; SMART-RETURN LOGIC TREE
;;
;; The smart-return function implements context-aware behavior for the
;; Return key in Org-mode. Below is a detailed breakdown of its decision tree:
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
;;      b. Delete the marker (bullet and optional checkbox) on the line.
;;      c. Insert a newline.
;;      d. If Org-mode auto-inserts a new list bullet or number on the
;;         new line (matching "^[ \t]*[-+*]\\|[0-9]+[.)]"), delete that marker.
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
;;      - Call display-online-image-in-new-buffer to fetch and display
;;        the image.
;;
;; 3. LINK HANDLING
;;    ----------------
;;    Condition: (org-link-at-point-p) AND (org-return-follows-link)
;;      - Determines if the point is on an Org-mode link and if link
;;        following is enabled.
;;
;;    Action:
;;      - Open the link using org-open-at-point.
;;
;; 4. ACTIVE REGION HANDLING
;;    -------------------------
;;    Condition: (use-region-p)
;;      - Checks if there is an active (selected) region.
;;
;;    Action:
;;      a. Delete the active region.
;;      b. Insert a newline with proper indentation using
;;         org-return-indent.
;;
;; 5. CHECKLIST ITEM HANDLING
;;    --------------------------
;;    Condition: (org-at-item-p) AND (smart-return--at-checklist-p)
;;      - Detects if the current list item is a checklist item.
;;
;;    Action:
;;      - Insert a new checklist item on a new line using
;;        smart-return--insert-checklist-item.
;;        * The new checklist item is always created as unchecked ("- [ ]")
;;          regardless of the current item's state.
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
;;      - Insert a new list item using org-insert-item.
;;
;; 7. DEFAULT ORG RETURN
;;    ----------------------
;;    Condition: (derived-mode-p 'org-mode)
;;      - Applies if the current major mode is Org-mode and none of the above
;;        conditions have been met.
;;
;;    Action:
;;      - Execute the standard org-return behavior.
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
and, optionally, a checkbox ([ ] or [X]) with no further text."
  (when (org-at-item-p)
    (save-excursion
      (beginning-of-line)
      (looking-at-p
       "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[[ Xx]\\][ \t]*\\)?$"))))

;;------------------------------------------------------------------------------
;; 2) IMAGE HANDLING
;;------------------------------------------------------------------------------

(defun org-url-at-point-is-image-p ()
  "Return t if the URL at point points to an image file."
  (let* ((url (thing-at-point 'url))
         (image-extensions '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico" "heic" "avif"))
         (regexp (concat "\\.\\("
                         (mapconcat 'identity image-extensions "\\|")
                         "\\)\\(?:\\?[^[:space:]]*\\)?\\(?:#[^[:space:]]*\\)?$")))
    (when url
      (string-match-p regexp url))))

(defun display-online-image-in-new-buffer (url)
  "Fetch and display an image from URL in a new buffer."
  (interactive "sEnter image URL: ")
  (if (and url (string-match-p "^https?://" url))
      (progn
        (message "Fetching image from %s..." url)
        (url-retrieve
         url
         (lambda (status)
           (if (plist-get status :error)
               (message "Error retrieving image: %s" (plist-get status :error))
             (progn
               (goto-char (point-min))
               (re-search-forward "\r?\n\r?\n" nil 'move)
               (let ((image-data (buffer-substring-no-properties (point) (point-max))))
                 (with-current-buffer (get-buffer-create "*Online Image*")
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (kill-all-local-variables)
                     (insert-image (create-image image-data nil t))
                     (set-buffer-modified-p nil)
                     (image-mode))
                   (display-buffer (current-buffer))))
               (kill-buffer (current-buffer))
               (message "Image fetched and displayed successfully.")))))
        nil t)
    (message "Invalid URL provided.")))

;;------------------------------------------------------------------------------
;; 3) LINK HANDLING
;;------------------------------------------------------------------------------

(defun org-link-at-point-p ()
  "Return t if the point is exactly on an Org-mode link."
  (let* ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (let ((begin (org-element-property :begin context))
            (end   (org-element-property :end context)))
        (and (>= (point) begin)
             (< (point) end))))))

;;------------------------------------------------------------------------------
;; 4) CHECKLIST HANDLING
;;------------------------------------------------------------------------------

(defun smart-return--at-checklist-p ()
  "Return non-nil if the current item is a checklist item.
A checklist item is assumed to start with a bullet followed by a checkbox,
for example \"- [ ] \" or \"- [X] \"."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*[-+*][ \t]+\\[[ Xx]\\][ \t]+")))

(defun smart-return--insert-checklist-item ()
  "Insert a new checklist item on a new line, preserving indentation.
The new line will have the same bullet but a blank (unchecked) checkbox."
  (let ((indent (current-indentation))
        bullet)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^[ \t]*\\([-+*]\\)[ \t]+\\(\\[[ Xx]\\]\\)[ \t]+")
        ;; Instead of preserving the current checkbox state, force an unchecked box.
        (setq bullet (concat (match-string 1) " " "[ ] "))))
    (unless bullet
      (setq bullet "- [ ] "))
    (end-of-line)
    (newline)
    (insert (make-string indent ?\s) bullet)))

;;------------------------------------------------------------------------------
;; 5) MAIN SMART-RETURN FUNCTION
;;------------------------------------------------------------------------------

(defun smart-return ()
  "Smart behavior for the Return key in Org-mode.
Handles:
- Escaping from lists (by hitting Return on an empty item, including checklists).
- Displaying images if the URL at point is an image.
- Opening links if point is on an Org link and `org-return-follows-link' is set.
- Deleting an active region.
- Inserting new list items, with special handling for checklist items."
  (interactive)
  (cond
   ;; 1) If in an empty list item (or empty checklist item), "escape" the list.
   ((org-in-empty-item-p)
    (org-beginning-of-item)
    ;; 1) Remove the current line's bullet/checkbox
    (delete-region (point) (line-end-position))
    ;; 2) Insert a new line
    ;; (newline)
    ;; 3) If Org inserted a bullet on the new line, remove it
    (when (looking-at "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+")
      (delete-region (point) (line-end-position)))
    ;; 4) Now remove any leftover indentation or whitespace
    (delete-horizontal-space)

    ;; Delete the entire marker on the current line.
    (delete-region (point) (line-end-position))
    ;; Insert a newline.
    (newline)
    ;; If Org auto-inserted a new list bullet or number on the new line, remove it.
    (when (looking-at "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+")
      (delete-region (point) (line-end-position))))

   ;; 2) If the URL at point is an image, display it.
   ((org-url-at-point-is-image-p)
    (display-online-image-in-new-buffer (thing-at-point 'url)))
   ;; 3) If point is on an Org link and following is enabled, open it.
   ((and (org-link-at-point-p) org-return-follows-link)
    (org-open-at-point))
   ;; 4) If a region is active, delete it and indent.
   ((use-region-p)
    (delete-region (region-beginning) (region-end))
    (org-return-indent))
   ;; 5) If in a list and the item is a checklist (and not empty), insert a new checklist item.
   ((and (org-at-item-p) (smart-return--at-checklist-p))
    (smart-return--insert-checklist-item))
   ;; 6) If in any list, insert a new list item.
   ((org-at-item-p)
    (org-insert-item))
   ;; 7) Otherwise, if in Org-mode, use the normal org-return behavior.
   ((derived-mode-p 'org-mode)
    (org-return))
   ;; 8) Fallback: just insert a newline.
   (t
    (newline))))

;; Bind smart-return to the Enter key in Org-mode.
(define-key org-mode-map (kbd "RET") 'smart-return)

(provide 'smart-return)

;;; smart-return.el ends here
