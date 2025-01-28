;;; smart-return.el --- Enhanced Return Key Functionality for Org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides smart behavior for the Return key in Org-mode,
;; handling contexts such as empty list items (to "escape" lists),
;; links, checklists, and images.
;;
;; For example, if you have a checklist item:
;;
;;     - [ ] Request access to Ironclad^
;;
;; and the item is not empty, a Return will insert a new checklist item:
;;
;;     - [ ] Request access to Ironclad
;;     - [ ] ^
;;
;; But if you hit Return on an empty list item (or an empty checklist item),
;; it "escapes" the list by creating a new line outside the list.

;;; Code:

(require 'org)
(require 'url)
(require 'cl-lib)

;;------------------------------------------------------------------------------
;; 1) EMPTY LIST ITEM DETECTION (including checklists)
;;------------------------------------------------------------------------------

(defun org-in-empty-item-p ()
  "Return t if the point is in an empty Org list item.
An empty item is one that only has a bullet (and an optional checkbox)
followed by no further text."
  (when (org-at-item-p)
    (save-excursion
      (beginning-of-line)
      ;; This regex matches a bullet (or number) followed by at least one space
      ;; and an optional checkbox ([ ] or [X]) and then nothing else.
      (looking-at-p "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[[ Xx]\\][ \t]*\\)?$"))))

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
The new line will have the same bullet and a blank checkbox."
  (let ((indent (current-indentation))
        bullet)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^[ \t]*\\([-+*]\\)[ \t]+\\(\\[[ Xx]\\]\\)[ \t]+")
        (setq bullet (concat (match-string 1) " " (match-string 2) " "))))
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
    (delete-region (point) (line-end-position))
    (delete-char 1)  ; Remove the newline
    (newline))
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
   ;; 5) If in a list and the item is a checklist (and not empty),
   ;;    insert a new checklist item.
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
