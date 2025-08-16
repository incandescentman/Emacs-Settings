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
  "Return t if the point is in an empty Org list item."
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
         (image-extensions
          '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico" "heic" "avif"))
         (regexp (concat "\\.\\("
                         (mapconcat #'identity image-extensions "\\|")
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
        t)
      (message "Invalid URL provided.")
      nil))

;;------------------------------------------------------------------------------
;; 3) LINK HANDLING - ONLY ONE DEFINITION!
;;------------------------------------------------------------------------------

(defun org-link-at-point-p ()
  "Return t if the point is on an Org-mode link."
  (let ((line (thing-at-point 'line t))
        (col (- (point) (line-beginning-position))))
    (when line
      (save-match-data
        (let ((start 0)
              (found nil))
          (while (and (not found)
                      (string-match "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
                                    line start))
            (let ((match-start (match-beginning 0))
                  (match-end (match-end 0)))
              (if (and (>= col match-start)
                       (<= col match-end))
                  (setq found t)
                  (setq start match-end))))
          found)))))

;;------------------------------------------------------------------------------
;; 4) CHECKLIST HANDLING
;;------------------------------------------------------------------------------

(defun smart-return--at-checklist-p ()
  "Return non-nil if the current item is a checklist item."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*[-+*][ \t]+\\[[ Xx]\\][ \t]+")))

(defun smart-return--insert-checklist-item ()
  "Insert a new checklist item on a new line."
  (let ((indent (current-indentation))
        bullet)
    (save-excursion
      (beginning-of-line)
      (when (looking-at
             "^[ \t]*\\([-+*]\\)[ \t]+\\(\\[[ Xx]\\]\\)[ \t]+")
        (setq bullet (concat (match-string 1) " [ ] "))))
    (unless bullet
      (setq bullet "- [ ] "))
    (end-of-line)
    (newline)
    (insert (make-string indent ?\s) bullet)))

;;------------------------------------------------------------------------------
;; 5) CACHE FUNCTIONS
;;------------------------------------------------------------------------------

(defun org-element-cache-pause ()
  (setq org-element-use-cache nil))

(defun org-element-cache-resume ()
  (setq org-element-use-cache t))

;;------------------------------------------------------------------------------
;; 6) PREVIEW FRAME HANDLING FOR file+emacs LINKS
;;------------------------------------------------------------------------------


;;; smart-return.el --- Enhanced Return Key Functionality for Org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; [Keep your existing commentary...]

;;; Code:

(require 'org)
(require 'url)
(require 'cl-lib)

;;------------------------------------------------------------------------------
;; 1) EMPTY LIST ITEM DETECTION (including checklists)
;;------------------------------------------------------------------------------

(defun org-in-empty-item-p ()
  "Return t if the point is in an empty Org list item."
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
         (image-extensions
          '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico" "heic" "avif"))
         (regexp (concat "\\.\\("
                         (mapconcat #'identity image-extensions "\\|")
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
        t)
      (message "Invalid URL provided.")
      nil))

;;------------------------------------------------------------------------------
;; 3) LINK HANDLING
;;------------------------------------------------------------------------------

(defun org-link-at-point-p ()
  "Return t if the point is on an Org-mode link."
  (let ((line (thing-at-point 'line t))
        (col (- (point) (line-beginning-position))))
    (when line
      (save-match-data
        (let ((start 0)
              (found nil))
          (while (and (not found)
                      (string-match "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]"
                                    line start))
            (let ((match-start (match-beginning 0))
                  (match-end (match-end 0)))
              (if (and (>= col match-start)
                       (<= col match-end))
                  (setq found t)
                  (setq start match-end))))
          found)))))

;;------------------------------------------------------------------------------
;; 4) CHECKLIST HANDLING
;;------------------------------------------------------------------------------

(defun smart-return--at-checklist-p ()
  "Return non-nil if the current item is a checklist item."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*[-+*][ \t]+\\[[ Xx]\\][ \t]+")))

(defun smart-return--insert-checklist-item ()
  "Insert a new checklist item on a new line."
  (let ((indent (current-indentation))
        bullet)
    (save-excursion
      (beginning-of-line)
      (when (looking-at
             "^[ \t]*\\([-+*]\\)[ \t]+\\(\\[[ Xx]\\]\\)[ \t]+")
        (setq bullet (concat (match-string 1) " [ ] "))))
    (unless bullet
      (setq bullet "- [ ] "))
    (end-of-line)
    (newline)
    (insert (make-string indent ?\s) bullet)))

;;------------------------------------------------------------------------------
;; 5) CACHE FUNCTIONS
;;------------------------------------------------------------------------------

(defun org-element-cache-pause ()
  (setq org-element-use-cache nil))

(defun org-element-cache-resume ()
  (setq org-element-use-cache t))

;;------------------------------------------------------------------------------
;; 6) PREVIEW FRAME HANDLING FOR file+emacs LINKS
;;------------------------------------------------------------------------------

(defvar smart-return-preview-frame nil
  "The frame used for previewing files.")

(defun smart-return-get-or-create-preview-frame ()
  "Get the preview frame, creating it if necessary."
  (if (and smart-return-preview-frame
           (frame-live-p smart-return-preview-frame))
      smart-return-preview-frame
      (setq smart-return-preview-frame
            (make-frame '((name . "File Preview")
                          (width . 120)
                          (height . 40))))))

(defun smart-return-open-file-in-preview-frame (file)
  "Open FILE in the preview frame and return focus to current frame."
  (when (and file (not (string-empty-p file)))  ; Guard against empty paths
    (let ((current-frame (selected-frame))
          (current-window (selected-window)))
      ;; Get or create preview frame
      (let ((preview-frame (smart-return-get-or-create-preview-frame)))
        ;; Switch to preview frame and open the file
        (with-selected-frame preview-frame
          ;; Open the new file and make it current
          (switch-to-buffer (find-file-noselect file))
          (raise-frame preview-frame))
        ;; Return focus to original frame
        (run-at-time 0.05 nil
                     (lambda (frame window)
                       (select-frame-set-input-focus frame)
                       (select-window window)
                       (raise-frame frame))
                     current-frame current-window)))))

;; FIXED ADVICE - Check the raw link text for file+emacs
(defun smart-return-advice-org-open (orig-fun &rest args)
  "Advice to intercept file+emacs links."
  (let ((context (org-element-context)))
    (if (eq (org-element-type context) 'link)
        (let* ((begin (org-element-property :begin context))
               (end (org-element-property :end context))
               (link-text (buffer-substring-no-properties begin end))
               (path (org-element-property :path context)))
          ;; Check if this is a file+emacs link by examining the raw text
          (if (string-match "\\[\\[file\\+emacs:" link-text)
              (progn
                (smart-return-open-file-in-preview-frame path)
                ;; Don't call the original function
                nil)
              ;; Call original for other links
              (apply orig-fun args)))
        ;; Not a link, call original
        (apply orig-fun args))))

;; Remove old advice and add new
(advice-remove 'org-open-at-point #'smart-return-advice-org-open)
(advice-add 'org-open-at-point :around #'smart-return-advice-org-open)

(defun smart-return-close-preview-frame ()
  "Close the preview frame if it exists."
  (interactive)
  (when (and smart-return-preview-frame
             (frame-live-p smart-return-preview-frame))
    (delete-frame smart-return-preview-frame)
    (setq smart-return-preview-frame nil)))

;;------------------------------------------------------------------------------
;; 7) MAIN SMART-RETURN FUNCTION - THIS WAS MISSING!
;;------------------------------------------------------------------------------

(defun smart-return ()
  "Perform a context-aware Return in Org-mode."
  (interactive)  ; This makes it a command!
  (cond
   ;; 1) Escape empty list item
   ((org-in-empty-item-p)
    (org-element-cache-pause)
    (unwind-protect
        (progn
          (org-beginning-of-item)
          (delete-region (point) (line-end-position))
          (newline)
          (when (looking-at "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+")
            (delete-region (point) (line-end-position)))
          (delete-horizontal-space))
      (org-element-cache-resume)))

   ;; 2) Handle image URLs
   ((org-url-at-point-is-image-p)
    (display-online-image-in-new-buffer (thing-at-point 'url)))

   ;; 3) Handle ALL Org links (file+emacs will be intercepted by advice)
   ((and (org-link-at-point-p) org-return-follows-link)
    (org-open-at-point))

   ;; 4) Handle active region
   ((use-region-p)
    (delete-region (region-beginning) (region-end))
    (org-return-indent))

   ;; 5) Handle checklist items
   ((and (org-at-item-p) (smart-return--at-checklist-p))
    (smart-return--insert-checklist-item))

   ;; 6) Handle other list items
   ((org-at-item-p)
    (org-insert-item))

   ;; 7) Default Org return
   ((derived-mode-p 'org-mode)
    (org-return))

   ;; 8) Fallback
   (t
    (newline))))

;;------------------------------------------------------------------------------
;; 8) KEY BINDINGS
;;------------------------------------------------------------------------------

(define-key org-mode-map (kbd "RET") #'smart-return)

(provide 'smart-return)
;;; smart-return.el ends here
