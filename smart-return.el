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
       "^[ \t]*\\(?:[-+*]\\|[0-9]+\\(?:\\.|\\)\\)[ \t]+\\(?:\\[[ Xx]\\][ \t]*\\)?$"))))

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
                         "\\)$")));;
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
                      (string-match "\\(\\[\\[\\([^]]+\\)\\]\\]\\)\\(?:\\[\\([^]]+\\)\\]\\)?\\}"
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
    (looking-at "^[ \t]*[-+*][ \t]+\[[ Xx]\][ \t]+")))

(defun smart-return--insert-checklist-item ()
  "Insert a new checklist item on a new line."
  (let ((indent (current-indentation))
        bullet)
    (save-excursion
      (beginning-of-line)
      (when (looking-at
             "^[ \t]*\([-+*]\)[ \t]+\\(?:\\[[ Xx]\]\)[ \t]+")
        (setq bullet (concat (match-string 1) " [ ] "))))
    (unless bullet
      (setq bullet "- [ ] "))
    (end-of-line)
    (newline)
    (insert (make-string indent ?	) bullet)))

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

(defun smart-return-create-and-position-preview-frame ()
  "Create a preview frame on the right side of the screen."
  (let* ((monitor-width (display-pixel-width))
         (monitor-height (display-pixel-height))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         (frame-width (floor (/ monitor-width char-width 2)))
         (frame-height (floor (/ monitor-height char-height))))
    ;; Create and position the new frame
    (let ((new-frame (make-frame
                      `((name . "File Preview")
                        (width . ,frame-width)
                        (height . ,frame-height)
                        (left . ,(/ monitor-width 2))
                        (top . 0)
                        (user-position . t)
                        (user-size . t)))))
      (setq smart-return-preview-frame new-frame)
      new-frame)))

(defun smart-return-get-or-create-preview-frame ()
  "Get the preview frame, creating and positioning it if necessary."
  (if (and smart-return-preview-frame (frame-live-p smart-return-preview-frame))
      (progn
        (raise-frame smart-return-preview-frame)
        smart-return-preview-frame)
    (smart-return-create-and-position-preview-frame)))

(defun smart-return-open-file-in-preview-frame (file)
  "Open FILE in the preview frame and return focus to the current frame."
  (message "Previewing file: %s" file)
  (when (and file (not (string-empty-p file)) (file-exists-p file))
    (let ((current-frame (selected-frame))
          (preview-frame (smart-return-get-or-create-preview-frame)))
      ;; Load the file in the preview frame
      (with-selected-frame preview-frame
        (switch-to-buffer (find-file-noselect file)))
      ;; Make the preview frame visible and raise it
      (make-frame-visible preview-frame)
      (raise-frame preview-frame)
      ;; Return focus to the original frame
      (run-at-time 0.1 nil
                   (lambda (frame)
                     (message "Returning focus to original frame.")
                     (select-frame-set-input-focus frame)
                     (raise-frame frame))
                   current-frame))))

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
;; 7) MAIN SMART-RETURN FUNCTION
;;------------------------------------------------------------------------------

(defun smart-return ()
  "Perform a context-aware Return in Org-mode."
  (interactive)
  (cond
   ;; 1) Escape empty list item
   ((org-in-empty-item-p)
    (org-element-cache-pause)
    (unwind-protect
        (progn
          (org-beginning-of-item)
          (delete-region (point) (line-end-position))
          (newline)
          (when (looking-at "^[ \t]*\\(?:[-+*]\\|[0-9]+\\(?:\\.|\\)\\)[ \t]+")
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