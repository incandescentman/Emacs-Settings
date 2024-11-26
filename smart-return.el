;;; smart-return.el --- Enhanced Return Key Functionality for Org-mode

;;; Commentary:
;; This package provides smart behavior for the Return key in Org-mode,
;; handling various contexts such as links, lists, and images efficiently.

;;; Code:

;; Ensure necessary packages are loaded
(require 'org)
(require 'url)
(require 'cl-lib)

;; Helper function to check if point is in an empty list item
(defun org-in-empty-item-p ()
  "Return t if the point is in an empty Org list item."
  (when (org-at-item-p)
    (save-excursion
      (beginning-of-line)
      (looking-at-p "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]*$"))))

;; Helper function to check if URL at point is an image
(defun org-url-at-point-is-image-p ()
  "Return t if the URL at point points to an image file."
  (let* ((url (thing-at-point 'url))
         (image-extensions '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico" "heic" "avif"))
         (regexp (concat "\\.\\("
                         (mapconcat 'identity image-extensions "\\|")
                         "\\)\\(?:\\?[^[:space:]]*\\)?\\(?:#[^[:space:]]*\\)?$")))
    (when url
      (string-match-p regexp url))))

;; Enhanced helper function to check if point is precisely on an Org link
(defun org-link-at-point-p ()
  "Return t if the point is exactly on an Org-mode link."
  (let* ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (let ((begin (org-element-property :begin context))
            (end (org-element-property :end context)))
        (and (>= (point) begin)
             (< (point) end))))))

;; Function to display image from URL in a new buffer
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
                   ;; Clear buffer content and reset local variables
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (kill-all-local-variables)
                     (insert-image (create-image image-data nil t))
                     (set-buffer-modified-p nil)
                     (image-mode))
                   (display-buffer (current-buffer))))
               ;; Clean up the temporary buffer used by url-retrieve
               (kill-buffer (current-buffer))
               (message "Image fetched and displayed successfully.")))))
        nil t))
  (message "Invalid URL provided."))

;; Main smart-return function
(defun smart-return ()
  "Smart behavior for the Return key in Org-mode."
  (interactive)
  (cond
   ;; Exit empty list item
   ((org-in-empty-item-p)
    (org-beginning-of-item)
    (delete-region (point) (line-end-position))
    (delete-char 1)  ; Delete the newline
    (newline)
                                        ;(reflash-indentation)
    )

   ;; Display image if URL at point is an image
   ((org-url-at-point-is-image-p)
    (display-image-in-new-buffer (thing-at-point 'url)))

   ;; Open Org link if at point and setting enabled
   ((and (org-link-at-point-p) org-return-follows-link)
    (org-open-at-point))

   ;; Delete active region and insert newline with indentation
   ((use-region-p)
    (delete-region (region-beginning) (region-end))
    (org-return-indent))

   ;; Insert new list item if in a list
   ((org-at-item-p)
    (org-insert-item))

   ;; Default Org-mode return behavior
   ((derived-mode-p 'org-mode)
    (org-return))

   ;; Fallback to simple newline
   (t
    (newline))))

;; Bind smart-return to Enter key in Org-mode
(define-key org-mode-map (kbd "RET") 'smart-return)

(provide 'smart-return)

;;; smart-return.el ends here
