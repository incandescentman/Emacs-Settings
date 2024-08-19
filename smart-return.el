;;; smart-return.el --- Enhanced Return Key Functionality for Org-mode

;; Ensure necessary packages are loaded
(require 'org)
(require 'url)
(require 'cl-lib)  ;; For common Lisp functions (e.g., cl-some)

;; Helper function to check if point is in an empty list item
(defun org-in-empty-item-p ()
  "Return t if the point is in an empty Org list item."
  (when (org-at-item-p)
    (let* ((element (org-element-lineage (org-element-context) '(item) t))
           (begin (org-element-property :contents-begin element))
           (end (org-element-property :contents-end element)))
      (or (not begin) (= begin end)))))

;; Helper function to check if URL at point is an image
(defun org-url-at-point-is-image-p ()
  "Return t if the URL at point points to an image file."
  (let* ((url (thing-at-point 'url))
         (image-extensions '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico"))
         (regexp (concat "\\.\\(" (mapconcat 'identity image-extensions "\\|") "\\)\\(?:\\?[^\s]*\\)?\\(?:#[^\s]*\\)?$")))
    (when url
      (string-match-p regexp url))))

;; Enhanced helper function to check if point is precisely on an Org link
(defun org-link-at-point-p ()
  "Return t if the point is exactly on an Org-mode link."
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (when (eq type 'link)
      (let ((begin (org-element-property :begin context))
            (end (org-element-property :end context)))
        (and (>= (point) begin)
             (< (point) end))))))

;; Function to display image from URL in a new buffer
(defun display-online-image-in-new-buffer (url)
  "Fetch and display an image from URL in a new buffer."
  (interactive "sEnter image URL: ")
  (when (and url (string-match-p "^https?://" url))
    (message "Fetching image from %s..." url)
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (message "Error retrieving image: %s" (plist-get status :error))
         (let ((image-data (buffer-substring-no-properties
                            (search-forward "\n\n" nil t)
                            (point-max))))
           (with-current-buffer (get-buffer-create "*Online Image*")
             (erase-buffer)
             (insert-image (create-image image-data nil t))
             (goto-char (point-min))
             (image-mode)
             (display-buffer (current-buffer)))
           (message "Image fetched and displayed successfully."))))
     nil t)))

;; Main smart-return function
(defun smart-return ()
  "Smart behavior for the Return key in Org-mode."
  (interactive)
  (cond
   ;; Exit empty list item
   ((org-in-empty-item-p)
    (let ((indent (org-get-indentation)))
      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position))
      (indent-line-to indent)
      (newline)))

   ;; Display image if URL at point is an image
   ((org-url-at-point-is-image-p)
    (display-online-image-in-new-buffer (thing-at-point 'url)))

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
