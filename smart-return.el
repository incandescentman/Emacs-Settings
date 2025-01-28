;;; smart-return.el --- Enhanced Return Key Functionality for Org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides smart behavior for the Return key in Org-mode,
;; handling various contexts such as lists, links, etc.
;;
;; Meanwhile, we override Org's link handling for http/https so that image URLs
;; are opened in Emacs, while non-image links go to the browser.

;;; Code:

(require 'org)
(require 'url)
;; (require 'cl-lib) ;; Not needed unless you use cl-assert, cl-loop, etc.

;;------------------------------------------------------------------------------
;; 1) CUSTOMIZABLE IMAGE EXTENSIONS
;;------------------------------------------------------------------------------

(defcustom smart-return-image-extensions
  '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico" "heic" "avif")
  "List of file extensions recognized as images."
  :type '(repeat string)
  :group 'smart-return)

;;------------------------------------------------------------------------------
;; 2) DISPLAY FUNCTION
;;------------------------------------------------------------------------------

(defun display-online-image-in-new-buffer (input)
  "Fetch and display an image from INPUT (URL or local file) in a new buffer.
If INPUT starts with http:// or https://, fetch asynchronously with `url-retrieve`.
Otherwise, treat it as a local file path."
  (interactive "sEnter image URL or file path: ")
  (let ((buffer-name (format "*Image: %s*" (file-name-nondirectory input))))
    (if (string-match-p "^https?://" input)
        ;;-----------------------------------------
        ;; REMOTE URL case
        ;;-----------------------------------------
        (progn
          (message "Fetching image from URL...")
          (url-retrieve
           input
           (lambda (status)
             (let ((retrieval-buffer (current-buffer)))
               (unwind-protect
                   (if (plist-get status :error)
                       (message "Error retrieving image: %s" (plist-get status :error))
                     (when (re-search-forward "\r?\n\r?\n" nil t)
                       (let* ((data (buffer-substring-no-properties (point) (point-max)))
                              (image-type (image-type-from-data data)))
                         (if image-type
                             (let ((buffer (get-buffer-create buffer-name)))
                               (with-current-buffer buffer
                                 (let ((inhibit-read-only t))
                                   (erase-buffer)
                                   (insert-image (create-image data image-type t))
                                   (goto-char (point-min))
                                   (image-mode)))
                               (pop-to-buffer buffer))
                           (message "Unsupported or invalid image format."))))

                     (let* ((data (buffer-substring-no-properties (point) (point-max)))
                            (image-type (image-type-from-data data)))
                       (if image-type
                           (let ((buffer (get-buffer-create buffer-name)))
                             (with-current-buffer buffer
                               (let ((inhibit-read-only t))
                                 (erase-buffer)
                                 (insert-image (create-image data image-type t))
                                 (goto-char (point-min))
                                 (image-mode)))
                             (pop-to-buffer buffer))
                         (message "Unsupported or invalid image format."))))
                 (kill-buffer retrieval-buffer))))))
      ;;-----------------------------------------
      ;; LOCAL FILE case
      ;;-----------------------------------------
      (if (file-exists-p input)
          (let ((buffer (get-buffer-create buffer-name)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (let ((image (create-image input)))
                  (if image
                      (progn
                        (insert-image image)
                        (goto-char (point-min))
                        (image-mode))
                    (message "Unsupported or invalid image format.")))))
            (pop-to-buffer buffer))
        (message "File does not exist: %s" input)))))

;;------------------------------------------------------------------------------
;; 3) OVERRIDE ORG LINK HANDLING (HTTP/HTTPS)
;;------------------------------------------------------------------------------

(defun smart-return--link-is-image-p (url)
  "Return non-nil if URL appears to point to an image (based on extension)."
  (let* ((case-fold-search t)
         (regex (concat "\\.\\("
                        (mapconcat #'identity smart-return-image-extensions "\\|")
                        "\\)\\(?:\\?[^[:space:]]*\\)?\\(?:#[^[:space:]]*\\)?$")))
    (string-match-p regex url)))

(defun smart-return--open-http-url-in-emacs-if-image (protocol raw-link)
  "If RAW-LINK looks like an image, open in Emacs; else open with `browse-url'.
PROTOCOL is \"http\" or \"https\" (without the colon). RAW-LINK is the rest of the URL."
  (let ((full-url (concat protocol ":" raw-link)))
    (if (smart-return--link-is-image-p full-url)
        (display-online-image-in-new-buffer full-url)
      ;; Fallback to normal web browser.
      (browse-url full-url))))

(with-eval-after-load 'org
  (org-link-set-parameters
   "http"
   :follow (lambda (link)
             (smart-return--open-http-url-in-emacs-if-image "http" link)))
  (org-link-set-parameters
   "https"
   :follow (lambda (link)
             (smart-return--open-http-url-in-emacs-if-image "https" link))))

;;------------------------------------------------------------------------------
;; 4) "SMART RETURN" COMMAND
;;------------------------------------------------------------------------------

(defun org-in-empty-item-p ()
  "Return t if the point is in an empty Org list item."
  (when (org-at-item-p)
    (save-excursion
      (beginning-of-line)
      (looking-at-p "[ \t]*\$begin:math:text$?:[-+*]\\\\|[0-9]+[.)]\\$end:math:text$[ \t]*$"))))

(defun org-link-at-point-p ()
  "Return t if the point is exactly on an Org-mode link."
  (let ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (let ((begin (org-element-property :begin context))
            (end   (org-element-property :end context)))
        (and (>= (point) begin) (< (point) end))))))

(defun smart-return ()
  "Perform context-aware Return actions in Org-mode.

Behaviors handled include:
- Exiting an empty list item
- Following an Org link if `org-return-follows-link' is non-nil
- Deleting the region if active
- Creating a new list item if in a list
- Otherwise behaving like `org-return'."
  (interactive)
  (cond
   ;; 1) Exit an empty list item
   ((org-in-empty-item-p)
    (org-beginning-of-item)
    (delete-region (point) (line-end-position))
    (delete-char 1)
    (newline))

   ;; 2) Follow an Org link if `org-return-follows-link' is non-nil
   ((and (org-link-at-point-p)
         org-return-follows-link)
    (org-open-at-point))

   ;; 3) If region is active, delete it and indent
   ((use-region-p)
    (delete-region (region-beginning) (region-end))
    (org-return-indent))

   ;; 4) If on a list item, create a new one
   ((org-at-item-p)
    (org-insert-item))

   ;; 5) Otherwise, if we're in Org-mode, do the normal `org-return'
   ((derived-mode-p 'org-mode)
    (org-return))

   ;; 6) Fallback to a plain newline
   (t
    (newline))))

;;------------------------------------------------------------------------------
;; 5) MINOR MODE
;;------------------------------------------------------------------------------

(defvar smart-return-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'smart-return)
    map)
  "Keymap for `smart-return-mode'.")

(define-minor-mode smart-return-mode
  "Minor mode for a custom Return key in Org-mode."
  :lighter " SR"
  :keymap smart-return-mode-map
  (if smart-return-mode
      (message "Smart Return mode enabled.")
    (message "Smart Return mode disabled.")))

;; Enable it automatically in Org-mode buffers, if desired:
(add-hook 'org-mode-hook #'smart-return-mode)

;;------------------------------------------------------------------------------
;; 6) ORG-YT CONFIG (unchanged, but shown for completeness)
;;------------------------------------------------------------------------------

(load "/Users/jay/emacs/external-packages/org-yt/org-yt.el")
(require 'org-yt)

(defun org-image-link (protocol link _description)
  "Interpret LINK as base64-encoded image data for `org-yt'."
  (cl-assert (string-match "\\`img" protocol) nil
             "Expected protocol type starting with img")
  (let ((buf (url-retrieve-synchronously
              (concat (substring protocol 3) ":" link))))
    (cl-assert buf nil
               "Download of image \"%s\" failed." link)
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (buffer-substring-no-properties (point) (point-max)))))

(org-link-set-parameters "imghttp"  :image-data-fun #'org-image-link)
(org-link-set-parameters "imghttps" :image-data-fun #'org-image-link)

(setq url-request-extra-headers '(("Referer" . "https://jaydixit.com/"))
      url-user-agent "Mozilla/5.0 ...")

(setq url-user-agent
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/98.0 Safari/537.36")

(setq url-gateway-method 'native)

(provide 'smart-return)

;;; smart-return.el ends here
