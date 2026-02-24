;;; org-yt.el --- Org youtube links.                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  U-ESI-INTERNAL\TOZ

;; Author: U-ESI-INTERNAL\TOZ <TOZ@smtp.1und1.de>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Idea from  https://emacs.stackexchange.com/questions/38098/org-mode-custom-youtube-link-syntax

;;; Code:

(require 'org)
(require 'org-element)

(defcustom org-yt-url-protocol "yt"
  "Protocol identifier for youtube links."
  :group 'org-yt
  :type 'string)

(defcustom org-yt-cache-directory (expand-file-name "yt-cache" user-emacs-directory)
  "Directory used to cache thumbnails."
  :group 'org-yt
  :type 'string
  )

(defcustom org-yt-use-cache t
  "When not nil, maintain a cache of downloaded thumbnails."
  :group 'org-yt
  :type 'boolean
  )

(defcustom org-yt-cache-limit 100
  "Maximal number of cached thumbnail image files."
  :group 'org-yt
  :type '(choice :format "%{%t%}: %[Cache Limit Type%] %v" :label "Enable/Disable Cache Limit" :tag "Delimit Cache Size" (const :tag "Unlimited" nil) (number :tag "Number of Images")))

;;; End of Customizations

(defconst org-yt-image-file-extension "jpg"
  "Extension for Youtube thumbnail image files.")

(defun org-yt-image-link (video-id)
  "Return image link for VIDEO-ID as string."
  (format "https://img.youtube.com/vi/%s/0.%s" video-id org-yt-image-file-extension))

(defun org-yt-video-link (video-id)
  "Return video link for VIDEO-ID as string."
  (concat "https://youtu.be/" video-id))

(defun org-image-update-overlay (file link &optional data-p refresh)
  "Create image overlay for FILE associtated with org-element LINK.
If DATA-P is non-nil FILE is not a file name but a string with the image data.
If REFRESH is non-nil don't download the file but refresh the image.
See also `create-image'.
This function is almost a duplicate of a part of `org-link-preview'."
  (when (or data-p (file-exists-p file))
    (let ((width
           ;; Apply `org-image-actual-width' specifications.
           (cond
            ((eq org-image-actual-width t) nil)
            ((listp org-image-actual-width)
             (or
              ;; First try to find a width among
              ;; attributes associated to the paragraph
              ;; containing link.
              (let ((paragraph
                     (let ((e link))
                       (while (and (setq e (org-element-property
                                            :parent e))
                                   (not (eq (org-element-type e)
                                            'paragraph))))
                       e)))
                (when paragraph
                  (save-excursion
                    (goto-char (org-element-property :begin paragraph))
                    (when
                        (re-search-forward
                         "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                         (org-element-property
                          :post-affiliated paragraph)
                         t)
                      (string-to-number (match-string 1))))))
              ;; Otherwise, fall-back to provided number.
              (car org-image-actual-width)))
            ((numberp org-image-actual-width)
             org-image-actual-width)))
          (old (get-char-property-and-overlay
                (org-element-property :begin link)
                'org-image-overlay)))
      (if (and (car-safe old) refresh)
          (image-flush (overlay-get (cdr old) 'display))
        (let ((image (create-image file
                                   (and (image-type-available-p 'imagemagick)
                                        width
                                        'imagemagick)
                                   data-p
                                   :width width)))
          (when image
            (let* ((link
                    ;; If inline image is the description
                    ;; of another link, be sure to
                    ;; consider the latter as the one to
                    ;; apply the overlay on.
                    (let ((parent
                           (org-element-property :parent link)))
                      (if (eq (org-element-type parent) 'link)
                          parent
                        link)))
                   (ov (make-overlay
                        (org-element-property :begin link)
                        (progn
                          (goto-char
                           (org-element-property :end link))
                          (skip-chars-backward " \t")
                          (point)))))
              (overlay-put ov 'display image)
              (overlay-put ov 'face 'default)
              (overlay-put ov 'org-image-overlay t)
              (overlay-put
               ov 'modification-hooks
               (list 'org-display-inline-remove-overlay))
              (push ov org-inline-image-overlays)
              ov)))))))

(defun org-yt-get-image (video-id)
  "Retrieve thumbnail image for VIDEO-ID."
  (condition-case err
      (let* ((url (org-yt-image-link video-id))
             (image-buf (url-retrieve-synchronously url)))
        (when image-buf
          (with-current-buffer image-buf
            (goto-char (point-min))
            (when (looking-at "HTTP/")
              (delete-region (point-min)
                             (progn (re-search-forward "\n[\n]+")
                                    (point))))
            (buffer-substring-no-properties (point-min) (point-max)))))
    (error
     (message "Retrieving thumbnail for video [%s] [%s]" video-id err)
     nil
     )))

(defun org-yt-image-cache-file-name (video-id)
  "Return absolute cache file name for VIDEO-ID."
  (expand-file-name (format "%s.%s" video-id org-yt-image-file-extension) org-yt-cache-directory))

(defun org-yt-image-in-cache (video-id)
  "Retrieve thumbnail for VIDEO-ID from cache."
  ;; try it, does it work, good.
  ;; Not? file not in cache or an error. there is nothing we can do
  (condition-case nil
      (with-temp-buffer
	(set-buffer-multibyte nil)
        (insert-file-contents-literally (org-yt-image-cache-file-name video-id))
        (let (
              (thumbnail(buffer-string))
              )
          ;; make sure we got something
          (if (> (string-bytes thumbnail) 0)
              thumbnail
            nil)))
    (error nil)))

(cl-defun org-yt-old-images-in-cache (&optional (max-cache-size org-yt-cache-limit))
  "Determine the oldest images exceeding the cache limit.
The age of images is determined by their access time.
The cache limit is given by MAX-CACHE-SIZE.
The default for MAX-CACHE-SIZE is `org-yt-cache-limit'.
Return nil when `org-yt-cache-limit' is not a positive number."
  (when (and (numberp max-cache-size)
	     (> max-cache-size 0))
    (nthcdr max-cache-size
	    (sort
	     (directory-files
	      org-yt-cache-directory
	      t
	      (format "\\.%s\\'" (regexp-quote org-yt-image-file-extension)))
	     (lambda (fn1 fn2)
	       (time-less-p
		(file-attribute-access-time (file-attributes fn2))
		(file-attribute-access-time (file-attributes fn1))))))))
;; Test:
;; (org-yt-cache-old-images 2)

(defun org-yt-image-to-cache (video-id image)
  "Save the thumbnail IMAGE for VIDEO-ID to the cache.
Always returns IMAGE, even if the save operation fails."
  ;; but only do if there is data
  (when (> (string-bytes image) 0)
    (condition-case err
        (progn
          ;; create directory if it does not exist
          (unless (file-directory-p org-yt-cache-directory)
              (make-directory org-yt-cache-directory t)
            )
          (with-temp-buffer
            (insert image)
            (write-region (point-min) (point-max)
                          (org-yt-image-cache-file-name video-id)))
          )
      (error
       (message "Unable to write video thumbnail for video [%s] to cache [%s]... continuing" video-id err)
       )))
  (dolist (old-file (org-yt-old-images-in-cache))
    (delete-file old-file))
  image)

(defun org-yt-get-image-for-id (video-id)
  "Retrieve thumbnail for VIDEO-ID.
Try cache first."
  (if org-yt-use-cache
      (or (org-yt-image-in-cache video-id)
          (org-yt-image-to-cache video-id (org-yt-get-image video-id)))
    (org-yt-get-image video-id)
    )
  )

(defconst org-yt-video-id-regexp "[-_[:alnum:]]\\{10\\}[AEIMQUYcgkosw048]"
  "Regexp matching youtube video id's taken from `https://webapps.stackexchange.com/questions/54443/format-for-id-of-youtube-video'.")

(defun org-yt-follow (video-id)
  "Open youtube with VIDEO-ID."
  (browse-url (org-yt-video-link video-id)))

(defun org-yt-preview (link-element)
  "Preview YouTube thumbnail for LINK-ELEMENT.
This function is used as the :preview parameter in `org-link-set-parameters'
for Org 9.8+ link preview system."
  (when (display-graphic-p)
    (let* ((video-id (org-element-property :path link-element))
           (description (org-element-contents link-element))
           (description-str (when description
                              (substring-no-properties
                               (org-element-interpret-data description)))))
      (when (and video-id (string-match org-yt-video-id-regexp video-id))
        (let ((image-data (org-yt-get-image-for-id video-id)))
          (when image-data
            (let ((ol (org-image-update-overlay image-data link-element t t)))
              (when (and ol description-str)
                (overlay-put ol 'after-string description-str))
              ol)))))))

(org-link-set-parameters org-yt-url-protocol
                         :follow #'org-yt-follow
                         :preview #'org-yt-preview)


;; Export

(defun org-yt-export (video-id description backend ext-plist)
  "Export youtube video with VIDEO-ID to BACKEND.
If DESCRIPTION is a string put it below the video.
EXT-PLIST is the data channel for the export backend."
  (let* ((video-link (org-yt-video-link video-id)))
    (org-export-string-as
     (concat
      (format "[[%s][%s]]" video-link (org-yt-image-link video-id))
      (when description
	(format " [[%s][%s]]" video-link description)))
     backend
     t
     ext-plist)))

(org-link-set-parameters "yt" :export #'org-yt-export)

(provide 'org-yt)
;;; org-yt.el ends here
