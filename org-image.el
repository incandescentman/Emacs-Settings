;; Source: https://gist.github.com/nico202/1c645c2a0a6cfb5a06bf2f6717d0cf54
;; [[elfeed:pragmaticemacs.com#http://pragmaticemacs.com/?p=752][A workflow to quickly add photos to org-mode notes]]
;; use case is taking a photo of a slide in a conference and uploading
;; it to iphone and get it on your computer. You then want to embed
;; it in an org-mode document by moving it to the subfolder and
;; renaming according to the current section of the org file, avoiding
;; name clashes

;; required libraries
(require 'dash)
(require 'swiper)
(require 's)
(require 'f)

;; This defaults to copy: move with prefix (C-u)
(defun org-insert-screenshot (arg)
  (interactive "P")
  (bjm/insert-image "~/Downloads/Screenshots/" not arg))

;; This defaults to copy: move with prefix (C-u)
(defun org-insert-iphone-photo (arg)
  (interactive "P")
  (bjm/insert-image "/Users/jay/Dropbox/Pics/saved-from-iPhone/" (not arg)))

;; Always copy, ignore prefix
(defun org-copy-iphone-photo (arg)
  (interactive "P")
  (bjm/insert-image "/Users/jay/Dropbox/Pics/saved-from-iPhone/" t))

;; Always copy, ignore prefix
(defun org-move-iphone-photo (arg)
  (interactive "P")
  (bjm/insert-image "/Users/jay/Dropbox/Pics/saved-from-iPhone/" arg))


(defun bjm/insert-image (image-dir copy)
  "Insert image from conference directory, rename and add link in
  current file.
The file is taken from a start directory set by `image-dir' and
copied/moved to the img subdirectory, renamed and embedded at the
point as an org-mode link. The user is presented with a list of files
in the start directory, from which to select the file to move, sorted
by most recent first. The `copy` argument decides if it will be copied
or moved"'
  (interactive)
  (let (file-list target-dir file-list-sorted start-file
                  start-file-full file-ext end-file end-file-base end-file-full
                  file-number subfolder)
    (setq subfolder "img")
    ;; clean directories from list but keep times
    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes image-dir)))

    ;; get target directory
    (setq target-dir (concat
                      (expand-file-name subfolder (file-name-directory (buffer-file-name)))
                      "/"))

    ;; sort list by most recent
    ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
    (setq file-list-sorted
          (mapcar #'car
                  (sort file-list
                        #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

    ;; use ivy to select start-file
    (let (action)
      (setq action (if copy "Copy" "Move"))
      (setq start-file (ivy-read
                        (concat action " selected file to " target-dir ":")
                        file-list-sorted
                        :re-builder #'ivy--regex
                        :sort nil
                        :initial-input nil)))

    ;; add full path to start file and end-file
    (setq start-file-full
          (expand-file-name start-file image-dir))
    ;; generate target file name from current org section
    (setq file-ext (file-name-extension start-file t))

    ;; my phone app doesn't add an extension to the image so I do it
    ;; here. If you want to keep the existing extension then use the
    ;; line above
    ;; (setq file-ext ".jpg")

    ;; get section heading and clean it up
    (setq end-file-base
          (concat (s-downcase
                   (s-dashed-words (nth 4 (org-heading-components))))))
    ;; shorten to first 40 chars to avoid long file names
    (setq end-file-base (s-left 40 end-file-base))
    ;; number to append to ensure unique name
    (setq file-number 1)
    (setq end-file (concat
                    end-file-base
                    (format "-%s" file-number)
                    file-ext))

    ;; increment number at end of name if file exists
    (message (concat target-dir end-file))
    (while (file-exists-p (concat target-dir end-file))
      ;; increment
      (setq file-number (+ file-number 1))
      (setq end-file (concat
                      end-file-base
                      (format "-%s" file-number)
                      file-ext)))

    ;; final file name including path
    (setq end-file-full
          (expand-file-name end-file target-dir))

    ;; if target-dir does not exists, create it, else copy-file will fail
    (if (not (f-directory? target-dir))
        (make-directory target-dir))
    ;; rename file if move it t
    (if copy
        (copy-file start-file-full end-file-full)
      (rename-file start-file-full end-file-full))
    (let (action)
      (setq action (if copy "Copied" "Moved"))
      (message action " %s to %s" start-file-full end-file))
    ;; insert link
    (insert (org-make-link-string (format "file:%s" (concat "./" subfolder "/" end-file))))
    ;; display image
;;    (org-display-inline-images t t)
))
