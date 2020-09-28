


(add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)

(defun pdf-cscroll-window-dual-p ()
  "Return t if current scroll window status is dual, else nil."
    (or (equal 'upper (alist-get 'pdf-scroll-window-status (window-parameters)))
        (equal 'lower (alist-get 'pdf-scroll-window-status (window-parameters)))))

(defun pdf-cscroll-close-window-when-dual ()
  (when (pdf-cscroll-window-dual-p)
    (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
      (save-excursion
        (if (equal window-status 'upper)
            (windmove-down)
          (windmove-up))
        (delete-window)
        (set-window-parameter nil 'pdf-scroll-window-status 'single)))))

(defun pdf-continuous-scroll-forward (&optional arg)
  "Scroll upward by ARG lines if possible, else go to the next page.

When `pdf-view-continuous' is non-nil, scrolling a line upward
at the bottom edge of the page moves to the next page."
  (interactive "p")
  (if pdf-continuous-scroll-mode
         (let ((hscroll (window-hscroll))
               (cur-page (pdf-view-current-page)))
	         (print (format
                   "window-total-height %s, frame-height %s\nnext line: vscroll value, second next line: output value (image-next-line)"
                   (window-total-height)
                   (frame-height))
                  (get-buffer-create "*pdf-scroll-log*"))
           (when (= (print
                     (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                     (get-buffer-create "*pdf-scroll-log*"))
                    (print (image-next-line arg) (get-buffer-create "*pdf-scroll-log*")))
	           (cond
	            ((not (window-full-height-p))
               (condition-case nil
                   (window-resize (get-buffer-window) -1 nil t)
                 (error (delete-window)
                        (set-window-parameter nil 'pdf-scroll-window-status 'single)))
               (image-next-line 1))
              (t
               (if (= (pdf-view-current-page) (pdf-cache-number-of-pages))
                   (message "No such page: %s" (+ (pdf-view-current-page) 1))
                 (display-buffer-in-direction
                  (current-buffer)
                  (cons '(direction . below) '((window-height . 1))))
                 (set-window-parameter nil 'pdf-scroll-window-status 'upper)
                 (windmove-down)
                 (set-window-parameter nil 'pdf-scroll-window-status 'lower)
                 (pdf-view-goto-page cur-page)
                 (pdf-view-next-page)
                 (when (/= cur-page (pdf-view-current-page))
                   (image-bob)
                   (image-bol 1))
                 (image-set-window-hscroll hscroll)
                 (windmove-up)
                 (image-next-line 1))))))
    (message "pdf-continuous-scroll-mode not activated")))

(defun pdf-continuous-scroll-backward (&optional arg)
  "Scroll downward by ARG lines if possible, else go to the previous page.

When `pdf-view-continuous' is non-nil, scrolling a line downward
at the top edge of the page moves to the previous page."
  (interactive "p")
  (if pdf-continuous-scroll-mode
      (let ((hscroll (window-hscroll))
            (cur-page (pdf-view-current-page)))
        (print
         "First line below: vscroll value. Then second line: output value of (image-previous-line)"
         (get-buffer-create "*pdf-scroll-log*"))
        (when (and (= (print
                       (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
                       (get-buffer-create "*pdf-scroll-log*"))
                      (print
                       (image-previous-line arg)
                       (get-buffer-create "*pdf-scroll-log*"))))
          (if (= (pdf-view-current-page) 1)
              (message "No such page: 0")
            (display-buffer-in-direction
             (current-buffer)
             (cons '(direction . above) '((window-height . 1))))
            (set-window-parameter nil 'pdf-scroll-window-status 'lower)
            (windmove-up)
            (set-window-parameter nil 'pdf-scroll-window-status 'upper)
            (pdf-view-goto-page cur-page)
            (pdf-view-previous-page)
            (when (/= cur-page (pdf-view-current-page))
              (image-eob)
              (image-bol 1))
            (image-set-window-hscroll hscroll)
            (window-resize (get-buffer-window) 1 nil t)))
        (cond ((< (window-total-height) (- (frame-height) window-min-height))
               (condition-case nil
                   (window-resize (get-buffer-window) 1 nil t)
                 (error nil)))
              ((= (window-total-height) (- (frame-height) window-min-height))
               (set-window-parameter nil 'pdf-scroll-window-status 'single)
               (windmove-down)
               (delete-window))))
    (message "pdf-continuous-scroll-mode not activated")))

(defun pdf-continuous-next-page (arg)
  (declare (interactive-only pdf-view-previous-page))
  (interactive "p")
  (if pdf-continuous-scroll-mode
      (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
        (let ((document-length (pdf-cache-number-of-pages)))
          (if (if (equal window-status 'upper)
                  (= (pdf-view-current-page) (- document-length 1))
                (= (pdf-view-current-page) document-length))
              (message "No such page: %s" (+ document-length 1))
            (cond ((equal window-status 'upper)
                   (windmove-down)
                   (with-no-warnings
                     (pdf-view-next-page arg))
                   (windmove-up)
                   (with-no-warnings
                     (pdf-view-next-page arg)))
                  ((equal window-status 'lower)
                   (windmove-up)
                   (with-no-warnings
                     (pdf-view-next-page arg))
                   (windmove-down)
                   (with-no-warnings
                     (pdf-view-next-page arg)))
                  (t (pdf-view-next-page))))))))

(defun pdf-continuous-previous-page (arg)
  (declare (interactive-only pdf-view-previous-page))
  (interactive "p")
  (if pdf-continuous-scroll-mode
      (let ((window-status (alist-get 'pdf-scroll-window-status (window-parameters))))
        (if (if (equal window-status 'lower)
                (= (pdf-view-current-page) 2)
              (= (pdf-view-current-page) 1))
            (message "No such page: 0")
          (cond ((equal window-status 'upper)
                 (windmove-down)
                 (with-no-warnings
                   (pdf-view-previous-page arg))
                 (windmove-up)
                 (with-no-warnings
                   (pdf-view-previous-page arg)))
                ((equal window-status 'lower)
                 (windmove-up)
                 (with-no-warnings
                   (pdf-view-previous-page arg))
                 (windmove-down)
                 (with-no-warnings
                   (pdf-view-previous-page arg)))
                (t (pdf-view-previous-page)))))))

(defun pdf-cscroll-view-goto-page (page &optional window)
  "Go to PAGE in PDF.

If optional parameter WINDOW, go to PAGE in all `pdf-view'
windows."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  (unless (and (>= page 1)
               (<= page (pdf-cache-number-of-pages)))
    (error "No such page: %d" page))
  (pdf-cscroll-close-window-when-dual)
  (pdf-view-goto-page page))

(defun pdf-cscroll-first-page ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (pdf-view-goto-page 1))

(defun pdf-cscroll-last-page ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (pdf-view-goto-page (pdf-cache-number-of-pages)))

(defun pdf-cscroll-kill-buffer-and-windows ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (kill-this-buffer))

(defun pdf-cscroll-toggle-mode-line ()
  (interactive)
  (if (not mode-line-format)
      (setq mode-line-format pdf-cscroll-mode-line-format)
    (setq pdf-cscroll-mode-line-format mode-line-format)
    (setq mode-line-format nil)))

(defun pdf-cscroll-toggle-narrow-mode-line ()
  (interactive)
  (if (plist-get (custom-face-attributes-get 'mode-line (selected-frame)) :height)
      (custom-set-faces
       (list 'mode-line
             (list
              (list t pdf-cscroll-mode-line-original-face))))
    (setq pdf-cscroll-mode-line-original-face
          (custom-face-attributes-get 'mode-line (selected-frame)))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(mode-line ((t (:background "black" :height 0.1)))))
    ))

(defun pdf-cscroll-annot-list-annotations ()
  (interactive)
  (pdf-cscroll-close-window-when-dual)
  (pdf-annot-list-annotations))


(setq pdf-continuous-scroll-mode-map (make-sparse-keymap))
(define-key pdf-continuous-scroll-mode-map  (kbd "C-n") #'pdf-continuous-scroll-forward)
(define-key pdf-continuous-scroll-mode-map  (kbd "C-p") #'pdf-continuous-scroll-backward)
(define-key pdf-continuous-scroll-mode-map  "n" #'pdf-continuous-next-page)
(define-key pdf-continuous-scroll-mode-map  "p" #'pdf-continuous-previous-page)
;; (define-key pdf-continuous-scroll-mode-map  (kbd "M-<") #'pdf-cscroll-view-goto-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M-g g") #'pdf-cscroll-view-goto-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M-g M-g") #'pdf-cscroll-view-goto-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M-<") #'pdf-cscroll-first-page)
(define-key pdf-continuous-scroll-mode-map  (kbd "M->") #'pdf-cscroll-last-page)
(define-key pdf-continuous-scroll-mode-map  "T" #'pdf-cscroll-toggle-mode-line)
(define-key pdf-continuous-scroll-mode-map  "M" #'pdf-cscroll-toggle-narrow-mode-line)
(define-key pdf-continuous-scroll-mode-map  "Q" #'pdf-cscroll-kill-buffer-and-windows)
(define-key pdf-continuous-scroll-mode-map  (kbd "C-c C-a l") #'pdf-cscroll-annot-list-annotations)


(when (boundp 'spacemacs-version)
  (evil-define-minor-mode-key 'evilified 'pdf-continuous-scroll-mode
    "j" #'pdf-continuous-scroll-forward
    "k" #'pdf-continuous-scroll-backward
    "J" #'pdf-continuous-next-page
    "K" #'pdf-continuous-previous-page
    (kbd "g t") #'pdf-cscroll-view-goto-page
    (kbd "g g") #'pdf-cscroll-first-page
    "G" #'pdf-cscroll-last-page
    "M" #'pdf-cscroll-toggle-mode-line
    "Q" #'pdf-cscroll-kill-buffer-and-windows)
  (spacemacs/set-leader-keys-for-minor-mode
    'pdf-continuous-scroll-mode
    (kbd "a l") #'pdf-cscroll-annot-list-annotations))

(define-minor-mode pdf-continuous-scroll-mode
  "Emulate continuous scroll with two synchronized buffers"
  nil
  " Continuous"
  pdf-continuous-scroll-mode-map
  (unless pdf-continuous-scroll-mode
    (pdf-cscroll-close-window-when-dual))
  (set-window-parameter nil 'pdf-scroll-window-status 'single))
