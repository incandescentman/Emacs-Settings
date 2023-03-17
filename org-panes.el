;;; org-panes.el --- show an org-file in three panes

;; Copyright (C) 2014 Florian Knupfer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer
;; email: (rot13 "sxahcsre@tznvy.pbz")

;;; Commentary:

;; This file provides a customizable `org-panes' function, which turns
;; an org-file into a three pane view.  Thereby your movement of point
;; is reflected in all panes and you can all three panes to navigate.
;; In the overview and contents pane, the visible part of the show all
;; pane is highlighted.  Visiting a non related buffer will
;; automatically kill the panes and clean up.  See following
;; screenshots:
;; http://i.imgur.com/IHpX57b.png
;; http://i.imgur.com/Zrhc7lG.png

;; Bugs and feature requests can be send via
;; https://github.com/knupfer/org-panes or directly using email.

;;; Code:

(require 'org)

(defgroup org-panes nil
  "Show multiple panes of the org file."
  :group 'org-mode)

(defcustom org-panes-overview-depth 1
  "Number of levels displayed in the overview buffer."
  :group 'org-panes
  :type 'integer)

(defcustom org-panes-contents-depth 10
  "Number of levels displayed in the contents buffer."
  :group 'org-panes
  :type 'integer)

(defcustom org-panes-split-overview-horizontally nil
  "Split behaviour of the overview and the contents buffer.

The show all buffer is always split vertically because of the
bigger space needs."
  :group 'org-panes
  :type 'boolean)

(defcustom org-panes-force-centering-text-vertically t
  "Add padding to overview and contents buffer.

This enables to center point even when there aren't enough
headings."
  :group 'org-panes
  :type 'boolean)

(defcustom org-panes-persist-panes t
  "Create automatically new panes when revisiting buffer.

Call org-panes to kill panes."
  :group 'org-panes
  :type 'boolean)

(defcustom org-panes-main-size 50
  "Percentage of the frame width used for the show all buffer."
  :group 'org-panes
  :type 'integer)

(defcustom org-panes-contents-size 60
  "Size of the contents buffer width/height.

A value of 50 uses the half of the available space."
  :group 'org-panes
  :type 'integer)

(defface org-panes-hide-tree-face
  '((t (:inherit font-lock-comment-face)))
  "Face used for not used overview structures."
  :group 'org-panes)

(defface org-panes-highlight-window-face
  '((t (:inherit error)))
  "Face used for highlighting stars to indicate visible region."
  :group 'org-panes)

(defvar org-panes-change-string nil)
(defvar org-panes-min nil)
(defvar org-panes-max nil)
(defvar org-panes-edited nil)
(defvar org-panes-line-pos-list '(0 0 0))
(defvar org-panes-topic nil)
(defvar org-panes-list nil)

(defun org-panes ()
  "Make different panes for an `org-mode' file.

Current point is shared between the buffers and the visible part
in the show all buffer is highlighted in the contents and
overview buffer."
  (interactive)
  (if (not (equal major-mode 'org-mode))
      (error "This is not an org file")
    (if (not org-panes-list)
        (progn
          (delete-other-windows)
          (setq org-panes-list (let ((b (buffer-name)))
                                 (list (concat b ":OVERVIEW")
                                       (concat b ":CONTENTS")
                                       b)))
          ;; center overview
          (setq org-panes-edited t
                ;; hide other trees
                org-panes-topic nil)
          (org-panes-establish-layout)
          (when org-panes-persist-panes
            (add-hook 'post-command-hook 'org-panes-persist nil t))
          (add-hook 'before-change-functions
                    (lambda (a b) (setq org-panes-edited t)) nil t)
          (add-hook 'post-command-hook 'org-panes-move-point)
          (org-panes-move-point)
          (redisplay)
          (message "org-panes created"))
      (remove-hook 'post-command-hook 'org-panes-persist t)
      (remove-hook 'before-change-functions
                   (lambda (a b) (setq org-panes-edited t)) t)
      (org-panes-stop-panes)
      (when org-panes-persist-panes
        (message "org-panes killed...")))))

(defun org-panes-establish-layout ()
  "Split and clone frames.  Fold org structures."
  (let* ((width (window-body-width))
         (height (window-body-height))
         (win-al (split-window-right (/ (* width org-panes-main-size) -100)))
         (win-co (if org-panes-split-overview-horizontally
                     (split-window-below)
                   (split-window-right (/ (* width org-panes-contents-size)
                                          -200)))))
    (org-panes-window-layout (car org-panes-list) org-panes-overview-depth)
    (if org-panes-split-overview-horizontally
        (window-resize nil (+ (/ (* height org-panes-contents-size) -100)
                              (/ height 2))))
    (select-window win-co)
    (org-panes-window-layout (cadr org-panes-list) org-panes-contents-depth)
    (select-window win-al)
    (show-all)
    (org-panes-move-point)))

(defun org-panes-window-layout (buf depth)
  "Clone current buffer and apply org stuff.

Cloned buffer will be called BUF and the structure shown up to
the level DEPTH."
  (clone-indirect-buffer buf nil)
  (switch-to-buffer buf nil t)
  (visual-line-mode -1)
  (hide-sublevels depth)
  (setq-local cursor-in-non-selected-windows nil))

(defun org-panes-persist ()
  "Respawn panes upon visit."
  (unless org-panes-list
    (remove-hook 'post-command-hook 'org-panes-persist t)
    (org-panes)))

(defun org-panes-stop-panes ()
  "Kill all panes and clean up."
  (while (let ((buf (pop org-panes-list)))
           (when org-panes-list
             (when (and buf (get-buffer buf))
               (let ((win (get-buffer-window buf)))
                 (when win (with-selected-window win (delete-window))))
               (kill-buffer buf)) t)))
  (remove-hook 'post-command-hook 'org-panes-move-point)
  (unless org-panes-persist-panes
    (message "org-panes killed...")))

(defun org-panes-move-point ()
  "Share point and highlight."
  (unless (active-minibuffer-window)
    (if (member (buffer-name) org-panes-list)
        (let ((old-win (selected-window)))
          (when (org-panes-changed-p)
            (let ((pos (save-excursion (move-beginning-of-line nil) (point)))
                  (win-list (mapcar 'get-buffer-window org-panes-list))
                  (win (get-buffer-window)))
              (mapc (lambda (x) (when (and x (not (equal x win)))
                                  (with-selected-window x
                                    (goto-char pos)
                                    (move-beginning-of-line nil))))
                    win-list)
              (when (nth 2 win-list)
                (with-selected-window (nth 2 win-list)
                  (unless (equal win (nth 2 win-list))
                    (set-window-start nil (point)))
                  (save-excursion
                    (goto-char (setq org-panes-min (window-start)))
                    (forward-line (window-body-height))
                    (setq org-panes-max (1- (point))))))
              (org-panes-overlay-dispatcher win-list)
              (setq org-panes-edited nil)))
          (select-window old-win))
      (unless (equal "*Org Src" (substring (buffer-name) 0
                                           (min (length (buffer-name)) 8)))
        (org-panes-stop-panes)))))

(defun org-panes-overlay-dispatcher (win-list)
  "Take a list of windows WIN-LIST and apply overlay functions."
  (when (cadr win-list)
    (with-selected-window (cadr win-list)
      (org-panes--remove-overlay 'org-panes-highlight)
      (when org-panes-edited
        (org-panes--remove-overlay 'org-panes-padding)
        (org-panes-center org-panes-contents-depth))
      (let* ((pos (org-panes--make-overlay))
             (new (org-panes-centering-position pos)))
        (when (and (or (< (point-min) (window-start))
                       (> (point-max) (window-end)))
                   (< 0 (abs (- (cadr org-panes-line-pos-list)
                                (+ (nth 2 pos) new)))))
          (setcar (cdr org-panes-line-pos-list) (+ (nth 2 pos) new))
          (recenter new)))))
  (when (car win-list)
    (with-selected-window (car win-list)
      (when (and (or (< (point-min) (window-start))
                     (> (point-max) (window-end)))
                 (not (equal (car org-panes-line-pos-list)
                             (setcar org-panes-line-pos-list
                                     (line-number-at-pos)))))
        (recenter))
      (org-panes--remove-overlay 'org-panes-highlight)
      (when org-panes-edited
        (org-panes--remove-overlay 'org-panes-padding)
        (org-panes-center org-panes-overview-depth))
      (org-panes--make-overlay t))))

(defun org-panes-centering-position (pos)
  "Take a list POS with the position of point in a tree.

Return a suggested value for `recenter'."
  (- (round (min (/ (* 0.5 (window-body-height) (car pos)) (cadr pos))
                 (car pos)))
     (round (/ (+ (window-body-height) (min (* 0.5 (window-body-height))
                                            (cadr pos))) 2.0))))

(defun org-panes-changed-p ()
  "Decide whether updating overlays is considered."
  (let ((old-string org-panes-change-string)
        (line-pos (line-number-at-pos))
        (height (window-body-height)))
    (if (equal (buffer-name) (nth 2 org-panes-list))
        (if (> (abs (- (nth 2 org-panes-line-pos-list)
                       (setcar (cddr org-panes-line-pos-list)
                               line-pos)))
               (/ height 2))
            (setq org-panes-change-string line-pos)
          (save-excursion
            (end-of-line)
            (let ((p (point)))
              (setq org-panes-change-string nil)
              (goto-char (window-start))
              (while (re-search-forward "^\\(*+\\) ..." (max (point)
                                                             (window-end)) t)
                (setq org-panes-change-string
                      (concat org-panes-change-string
                              (match-string-no-properties 0)
                              (when (and (= (length (match-string 1)) 1)
                                         (> (match-end 0) p)) "P")))))))
      (setq org-panes-change-string line-pos))
    (unless (equal old-string org-panes-change-string) t)))

(defun org-panes-center (depth)
  "Add padding to overview and contents to allow centering.

DEPTH is the deepest level shown in the buffer."
  (when org-panes-force-centering-text-vertically
    (let ((len 0))
      (save-excursion (goto-char (point-min))
                      (while (re-search-forward "^\\(*+\\) " nil t)
                        (when (<= (- (match-end 1) (match-beginning 1)) depth)
                          (setq len (1+ len)))))
      (when (< 0 (setq len (/ (- (window-body-height) len) 2)))
        (let ((ov (make-overlay 0 0)))
          (overlay-put ov 'category 'org-panes-padding)
          (overlay-put ov 'before-string (make-string len ?\n)))))))

(defun org-panes-analyse-tree (winbegin)
  "Retrieve start topic and end of current tree.

The start is only the start of the tree if it is before WINBEGIN,
if not it is WINBEGIN, which is the beginning of the current
window."
  (let (start topic (end (point-max)))
    (end-of-line)
    (re-search-backward "^* " nil t)
    (setq start (point))
    (when (re-search-forward "^* .*" nil t)
      (setq topic (match-string 0))
      (when (re-search-forward "^* " nil t)
        (setq topic (concat topic (match-string 0)))
        (setq end (progn (beginning-of-line) (point)))))
    (if (equal topic org-panes-topic)
        (progn (setq topic nil) (goto-char (min winbegin start)))
      (goto-char (point-min))
      (org-panes--remove-overlay 'org-panes-hide))
    (list start topic end)))

(defun org-panes--make-overlay (&optional update-topic)
  "Put the different overlays for highlighting.

It returns the size of the overview tree.  Change topic if the
topic has changed and UPDATE-TOPIC is non-nil."
  (save-excursion
    (let* ((a (min (point) org-panes-min))
           (b (max (point) org-panes-max))
           (tree-size 0)
           (point-pos 0)
           (old-point (point))
           (res (org-panes-analyse-tree a))
           (start (pop res))
           (topic (pop res))
           (end (pop res)))
      (while (re-search-forward "^*+ \\(.*\\)" (unless topic (max b end)) t)
        (let ((p (match-beginning 1)))
          (when (and (> p a) (< p b))
            (let ((ov (make-overlay (- p 2) (1- p))))
              (overlay-put ov 'category 'org-panes-highlight)
              (overlay-put ov 'face 'org-panes-highlight-window-face)))
          (if (and topic (or (< p start) (> p end)))
              (let ((ov (make-overlay p (match-end 1))))
                (overlay-put ov 'category 'org-panes-hide)
                (overlay-put ov 'face 'org-panes-hide-tree-face))
            (setq tree-size (1+ tree-size))
            (when (< p old-point) (setq point-pos (1+ point-pos))))))
      (when (and topic update-topic) (setq org-panes-topic topic))
      (list point-pos tree-size start))))

(defun org-panes--remove-overlay (tag)
  "Delete all overlays with TAG in current buffer."
  (remove-overlays nil nil 'category tag))

(provide 'org-panes)

;;; org-panes.el ends here
