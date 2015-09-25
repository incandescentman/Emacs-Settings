;;; package --- Summary
;;; This is a fuction to help get a frame created for alfred-org-capture

;;; Commentary:
;;;
;;;  This is only an mvp, taken from: http://comments.gmane.org/gmane.emacs.orgmode/76348

;;; Code:
(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 180) (height . 46)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))

;;; alfred-org-capture.el ends here
