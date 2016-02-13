;;; -*- lexical-binding: t -*-

;; On OS X, just the name of the app suffices.
;; On other platforms, need the path to the program if not on PATH.
(setq uo-programs
      (cl-case system-type
        ('darwin
         '(
           ("textedit" . "TextEdit")
           ("finder" . "Finder")
           ("chrome" . "Google Chrome")
           ("byword" . "Byword")
           ("omm" . "OmmWriter")
           ("firefox" . "Firefox")
           ("safari" . "Safari")
           ("mail" . "Mail")
           ))
        ('windows-nt
         '(
           ("sublime" . "sublime.exe")
           ("vim" . "gvim.exe")))
        (t
         '(
           ("sublime" . "/usr/bin/sublime")
           ("vim" . "/usr/bin/gvim")))))


(defun uo-path-prefix ()
    (cl-case system-type
      ('darwin "open -a")
      (t "")))

(defun uo-generate-functions (program)
  (let ((name (car program))
        (path (cdr program)))
    (fset (intern (format "uo-%s-file" name))
          (lambda () (interactive)
            (shell-command (format "%s \"%s\" %s" (uo-path-prefix) path buffer-file-name))))
    (fset (intern (format "uo-%s-directory" name))
          (lambda () (interactive) (shell-command (format "%s \"%s\" ." (uo-path-prefix) path))))))

(mapcar 'uo-generate-functions uo-programs)
