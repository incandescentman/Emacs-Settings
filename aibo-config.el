;; -----------------------------
;; Python and Aibo Configuration
;; -----------------------------

;; Ensure use-package is installed

;; 1. Import environment variables using exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH" "VIRTUAL_ENV"))
  (exec-path-from-shell-initialize))

;; 2. Activate pyvenv and set the virtual environment
(use-package pyvenv
  :config
  (pyvenv-activate "/Users/jaydixit/.virtualenvs/aibo")
  (pyvenv-tracking-mode 1))

;; 3. Set Python interpreter for Emacs
(setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python"))
(setq python-shell-interpreter-args "-i")

;; 4. Add virtualenv bin to exec-path
(add-to-list 'exec-path (concat pyvenv-virtual-env "bin"))

;; 5. Add virtualenv bin to PATH environment variable
(setenv "PATH" (concat (concat pyvenv-virtual-env "bin:") (getenv "PATH")))

;; Optionally, ensure aibo uses the correct Python interpreter
(setq aibo-python-interpreter (concat pyvenv-virtual-env "bin/python"))


(use-package dash :ensure t)
(use-package ht :ensure t)
(use-package ivy :ensure t)
(use-package request :ensure t)
(use-package s :ensure t)
(use-package ts :ensure t)
(use-package uuidgen :ensure t)
(use-package websocket :ensure t)

;; 6. Load and configure aibo package
(when (file-exists-p "~/github/aibo/elisp")
  (add-to-list 'load-path "/Users/jaydixit/github/aibo/elisp"))

(require 'aibo)

(condition-case err
    (aibo:start-server)
  (error (message "Failed to start AIBO server: %S" err)))


;; (add-to-list 'ivy-ignore-buffers "\\*Aibo"))

;; disable warning
;; toggle-debug-on-error


(defvar aibo:mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x C-r") #'aibo:refresh-homepage)
    (define-key map (kbd "C-c p s")     #'aibo:message-search)
    map))


;;   (global-set-key (kbd "C-M-h") 'aibo:homepage)
;;   (global-set-key (kbd "C-M-s") 'aibo:message-search)
;;   (global-set-key (kbd "C-M-y") 'aibo:rephrase-yank)
;;   (global-set-key (kbd "M-/") 'aibo:create-conversation)



;; (defvar aibo:conversation-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-x C-r") #'aibo:refresh-current-conversation)
;;     (define-key map (kbd "C-c C-t")     #'aibo:set-current-conversation-title)
;;     (define-key map (kbd "C-c C-k")     #'aibo:remove-message-at-point)
;;     (define-key map (kbd "C-c C-x C-k") #'aibo:remove-messages-after-point)
;;     (define-key map (kbd "C-c C-e")     #'aibo:edit-message-at-point)
;;     (define-key map (kbd "C-c C-w")     #'aibo:copy-message-contents-at-point)
;;     (define-key map (kbd "C-c C-c")     #'aibo:regenerate-current-conversation-last-assistant-message)
;;     (define-key map (kbd "C-c C-x C-t") #'aibo:generate-current-conversation-title)
;;     (define-key map (kbd "M-RET")       #'aibo:submit-user-message)
;;     map))



;; Jay's key bindings
