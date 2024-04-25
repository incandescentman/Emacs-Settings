;;; chatgpt2org.el --- Converts HTML clipboard content from ChatGPT to Org-mode format

;; Authors: Jay Dixit <jaydixit.work@gmail.com>, ChatGPT 4
;; URL: https://github.com/incandescentman/chatgpt2org
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This package provides a function `chatgpt2org' that converts HTML content
;; from ChatGPT in the clipboard to Org-mode format.

;;; Code:

(defun chatgpt2org ()
 "Convert clipboard contents from HTML to Org, remove base64-encoded images, and then paste (yank)."
 (interactive)
 (let* ((cmd "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org")
        (org-content (shell-command-to-string cmd)))
   (setq org-content (replace-regexp-in-string "\\[\\[data:image[^]]*\\]\\]" "" org-content :fixedcase :literal))

   ;; Replace links
   (setq org-content (replace-regexp-in-string "^\\[\\[https://chat.openai.com.*$" "" org-content))
   (setq org-content (replace-regexp-in-string "^\\[\\[https://lh3.googleusercontent.*$" "" org-content))

   ;; Replace excessive newlines
   (setq org-content (replace-regexp-in-string "\\n\\n\\n\\n\\n\\n\\n" "\\n\\n" org-content))
   (setq org-content (replace-regexp-in-string "\\n\\n\\n\\n" "\\n\\n" org-content))

   ;; Remove unnecessary symbols and strings
   (setq org-content (replace-regexp-in-string "^<<.*\n" "" org-content))
   (setq org-content (replace-regexp-in-string "￼" "" org-content))
   (setq org-content (replace-regexp-in-string " " " " org-content))
   (setq org-content (replace-regexp-in-string "\\\\\\\\" "" org-content))


   (setq org-content (replace-regexp-in-string "”" "\"" org-content))
   (setq org-content (replace-regexp-in-string "Okay" "OK" org-content))
   (setq org-content (replace-regexp-in-string "okay" "OK" org-content))
   (setq org-content (replace-regexp-in-string "“" "\"" org-content))



   ;; Remove properties
   (setq org-content (replace-regexp-in-string ":PROPERTIES:\n\\(.*\n\\)*?:END:" "" org-content))
   (setq org-content (replace-regexp-in-string ":PROPERTIES:\\([^\000]*?\\):END:" "" org-content))

   ;; Fix a bug involved with parsing code blocks
   (setq org-content (replace-regexp-in-string "\\(#\\+begin_example\\)\n\\s-*\\([a-zA-Z]*\\)Copy code" "\\1 \\2\n" org-content))

   ;; Replace "=" enclosed text with "~" enclosed text
   (setq org-content (replace-regexp-in-string "\\(\\W\\|=\\|^\\)=\\([^=]*\\)=\\(\\W\\|=\\|$\\)" "\\1~\\2~\\3" org-content))

   ;; Add two line breaks before #+begin for both src and example, and one line break before #+end, and remove leading spaces
   (setq org-content (replace-regexp-in-string "\\(\n\\)?\\s-+\\(#\\+begin_\\(src\\|example\\)\\)" "\n\n\\2" org-content))
   (setq org-content (replace-regexp-in-string "\\(\n\\)?\\s-+\\(#\\+end_\\(src\\|example\\)\\)" "\n\\2" org-content))

   (kill-new org-content)
   (yank)))


;; This part is stilla kinda experimental:
(defun html2org-clipboard-and-unfill-paragraph ()
 "Convert clipboard contents from HTML to Org, remove base64-encoded images, unfill paragraph and then paste (yank)."
 (interactive)
 (html2org-clipboard)
 (let* ((org-content (car kill-ring))) ;; get the most recent item in the kill ring, which is the result of html2org-clipboard

 ;; Unfill paragraph
  (with-temp-buffer
  (insert org-content)
  (goto-char (point-min))
  (while (not (eobp))
   (unfill-paragraph)
   (forward-paragraph))
  (setq org-content (buffer-string)))

  (kill-new org-content)
  (yank)))


(provide 'chatgpt2org)

;;; chatgpt2org.el ends here
