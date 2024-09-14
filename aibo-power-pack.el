;;; aibo-power-pack.el --- Enhanced Aibo commands -*- lexical-binding: t -*-

;; Required dependencies
(require 'aibo-api)
(require 'aibo-types)
(require 'aibo-conversation)
(require 'ht)
(require 'subr-x) ;; For string-trim and string-empty-p

;; aibo:question function
(defun aibo:question ()
  "Prompt for a question and process it using the 'Question' template."
  (interactive)
  (let* ((content (string-trim (read-string "Enter your question: ")))
         (template (aibo:get-conversation-template :short-name "q")))
    (if (string-empty-p content)
        (message "Question cannot be empty.")
      (if template
          (let ((message-inputs (aibo:get-conversation-template-message-inputs
                                 :template template
                                 :content content)))
            (aibo:api-create-conversation
             :message-inputs message-inputs
             :on-success
             (lambda (conversation)
               (aibo:go-to-conversation :conversation conversation)
               (aibo:stream-assistant-message
                :conversation-id (ht-get conversation "id"))))
            :on-error
            (lambda (error)
              (message "Failed to create conversation: %s" error))))
      (message "Error: 'Question' template not found."))))

;; aibo:generate-template function
(defun aibo:generate-template (instruction content)
  "Generate a conversation template with INSTRUCTION and CONTENT."
  (aibo:ConversationTemplate
   :short-name "dynamic"
   :name "Dynamic Template"
   :action-type :new-conversation
   :get-message-inputs
   (lambda (_)
     (list
      (ht ("role" "system")
          ("contents" (list
                       (ht ("kind" "text")
                           ("text" "You are a knowledgeable and detail-oriented AI assistant. Please follow the user's instructions carefully and provide clear and concise responses.")))))
      (ht ("role" "user")
          ("contents" (list
                       (ht ("kind" "text")
                           ("text" (format "%s\n\n```text\n%s\n```" instruction content))))))))))

;; aibo:process-content function
(defun aibo:process-content (instruction content)
  "Process CONTENT with given INSTRUCTION using Aibo API."
  (let ((template (aibo:generate-template instruction content)))
    (if template
        (let ((message-inputs (aibo:get-conversation-template-message-inputs
                               :template template
                               :content content)))
          (aibo:api-create-conversation
           :message-inputs message-inputs
           :on-success
           (lambda (conversation)
             (aibo:go-to-conversation :conversation conversation)
             (aibo:stream-assistant-message
              :conversation-id (ht-get conversation "id"))))
          :on-error
          (lambda (error)
            (message "Failed to process content: %s" error))))
    (message "Error: Failed to generate conversation template.")))

;;;###autoload
(defun aibo:region ()
  "Prompt for instruction and process the selected region using Aibo."
  (interactive)
  (if (use-region-p)
      (let* ((instruction (string-trim (read-string "Instruction: ")))
             (content (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
        (if (string-empty-p instruction)
            (message "Instruction cannot be empty.")
          (aibo:process-content instruction content)))
    (message "No region selected. Please select a region and try again.")))

;;;###autoload
(defun aibo:buffer ()
  "Prompt for instruction and process the entire buffer using Aibo."
  (interactive)
  (let* ((instruction (string-trim (read-string "Instruction: ")))
         (content (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p instruction)
        (message "Instruction cannot be empty.")
      (aibo:process-content instruction content))))

;; OPTIONAL KEYBINDINGS

;; Define the prefix keymap
(defvar aibo-prefix-map (make-sparse-keymap)
  "Keymap for Aibo-related commands.")

;; Bind commands within the prefix map
(define-key aibo-prefix-map (kbd "M-i") #'aibo:question)
(define-key aibo-prefix-map (kbd "r")    #'aibo:region)
(define-key aibo-prefix-map (kbd "b")    #'aibo:buffer)

;; Assign the prefix map to M-i globally
(global-set-key (kbd "M-i") aibo-prefix-map)

;; Optional: Integrate with which-key for better discoverability
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "M-i" "Aibo Commands"))


(provide 'aibo-power-pack)
;;; aibo-power-pack.el ends here
