(defun aibo:question ()
  "Create a new Aibo conversation using the 'Question' template."
  (interactive)
  (let* ((content (read-string "Enter your question: "))
         (template (aibo:get-conversation-template :short-name "q"))
         (message-inputs (aibo:get-conversation-template-message-inputs
                          :template template
                          :content content)))
    (aibo:api-create-conversation
     :message-inputs message-inputs
     :on-success
     (lambda (conversation)
       (aibo:go-to-conversation :conversation conversation)
       (aibo:stream-assistant-message
        :conversation-id (ht-get conversation "id"))))))



;;; Custom Aibo commands for processing region and buffer -*- lexical-binding: t -*-

(defun aibo:generate-template (instruction content)
  "Generate a temporary conversation template with INSTRUCTION and CONTENT."
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
                           ("text" "You are a helpful AI assistant.")))))
      (ht ("role" "user")
          ("contents" (list
                       (ht ("kind" "text")
                           ("text" (format "%s:\n\n%s" instruction content))))))))))

(defun aibo:process-content (content instruction)
  "Process CONTENT with given INSTRUCTION using Aibo API."
  (let* ((template (aibo:generate-template instruction content))
         (message-inputs (aibo:get-conversation-template-message-inputs
                          :template template
                          :content content)))
    (aibo:api-create-conversation
     :message-inputs message-inputs
     :on-success
     (lambda (conversation)
       (aibo:go-to-conversation :conversation conversation)
       (aibo:stream-assistant-message
        :conversation-id (ht-get conversation "id"))))))

;;;###autoload
(defun aibo:region ()
  "Prompt for instruction and process the selected region using Aibo."
  (interactive)
  (if (use-region-p)
      (let* ((instruction (read-string "Instruction: "))
             (content (buffer-substring-no-properties (region-beginning) (region-end))))
        (aibo:process-content content instruction))
    (message "No region selected. Please select a region and try again.")))

;;;###autoload
(defun aibo:buffer ()
  "Prompt for instruction and process the entire buffer using Aibo."
  (interactive)
  (let* ((instruction (read-string "Instruction: "))
         (content (buffer-substring-no-properties (point-min) (point-max))))
    (aibo:process-content content instruction)))
