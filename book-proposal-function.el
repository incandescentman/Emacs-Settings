(defun open-book-proposal-and-insert-task ()
  "Open the book proposal file, go to today's entry, move to line 16, and prompt for the main task."
  (interactive)
  ;; Open the specified file
  (find-file "/Users/jay/Dropbox/roam/project/20230727171148-july_2023_book_proposal_plan.org")
  ;; Call the org-roam-dailies-goto-today function
  (org-roam-dailies-goto-today)
  ;; Go to line 16
  (goto-line 16)
  ;; Prompt for the main task and insert it into the buffer
  (let ((task (read-string "What is the main thing you want to get done today for your book? ")))
    (insert "\n** Bookwriting\n*** TODO " task "\n")))

(open-book-proposal-and-insert-task)
