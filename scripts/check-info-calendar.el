(defvar my-info-calendar-nodes
  '("Calendar/Diary"
    "Calendar Motion"
    "Scroll Calendar"
    "Counting Days"
    "General Calendar"
    "Writing Calendar Files"
    "Holidays"
    "Sunrise/Sunset"
    "Lunar Phases"
    "Other Calendars"
    "Diary"
    "Daylight Saving"
    "Time Intervals"
    "Advanced Calendar/Diary Usage")
  "List of Calendar-related Info nodes that should exist in the Emacs manual.")

(require 'info)

(defun my-info-visit-calendar-from-dir ()
  "Jump to the Calendar entry via the Info directory menu."
  (Info-find-node "dir" "Top")
  (Info-menu "Calendar" t))

(defun my-info-assert-node (node)
  "Ensure NODE exists inside the Emacs manual."
  (condition-case err
      (progn
        (Info-find-node "emacs" node)
        (message "Found node: %s" node))
    (error
     (message "Failed to find node %s: %s" node err)
     (kill-emacs 1))))

(my-info-visit-calendar-from-dir)
(dolist (node my-info-calendar-nodes)
  (my-info-assert-node node))

(kill-emacs 0)
