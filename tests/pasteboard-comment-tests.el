;;; pasteboard-comment-tests.el --- Comment preservation regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../pasteboard-copy-and-paste-functions.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst pasteboard-test-response
  "Your comments sharpen the strategy considerably. And I agree with your pushback on my suggestion to just start sending emails.

You have a small number of valuable relationships with people who know your work. It makes sense to think carefully about how to approach them to maximize the chances of getting introductions. That's not overengineering. That's good salesmanship.

# yes, agreed. that said, I think we've now fully thought through the strategy. so I will send the emails today, with the strategy we just defined in mind.


I'd be cautious about building an entire niche around one person's observation, but it's absolutely worth testing. And your existing experience with skeptics provides a natural starting point.

# but it rings true. as a creative person, successful nonfiction writer, and Yale writing professor teaching AI, I obviously have something businessy tech bros and techie software engineers don't have. and I'm creative and sincere and people love me.


You could eventually ask permission to use something she says as a testimonial. But don't make that the premise of the call.

# but I'll be transparent that it might go that way. e.g. \"and if you're comfortable, maybe I'll turn it into a testimonial. doesn't have to use your name or organization name!\" maybe something like that 


** One bigger addition: optimize for a conversation, not just a yes
# this is an excellent point. Also, it points to a larger referral strategy. I know tons of successful people and friends at various organizations. I want to eventually ask them for referrals. not just from past clients. this will be phase 2 of my referral-seeking strategy.


I'd start with Joel or Jenny, whichever relationship is stronger.

# I'll email both today.")

(ert-deftest pasteboard-response-comments-with-and-without-org-heading ()
  (dolist (text (list pasteboard-test-response
                     (replace-regexp-in-string
                      "^\\*\\* One bigger addition:.*\n" "" pasteboard-test-response)))
    (let* ((output (pasteboard--clean-string text))
           (comments (seq-filter (lambda (line) (string-prefix-p "# " line))
                                 (split-string text "\n"))))
      (should (= (length comments) 5))
      (dolist (comment comments)
        (should (member comment (split-string output "\n")))))))

(ert-deftest pasteboard-mixed-markdown-and-comments ()
  (let* ((comment "# yes, keep **this** and [this](https://example.com), “quotes”, and &#x20; exactly as written.  ")
         (text (concat "# Title\n\n### Details\n\n**bold**\n\n- first\n- second\n\n"
                       comment "\n# yes\n# agreed\n"))
         (output (pasteboard--clean-string text)))
    (should (eq (plist-get (pasteboard--analyse-clipboard-text text) :style) 'markdown))
    (should (member comment (split-string output "\n")))
    (should (member "* Title" (split-string output "\n")))
    (should (member "*** Details" (split-string output "\n")))
    (should (member "# yes" (split-string output "\n")))
    (should (member "# agreed" (split-string output "\n")))
    (should (= (plist-get (pasteboard--analyse-clipboard-text comment) :md-score) 0))))

(ert-deftest pasteboard-org-context-and-ordered-lists ()
  (should (eq (plist-get (pasteboard--analyse-clipboard-text "** Section\n# agreed\n") :style) 'org))
  (should (string-match-p "# agreed" (pasteboard--clean-string "** Section\n# agreed\n")))
  (dolist (case '(("1. item" . ordered-bullet) ("dd. item" . text)))
    (with-temp-buffer
      (insert (car case))
      (should (eq (pasteboard--line-kind-at-pos (point-min)) (cdr case)))))
  (should (= (plist-get (pasteboard--analyse-clipboard-text "1. item\n2. item") :md-score) 2))
  (should (= (plist-get (pasteboard--analyse-clipboard-text "dd. item") :md-score) 0)))

(ert-deftest pasteboard-comment-shape-and-code-protection ()
  (dolist (comment '("# Agreed." "# Why?" "# Yes!" "# Note:"))
    (should (equal (pasteboard--clean-string comment) comment)))
  (let ((code "#+begin_src text\n# Keep this.&#x20;\n#+end_src\n"))
    (should (equal (pasteboard--clean-string code) code)))
  (should (equal (pasteboard--clean-string "Test.&#x20;") "Test.")))

(ert-deftest pasteboard-uppercase-heading-boundary ()
  ;; Bind case-fold-search true to verify the classifier is explicitly case-sensitive.
  (let ((case-fold-search t))
    (dolist (comment '("# yes" "# agreed" "# yes, agreed. that said…" "# I'll email both today." "# What next?"))
      (should (equal (pasteboard--clean-string comment) comment))
      (should (= (plist-get (pasteboard--analyse-clipboard-text comment) :md-score) 0)))
    (should (equal (pasteboard--clean-string "# Title") "* Title"))
    ;; Known edge case: a capitalized, unpunctuated comment looks like a title.
    (should (equal (pasteboard--clean-string "# Love this") "* Love this"))))
