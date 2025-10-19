;;; jay-org-roam-templates.el --- Org-roam templates (modular)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture templates and their factory live here to keep core lean.
;; Load after org (and before org-roam if you want variables ready), or simply
;; (require 'jay-org-roam-templates) from init. No hard dependency on org-roam.

;;; Code:

;; Template factory -------------------------------------------------------------
(defun jay/roam-template (key label dir tag)
  "Helper to build an org-roam capture template.
KEY is the capture key, LABEL the description, DIR the subdirectory,
TAG the filetag."
  `(,key ,label plain "- Links ::\n- Source ::\n\n* ${title}\n%?"
         :target (file+head ,(format "%s/%%<%%Y%%m%%d%%H%%M%%S>-${slug}.org" dir)
                            ,(format "#+TITLE: ${title}\n#+FILETAGS: :%s:" tag))
         :unnarrowed t))

;; Full template set ------------------------------------------------------------
(setq org-roam-capture-templates
      (list
       ;; Custom (verbatim)
       '("A" "accountability and task capture" plain "- Links ::\n- Source ::\n\n* ${title}\n%?"
         :target (file+head "accountability/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :accountability:")
         :unnarrowed t)
       '("g" "ChatGPT Outputs" plain "- Links ::\n- Source ::\n\n* ${title}\n%?"
         :target (file+head "chatgpt-outputs/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :gpt:")
         :unnarrowed t)
       '("I" "intelligence" plain "- Links ::\n- Source ::\n\n\n* ${title}\n%?"
         :target (file+head "AI/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :intelligence:")
         :unnarrowed t)
       '("l" "logistics of OpenAI" plain "- Links ::\n- Source ::\n\n* ${title}\n%?"
         :target (file+head "logistics/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :library:")
         :unnarrowed t)
       '("M" "Momentum --- 2025 job hunt" plain "- Links ::\n- Source ::\n\n* ${title}\n%?"
         :target (file+head "job-hunt-2025/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :momentum:")
         :unnarrowed t)
       '("O" "Outline / Structure / Schelling Points" plain "- Links ::\n- Source ::\n\n* ${title}\n%?"
         :target (file+head "structure/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :structure:")
         :unnarrowed t)
       '("p" "person" plain "- Links :: [[id:20240426T130414.177117][🌐 People]]\n- Source ::\n\n* ${title}\n%?"
         :target (file+head "person/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :person:")
         :unnarrowed t)
       '("W" "writers" plain "- Links ::\n- Source ::\n\n* ${title}\n%?"
         :target (file+head "writers/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :writers:person:")
         :unnarrowed t)
       '("z" "zork (custom path)" plain "- Links ::\nSource ::\n\n\n* ${title}\n%?"
         :target (file+head (lambda () (concat (read-string "Enter file path: ") "/%<%Y%m%d%H%M%S>-${slug}.org")) "#+TITLE: ${title}\n#+FILETAGS: :work:")
         :unnarrowed t)

       ;; Factory-based
       (jay/roam-template "b" "books" "books" "books")
       (jay/roam-template "C" "Claude outputs" "claude-outputs" "claude")
       (jay/roam-template "e" "emacs" "emacs" "emacs")
       (jay/roam-template "m" "mantras and intentions" "mantras" "mantras")
       (jay/roam-template "n" "note" "notes" "note")
       (jay/roam-template "o" "OpenAI, i.e. work" "notes" "work")
       (jay/roam-template "P" "photography" "photography" "photography")
       (jay/roam-template "q" "quotes about AI" "quotes" "quote")
       (jay/roam-template "S" "Socratic AI" "socratic" "socratic")
       (jay/roam-template "s" "Storytelling and Writing" "storytelling" "storytelling")
       (jay/roam-template "T" "Travel" "travel" "travel")
       (jay/roam-template "w" "lectures and public talks" "lectures" "lectures")
       (jay/roam-template "X" "exemplars" "exemplars" "exemplars")
       (jay/roam-template "x" "cuts" "cuts" "cuts")
       (jay/roam-template "$" "consumerist" "consumerist" "memoir")))

(provide 'jay-org-roam-templates)
;;; jay-org-roam-templates.el ends here
