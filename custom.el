(require 'org)
(disable-theme 'zenburn)


;; from live

(load "~/gnulisp/writegood-jay.el")
;; (add-to-list 'custom-theme-load-path "~/Dropbox/emacs/prelude/personal/sublime-themes-jay/")

(load "~/Dropbox/emacs/prelude/personal/gnu-emacs-startup.el")
(load "~/Dropbox/emacs/prelude/personal/shared-functions.el")

;; I've been wanting to do this for so long. :-)
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<M-S-left>") nil)
     (define-key org-mode-map (kbd "<M-S-right>") nil)
     (define-key org-mode-map (kbd "<M-S-up>") nil)
     (define-key org-mode-map (kbd "<M-S-down>") nil)
     (define-key org-mode-map (kbd "<M-left>") nil)
     (define-key org-mode-map (kbd "<M-right>") nil)
     (define-key org-mode-map (kbd "<M-right>") nil)
     (define-key org-mode-map [C-S-right] 'org-shiftmetaright)
     (define-key org-mode-map [C-S-left] 'org-shiftmetaleft)
     (define-key org-mode-map [C-right] 'org-metaright)
     (define-key org-mode-map [C-left] 'org-metaleft)
     (define-key org-mode-map [C-up] 'org-metaup)
     (define-key org-mode-map [C-down] 'org-metadown)
     (define-key org-mode-map [C-S-return] 'org-insert-todo-heading)
     ))


(eval-after-load "orgstruct"
  '(progn
     (define-key orgstruct-mode-map (kbd "<M-S-left>") nil)
     (define-key orgstruct-mode-map (kbd "<M-S-right>") nil)
     (define-key orgstruct-mode-map (kbd "<M-S-up>") nil)
     (define-key orgstruct-mode-map (kbd "<M-S-down>") nil)
     (define-key orgstruct-mode-map (kbd "<M-left>") nil)
     (define-key orgstruct-mode-map (kbd "<M-right>") nil)
     (define-key orgstruct-mode-map (kbd "<M-up>") nil)
     (define-key orgstruct-mode-map (kbd "<M-down>") nil)

     (define-key orgstruct-mode-map (kbd "<S-right>") nil)
 (define-key orgstruct-mode-map (kbd "<M-return>") nil)
     ))



     (define-key orgstruct-mode-map (kbd "<M-S-left>") nil)
     (define-key orgstruct-mode-map (kbd "<M-S-right>") nil)
     (define-key orgstruct-mode-map (kbd "<M-S-up>") nil)
     (define-key orgstruct-mode-map (kbd "<M-S-down>") nil)
     (define-key orgstruct-mode-map (kbd "<M-left>") nil)
     (define-key orgstruct-mode-map (kbd "<M-right>") nil)
     (define-key orgstruct-mode-map (kbd "<M-up>") nil)
     (define-key orgstruct-mode-map (kbd "<M-down>") nil)

     (define-key orgstruct-mode-map [C-S-right] 'org-shiftmetaright)
     (define-key orgstruct-mode-map [C-S-left] 'org-shiftmetaleft)
     (define-key orgstruct-mode-map [C-right] 'org-metaright)
     (define-key orgstruct-mode-map [C-left] 'org-metaleft)
     (define-key orgstruct-mode-map [C-up] 'org-metaup)
     (define-key orgstruct-mode-map [C-down] 'org-metadown)
     (define-key orgstruct-mode-map [C-S-return] 'org-insert-todo-heading)






;; mypackages list the packages you want
(setq package-list '(magit ack-and-a-half expand-region gist  helm helm-projectile magit magithub markdown-mode paredit projectile rainbow-mode scss-mode solarized-theme volatile-highlights yasnippet zenburn-theme dired+ twittering-mode dired-hacks-utils dired-single sublime-themes cyberpunk-theme popup yasnippet xml-rpc))

;; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")

                         ("melpa" . "http://melpa.milkbox.net/packages/")

                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))





; (define-key my-keys-minor-mode-map (kbd "<S-down>") nil)
; (define-key my-keys-minor-mode-map (kbd "<S-up>") nil)



;;auto-complete mode
(require 'auto-complete)
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode
                                    sass-mode  csv-mode
                                    html-mode sh-mode
                                    lisp-mode  markdown-mode  ))
  (add-to-list 'ac-modes mode))
;;;;Key triggers
(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map (kbd "M-RET") 'ac-help)
(define-key ac-completing-map "\r" 'nil)
(ac-set-trigger-key "TAB")



;; find out how to globally turn off fucking piece of shit guru mode.
(setq prelude-guru nil)


;; projectile-find-dir



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-all-caps nil)
 '(abbrev-file-name "~/Dropbox/elisp/.abbrev_defs")
 '(ac-auto-show-menu 2.0)
 '(ac-auto-start 4)
 '(ac-candidate-menu-min 3)
 '(blink-cursor-mode nil)
 '(buffer-stack-show-position nil)
 '(buffer-stack-untracked (quote ("KILL" "*Compile-Log*" "*Compile-Log-Show*" "*Group*" "*Completions*" "*Messages*" "*Help*" "*Archive*" "*Agenda*" "*fontification*" "*Warnings*" "*prolific*" "*750words*")))
 '(calendar-latitude 40.7)
 '(case-fold-search t)
 '(ccm-recenter-at-end-of-file t)
 '(clean-buffer-list-delay-general 1)
 '(column-number-mode nil)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".tex" ".mm" "Icon" ".html" ".zip")))
 '(compose-mail-user-agent-warnings nil)
 '(cua-highlight-region-shift-only t)
 '(cua-mode nil nil (cua-base))
 '(cursor-type (quote box) t)
 '(custom-safe-themes (quote ("e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "442a0fe27702c0633ac2699ddc5f67a4315be6714804c8e77d0d325940f65f64" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" default)))
 '(debug-on-error t)
 '(deft-directory "~/Dropbox/writing/notationaldata/")
 '(delete-window-preserve-buffer (quote ("*scratch*" "current-book-research.txt" "accountability.txt")))
 '(dired-clean-up-buffers-too nil)
 '(dired-details-hidden-string "")
 '(dired-kept-versions 8)
 '(dired-sort-menu-invalid-options-remote nil)
 '(display-time-mode t)
 '(edit-server-default-major-mode (quote org-mode))
 '(edit-server-new-frame t)
 '(eshell-load-hook (quote ((lambda nil (abbrev-mode -1)))))
 '(flyspell-abbrev-p t)
 '(flyspell-mark-duplications-exceptions (quote ((nil "that" "had" "ha" "something" "blah") ("\\`francais" "nous" "vous"))))
 '(flyspell-use-global-abbrev-table-p t)
 '(global-flyspell-mode t)
 '(gmm/auto-mode-list (quote ("[\\\\/]mail-google-com.*\\.\\(ckr\\|gmm\\|html?\\|txt\\)\\'" "[\\\\/]itsalltext[\\\\/]mail\\.google\\..*\\.txt\\'")))
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "devonthink")))
 '(grep-find-ignored-files (quote (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.pfsl" "*.dfsl" "*.p64fsl" "*.d64fsl" "*.dx64fsl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.pdf" "*.tex" "*.html" "*.mm" "*.js" "*.doc" "*.docx" "*.xls" "*.jpg" "*.png" "*.xlsx" "*devonthink*" "*.gif" "Icon*" "*fontification*" "*helm*" "*750words*")))
 '(grep-highlight-matches (quote always))
 '(helm-mini-default-sources (quote (helm-source-buffers-list helm-source-recentf)))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "pdf" "tex" "html" ".mm" "Icon*")))
 '(ido-save-directory-list-file "~/Dropbox/emacs/prelude/personal/savefile/ido.hist")
 '(ido-use-faces t)
 '(ido-use-url-at-point t)
 '(initial-buffer-choice "~/Dropbox/writing/notationaldata/playful.org")
 '(initial-major-mode (quote org-mode))
 '(mail-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(mail-kill-buffer-on-exit t)
 '(make-backup-files t)
 '(message-draft-headers (quote (From References Date)))
 '(message-kill-buffer-on-exit t)
 '(message-required-headers (quote (From (optional . References))))
 '(message-send-hook (quote (recent-addresses-add-headers)))
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(mml-default-directory "~/Dropbox/writing/notationaldata/emacs-mail-message-mode-messages")
 '(only-global-abbrevs t)
 '(openwith-associations (quote (("\\.pdf\\'" "open" (file)) ("\\.mp3\\'" "xmms" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))
 '(org-M-RET-may-split-line (quote ((item . t))))
 '(org-activate-links (quote (bracket plain radio tag date footnote)))
 '(org-agenda-files (quote ("~/Dropbox/writing/notationaldata/prolific.org" "~/Dropbox/writing/notationaldata/playful.org")))
 '(org-agenda-jump-prefer-future t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-timegrid-use-ampm t)
 '(org-archive-location "archive/%s_archive::")
 '(org-ascii-headline-spacing (quote (1 . 1)))
 '(org-ascii-table-use-ascii-art t)
 '(org-ascii-underline (quote ((ascii 61 45 45) (latin1 61 45 45) (utf-8 9552 9472 9548 9476 9480))))
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
 '(org-bullets-bullet-list (quote (" ")))
 '(org-bullets-face-name (quote \"Courier\"))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-auto-clock-resolution t)
 '(org-clock-idle-time 5)
 '(org-clock-in-resume t)
 '(org-clock-persist-query-resume nil)
 '(org-clock-report-include-clocking-task t)
 '(org-clocktable-defaults (quote (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))
 '(org-closed-string "COMPLETED:")
 '(org-confirm-babel-evaluate nil)
 '(org-ctrl-k-protect-subtree t)
 '(org-custom-properties (quote (">")))
 '(org-default-notes-file "~/Dropbox/writing/notationaldata/notes.txt")
 '(org-display-custom-times nil)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "SOURCE")))
 '(org-edit-src-content-indentation 4)
 '(org-ellipsis (quote org-warning))
 '(org-enable-fixed-width-editor nil)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-allow-bind-keywords t)
 '(org-export-blocks-witheld (quote (hidden)))
 '(org-export-date-timestamp-format "%Y%m%d %I:%M%p")
 '(org-export-html-inline-image-extensions (quote ("png" "jpeg" "jpg" "gif" "svg" "tif" "gif")))
 '(org-export-html-style "<link rel='stylesheet' type='text/css' href='~/Dropbox/web-design/custom-css/gmail.css' /> <link rel='stylesheet' type='text/css' href='http://jaydixit.github.io/custom-css/gmail.css' />")
 '(org-export-html-style-extra "<link rel='stylesheet' type='text/css' href='~/Dropbox/web-design/custom-css/gmail.css' /> <link rel='stylesheet' type='text/css' href='http://jaydixit.github.io/custom-css/gmail.css' />")
 '(org-export-html-style-include-default t)
 '(org-export-latex-date-format "%d %B %Y.")
 '(org-export-latex-emphasis-alist (quote (("*" "\\emph{%s}" nil) ("/" "\\textit{%s}" nil) ("_" "\\underline{%s}" nil) ("+" "\\st{%s}" nil) ("=" "\\verb" t) ("~" "\\verb" t))))
 '(org-export-latex-image-default-option "width=20.5cm")
 '(org-export-latex-verbatim-wrap (quote ("\\begin{quote}" . "\\end{quote}")))
 '(org-export-preserve-breaks t)
 '(org-export-time-stamp-file nil)
 '(org-export-with-clocks t)
 '(org-export-with-drawers t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)
 '(org-extend-today-until 8)
 '(org-fontify-done-headline t)
 '(org-fontify-emphasized-text t)
 '(org-footnote-define-inline t)
 '(org-footnote-section "Footnotes")
 '(org-footnote-tag-for-non-org-mode-files "Footnotes:")
 '(org-headline-done ((t (:strike-through t))))
 '(org-hidden-keywords (quote (author title)) nil nil "#+BEGIN_QUOTE")
 '(org-hide-block-startup nil)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-html-container-element "div")
 '(org-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s </h2>
<div id=\"footnote\">
%s
</div>
</div>")
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(org-html-html5-fancy t)
 '(org-html-postamble nil)
 '(org-html-text-markup-alist (quote ((bold . "<strong>%s</strong>") (code . "<blockquote>%s</blockquote>") (italic . "<em>%s</em>") (strike-through . "<del>%s</del>") (underline . "<span class=\"underline\">%s</span>") (verbatim . "<code>%s</code>"))))
 '(org-html-toplevel-hlevel 2)
 '(org-indent-indentation-per-level 2)
 '(org-indent-mode-turns-off-org-adapt-indentation nil)
 '(org-indent-mode-turns-on-hiding-stars nil)
 '(org-insert-mode-line-in-empty-file t)
 '(org-latex-text-markup-alist (quote ((bold . "\\textbf{%s}") (code . verb) (italic . "\\textit{%s}") (strike-through . "\\sout{%s}") (underline . "\\uline{%s}") (verbatim . protectedtext))))
 '(org-latex-toc-command "\\tableofcontents
\\newpage
")
 '(org-list-allow-alphabetical t)
 '(org-list-indent-offset 3)
 '(org-log-done nil)
 '(org-log-note-clock-out nil)
 '(org-log-refile nil)
 '(org-mac-Skim-highlight-selection-p t)
 '(org-mac-grab-Firefox+Vimperator-p nil)
 '(org-mac-grab-Firefox-app-p nil)
 '(org-mac-grab-Mail-app-p nil)
 '(org-mac-grab-Safari-app-p nil)
 '(org-mac-grab-Together-app-p nil)
 '(org-modules (quote (org-bbdb org-bibtex org-gnus org-info org-annotate-file org-bullets org-invoice org-mac-iCal org-mac-link org-panel org-secretary org-velocity org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-choose org-collector org-invoice)))
 '(org-n-level-faces 9)
 '(org-odd-levels-only nil)
 '(org-pomodoro-format "Pomodoro: %s")
 '(org-pomodoro-killed-sound "~/sounds/autodestructsequencearmed_ep.mp3")
 '(org-pomodoro-length 50)
 '(org-pomodoro-long-break-format "Long Break: %s")
 '(org-pomodoro-long-break-sound "~/sounds/tng-computer-programcomplete.mp3")
 '(org-pomodoro-play-ticking-sounds nil)
 '(org-pomodoro-short-break-format "Short Break: %s")
 '(org-pomodoro-short-break-sound "~/sounds/tng-picard-engage.mp3")
 '(org-pomodoro-sound "~/sounds/large-applause.mp3")
 '(org-pomodoro-ticking-sound "~/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/com.taptanium.thunderstorm.DreamQuest_preview.m4a")
 '(org-priority-faces nil)
 '(org-provide-checkbox-statistics t)
 '(org-replace-disputed-keys nil)
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-src-preserve-indentation t)
 '(org-startup-align-all-tables t)
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-support-shift-select (quote always))
 '(org-time-clocksum-use-effort-durations t)
 '(org-time-stamp-custom-formats (quote ("<%a %b %d>" . "<%m/%d %a %I:%M%p>")))
 '(org-use-speed-commands t)
 '(org-yank-adjusted-subtrees t)
 '(org2blog/wp-confirm-post nil)
 '(org2blog/wp-default-categories (quote ("inspiration" "personal growth" "miscellany")))
 '(org2blog/wp-keep-new-lines t)
 '(org2blog/wp-show-post-in-browser t)
 '(org2blog/wp-use-tags-as-categories t)
 '(osx-browse-prefer-background nil)
 '(osx-browse-prefer-browser "com.google.Chrome")
 '(osx-browse-prefer-new-window t)
 '(pomodoro-break-time 10)
 '(pomodoro-work-time 50)
 '(reb-re-syntax (quote string))
 '(recent-addresses-file "~/Dropbox/emacs/prelude/recent-addresses")
 '(recentf-exclude (quote (".html" ".tex" "*message*" "org-clock-save.el" "\\recent-addresses\\'" "\\ido.last\\'" "\\ido.hist\\'" "elpa" ".bmk" ".jabber" "helm")))
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 999)
 '(recentf-save-file "~/Dropbox/emacs/savefile/recentf")
 '(safe-local-variable-values (quote ((eval when (fboundp (quote rainbow-mode)) (rainbow-mode 1)))))
 '(send-mail-function (quote sendmail-send-it))
 '(smartparens-global-mode nil)
 '(smex-prompt-string "I love you. ")
 '(standard-indent 3)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(undo-limit 800000)
 '(user-full-name "Jay Dixit")
 '(user-mail-address "dixit@aya.yale.edu")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visual-line-mode nil t)
 '(web-mode-load-hook (quote ((lambda nil (abbrev-mode -1)))))
 '(writeroom-global-effects (quote (writeroom-toggle-menu-bar-lines writeroom-toggle-tool-bar-lines writeroom-toggle-vertical-scroll-bars))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((t (:strike-through t))))
 '(tabula-rasa-cursor ((t (:inherit nil :foreground "red" :inverse-video t))) t t)
 '(writegood-weasels-face ((t (:underline (:color "orange" :style wave))))))





                                        ; (set-face-attribute 'default nil :font "Inconsolata")
                                        ; (set-face-attribute 'default nil :font "Courier" :height 220)


;; (require 'google-contacts)
;; (require 'google-contacts-message)
;; (require 'google-weather)





(scroll-bar-mode -1)

(defun scrollbar-init ()
  (interactive)
  (scroll-bar-mode -1)
  )
