;; gnugol.el - Web search using the gnugol command line utility
;; Copyright (C) 2010 Dave Täht
;; License:    GNU Public License, version 3
;; Author:     Dave Taht
;; Maintainer: d + gnugol AT taht.net
;; Created:    Dec-2008
;; Version:    See git tree
;; Keywords:   extensions, web, search, google

;; This is an interface to the gnugol command line
;; web search utility, which can be obtained at:
;; http://gnugol.taht.net

;; I find gnugol useful enough to stick on a function key
;; in my keybindings.el file elsewhere.
;; (define-key global-map [f6] 'gnugol)

;; FIXME: Convert all to defcustom and add support for args

(defcustom gnugol-cmd "gnugol"
  "Shell command to invoke gnugol."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-opts nil
  "Additional default options for gnugol."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-engine nil
  "Default search engine backend for gnugol. Presently supported are:
  google: Full support (license key recommended)
  duck: (duckduckgo)
  bing: (with a license key)
  dummy: (useful for testing)
  credits: various options like about, licenses etc."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-nresults 8
  "Default number of results for gnugol to return."
  :type 'integer
  :group 'gnugol)

(defcustom gnugol-default-safe-mode "1"
  "Default safe mode to search in: 0 = none, 1 = moderate, 2 = active"
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-header "0"
  "Default output header for gnugol. 0 = no header. "
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-footer "0"
  "Default output footer for gnugol. 0 = no footer"
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-output-format "org"
  "Default output format for gnugol."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-output-buffer "*gnugol*"
  "Output buffer. Set this to something like ~/org/gnugol_history.org if you want to keep your history."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-input-language nil
  "Set this to your preferred language 2 character code if you want to override the LANG variable in your environment."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-default-output-language nil
  "Set this to the preferred language 2 character code to **restrict** the results you get to this language."
  :type 'string
  :group 'gnugol)

(defcustom gnugol-search-maxlen 200
  "Maximum string length of search term. This saves on sending accidentally large queries to the search engine."
  :type 'integer
  :group 'gnugol)

(defcustom gnugol-default-search-maxwords 4
  "Maximum number of words to search for."
  :type 'integer
  :group 'gnugol)

(defcustom gnugol-default-output-mode-sensitive nil
  "Be sensitive to the current buffer mode. Output results in that format."
  :type 'boolean
  :group 'gnugol)

(defcustom gnugol-default-timestamp-output nil
  "Timestamp the output."
  :type 'boolean
  :group 'gnugol)

;; FIXME Haven't decided if doing a ring buffer would be useful

(defcustom gnugol-ring-max 16
  "Maximum length of search ring before oldest elements are thrown away."
  :type 'integer
  :group 'gnugol)

;; FIXME figure out how to search the buffer-modes

;; (defun gnugol-get-output-mode
;;  "Get the gnugol output mode from the current buffer mode."
;;  (if gnugol-default-output-mode-sensitive 
;;      () 
;;    ("org")))


(defun gnugol-url-encode (str)
  "URL-encode STR."
  (interactive "sURL-encode: ")
  (message "%s" (url-hexify-string str)))

(defun gnugol-url-decode (str)
  "URL-decode STR."
  (interactive "sURL-decode: ")
  (message "%s" (decode-coding-string
		 (url-unhex-string str)
		 'utf-8)))

;; FIXME: gnugol should act more like "woman"
;; FIXME: gnugol should maybe output info and LISP format
;; FIXME: If there is a visible gnugol buffer change focus to that rather than the current
;; FIXME: Add hooks for further washing the data

;; FIXME: add next, prev, and refresh buttons 
;        [[gnugol: :next :pos 4 str][more]] [[gnugol: prev][prev]] [[refresh]]
;; FIXME: Document this shell shortcut into org or write org mode version
;; FIXME: search forward in the buffer for an existing
;;        set of keywords rather than call gnugol
;;        (if (search-forward (concat "[[Search: " str )) () (gnugol-cmd str)))?
;; FIXME: Sanitize the shell arguments to prevent abuse !! For example no CRs
;;        regexp? I'm *nearly* certain that url escaping and shell quoting the args
;;        is good enough.
;; FIXME: actually, going to the 4th char in on the title would work best
;; FIXME: make gnugol opts be local
;; FIXME: CNTR-U should set the position
;; FIXME: a query with the same keywords as the previous should fetch more 
;;        results (maybe)

(defun gnugol (str)
  "Search the web via gnugol, bring up results in org buffer."
  (interactive "sSearch: ")
  (if (< (length str) gnugol-search-maxlen)
      (let (newbuffer)
	(setq gnugol-opts (concat (if gnugol-default-opts (concat gnugol-default-opts) ()) 
				  (if gnugol-default-nresults (concat " -n " (int-to-string gnugol-default-nresults) ()))
				  (if gnugol-default-engine (concat " -e " gnugol-default-engine))
				  (if gnugol-default-output-format (concat " -o " gnugol-default-output-format) ())
				  (if gnugol-default-header (concat " -H " gnugol-default-header) ())
				  (if gnugol-default-footer (concat " -F " gnugol-default-footer) ())
;;				  (if gnugol-default-safe-mode (concat " -S " gnugol-default-safe-mode) ())
				  (if gnugol-default-input-language (concat " -l " gnugol-default-input-language) ())
				  (if gnugol-default-output-language (concat " -L " gnugol-default-output-language) ())
				  ))	
	(setq gnugol-full-cmd  (concat gnugol-cmd " " gnugol-opts " -U -- " 
				       (shell-quote-argument 
					(gnugol-url-encode str))))
;; FIXME: Open the file, or reload the file if not a *gnugol* buffer
	(setq newbuffer (get-buffer-create gnugol-default-output-buffer))
	(set-buffer newbuffer)
;; FIXME: Set mode of buffer based on the extension
	(org-mode)
	(goto-char (point-min))
	;; FIXME what we want to do is something like this but I'm getting it wrong
	;; (if (search-forward (concat "[Search: " str "]")) () 
;;	(message "%s" gnugol-full-cmd)	
	(save-excursion 
	  (insert-string (concat "* [[gnugol: " str "][Search: " str "]]\n"))
	  (insert 
	   (shell-command-to-string gnugol-full-cmd)
	    )
	  (switch-to-buffer newbuffer)
	  )
	;; (goto-char (+ point-min 4))
	)
    ( (beep) (message "search string too long"))))

(defun gnugol-search-selection ()
  "Do a gnugol search based on a region"
  (interactive)
  (let (start end term url)
    (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
        (progn
          (setq start (region-beginning)
                end   (region-end))
          (if (> (- start end) gnugol-search-maxlen)
              (setq term (buffer-substring start (+ start gnugol-search-maxlen)))
            (setq term (buffer-substring start end)))
          (gnugol term))
      (beep)
      (message "Region not active"))))

;; Do I really understand lexical scoping yet?

(defun gnugol-search-dummy(str)
  "Search the dummy engine via gnugol. (Useful for debugging)"
  (interactive "sSearch: ")
  (let (gnugol-default-engine)
    (setq gnugol-default-engine "dummy")
    (gnugol str)
    )
)

(defun gnugol-search-credits(str)
  "Search the local credits engine via gnugol."
  (interactive "sSearch: ")
  (let (gnugol-default-engine)
    (setq gnugol-default-engine "credits")
    (gnugol str)
    )
)

(defun gnugol-search-bing(str)
  "Search bing via gnugol."
  (interactive "sSearch: ")
  (let (gnugol-default-engine)
    (setq gnugol-default-engine "bing")
    (gnugol str)
    )
)

(defun gnugol-search-duck(str)
  "Search duckduckgo via gnugol."
  (interactive "sSearch: ")
  (let (gnugol-default-engine)
    (setq gnugol-default-engine "duck")
    (gnugol str)
    )
)

(defun gnugol-search-stackapps(str)
  "Search stackapps via gnugol."
  (interactive "sSearch: ")
  (let (gnugol-default-engine)
    (setq gnugol-default-engine "stackapps")
    (gnugol str)
    )
)

(defun gnugol-search-google(str)
  "Search google via gnugol."
  (interactive "sSearch: ")
  (let (gnugol-default-engine)
    (setq gnugol-default-engine "google")
    (gnugol str)
    )
)

;; This are examples of using a site specific search

(defun gnugol-search-emacswiki(str)
  "Search emacswiki via gnugol."
  (interactive "sSearch: ")
  (gnugol-search-google (concat "site:www.emacswiki.org " str))
  )

(defun gnugol-search-gnugol(str)
  "Search gnugol site via gnugol."
  (interactive "sGnugol Search: ")
  (gnugol-search-google (concat "site:gnugol.taht.net " str))
  )

(defun gnugol-search-koders(str)
  "Search koders.com site via gnugol."
  (interactive "sSearch Koders: ")
  (gnugol-search-google (concat "site:www.koders.com " str))
  )

(defun gnugol-search-stackapps-google(str)
  "Search stackoverflow site via gnugol."
  (interactive "sSearch stackapps: ")
  (gnugol-search-google (concat "site:stackoverflow.com " str))
  )

;; It would be nice to do the above via command completion
;; (gnugol-search-site arg, arg), but this isn't right
;; I need to prepend args somehow
;; (defun gnugol-search-site(str)
;;   "Search any specific site via gnugol."
;;   (interactive "site: ")
;;   (gnugol-search-google (concat "site:" str))
;;   )

;; And this is just around so that I test output formats
;; It suffers because I would like it to toss output
;; into a buffer formatted in that format...

(defun gnugol-test(str)
  "Output gnugol's test data."
  (interactive "sMode: ")
  (let (gnugol-default-output-format)
    (setq gnugol-default-output-format str)
    (gnugol-search-dummy "all")
    )
)

;; FIXME: I'd really like a way to split the current window 
;;        at 80 characters and bring up the search on the 
;;        right. AND override my default url opener to be
;;        the internal emacs web browser for sites on a
;;        whitelist.
;; FIXME: NOTHING BELOW HERE ACTUALLY WORKS at all YET
;; (in contrast to the above which only sort of works)
;; FIXME: add hooks for additional modes - 
;; FIXME: For the into-pt stuff, be sensitive to the mode
;;        If I'm in markdown format, return markdown
;;        org, do org
;;        html, do html. Etc.
;;        C mode, put it in comments
;;        etc
;; FIXME: simplify navigation in org-mode buffer with minor mode
;;        add n and p to move to the links? CNTRL-arrows are good enough 
;;        
;; FIXME: Add robust interface
;; gnugol-thing-at-pt
;; gnugol-into-pt
;; gnugol-thing-at-pt-into-pt



(provide 'gnugol)

