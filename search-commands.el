;;; search-commands.el --- Search command helpers  -*- lexical-binding: t; -*-

(defun roam-rg-search ()
 "Search org-roam directory using consult-ripgrep. With live-preview."
 (interactive)
 (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
 (consult-ripgrep org-roam-directory)))

(defun counsel-ag-roam ()
 "Do counsel-ag on the org roam directory"
 (interactive)
 (counsel-ag nil org-roam-directory))

(defun consult-ripgrep-current-directory ()
 "Search current directory using consult-ripgrep."
 (interactive)
 (consult-ripgrep default-directory))

(defun jay/smart-ripgrep (&optional arg)
 "Context-aware ripgrep search.
In an org-roam buffer: search all of `org-roam-directory'.
With prefix ARG in an org-roam buffer: search current directory only.
Outside org-roam: always search current directory."
 (interactive "P")
 (if (and (not arg)
          (bound-and-true-p org-roam-directory)
          (buffer-file-name)
          (string-prefix-p (expand-file-name org-roam-directory)
                           (expand-file-name (buffer-file-name))))
     (roam-rg-search)
   (consult-ripgrep-current-directory)))


(defun deadgrep-current-directory ()
 "Search current directory using deadgrep."
 (interactive)
 (let ((search-term (read-string "Search term: ")))
  (deadgrep search-term default-directory)))

(defalias 'deadgrep 'deadgrep-project-directory)

(defalias 'rg-current-directory 'rg-dwim-current-dir)
(defalias 'rg-project-directory 'rg-dwim-project-dir)
(defalias 'rg-current-file 'rg-dwim-current-file)

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :commands (consult-org-roam-mode
             consult-org-roam-file-find
             consult-org-roam-backlinks
             consult-org-roam-forward-links
             consult-org-roam-search)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
  consult-org-roam-forward-links
  :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  ;; ("C-c n e" . consult-org-roam-file-find)
  ;; ("C-c n b" . consult-org-roam-backlinks)
  ;; ("C-c n l" . consult-org-roam-forward-links)
  ;; ("C-c n r" . consult-org-roam-search)
)

;; (define-key key-minor-mode-map (kbd "s-G") 'counsel-projectile-grep); this fails to ignore files specified in .projectile
;; (define-key key-minor-mode-map (kbd "s-G") 'projectile-grep) ; this successfully ignores those files but isn't incremental

(use-package affe
 :commands (affe-find affe-grep)
 :config
 ;; Manual preview key for `affe-grep'
 (consult-customize affe-grep :preview-key "M->"))

(use-package ffap)
(defun find-file-at-point-or-affe-find ()
 "Open the file at point if one exists, otherwise run affe-find."
 (interactive)
 (let ((filename (ffap-file-at-point))) ; Check if there is a file at point
 (if filename
  (find-file filename) ; Open file at point if it exists
  (affe-find)))) ; Otherwise, run affe-find

(defun affe-grep-gnulisp-directory (&optional initial)
 "Fuzzy grep in the /Users/jay/gnulisp directory with optional INITIAL input."
 (interactive "P")
 (affe-grep "/Users/jay/gnulisp" initial))

(defun affe-grep-yasnippets-directory (&optional initial)
 "Fuzzy grep in the /Users/jay/gnulisp directory with optional INITIAL input."
 (interactive "P")
 (affe-grep "/Users/jay/emacs/interesting-snippets/org-mode" initial))

;; (define-key key-minor-mode-map (kbd "s-/ g r") 'consult-org-roam-search)
;; (define-key key-minor-mode-map (kbd "s-/ g r") 'affe-grep-org-roam)

(defun counsel-find-file-in-yasnippets ()
 "Use counsel-find-file to search for files in the org-mode snippets directory."
 (interactive)
 (let ((default-directory "/Users/jay/emacs/interesting-snippets/org-mode/"))
 (counsel-find-file)))

(defconst jay/proposal-directory-candidates
  '("/Users/jay/Dropbox/writing/both/proposal"
    "/Users/jay/Library/CloudStorage/Dropbox/writing/both/proposal"
    "/Users/jay/Dropbox/writing/proposal"
    "/Users/jay/Library/CloudStorage/Dropbox/writing/proposal"))

(defconst jay/book-directory-candidates
  '("/Users/jay/Dropbox/writing/both/book"
    "/Users/jay/Library/CloudStorage/Dropbox/writing/both/book"
    "/Users/jay/Dropbox/writing/book"
    "/Users/jay/Library/CloudStorage/Dropbox/writing/book"))

(defun jay/first-existing-directory (candidates label)
  "Return the first existing directory from CANDIDATES for LABEL.
Signal a helpful error if none of the directories exist."
  (or (seq-find #'file-directory-p candidates)
      (user-error "No %s directory found. Checked: %s"
                  label
                  (mapconcat #'identity candidates ", "))))

(defun jay/proposal-directory ()
  "Return Jay's live proposal directory."
  (jay/first-existing-directory jay/proposal-directory-candidates "proposal"))

(defun jay/book-directory ()
  "Return Jay's live book directory."
  (jay/first-existing-directory jay/book-directory-candidates "book"))

(defun fzf-find-file--proposal-directory ()
 "Use counsel-fzf to search for files in Jay's proposal directory."
 (interactive)
 (counsel-fzf nil (jay/proposal-directory)))

(defalias 'search-filename-proposal-directory 'fzf-find-file--proposal-directory)


(defun fzf-find-file--book-directory ()
 "Use counsel-fzf to search for files in Jay's book directory."
 (interactive)
 (counsel-fzf nil (jay/book-directory)))

(defalias 'search-filename-book-directory 'fzf-find-file--book-directory)

(defun counsel-fzf-both-proposal-and-book-dirs ()
  "Search in both Jay's proposal and book directories."
  (interactive)
  (let* ((dir1 (jay/proposal-directory))
         (dir2 (jay/book-directory))
         ;; Use directory-files-recursively to list all files in dir1.
         ;; This function recursively lists files in a directory and its subdirectories.
         (files1 (directory-files-recursively dir1 ""))
         ;; Similarly, list all files in dir2.
         (files2 (directory-files-recursively dir2 ""))
         ;; Combine the file lists from both directories into all-files.
         (all-files (append files1 files2)))
    (let ((file (completing-read "Find file: " all-files nil t)))
      (when (and file (not (string-empty-p file)))
        (find-file file)))))


(defalias 'search-filename-both-book-and-proposal-directories 'counsel-fzf-both-proposal-and-book-dirs)

;; works but is overly complex. Do not use. use counsel-projectile-ag instead.
(defun rg-search-book-and-proposal-dirs (&optional initial-input)
 "Search using ripgrep in Jay's book and proposal directories with optional INITIAL-INPUT."
 (interactive "sEnter search pattern: ")
 (let* ((dir1 (jay/book-directory))
        (dir2 (jay/proposal-directory))
        (input (if (and initial-input (not (equal initial-input "")))
                   initial-input
                 ".*"))
        (rg-cmd (mapconcat #'identity
                           (list "rg --vimgrep --color=always --hidden -g '!.git'"
                                 (shell-quote-argument input)
                                 (shell-quote-argument dir1)
                                 (shell-quote-argument dir2))
                           " ")))
  (compilation-start rg-cmd 'grep-mode)))

;; don't need any of this. I put both directories in the same projectile project ("both"), so just use counsel-projectile-ag


(defalias 'grep-both-directories 'counsel-projectile-ag)
(defalias 'grep-book-directory 'consult-ripgrep-current-directory)
(defalias 'grep-proposal-directory 'consult-ripgrep-current-directory)


(defun projectile-display-project-root ()
 "Show the root directory of the current Projectile project."
 (interactive)
 (if (projectile-project-p)
   (message "Current project root: %s" (projectile-project-root))
  (message "Not in a project")))

(setq counsel-projectile-find-file-matcher #'counsel--find-file-matcher)

(setq counsel-find-file-ignore-regexp "\\.html")

(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
			  (buffer-substring-no-properties
			   (progn (skip-syntax-backward "w_") (point))
			   (progn (skip-syntax-forward "w_") (point)))
			  "\\>")))
      (if (and isearch-case-fold-search
	       (eq 'not-yanks search-upper-case))
	  (setq string (downcase string)))
      (setq isearch-string string
	    isearch-message
	    (concat isearch-message
		    (mapconcat 'isearch-text-char-description
			       string ""))
	    isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

;; (global-set-key "\C-cw" 'my-isearch-word-at-point)

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  "After isearch, move point to the beginning of the match and recenter the view."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)
    (recenter-top-bottom)))

(defun isearch-from-buffer-start ()
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (isearch-forward))

(use-package projectile
:defer t
  :diminish projectile-mode
  :custom
  (projectile-completion-system 'default) ;; Vertico integrates via default
  (projectile-enable-caching t)

  (projectile-globally-ignored-file-suffixes '("docx" "jpg" "png" "tmp" "tex" "html" "pdf" "pptx" "tmp"))

  (projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".tmp" "archived-work" "images" "excel-mindnode-omni" "archive"))

  (projectile-globally-ignored-files '("TAGS" ".DS_Store" ".projectile" ".dropbox"))

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :bind
  (:map projectile-mode-map
        ("s-o" . nil)))

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
;;  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
;; (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
;  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
 )

;;; Tell ispell.el that ’ can be part of a word.
(setq ispell-local-dictionary-alist
      `((nil "[[:alpha:]]" "[^[:alpha:]]"
             "['\x2019]" nil ("-B") nil utf-8)))

;;; Don't send ’ to the subprocess.
(defun replace-curly-apostrophe (args)
  (cons (replace-regexp-in-string
         "’" "'" (car args))
        (cdr args)))
(advice-add #'ispell-send-string :filter-args
            #'replace-curly-apostrophe)

;;; Convert ' back to ’ from the subprocess.
(defun replace-curly-quotes (args)
  (if (not (derived-mode-p 'org-mode))
      args
    (cons (replace-regexp-in-string
           "'" "’" (car args))
          (cdr args))))
(advice-add #'ispell-parse-output :filter-args
            #'replace-curly-quotes)

(defun ignore-case-in-searches ()
 (interactive)
 (setq case-fold-search t)
 )

(defun isearch-forward-ignore-case ()
(interactive)
(ignore-case-in-searches)
(isearch-forward)
)

;; Example configuration for Consult
(use-package consult
:defer t
 :after (consult-eglot)
 ;; Replace bindings. Lazily loaded due by `use-package'.
 :bind (
;; ("C-s s" . consult-line)
;; 	 ("C-s e" . consult-eglot-symbols)
;; 	 ("C-s r" . consult-ripgrep)
;; ("s-G" . consult-ripgrep)
;; 	 ("C-s f" . consult-find)
;; 	 ("C-s l" . consult-flymake)
;; 	 ("C-s n" . consult-focus-lines)
;; 	 ("C-s I" . consult-project-imenu)
;; 	 ("C-s o" . consult-outline)
;; 	 ("C-s b" . consult-bookmark)
;; 	 ("C-s m" . consult-man)

   )

 ;; Enable automatic preview at point in the *Completions* buffer. This nt when you use the default completion UI.
 :hook (completion-list-mode . consult-preview-at-point-mode)

 ;; The :init configuration is always executed (Not lazy)
 :init

 (global-unset-key (kbd "C-s"))

 ;; Optionally configure the register formatting. This improves the register
 ;; preview for `consult-register', `consult-register-load',
 ;; `consult-register-store' and the Emacs built-ins.
 (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)

 ;; Optionally tweak the register preview window.
 ;; This adds thin lines, sorting and hides the mode line of the window.
 (advice-add #'register-preview :override #'consult-register-window)

 ;; Use Consult to select xref locations with preview
 (setq xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)

 ;; Configure other variables and modes in the :config section,
 ;; after lazily loading the package.
 :config

 ;; Optionally configure preview. The default value
 ;; is 'any, such that any key triggers the preview.
 ;; (setq consult-preview-key 'any)
 ;; (setq consult-preview-key (kbd "M-."))
 ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
 ;; For some commands and buffer sources it is useful to configure the
 ;; :preview-key on a per-command basis using the `consult-customize' macro.
 (consult-customize
  consult-theme
  :preview-key '(:debounce 0.2 any)
  consult-ripgrep consult-git-grep consult-grep
  consult-bookmark consult-recent-file consult-xref
  consult--source-bookmark consult--source-recent-file
  consult--source-project-recent-file
  ;; :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
 ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
 ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
 ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
 ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ))

(key-chord-define-global "SS" 'consult-line)

(use-package orderless

 :defer t
 :ensure t
 :custom
 (completion-styles '(orderless))
 (completion-category-overrides '((command (styles . (partial-completion))))))

(use-package marginalia
:defer t
:ensure t
 :delight
 :custom
 (marginalia-max-relative-age 0)
 (marginalia-align 'right)
 :init
 (marginalia-mode))

 (use-package vertico
:defer t
:ensure t
   :init
 (vertico-mode)

 ;; Different scroll margin
 ;; (setq vertico-scroll-margin 0)

 ;; Show more candidates
 ;; (setq vertico-count 20)

 (setq vertico-resize t)

 ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
 ;; (setq vertico-cycle t)
 )

(defun consult-recenter-advice (orig-fn &rest args)
 "Advice for `recenter' to avoid errors when the window is not displaying the current buffer."
 (when (eq (window-buffer) (current-buffer))
  (apply orig-fn args)))

(advice-add 'recenter :around #'consult-recenter-advice)

;; (setq consult-git-grep-args )

;; (rg-enable-default-bindings)

(use-package deadgrep
  :defer)

(defun timu/search-org-files ()
  "Grep for a string in the `~/org' using `rg'."
  (interactive)
(consult-ripgrep "~/org" ""))

 (defun timu/search-project-files ()
  "Grep for a string in the `~/projects' using `rg'."
  (interactive)
(consult-ripgrep "~/projects" ""))

(defvar jay/recent-file-parent-column-min 24
  "Minimum width, in characters, reserved for the parent-path column
in `jay/consult-recent-file-folder-first'.")

(defvar jay/recent-file-parent-column-max 52
  "Maximum width, in characters, reserved for the parent-path column.
Longer parent paths are left-truncated with a leading ellipsis so the
filename column still lines up and the most distinctive (deepest) part
of the path stays visible.")

(defvar jay/recent-file-display-order 'folder-first
  "Visual order for the `jay/consult-recent-file-folder-first' picker.
- `folder-first': parent path on the left (shadow face), basename on the
  right. Better when many candidates share basenames and the folder is
  the disambiguator.
- `filename-first': basename on the left at column 0, parent path on the
  right (shadow face). Better when basenames are mostly unique and you
  want the typed-against text leftmost.
The candidate list and matching behavior are identical either way; only
the display layout changes. Flip with `setq' for an A/B trial.")

;; Jay's active preference (overrides the defvar default above). The
;; defvar's documented default stays `folder-first' as the neutral
;; baseline so the docstring still describes both options accurately;
;; this `setq' is the actual choice for this config. Flip back with
;; `(setq jay/recent-file-display-order 'folder-first)' if the
;; basename-first layout doesn't pan out after a few days of use.
(setq jay/recent-file-display-order 'filename-first)

(defun jay/recent-file--canonical (path)
  "Return PATH canonicalized: symlinks resolved, HOME abbreviated to \"~\".
Resolving via `file-truename' collapses the macOS CloudStorage
Dropbox symlink to its real location (e.g. `/Users/jay/Dropbox/...'),
so recentf entries that arrived under either spelling deduplicate
cleanly. Returns nil for inputs that do not exist or are remote."
  (let ((expanded (ignore-errors (expand-file-name (or path "")))))
    (when (and expanded
               (not (file-remote-p expanded))
               (file-exists-p expanded))
      (abbreviate-file-name (file-truename expanded)))))

(defvar jay/recent-file-ignored-prefixes '("/private/tmp/")
  "Canonical-path prefixes to exclude from
`jay/consult-recent-file-folder-first'. Each entry is matched with
`string-prefix-p' against the symlink-resolved path, so on macOS
`/tmp/...' entries (which `file-truename' resolves to `/private/tmp/...')
are caught by the single `/private/tmp/' prefix.")

(defun jay/recent-file--ignored-p (canonical)
  "Return non-nil when CANONICAL should be hidden from the recent-file picker.
Compared against `jay/recent-file-ignored-prefixes'."
  (seq-some (lambda (prefix) (string-prefix-p prefix canonical))
            jay/recent-file-ignored-prefixes))

(defun jay/recent-file-candidates-folder-first ()
  "Return a deduplicated list of canonical recent-file paths.
Order follows `recentf-list' (most recent first). Each entry is an
abbreviated, symlink-resolved path like =~/Dropbox/roam/foo.org=; the
folder-first column layout is applied later by
`jay/recent-file--affix' via `affixation-function' metadata, so these
strings stay clean and remain safe to hand to Embark, Marginalia,
`find-file', etc.

Paths matching any prefix in `jay/recent-file-ignored-prefixes' are
dropped from the candidate list."
  (let ((seen (make-hash-table :test 'equal))
        (paths nil))
    (dolist (entry recentf-list)
      (let ((canonical (jay/recent-file--canonical entry)))
        (when (and canonical
                   (not (jay/recent-file--ignored-p canonical))
                   (not (gethash canonical seen)))
          (puthash canonical t seen)
          (push canonical paths))))
    (nreverse paths)))

(defun jay/recent-file--affix (paths)
  "Return affixation triples (CAND PREFIX SUFFIX) for PATHS.
CAND is the basename (what Vertico displays as the completion); the
parent-directory column is rendered in `shadow' face on either side of
CAND, controlled by `jay/recent-file-display-order'. PATHS must be
the candidate list as passed to the completion UI so widths reflect
the currently visible set."
  (let* ((parents
          (mapcar (lambda (p)
                    (directory-file-name (or (file-name-directory p) "")))
                  paths))
         (basenames (mapcar #'file-name-nondirectory paths))
         (widest-parent (apply #'max 0 (mapcar #'length parents)))
         (parent-col (min jay/recent-file-parent-column-max
                          (max jay/recent-file-parent-column-min
                               widest-parent)))
         (basename-col (apply #'max 0 (mapcar #'length basenames)))
         (filename-first-p (eq jay/recent-file-display-order 'filename-first)))
    (cl-mapcar
     (lambda (path parent base)
       (let* ((shown
               (if (> (length parent) parent-col)
                   (concat "…"
                           (substring parent
                                      (1+ (- (length parent) parent-col))))
                 parent))
              (parent-pad (make-string (max 0 (- parent-col (length shown)))
                                       ?\s))
              (parent-display
               (concat (propertize shown 'face 'shadow) parent-pad)))
         (if filename-first-p
             (let ((base-pad
                    (make-string (max 0 (- basename-col (length base))) ?\s)))
               (list base "" (concat base-pad "  " parent-display)))
           (list base (concat parent-display "  ") ""))))
     paths parents basenames)))

(defun jay/consult-recent-file-folder-first ()
  "Find a recent file using a folder-first picker.
Candidates are canonical abbreviated paths (so Embark and Marginalia
work on real file paths, not on display strings); the folder-first,
column-aligned visual layout is applied by `jay/recent-file--affix'
via `affixation-function' metadata. Order follows `recentf-list'
(most recent first) — `:sort nil' and identity sort functions prevent
Vertico from re-sorting."
  (interactive)
  (require 'recentf)
  (unless recentf-mode (recentf-mode 1))
  (require 'consult)
  (let ((paths (jay/recent-file-candidates-folder-first)))
    (unless paths
      (user-error "No recent files found"))
    (let* ((table
            (lambda (string pred action)
              (if (eq action 'metadata)
                  ;; Use a custom `jay/recent-file' category (not `file')
                  ;; so Marginalia does not add its file annotator
                  ;; (permissions / size / mtime). Embark is routed to
                  ;; `embark-file-map' for this category via
                  ;; `embark-keymap-alist' below, so the five custom
                  ;; file actions (L/B/A/V/P) and the built-ins still
                  ;; work.
                  `(metadata
                    (category . jay/recent-file)
                    (display-sort-function . identity)
                    (cycle-sort-function   . identity)
                    (affixation-function . ,#'jay/recent-file--affix))
                (complete-with-action action paths string pred))))
           (choice
            (consult--read
             table
             :prompt "Recent file: "
             :sort nil
             :require-match t
             :category 'jay/recent-file
             :history 'file-name-history
             :state (when (fboundp 'consult--file-preview)
                      (consult--file-preview)))))
      (find-file choice))))

(defun jay/embark-file-copy-as-org-link (file)
  "Copy FILE to the kill ring as an Org `[[file:...][basename]]' link.
Uses `abbreviate-file-name' so the link is portable across the
HOME-expansion variants on this machine."
  (interactive "fFile: ")
  (let* ((short (abbreviate-file-name file))
         (link (format "[[file:%s][%s]]" short (file-name-base file))))
    (kill-new link)
    (message "Copied: %s" link)))

(defun jay/embark-file-copy-basename (file)
  "Copy just the basename of FILE to the kill ring."
  (interactive "fFile: ")
  (let ((base (file-name-nondirectory file)))
    (kill-new base)
    (message "Copied: %s" base)))

(defun jay/embark-file-copy-abbrev-path (file)
  "Copy FILE to the kill ring with HOME abbreviated to \"~\"."
  (interactive "fFile: ")
  (let ((short (abbreviate-file-name file)))
    (kill-new short)
    (message "Copied: %s" short)))

(defun jay/embark-file-reveal-in-finder (file)
  "Reveal FILE in the macOS Finder, with the file selected."
  (interactive "fFile: ")
  (require 'reveal-in-finder)
  (let* ((abs (expand-file-name file))
         (dir (file-name-directory abs))
         (base (file-name-nondirectory abs)))
    (reveal-in-finder-as dir base)))

(defun jay/embark-file-open-parent-in-dired (file)
  "Open the parent directory of FILE in Dired, with point on FILE."
  (interactive "fFile: ")
  (let ((abs (expand-file-name file)))
    (dired (file-name-directory abs))
    (dired-goto-file abs)))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "L")   #'jay/embark-file-copy-as-org-link)
  (define-key embark-file-map (kbd "B")   #'jay/embark-file-copy-basename)
  (define-key embark-file-map (kbd "A")   #'jay/embark-file-copy-abbrev-path)
  (define-key embark-file-map (kbd "s-|") #'jay/embark-file-copy-abbrev-path)
  (define-key embark-file-map (kbd "V")   #'jay/embark-file-reveal-in-finder)
  (define-key embark-file-map (kbd "P")   #'jay/embark-file-open-parent-in-dired)
  ;; Route the custom `jay/recent-file' category (used by
  ;; `jay/consult-recent-file-folder-first' and `fasd-find-file') to
  ;; the same keymap as plain files, so all five actions above and
  ;; Embark's built-ins work on `s-r' / `s-R' candidates. The custom
  ;; category exists solely to suppress Marginalia's file annotator
  ;; (permissions / size / mtime) — Embark behavior is unchanged.
  (add-to-list 'embark-keymap-alist '(jay/recent-file . embark-file-map)))

(defun timu/org-go-to-heading (&optional arg)

"Go to an outline heading with `consult-org-heading'. Also move the heading to the top of the buffer with `evil-scroll-line-to-top'"

  (interactive)

(consult-org-heading)
(evil-scroll-line-to-top arg))

(use-package counsel-fd
  :defer t)

(setq consult-locate-args "fasd -t -l -R")

(use-package counsel :defer t)

(with-eval-after-load 'vertico
;; (define-key vertico-map (kbd "M-o") 'embark-act)
)

(defun isearch-forward-word-at-point ()
  "Search forward for the next occurrence of the word at point.
If invoked again, move to the next instance of the word."
  (interactive)
  (let ((word (thing-at-point 'word t)))  ;; Get word at point, non-nil for empty words
    (if (not word)
        (message "No word at point")
      (if (eq last-command this-command)
          (isearch-repeat-forward)  ;; Continue searching if repeated
        (progn
          (isearch-forward nil 1)
          (isearch-yank-string word))))))
