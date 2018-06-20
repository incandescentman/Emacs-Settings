(defconst ag-lang-tools-packages
  '((mw-thesaurus
     :location local
                  ;; (recipe :fetcher github :repo "agzam/mw-thesaurus.el")
                  )

    ;; sdcv-mode is for browsing Stardict format dictionaries in Emacs
    ;;
    ;; to get Webster’s Revised Unabridged Dictionary
    ;; 1) download it from https://s3.amazonaws.com/jsomers/dictionary.zip
    ;; 2) unzip it twice and put into ~/.stardict/dic
    ;; 3) Install sdcv, a command-line utility for accessing StarDict dictionaries
    ;;
    ;; you can find more dicts in stardict format here: http://download.huzheng.org/dict.org/
    ;; don't get the package from MELPA - it's been reported broken
    (sdcv-mode :location (recipe
                          :fetcher github
                          :repo "gucong/emacs-sdcv"))))

(defun ag-lang-tools/init-mw-thesaurus ()
  (use-package mw-thesaurus
    :demand t
    :config
    (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
    (spacemacs/set-leader-keys
      "xlm" #'mw-thesaurus--lookup-at-point
      "xAg" #'add-global-abbrev
      "xAl" #'add-mode-abbrev)))

(defun ag-lang-tools/init-sdcv-mode ()
  (use-package sdcv-mode
    :demand t
    :config
    (add-hook 'sdcv-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

    (defun sdcv-search-at-point ()
      (interactive)
      (sdcv-search (ag/region-or-word-at-point-str) nil nil t))

    (spacemacs/set-leader-keys "xll" #'sdcv-search-at-point)

    (evil-define-key 'normal sdcv-mode-map "q" #'sdcv-return-from-sdcv)
    (evil-define-key 'normal sdcv-mode-map "n" #'sdcv-next-entry)
    (evil-define-key 'normal sdcv-mode-map "p" #'sdcv-previous-entry)
    (evil-define-key 'normal sdcv-mode-map (kbd "RET") #'sdcv-search-at-point)
    (evil-define-key 'normal sdcv-mode-map "a" #'sdcv-search-at-point)))
