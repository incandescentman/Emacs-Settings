(setq-default ispell-program-name (executable-find "hunspell")) 
(setq ispell-really-hunspell t) 
(setq flyspell-default-dictionary "en_US")

(setq ispell-dictionary "en_US")
(setq ispell-program-name "/usr/local/bin/hunspell")
(setenv "DICTIONARY" "en_US") 

(setq ispell-program-name "hunspell")
   (eval-after-load "ispell"
    '(progn (defun ispell-get-coding-system () 'utf-8)))

(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:][:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))) 

(setq ispell-extra-args '("-d en_US"))
