;;; recent-addresses-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (recent-addresses-add-bcc recent-addresses-add-cc
;;;;;;  recent-addresses-add-to recent-addresses-add-first-to recent-addresses-load
;;;;;;  recent-addresses-mode) "recent-addresses" "recent-addresses.el"
;;;;;;  (20765 44953))
;;; Generated autoloads from recent-addresses.el

(defvar recent-addresses-mode nil "\
Non-nil if Recent-Addresses mode is enabled.
See the command `recent-addresses-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `recent-addresses-mode'.")

(custom-autoload 'recent-addresses-mode "recent-addresses" nil)

(autoload 'recent-addresses-mode "recent-addresses" "\
Minor mode for keeping track of recently used email addresses.
Addresses are collected when sending through `message-mode' and when reading in
`gnus'.  The headers collected are defined in `recent-addresses-headers' and
`recent-addresses-headers-received'.

Addresses can be added from email messages with `recent-addresses-add-headers'
and `recent-addresses-add-headers-received', as well as manually with
`recent-addresses-add'.

To insert addresses, use `recent-addresses-add-to',
`recent-addresses-add-cc' or `recent-addresses-add-bcc'.  If you want to
be prompted for this automatically when you create an email, add the
following to your .emacs:

\(add-hook 'message-setup-hook 'recent-addresses-add-first-to)

\\{recent-addresses-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'recent-addresses-load "recent-addresses" "\
Load the previously collected addresses from `recent-addresses-file'.
Unless FORCE is set, an existing list will not be overwritten.

\(fn &optional FORCE)" nil nil)

(autoload 'recent-addresses-add-first-to "recent-addresses" "\
Prompt the user for the To: header, unless there already is one.
The address is formatted according to `recent-addresses-insert-style'.
This function is safe to be run from `message-setup-hook' like this:

\(add-hook 'message-setup-hook 'recent-addresses-add-first-to)

\(fn)" nil nil)

(autoload 'recent-addresses-add-to "recent-addresses" "\
Read an email address and add it to To:.
The address is formatted according to `recent-addresses-insert-style'.

\(fn)" t nil)

(autoload 'recent-addresses-add-cc "recent-addresses" "\
Read an email address and add it to CC:.
The address is formatted according to `recent-addresses-insert-style'.

\(fn)" t nil)

(autoload 'recent-addresses-add-bcc "recent-addresses" "\
Read an email address and add it to BCC:.
The address is formatted according to `recent-addresses-insert-style'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("recent-addresses-pkg.el") (20765 44953
;;;;;;  594658))

;;;***

(provide 'recent-addresses-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; recent-addresses-autoloads.el ends here
