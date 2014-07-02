;;; pastebin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pastebin pastebin-buffer pastebin) "pastebin"
;;;;;;  "pastebin.el" (20596 9781 549907 109000))
;;; Generated autoloads from pastebin.el

(let ((loads (get 'pastebin 'custom-loads))) (if (member '"pastebin" loads) nil (put 'pastebin 'custom-loads (cons '"pastebin" loads))))

(autoload 'pastebin-buffer "pastebin" "\
Send the whole buffer to pastebin.com.
Optional argument domain will request the virtual host to use,
eg:'emacs.pastebin.com' or 'mylocalpastebin.com'.

\(fn &optional DOMAIN)" t nil)

(autoload 'pastebin "pastebin" "\
Send the region to the pastebin service specified by domain.

See pastebin.com for more information about pastebin.

Called interactively pastebin uses the current region for
preference for sending... if the mark is NOT set then the entire
buffer is sent.

Argument START is the start of region.
Argument END is the end of region.

If domain is used pastebin prompts for a domain defaulting to
'pastebin-default-domain' so you can send requests or use a
different domain.

\(fn START END &optional DOMAIN)" t nil)

;;;***

;;;### (autoloads nil nil ("pastebin-pkg.el") (20596 9781 661403
;;;;;;  257000))

;;;***

(provide 'pastebin-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pastebin-autoloads.el ends here
