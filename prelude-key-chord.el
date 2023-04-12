;;; prelude-key-chord.el --- Key chord setup
;;
;; Copyright © 2011-2013 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configure key-chord key bindings.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
;; (prelude-require-package 'key-chord)

(require 'key-chord)

(key-chord-define-global "JJ" 'helm-multi-swoop-org)
;; (key-chord-define-global "jj" 'imenu)


(key-chord-define-global "KK" 'helm-imenu) ;
(key-chord-define-global "kk" 'consult-outline) ;


;; (key-chord-define-global "jk" 'ace-jump-line-mode)

(key-chord-define-global "JK" 'avy-move-line)


;; (key-chord-define-global "jk" 'ace-jump-char-mode)
;; (key-chord-define-global "JJ" 'prelude-switch-to-previous-buffer)
(key-chord-define-global "UU" 'undo-tree-visualize)
(key-chord-define-global "XX" 'helm-M-x)
(key-chord-define-global "YY" 'repeat-last-command)
;; (key-chord-define-global "yy" 'browse-kill-ring)
(key-chord-define-global "WW" 'consult-git-grep)
(key-chord-define-global "CC" 'turn-on-auto-capitalize-mode)
(key-chord-define-global "LL" 'turn-off-auto-capitalize-mode)
(key-chord-define-global "TT" 'titlecase-dwim)
(key-chord-define-global "DD" 'bjm/ivy-dired-recent-dirs)
;; (key-chord-define-global "ZZ" 'repeat-complex-command)
(key-chord-define-global "ZZ" 'undo-tree-undo)
(key-chord-define-global "RR" 'replace-regexp)
(key-chord-define-global "SS" 'swiper)
(key-chord-define-global "FF" 'isearch-forward-regexp)
;; (key-chord-define-global "HH" 'cheatsheet-show)
;; (key-chord-define-global "PP" 'pcre-regexp-from-list-of-words)


(key-chord-define-global "EE" 'fasd-find-file)

(key-chord-define-global "MM" 'mc/mark-all-dwim)
(key-chord-define-global "AA" 'inverse-add-global-abbrev)


;; (key-chord-define-global "NN" 'ni-narrow-to-region-indirect-other-window)
;; (key-chord-define-global "NN" 'org-narrow-to-subtree)

;; (key-chord-define-global "II" 'god-mode-all)

;; (setq key-chord-one-key-delay 0.11)

;; John Cook's post http://www.johndcook.com/blog/2015/02/01/rare-bigrams/
;; provides a list of rare bi-grams that would work great for key-chords.

;; Below list is based off that after removing all the key-chord duplicates
;; like `xs' and `sx'.

;;      fb
;;      gb gp
;;  jj  jc jf jg jh jk jl jm jp jq js jt jv jw jx jy jz
;;  kk
;;  qq  qb qf qg qh qk ql qm qp qt qv qw qx qy qz
;;  vv  vc vf vg vh vk vm vp vw vz
;;  ww
;;      xb xd xg xk xm xs xw
;;  yy
;;      zb zd zf zg zk zm zp zs zw zx

;; (key-chord-define-global "  " 'smart-period)


(key-chord-mode +1)

(provide 'prelude-key-chord)

;;; prelude-key-chord.el ends here

