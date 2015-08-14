(add-to-list 'load-path (concat dotfiles-dir "zone-matrix"))
(require 'zone-matrix)
(require 'zone-matrix-settings)
(require 'zone-settings)

(setq zone-programs [zone-matrix])
(zone-when-idle 60)
