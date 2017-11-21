(require 'neotree)
(global-set-key [f5] 'neotree-toggle)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(provide 'init-neotree)