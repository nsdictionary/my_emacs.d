(require 'projectile)

(projectile-global-mode)

(setq projectile-git-submodule-command nil)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)

(global-set-key (kbd "C-S-f") 'projectile-switch-project)

(provide 'init-projectile)

