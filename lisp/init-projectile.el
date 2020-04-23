(require 'projectile)
(projectile-global-mode)
(setq projectile-git-submodule-command nil)

(global-set-key (kbd "C-s-f") 'projectile-switch-project)
(provide 'init-projectile)
