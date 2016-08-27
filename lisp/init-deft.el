;; deft settings

(require 'deft)

(setq
 deft-extensions '("org" "txt" "md" "taskpaper")
 deft-default-extension "org"
 deft-directory "~/Dropbox/wiki"
 deft-text-mode 'org-mode
 deft-auto-save-interval 0
 deft-use-filename-as-title t
 deft-recursive t)

(global-set-key (kbd "<f3>") 'deft)

(provide 'init-deft)
