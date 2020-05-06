;; deft settings

(require 'deft)

(setq
 deft-extensions '("org" "clj" "txt" "md" "taskpaper")
 deft-default-extension "org"
 deft-directory "~/Dropbox/Archive/wiki.deft"
 deft-text-mode 'org-mode
 deft-auto-save-interval 0
 deft-use-filename-as-title t
 deft-recursive t)

(provide 'init-deft)
