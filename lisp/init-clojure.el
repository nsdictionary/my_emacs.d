;;; Setting clojure environment

(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'ac-cider)

(global-set-key (kbd "C-c C-<return>") 'cider-eval-last-sexp)

(add-hook 'cider-repl-mode-hook
  '(lambda ()
     (define-key cider-repl-mode-map [up] 'cider-repl-backward-input)
     (define-key cider-repl-mode-map [down] 'cider-repl-forward-input)))

;; in org mode
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(provide 'init-clojure)
