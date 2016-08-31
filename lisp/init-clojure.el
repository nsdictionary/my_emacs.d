;;; Setting clojure environment

(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'ac-cider)

(global-set-key  [M-return] 'cider-eval-last-sexp)

(provide 'init-clojure)
