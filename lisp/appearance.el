;; set default theme
(load-theme 'leuven t)

;; set line number
(global-linum-mode 1)

;; highlight cursor line
(global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; set default font
(set-default-font "DejaVu Sans Mono Book 12")

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

(provide 'appearance)
