;; set default theme
(load-theme 'nimbus t)

;; set line number
(global-linum-mode 1)

;; set scrollbar
(global-yascroll-bar-mode 1)

;; highlight cursor line
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray13")
(set-face-attribute 'region nil :background "#f5f5dc" :foreground "black")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; set default font
(set-frame-font "DejaVu Sans Mono 14" nil t)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Disable newline arrow symbol
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; Highlight indent guide
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)

(provide 'appearance)
