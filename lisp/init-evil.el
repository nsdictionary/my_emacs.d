(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(evil-vimish-fold-mode 1)

(use-package evil
  :config
  (evil-mode 1)
  ;; set initial evil state for particular modes
  (cl-loop for (mode . state) in '((cider-test-report-mode . emacs)
                                   (deft-mode              . emacs)
                                   (dired-mode             . normal)
                                   (magit-mode             . normal)
                                   (magit-status-mode      . emacs)
                                   (magit-diff-mode        . normal)
                                   (magit-log-mode         . normal)
                                   (magit-process-mode     . normal)
                                   (magit-popup-mode       . emacs)
                                   ;; this allows vi-mode in shells
                                   (term-mode              . emacs)
                                   (tide-references-mode   . emacs)
                                   (xref--xref-buffer-mode . emacs))
           do (evil-set-initial-state mode state)))

;; Using to address evil-mode ^r undo bug
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (evil-set-undo-system 'undo-tree)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Evil key setup
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-insert-state-map "\C-a" 'beginning-of-line)
(define-key evil-visual-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-motion-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-normal-state-map "\C-f" 'projectile-find-file)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-visual-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
(define-key evil-insert-state-map "\C-k" 'kill-line)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "SPC") 'evil-search-forward)
(define-key evil-normal-state-map (kbd "<tab>") 'org-cycle)

;; :q should kill the current buffer rather than quitting emacs entirely
(evil-ex-define-cmd "q" 'kill-this-buffer)
;; Need to type out :qq to close emacs
(evil-ex-define-cmd "qq" 'evil-quit)

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

(define-key evil-normal-state-map (kbd "C-p") (lambda ()
                    (interactive)
                    (previous-line 1)
                    (evil-scroll-line-up 1)
                    ))

(define-key evil-normal-state-map (kbd "C-n") (lambda ()
                      (interactive)
                      (next-line 1)
                      (evil-scroll-line-down 1)
                      ))

(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))

;; evil surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; evil leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "e" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "d" 'dired
  "s" 'split-window-below
  "v" 'split-window-right
  "nn" 'neotree-toggle
  "z" 'eshell
  "c" 'comment-line
  )

(eval-after-load "magit"
                 (evil-leader/set-key "g" 'magit-status))


;; terminal
(evil-set-initial-state 'term-mode 'emacs)

;; neotree
(add-hook 'neotree-mode-hook
      (lambda ()
        (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
        (define-key evil-normal-state-local-map (kbd "i") 'neotree-enter-horizontal-split)
        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
        (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
        (define-key evil-normal-state-local-map (kbd "U") 'neotree-select-up-node)
        (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)))

(provide 'init-evil)
