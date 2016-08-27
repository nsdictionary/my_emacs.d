;;; Key bindings

;; Completion at point
(global-set-key (kbd "C-,") 'completion-at-point)

;; redo+
(require 'redo+)
(global-set-key (kbd "M-_") 'redo)

;; marked 2
(defun markdown-preview-file ()
  "use Marked 2 to preview the current file"
  (interactive)
  (shell-command
   (format "open -a 'Marked 2.app' %s"
           (shell-quote-argument (buffer-file-name))))
  )
(global-set-key "\C-cm" 'markdown-preview-file)

;; shift region
(global-set-key (kbd "C-c >") (kbd (format "C-u %d C-x TAB" tab-width)))
(global-set-key (kbd "C-c <") (kbd (format "C-u - %d C-x TAB" tab-width)))

;; Help teach to unlearn the arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))


;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

(provide 'key-bindings)
