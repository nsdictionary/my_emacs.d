;;; Key bindings

;; Delete selection mode
(delete-selection-mode t)

;; Completion at point
(global-set-key (kbd "C-,") 'completion-at-point)

;; redo+
(require 'redo+)

;; marked 2
(defun markdown-preview-file ()
  "use Marked 2 to preview the current file"
  (interactive)
  (shell-command
   (format "open -a 'Marked 2.app' %s"
           (shell-quote-argument (buffer-file-name))))
  )
(global-set-key "\C-cm" 'markdown-preview-file)

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

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; Move windows, even in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Create scratch buffer
(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Deft
(global-set-key [f2] 'deft)
(global-set-key (kbd "C-x C-g") 'deft-find-file)

;; you can select the key you prefer to
(define-key global-map (kbd "C-;") 'ace-jump-mode)

;; Multiple cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Line movement
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Magit
(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (magit-status)
  (unless prefix
    (delete-other-windows)))

(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Clear buffer on eshell mode
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
         (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;; Smart beginning of line
(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key "\C-a" 'smart-beginning-of-line)

;; Toggle fullscreen
(defun toggle-fullscreen() "toggle-fullscreen, same as clicking the
 corresponding titlebar icon in the right hand corner of Mac app windows"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (pcase (frame-parameter nil 'fullscreen)
     (`fullboth nil)
     (`fullscreen nil)
     (_ 'fullscreen))))
(global-set-key (kbd "C-s-f") 'toggle-fullscreen)

;; Move focus after split window
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; Resize window
(global-set-key (kbd "<C-s-up>") 'shrink-window)
(global-set-key (kbd "<C-s-down>") 'enlarge-window)
(global-set-key (kbd "<C-s-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-s-right>") 'enlarge-window-horizontally)
(provide 'key-bindings)
