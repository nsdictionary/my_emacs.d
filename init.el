;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; remove tutorial message
(setq inhibit-startup-message t)

;; set cursor type
(setq-default cursor-type 'bar) ; box | bar | nbar

;; is MacOS ?
(setq is-mac (equal system-type 'darwin))

;; change command to meta, and ignore option
(when is-mac
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (global-set-key (kbd "M-a") 'mark-whole-buffer))

;; set line number
(global-linum-mode 1)

;; highlight cursor line
(global-hl-line-mode 1)

;; disable save mode
(desktop-save-mode 0)
(setq enable-local-variables nil)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; korean setting
(global-set-key [?\S- ] 'toggle-korean-input-method)
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

;; Scratch message
(setq-default initial-scratch-message
  (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; tab settings
(setq-default indent-tabs-mode nil) ; soft tab
(setq-default tab-width 4) ; tab size 4
(global-set-key (kbd "TAB") 'self-insert-command) ; self insert tab

;; display cursor position
(column-number-mode t)

;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; remove all trailing space when you save
(add-hook 'write-file-hooks
          'delete-trailing-whitespace)

;; ignore ring-bell sound
(setq ring-bell-function 'ignore)

;; Set path to dependencies
(setq settings-dir
      (expand-file-name "lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)

;; Machinery for installing required packages
(require 'init-elpa)

;; Load configs for specific features and modes
(require-package 'better-defaults)
(require-package 'dash)
(require-package 'flx)
(require-package 'flx-ido)
(require-package 'ido-vertical-mode)
(require-package 'ido-at-point)
(require-package 'ido-ubiquitous)
(require-package 'deft)
(require-package 'fill-column-indicator)
(require-package 'smex)
(require-package 'redo+)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Setup extensions
(eval-after-load 'ido '(require 'init-ido))
(require 'init-org)
(require 'init-deft)
