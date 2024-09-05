;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; Emacs version check
(let ((minver "24.3"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "24.5")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; remove tutorial message
(setq inhibit-startup-message t)

;; is MacOS ?
(setq is-mac (equal system-type 'darwin))

;; disable completion window
(setq completion-auto-help nil)

;; change command to meta, and ignore option
(when is-mac
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (global-set-key (kbd "M-a") 'mark-whole-buffer)
  (let ((usr-local "/usr/local/bin"))
    (add-to-list 'exec-path usr-local)
    (setenv "PATH" (concat usr-local path-separator (getenv "PATH")))))

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
(require-package 'ido-completing-read+)
(require-package 'deft)
(require-package 'fill-column-indicator)
(require-package 'smex)
(require-package 'php-mode)
(require-package 'yaml-mode)
(require-package 'ace-jump-mode)
(require-package 'multiple-cursors)
(require-package 'smooth-scrolling)
(require-package 'guide-key)
(require-package 'paredit)
(require-package 'move-text)
(require-package 'magit)
(require-package 'jump-char)
(require-package 'change-inner)
(require-package 'nimbus-theme)
(require-package 'browse-kill-ring)
(require-package 'yascroll)
(require-package 'neotree)
(require-package 'all-the-icons)
(require-package 'projectile)
(require-package 'highlight-indent-guides)
(require-package 'multi-term)
(require-package 'key-chord)
(require-package 's)

;; evil mode
(require-package 'evil)
(require-package 'use-package) ; for evil-surround
(require-package 'evil-surround)
(require-package 'evil-leader)
(require-package 'evil-vimish-fold)
(require-package 'undo-tree)

;; Setting appearance
(require 'appearance)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "grey")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; load configurations
(require 'key-bindings)
(eval-after-load 'ido '(require 'init-ido))
(require 'init-org)
(require 'init-deft)
;(require 'init-clojure)
(require 'init-paredit)
(require 'init-neotree)
(require 'init-projectile)
(require 'init-evil)
(require 'init-term)

;; Enable smooth scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; set programming mode
(defun setup-programming-mode ()
  (fci-mode 1)
  )

(add-hook 'yaml-mode-hook 'prog-mode)
(add-hook 'php-mode-hook 'setup-programming-mode)
(add-hook 'clojure-mode-hook 'setup-programming-mode)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Stop Emacs from Writing package-selected-packages into init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
       (load custom-file))
