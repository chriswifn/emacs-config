;; Encoding
(setq locale-encoding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Dialog
(setq use-dialog-box nil)
(setq use-file-dialog nil)


;; Disable backups and auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)


;; Change yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)


;; Line numbers and hl-line
(setq display-line-numbers-type 'relative)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode 1)
(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(dolist (mode '(treemacs-mode-hook
		vterm-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))


;; follow symlinks
(setq find-file-visit-truename t)


;; Appearance

;; Remove startup screen
(setq inhibit-startup-message t)

;; Fringes
(set-fringe-mode 0)

;; Disable menus and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable blinking cursor
(blink-cursor-mode -1)


;; Auto complete brackets and "
(setq electric-pair-pairs '(
			    (?\{ . ?\})
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\" . ?\")
			    ))
(electric-pair-mode t)

