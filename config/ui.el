;; Font
(defvar em/default-font-size 90)
(defvar em/default-variable-font-size 120)

(set-face-attribute 'default nil :font "Monoid" :height em/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Monoid" :height em/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height em/default-variable-font-size :weight 'regular)
(add-to-list 'default-frame-alist '(font . "Monoid-9"))


;; Color theme
(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-italic-construct nil
        modus-themes-subtle-line-numbers nil
        modus-themes-mode-line '(borderless)
        modus-themes-syntax '(yellow-comments faint green-strings)
        modus-themes-org-blocks 'gray-background))
(define-key global-map (kbd "C-c t") #'modus-themes-toggle)

;; load the theme 
(load-theme 'modus-vivendi t)


;; a better modeline
(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-height 26
        doom-modeline-icon t 
        doom-modeline-lsp t)
  (doom-modeline-mode 1))


;; icons
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))


;; some useful output to display in the modeline
(display-battery-mode 1)
(column-number-mode 1)


;; display the current time and date in the minibuffer
(defun display-current-time ()
  (interactive)
  (message (format-time-string "%Y-%m-%d %H:%M:%S")))
(define-key global-map (kbd "<f1>") #'display-current-time)


;; display wifi/eth/vpn status in the minibuffer
(defun display-wifi-status ()
  (interactive)
  (message (shell-command-to-string "sb-internet-emacs")))
(define-key global-map (kbd "<f2>") #'display-wifi-status)


;; beacon - keep track of the cursor
(use-package beacon
  :straight t
  :init
  (beacon-mode 1))

