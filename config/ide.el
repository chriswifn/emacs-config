;; THIS IS WHERE I INSTALL "IDE-LIKE" PACKAGES


;; rip-grep
(use-package rg
  :straight t
  :init
  (rg-enable-default-bindings))


;; async
(use-package async
  :straight t
  :init
  (dired-async-mode 1))


;; vterm - the best terminal emulator expirience for emacs
(use-package vterm
  :straight t
  :init
  (setq vterm-timer-delay 0.01))


;; projectile and projectile-ibuffer
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-modes "org-mode"))
(setq projectile-indexing-method 'hybrid)

(use-package ibuffer-projectile
  :straight t
  :config 
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


;; magit
(use-package magit
  :straight t
  :config
  (setq magit-push-always-verify nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-repository-directories
        '(("~/.local/src"  . 2)
          ("~/.config/" . 2)))
  (setq git-commit-summary-max-length 50)
  :bind
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-list-repositories))


;; perspective
(use-package perspective
  :straight t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))  ; pick your own prefix key here
  :init
  (persp-mode))


;; rainbow mode - color in hex codes
(use-package rainbow-mode
  :straight t)
