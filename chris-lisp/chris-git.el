;;; chris-git.el --- Git and Project configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; projectile: manage projects
(use-package projectile
  :general
  (chris/leader-keys
    "p" '(:keymap projectile-command-map :wk "projectile"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-modes "org-mode")
  (setq projectile-indexing-method 'hybrid)
  :init
  (projectile-mode +1))

;; ibuffer-projectile: sort ibuffer according to projects
(use-package ibuffer-projectile
  :config 
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; sort ibuffer according to projects
;; keeps ibuffer organized
(use-package ibuffer-projectile
  :config 
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; magit: git integration
(use-package magit
  :general
  (chris/leader-keys
    "g" '(:ignore :wk "git")
    "gg" '(magit-status :wk "status")
    "gG" '(magit-list-repositories :wk "list repositories"))
  :config
  (setq magit-push-always-verify nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-repository-directories
        '(("~/code"  . 2)
          ("~/" . 2)))
  (setq git-commit-summary-max-length 50)
  :bind
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-list-repositories))

(provide 'chris-git)
