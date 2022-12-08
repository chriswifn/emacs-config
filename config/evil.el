;; undo system
(use-package undo-fu
  :straight t)

(use-package evil
  :straight t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-fine-undo 'fine)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-nerd-commenter
  :straight t
  :config
  (evilnc-default-hotkeys))
