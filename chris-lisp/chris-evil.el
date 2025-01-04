;;; chris-evil.el -*- lexical-binding: t; -*-

(use-package evil
  :init
  (setq evil-search-module 'isearch)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo) ;; Emacs 28+
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; C-d: center cursor after jump
(defun chris/scroll-down-and-center ()
  "Scroll down and center the text to the screen"
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center (line-number-at-pos)))
(define-key evil-motion-state-map (kbd "\C-d") 'chris/scroll-down-and-center)

;; C-u: center cursor after jump
(defun chris/scroll-up-and-center ()
  "Scroll up and center the text to the screen"
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center (line-number-at-pos)))
(define-key evil-motion-state-map (kbd "\C-u") 'chris/scroll-up-and-center)

(provide 'chris-evil)
