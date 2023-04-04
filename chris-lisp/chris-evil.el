;;; chris-evil.el --- Evil configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; evil-mode: superior editing expirience 
(use-package evil
  :general
  (chris/leader-keys
    "w" '(:keymap evil-window-map :wk "window"))
  :init
  (setq evil-search-module 'isearch)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  ;; for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; splits
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right nil)
  ;; C-i jump
  (setq evil-want-C-i-jump nil)
  ;; undo
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode t))

;; evil-collection: for non text editing buffers
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

;; evil-commentary: quick commenting
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
