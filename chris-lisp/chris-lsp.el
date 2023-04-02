;;; chris-lsp.el --- Lsp configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; lsp-mode: lsp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :commands (lsp lsp-deferred))

;; dap-mode:
(use-package dap-mode)

;; python
(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
		   (require 'lsp-pyright)
		   (lsp-deferred))))


(provide 'chris-lsp)
