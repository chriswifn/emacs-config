;;; chris-lsp.el --- Lsp configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; lsp-mode: lsp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-enable-links nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-ui-doc-enable nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq web-mode-enable-current-element-highlight t)
  (lsp-enable-which-key-integration t)
  :commands
  (lsp lsp-deferred))

;; lsp-ui: ui enhancements
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  :hook
  (lsp-mode . lsp-ui-mode))

;; lsp-treemacs: nicer ui
(use-package lsp-treemacs
  :after lsp)

;; dap-mode:
(use-package dap-mode)

;; python
(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
		   (require 'lsp-pyright)
		   (lsp-deferred))))


(provide 'chris-lsp)
