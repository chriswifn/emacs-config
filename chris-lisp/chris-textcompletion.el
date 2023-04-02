;;; chris-textcompletion.el --- Text completion configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; company: completion
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :init
  (global-company-mode))

(provide 'chris-textcompletion)
