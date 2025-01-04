;;; chris-programming.el -*- lexical-binding: t; -*-

(use-package direnv
  :init
  (direnv-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (scala "https://github.com/tree-sitter/tree-sitter-scala")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (c "https://github.com/tree-sitter/tree-sitter-c")))

;; run this line once or every time a grammar needs an update
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (go-mode . go-ts-mode)
        ;;(scala-mode . scala-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)))

;; maximum fontification
(setq treesit-font-lock-level 4)

(add-hook 'before-save-hook 'chris/prog-nuke-trailing-whitespace)

(defun chris/prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(defun chris/eglot-organize-imports ()
  (interactive)
  (eglot-code-actions nil nil "source.organizeImports" t))
(add-hook 'before-save-hook 'chris/eglot-organize-imports nil t)
(add-hook 'before-save-hook 'eglot-format-buffer)

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package pyvenv)

(use-package company
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.5)
  :config
  (global-company-mode))

(provide 'chris-programming)
