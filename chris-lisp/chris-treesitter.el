;;; chris-treesitter.el --- Treesitter configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; tree-sitter languages for treesitter support
(use-package tree-sitter-langs)

;; tree-sitter (syntax parsing sitting in a tree)
(use-package tree-sitter
  :defer t
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; enable tree-sitter globally
  (global-tree-sitter-mode)
  :custom
  ;; no italics (because italics are for maniacs
  (custom-set-faces
   '(italic ((t nil)))
   '(tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face)))))
  :config
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t))

(use-package evil-textobj-tree-sitter)
(define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-outer-text-objects-map "C"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-outer-text-objects-map "a"
    (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

(provide 'chris-treesitter)
