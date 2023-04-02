;;; chris-completion.el --- Completion configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; vertico: vertical completion
(use-package vertico
  :init
  (vertico-mode))

;; savehist: save history
(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

;; orderless: completion style 
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; recentf: save recently edited files
(use-package recentf
  :straight (:type built-in)
  :init
  (recentf-mode 1))

;; consult: search and navigation commands based on completing-read 
(use-package consult
  :general
  (chris/leader-keys
    "fr" '(consult-recent-file :wk "recent file")
    "bb" '(consult-buffer :wk "switch to buffer"))
  :init
  (setq consult-preview-key nil))

(define-key global-map (kbd "C-s") 'consult-line)

(provide 'chris-completion)


