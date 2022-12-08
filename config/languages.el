;; python
(use-package python-mode
  :straight t)

;; haskell
(use-package haskell-mode 
  :straight t)

;; lua
(use-package lua-mode 
  :straight t)

;; yaml
(use-package yaml-mode 
  :straight t)

;; emmet (web-dev)
(use-package emmet-mode
  :straight t)

;; php
(use-package php-mode 
  :straight t)

;; matlab
(straight-use-package 'matlab-mode)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command-switches '("-nosplash" "-nodesktop"))
(setq matlab-shell-command "matlab")
