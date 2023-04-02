;;; chris-lang.el --- Lang configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))

(defun haskell-evil-open-above ()
  (interactive)
  (evil-beginning-of-line)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

(defun haskell-evil-open-below ()
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))

(evil-define-key 'normal haskell-mode-map
  "o" 'haskell-evil-open-below
  "O" 'haskell-evil-open-above)

;; lua
(use-package lua-mode
  :mode ("\\.lua\\'". lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (defun chris/open-lua-repl ()
    "open lua repl in horizontal split"
    (interactive)
    (lua-show-process-buffer))
  :init
  (setq lua-indent-level 4
	lua-indent-string-contents t))

;; python
(use-package python-mode
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq python-indent 4))

;; racket
(use-package racket-mode
  :interpreter ("racket" . racket-mode))

(defun chris/racket-run-and-switch-to-repl ()
  "Call `racket-run-and-switch-to-repl' and enable insert state"
  (interactive)
  (racket-run-and-switch-to-repl)
  (when (buffer-live-p (get-buffer racket-repl-buffer-name))
    (with-current-buffer racket-repl-buffer-name
      (evil-insert-state))))

;; markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; matlab
(straight-use-package 'matlab-mode)

(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command-switches '("-nosplash" "-nodesktop"))
(setq matlab-shell-command "matlab")

(defun chris/matlab-shell-run-buffer ()
  "Run matlab code"
  (interactive)
  (matlab-shell-run-command (concat "cd " default-directory))
  (matlab-shell-run-region (point-min) (point-max)))

(provide 'chris-lang)
