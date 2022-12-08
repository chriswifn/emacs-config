;; some basic settings
(setq org-ellipsis " ")
(setq orc-src-fontify-natively t)
(setq src-tab-acts-natively t)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-fontify-whole-block-delimiter-line t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'org-indent-mode)


;; configure babel languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))
