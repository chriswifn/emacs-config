;;; chris-emacs-org.el -*- lexical-bindings: t; -*-
(require 'org)

(setq org-return-follows-link t
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-pretty-entities t
      org-image-actual-width nil)

(plist-put org-format-latex-options :scale 2.0)

(setq org-agenda-files (list "~/media/org"))

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'before-save-hook 'time-stamp nil)
;;(add-hook 'org-mode-hook #'variable-pitch-mode)

(setq org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
                          (sequence "BLOCKED(b)" "|" "CANCELLED(c)")))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell .t)
                               (js . t)
                               (emacs-lisp .t)
                               (python . t)
                               (clojure . t)
                               (ruby . t)))

(provide 'chris-emacs-org)
