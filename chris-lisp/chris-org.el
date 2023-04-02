;;; chris-org.el --- Org configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package org
  :straight (:type built-in)
  :config
  ;; org source code block language configuration
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (latex . t)
       (shell . t)))
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (setq org-edit-src-content-indentation 0)
  (setq org-directory "~/org")
  (setq org-hide-emphasis-markers t)
  (setq org-default-notes-file (concat org-directory "/notes.org")))

;; org-agenda
(setq org-agenda-files '("~/org/Agenda.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-done 'time)

(provide 'chris-org)
