;;; chris-denote.el -*- lexical-binding: t; -*-

(setq chris-notes-directory "~/media/org/notes/")

(use-package denote
  :init
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic)))))

(setq denote-directory (expand-file-name "~/media/org/notes"))

(provide 'chris-denote)
