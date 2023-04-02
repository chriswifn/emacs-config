;;; chris-misc.el --- Misc configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; denote: notes
(use-package denote
  :general
  (chris/leader-keys
    "d" '(:ignore :wk "denote")
    "dd" '(denote :wk "denote")
    "do" '(denote-open-or-create :wk "denote open or create"))
  :config
  (setq denote-directory (expand-file-name "~/media/notes"))
  (setq denote-known-keywords '("emacs" "programming" "administration" "linux"))
  :hook
  (dired-mode . denote-dired-mode)
  (dired-mode . dired-hide-details-mode))

;; sudo-edit:
(use-package sudo-edit)

;; simple-httpd
(use-package simple-httpd)

;; rainbow-mode:
(use-package rainbow-mode)

;; emms: music player 
(use-package emms)

(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-mode-line 0)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/media/music/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

(provide 'chris-misc)
