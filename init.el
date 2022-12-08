;; Straight Package Manager Setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)


;; straight use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; Garbage Collection
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))


;; load my modules
(load "~/.emacs.d/config/ui.el")
(load "~/.emacs.d/config/general.el")
(load "~/.emacs.d/config/evil.el")
(load "~/.emacs.d/config/keybindings.el")
(load "~/.emacs.d/config/packages.el")
(load "~/.emacs.d/config/org.el")
(load "~/.emacs.d/config/development.el")
(load "~/.emacs.d/config/languages.el")
(load "~/.emacs.d/config/ide.el")
