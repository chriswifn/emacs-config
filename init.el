;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; bootstrap straight.el and install use-package
(setq straight-use-package-by-default t)
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
(straight-use-package 'use-package)

;; setting some sane defaults
(setq user-full-name "Christian Hageloch")
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq indent-tabs-mode nil)
(setq blink-cursor-mode nil)

;; file related settings 
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq find-file-visit-truename t)
(set-default-coding-systems 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)

;; line editing
(setq display-line-numbers-type 'relative)
(global-hl-line-mode 1)

;; reload the configuration
(defun chris/config-reload()
  "Reload the configuration file"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(define-key global-map (kbd "C-c r") 'chris/config-reload)

;; custom and abbrev file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(setq abbrev-file-name (locate-user-emacs-file "abbrev.el"))

;; custom configuration for more advanced packages
(add-to-list 'load-path "~/.emacs.d/chris-lisp")
(require 'chris-general)
(require 'chris-evil)
(require 'chris-buffers)
(require 'chris-dired)
(require 'chris-ui)
(require 'chris-tabbar)
(require 'chris-nmcli)
(require 'chris-completion)
(require 'chris-org)
(require 'chris-lang)
(require 'chris-term)
(require 'chris-treesitter)
(require 'chris-git)
(require 'chris-lsp)
(require 'chris-textcompletion)
(require 'chris-misc)
