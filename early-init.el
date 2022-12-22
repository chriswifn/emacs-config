;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
(setq gc-cons-threshold (* 50 1000 1000))
(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-splash-screen t
      use-file-dialog nil)
;;; early-init.el ends here
