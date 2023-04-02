;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;; disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; gc thrash 
(setq gc-cons-threshold (* 100 1024 1024))

;; disable some gui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-splash-screen t
      use-file-dialog nil)
