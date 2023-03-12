;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
(setq package-enable-at-startup nil)

(setq gc-cons-threshold (* 100 1024 1024))
  (menu-bar-mode -1) ;; disable menubar
  (tool-bar-mode -1) ;; disable toolbar
  (scroll-bar-mode -1) ;; disable scrollbar
  (tooltip-mode -1) ;; disable tooltips

  (setq inhibit-splash-screen t
	use-file-dialog nil)
