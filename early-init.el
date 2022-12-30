;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
(setq gc-cons-threshold (* 50 1000 1000))
(setq package-enable-at-startup nil)

(defun chris/toggle-gui-elements (&optional on-off)
  "Toggle menu bar, tool bar, scroll bars, and tool tip modes. If
  optional ON-OFF is not specified, then toggle on/off state. If
  ON-OFF is 0 or 1, then turn gui elements OFF or ON respectively."
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode (or on-off (not scroll-bar-mode))))
  (when  (fboundp 'tool-bar-mode)
    (tool-bar-mode (or on-off (not tool-bar-mode))))
  (unless (memq (window-system) '(mac ns))
    (when (fboundp 'menu-bar-mode)
      (menu-bar-mode (or on-off (not menu-bar-mode)))))
  ;; tooltips in echo-aera
  (when (fboundp 'tooltip-mode)
    (tooltip-mode (or on-off (not tooltip-mode)))))

(chris/toggle-gui-elements 0)

(setq inhibit-splash-screen t
      use-file-dialog nil)
