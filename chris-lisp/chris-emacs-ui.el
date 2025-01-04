;;; chris-emacs-ui.el -*- lexical-binding: t; -*-

(defvar chris-fixed-font
  (when window-system
    (or
     (seq-first
      (seq-filter (lambda (font) (when (x-list-fonts font) font))
                  '("UbuntuMono"
                    "Source Code Pro"
                    "monospace")))
     "monospaced"))
  "My fixed width font based on what I have installed.")

(defvar chris-variable-font
  (when window-system
    (or
     (seq-first
      (seq-filter (lambda (font) (when (x-list-fonts font) font))
                  '("Iosevka Comfy Duo"
                    "Sans Serif")))
     (warn "Cannot find a Serif Font. Install one."))))

(defun chris-set-font-size (size)
  "Set the default font size as well as equalize the fixed and
variable fonts."
  (let ((fav-font (format "%s-%d" chris-fixed-font size)))
    (set-face-attribute 'default nil :font fav-font)
    (set-face-attribute 'fixed-pitch nil :family chris-fixed-font :inherit 'default :height 1.0)
    (set-face-attribute 'variable-pitch nil :family chris-variable-font :inherit 'default :height 1.2)))

(defun chris-home-lab ()
  "Quickly set reset my font size when laptop is connected to home lab."
  (interactive)
  (chris-set-font-size 24))

(defun chris-external ()
  "Quickly set reset my font size when laptop is used externally."
  (interactive)
  (chris-set-font-size 12))

(chris-home-lab)

;; (setq modus-themes-mixed-fonts t)
;; (load-theme 'modus-vivendi t)
;; (use-package color-theme-sanityinc-tomorrow)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (load-theme 'doom-gruvbox t))

(use-package spacious-padding
  :init
  (spacious-padding-mode t))

(provide 'chris-emacs-ui)
