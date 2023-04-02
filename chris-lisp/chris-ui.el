;;; chris-ui.el --- Ui configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; modus-themes: some nice looking dark and light themes
(use-package modus-themes
  :general
  (chris/leader-keys
    "t" '(:ignore :wk "theme")
    "tt" '(modus-themes-toggle :wk "toggle theme"))
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-construct nil
	modus-themes-headings
	'((1 . (1.5))
	  (2 . (1.4))
	  (3 . (1.3))
	  (4 . (1.2))
	  (t . (1.1)))
        modus-themes-common-palette-overrides
        '(
	  (comment yellow-faint)
	  (string green-faint)
	  (prose-done green-faint)
	  (prose-todo red-faint)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified))))

;; load the theme based on the theme of the system
(if (string-match
     "modus-vivendi"
     (shell-command-to-string "cat ~/.config/sway/active-theme"))
    (load-theme 'modus-vivendi)
  (load-theme 'modus-operandi))

;; fontaine: font configuration
(use-package fontaine
  :general
  (chris/leader-keys
    "tf" '(fontaine-set-preset :wk "fontaine set preset")
    "tF" '(fontaine-set-face-font :wk "fontaine set font"))
  :config
  (setq fontaine-presets
        '((tiny
           :default-family "Monoid"
           :default-height 70)
          (small
           :default-family "Monoid"
           :default-height 80)
          (regular
           :default-height 90)
          (medium
           :default-height 120)
          (large
           :default-weight semilight
           :default-height 140
           :bold-weight extrabold)
          (t
           :default-family "Monoid"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Monoid"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil))))

;; Recover last preset or fall back to desired style from
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; doom-modeline: a mode-line that gets out of your way
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(provide 'chris-ui)
