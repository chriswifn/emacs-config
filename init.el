;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is generated from readme.org.

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
(straight-use-package 'org)

(use-package emacs
  :config
  (setq user-full-name "Christian Hageloch")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  (setq indent-tabs-mode nil) ;; no tabs

  (setq make-backup-files nil) ;; keep everything under vc 
  (setq auto-save-default nil)

  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; no need to create lockfiles

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (setq display-line-numbers-type 'relative)

  (column-number-mode)

  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (global-hl-line-mode t)

  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

  ;; follow symlinks
  (setq find-file-visit-truename t)

  ;; encoding
  (set-charset-priority 'unicode) ;; utf8 in every nook and cranny
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

;; function to reload config
(defun config-reload ()
  "Uncle dev created a function to reload Emacs config."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c r") 'config-reload)

(use-package electric
  :straight (:type built-in)
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens 
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package general
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer chris/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer chris/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"	;; unbind find file read only
    "C-x C-z"	;; unbind suspend frame
    "C-x C-d"	;; unbind list directory
    "<mouse-2>") ;; pasting with mouse wheel click

  (chris/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  ;; file
  (chris/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file")
    "fz" '(affe-find :wk "fuzzy finder")
    "fg" '(affe-grep :wk "fuzzy finder (grep)")
    "fr" '(consult-recent-file :wk "Recent files")
    "fs" '(save-buffer :wk "Save file")
    "fu" '(sudo-edit-find-file :wk "Sudo find file")
    "fy" '(dt/show-and-copy-buffer-path :wk "Yank file path")
    "fC" '(copy-file :wk "Copy file")
    "fD" '(delete-file :wk "Delete file")
    "fR" '(rename-file :wk "Rename file")
    "fS" '(write-file :wk "Save file as...")
    "fU" '(sudo-edit :wk "Sudo edit file"))

  ;; buffer 
  (chris/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bi" '(persp-ibuffer :wk "ibuffer perspective")
    "bI" '(ibuffer :wk "ibuffer")
    "bb" '(consult-buffer :wk "switch buffer")
    "bf" '(toggle-maximize-buffer :wk "Toggle maximize buffer")
    "bc" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer other window")
    "bk" '(kill-current-buffer :wk "Kill current buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "bB" '(ibuffer-list-buffers :wk "Ibuffer list buffers")
    "br" '(revert-buffer :wk "Revert Buffer")
    "bK" '(kill-buffer :wk "Kill buffer"))

  ;; dired
  (chris/leader-keys
    "d" '(:ignore t :wk "dired")
    "dd" '(dired :wk "Open Dired")
    "dj" '(dired-jump :wk "Jump to current directory in dired"))

  ;; code
  ;; see 'flymake'
  (chris/leader-keys
    "c" '(:ignore t :wk "code")
    "cc" '(compile :wk "Compile")
    "cd" '(flymake-show-buffer-diagnostics :wk "show lsp diagnostics")
    "cm" '(open-matlab-shell :wk "Open Matlab shell"))

  ;; hydra
  (chris/leader-keys
    "h" '(:ignore t :wk "hydra")
    "hf" '(hydra-text-scale/body :wk "scale text")
    "hs" '(hydra-split-size/body :wk "split size"))

  ;; emms
  (chris/leader-keys
    "m" '(:ignore t :wk "emms")
    "mm" '(emms :wk "emms")
    "mb" '(emms-smart-browse :wk "EMMS Smart Browse")
    "mi" '(emms-show :wk "EMMS show current song")
    "mn" '(emms-next :wk "EMMS next song")
    "mp" '(emms-previous :wk "EMMS previous song")
    "ml" '(emms-seek-forward :wk "EMMS go 10s forward")
    "mt" '(emms-toggle-repeat-track :wk "EMMS toggle repeat")
    "mh" '(emms-seek-backward :wk "EMMS go 10s backward"))

  ;; toggle dis
  (chris/leader-keys
    "t" '(:ignore t :wk "toggle")
    "tr" '(config-reload :wk "config")
    "tl" '(chris/toggle-line-numbers :wk "linenumbers")
    "tt" '(modus-themes-toggle :wk "theme"))

  ;; open
  (chris/leader-keys
    "o" '(:ignore t :wk "open")
    "ot" '(vterm :wk "vterm")
    "oe" '(eshell :wk "eshell")
    "of" '(fontaine-set-preset :wk "fontaine")
    "ow" '(woman :wk "woman")
    "of" '(chris/olivetti-mode :wk "olivetti")
    "ou" '(undo-tree-visualize :wk "undo-tree")
    "ol" '(org-toggle-link-display :wk "Display org links")
    "oc" '(org-capture :wk "org campture")
    "oo" '(occur "^*+" :wk "org sidebar")))

(use-package evil
  :general
  (chris/leader-keys
    "w" '(:keymap evil-window-map :wk "window")) ;; window bindings
  :init
  (setq evil-search-module 'isearch)

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-tree) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode t) ;; globally enable evil mode
  ;; set the initial state for some kinds of buffers.
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

(use-package evil-collection ;; evilifies a bunch of things
    :after evil
    :init
    (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
    ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
    ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
    ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
    ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
    :config
    (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode)) ;; globally enable evil-commentary

(use-package link-hint
  :general
  (chris/leader-keys
    "l" '(link-hint-open-link :wk "open link"))
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
  (setq link-hint-avy-style 'pre))

(defun scroll-down-and-center ()
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(define-key evil-motion-state-map "\C-d" 'scroll-down-and-center)

(defun scroll-up-and-center ()
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(define-key evil-motion-state-map "\C-u" 'scroll-up-and-center)

(use-package which-key
  :after evil
  :init (which-key-mode)
  :config
  (which-key-setup-minibuffer))

(use-package fontaine
  :config
  (setq x-underline-at-descent-line t)
  (setq-default text-scale-remap-header-line t)
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  :init
  (setq fontaine-presets
        '((tiny
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 70)
          (small
           :default-family "Iosevka Comfy Fixed"
           :default-height 90)
          (regular
           :default-height 110)
          (medium
           :default-height 120)
          (large
           :default-weight semilight
           :default-height 140
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy Wide"
           :default-weight regular
           :default-height 110 
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil))))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; some useful output to display in the modeline
(display-battery-mode 1)
(column-number-mode 1)


;; display the current time and date in the minibuffer
(defun display-current-time ()
  (interactive)
  (message (format-time-string "%Y-%m-%d %H:%M:%S")))
(define-key global-map (kbd "<f1>") #'display-current-time)


;; display wifi/eth/vpn status in the minibuffer
(defun display-wifi-status ()
  (interactive)
  (message (shell-command-to-string "sb-internet-emacs")))
(define-key global-map (kbd "<f2>") #'display-wifi-status)

(use-package beacon
  :init
  (beacon-mode 1))

(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-italic-construct nil
        modus-themes-subtle-line-numbers nil
        modus-themes-hl-line '(intense)
        modus-themes-mode-line '(borderless)
        modus-themes-syntax '(faint green-strings alt-syntax)
        modus-themes-headings
        '((1 . (1.6 rainbow))
          (2 . (1.4 rainbow))
          (3 . (1.2 rainbow))
          (t . (1.0 rainbow)))
        modus-themes-org-blocks 'gray-background))
(define-key global-map (kbd "C-c t") #'modus-themes-toggle)
(modus-themes-load-vivendi)

(use-package doom-modeline
  :init
  (setq doom-modeline-height 30
        doom-modeline-icon 1
        doom-modeline-lsp t)
  (doom-modeline-mode 1))

(defun chris/toggle-line-numbers ()
  "Toggles the display of line numbers. Applies locally to the current buffer"
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1)
    (display-line-numbers-mode)))

(use-package vertico
  :init
  (vertico-mode +1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((fie (styles partial-completion)))))

(use-package consult
  :init
  (setq consult-preview-key "$")
  :bind ("C-s" . 'consult-line))

(recentf-mode +1)

(use-package affe
  :after orderless
  :init
  (setq affe-regexp-function #'orderless-pattern-compiler
	affe-highlight-function #'orderless-highlight-matches)
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; anzu
(use-package evil-anzu
  :after evil
  :init
  (global-anzu-mode))

(use-package sudo-edit)

(use-package 0x0
  :general
  (chris/leader-keys
    "x" '(:ignore t :wk "web")
    "x;" '(0x0-dwim t :wk "0x0 dwim")
    "xt" '(0x0-upload-text :wk "0x0 upload text")
    "xf" '(0x0-upload-file :wk "0x0 upload file")
    "xk" '(0x0-upload-kill-ring :wk "0x0 upload kill ring")
    "xp" '(0x0-popup :wk "0x0 popup")
    "xs" '(0x0-shorten-uri :wk "0x0 shorten url")))

(use-package projectile
  :general
  (chris/leader-keys "p" '(:keymap projectile-command-map :wk "projectile"))
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-modes "org-mode"))
(setq projectile-indexing-method 'hybrid)

(use-package ibuffer-projectile
  :config 
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package perspective
  :general
  (chris/leader-keys
    "i" '(:keymap perspective-map :wk "perspective"))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p")) 
  :init
  (persp-mode))

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
	   (jump-to-register '_) 
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows))))

(use-package hydra
  :defer t)

;; scale text
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; split size
(defhydra hydra-split-size (:timeout 4)
  "increase/decrease split size"
  ("h" shrink-window-horizontally)
  ("j" enlarge-window)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("n" balance-windows)
  ("f" nil "finished" :exit t))

(use-package openwith
  :config
  (setq openwith-associations
	(list
	 (list (openwith-make-extension-regexp
		'("xbm" "pbm" "pgm" "ppm" "pnm"
		  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
	       "sxiv"
	       '(file))
	 (list (openwith-make-extension-regexp
		'("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
	       "libreoffice"
	       '(file))
	 '("\\.lyx" "lyx" (file))
	 '("\\.chm" "kchmviewer" (file))
	 (list (openwith-make-extension-regexp
		'("pdf" "ps" "ps.gz" "dvi"))
	       "zathura"
	       '(file))
	 ))
  (openwith-mode 1))

(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
			       "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
				 "Juni" "Juli" "August" "September"
				 "Oktober" "November" "Dezember"])
(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

;; Feiertage für Bayern, weitere auskommentiert
(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        ;; (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        ;; (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(setq calendar-holidays holiday-christian-holidays)

(use-package olivetti
  :config
  (setq olivetti-body-width 0.65)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  ;; hide/show the mode-line
  (define-minor-mode chris/hidden-mode-line-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if chris/hidden-mode-line-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update)))

  (define-minor-mode chris/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.
Fringes are disabled.  The modeline is hidden, except for
`prog-mode' buffers (see `chris/hidden-mode-line-mode')."
    :init-value nil
    :global nil
    (if chris/olivetti-mode
        (progn
          (olivetti-mode 1)
	  (olivetti-set-width 120)
          (set-window-fringes (selected-window) 0 0)
          (unless (derived-mode-p 'prog-mode)
            (chris/hidden-mode-line-mode 1))
          (window-divider-mode 1))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (unless (derived-mode-p 'prog-mode)
        (chris/hidden-mode-line-mode -1))
      (window-divider-mode -1)
      )))

(use-package engine-mode
  :general
  (chris/leader-keys
    "e" '(:keymap engine-mode-prefixed-map :wk "engine-mode"))
  :config

  ;; duckduckgo search engine
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  ;; google search engine
  (defengine google
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  ;; wikipedia search engine
  (defengine wikipedia
    "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  ;; stack-overflow search engine
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  )

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :init
  (global-undo-tree-mode))

(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minium-prefix-length 3))

(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 1))
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-auto-odd-face-perc 30)
  (setq highlight-indent-guides-auto-even-face-perc 35)
  (setq highlight-indent-guides-auto-character-face-perc 40)
  (setq highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package eglot
  :commands eglot)

(use-package tree-sitter-langs)

(use-package tree-sitter
  :defer t
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode)
  :custom
  (custom-set-faces
   '(italic ((t nil)))
   '(tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face)))))
  :config
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t))

(use-package evil-textobj-tree-sitter
  :straight t
  :init
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

(setq org-ellipsis " ")
(setq orc-src-fontify-natively t)
(setq src-tab-acts-natively t)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-fontify-whole-block-delimiter-line t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)
(setq org-hide-emphasis-markers t)
(setq org-src-preserve-indentation 1)
(setq org-edit-src-content-indentation 0)
(setq org-log-done t)

;; configure babel languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/orgroam")
  (org-roam-compeltion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :general
  (chris/leader-keys
    "r" '(:ignore t :wk "org-roam")
    "rt" '(org-roam-buffer-toggle :wk "toggle org-roam buffer")
    "rf" '(org-roam-node-find :wk "find node")
    "ri" '(org-roam-node-insert :wk "insert node"))
  (chris/leader-keys "rd" '(:keymap org-roam-dailies-map :wk "dailies"))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(use-package haskell-mode)

;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython"))

(use-package lua-mode)

(use-package yaml-mode)

(use-package emmet-mode)

(use-package php-mode)

(straight-use-package 'matlab-mode)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command-switches '("-nosplash" "-nodesktop"))
(setq matlab-shell-command "matlab")

(defun open-matlab-shell ()
  (interactive)
  (split-window-below 40)
  (other-window 1)
  (matlab-shell))

(use-package magit
  :general
  (chris/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status")
    "gG" '(magit-list-repositories :wk "list repos"))
  :config
  (setq magit-push-always-verify nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-repository-directories
        '(("~/.local/src"  . 2)
          ("~/.config/" . 2)))
  (setq git-commit-summary-max-length 50)
  :bind
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-list-repositories))

(use-package vterm
  :init
  (setq vterm-timer-delay 0.01))

(use-package exec-path-from-shell)

(use-package eshell
  :straight (:type built-in)
  :init
  (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
   eshell-scroll-to-bottom-on-input 'all
   eshell-error-if-no-glob t
   eshell-hist-ignoredups t
   eshell-save-history-on-exit t
   eshell-prefer-lisp-functions nil
   eshell-destroy-buffer-when-process-dies t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "htop")
              (add-to-list 'eshell-visual-commands "pulsemixer")
              (add-to-list 'eshell-visual-commands "top"))))

(add-hook 'eshell-mode-hook (lambda ()
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "ff" "find-file $1")
                              (eshell/alias "emacs" "find-file $1")
                              (eshell/alias "ee" "find-file-other-window $1")
                              (eshell/alias "gd" "magit-diff-unstaged")
                              (eshell/alias "gds" "magit-diff-staged")
                              (eshell/alias "d" "dired $1")

                              ;; The 'ls' executable requires the Gnu version on the Mac
                              ;; use exa because it looks nicer
                              (let ((ls-temp (if (file-exists-p "/usr/bin/exa")
                                                 "/usr/bin/exa"
                                               "/bin/ls")))
                                (eshell/alias "ls" (concat ls-temp " -al --color=always --group-directories-first")))))
(setq tramp-default-method "ssh")


(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))   ;; The echo command suppresses output

(defun eshell/find (&rest args)
  "Wrapper around the ‘find’ executable."
  (let ((cmd (concat "find " (string-join args))))
    (shell-command-to-string cmd)))

(defun my/eshell/clear ()
  "Clear `eshell' buffer.

   Similar to the behavior of `comint-clear-buffer' in `shell'."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear-scrollback)
    (eshell-emit-prompt)
    (insert input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map "\C-c\ \M-o" 'my/eshell/clear)))

(defun eshell/mkdir-and-cd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

(use-package rainbow-mode)

(use-package rg
  :init
  (rg-enable-default-bindings))

(use-package async
  :init
  (dired-async-mode 1))

(use-package emms)
(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-mode-line 0)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
