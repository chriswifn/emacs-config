;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

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
  :straight (:type built-in)
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

  ;; follow symlinks
  (setq find-file-visit-truename t)

  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)

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

(defun config-reload ()
  "Reload the configuration file"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(use-package electric
  :straight (:type built-in)
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens 
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package general
  :config
  ;; integrate general with evil
  (general-evil-setup)
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
    "SPC" '(execute-extended-command :wk "execute command"))) ;; an alternative to 'M-x'

(chris/leader-keys
  "f" '(:ignore t :wk "file")
  "ff" '(find-file :wk "find file")
  "fz" '(affe-find :wk "fuzzy finder")
  "fg" '(affe-grep :wk "fuzzy finder (grep)")
  "fr" '(consult-recent-file :wk "Recent files")
  "fs" '(save-buffer :wk "Save file")
  "fu" '(sudo-edit-find-file :wk "Sudo find file")
  "fC" '(copy-file :wk "Copy file")
  "fD" '(delete-file :wk "Delete file")
  "fR" '(rename-file :wk "Rename file")
  "fS" '(write-file :wk "Save file as...")
  "fU" '(sudo-edit :wk "Sudo edit file"))

(chris/leader-keys
  "b" '(:ignore t :wk "buffer")
  "bi" '(ibuffer :wk "ibuffer")
  "bb" '(consult-buffer :wk "switch buffer")
  "bf" '(chris/toggle-maximize-buffer :wk "Toggle maximize buffer")
  "bc" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer other window")
  "bk" '(kill-current-buffer :wk "Kill current buffer")
  "bv" '(chris/buffers-vc-root :wk "Buffers in project root") 
  "bm" '(chris/buffers-major-mode :wk "Buffers with same major mode")
  "bn" '(next-buffer :wk "Next buffer")
  "bp" '(previous-buffer :wk "Previous buffer")
  "bB" '(ibuffer-list-buffers :wk "Ibuffer list buffers")
  "br" '(revert-buffer :wk "Revert Buffer")
  "bK" '(kill-buffer :wk "Kill buffer"))

(chris/leader-keys
  "t" '(:ignore t :wk "toggle")
  "tr" '(config-reload :wk "config")
  "tl" '(chris/toggle-line-numbers :wk "linenumbers")
  "tm" '(chris/hide-mode-line-mode :wk "linenumbers")
  "tt" '(modus-themes-toggle :wk "theme"))

(chris/leader-keys
  "o" '(:ignore t :wk "open")
  "ot" '(vterm :wk "vterm")
  "oe" '(eshell :wk "eshell")
  "os" '(fontaine-set-preset :wk "fontaine")
  "ow" '(woman :wk "woman")
  "of" '(chris/olivetti-mode :wk "olivetti")
  "ou" '(undo-tree-visualize :wk "undo-tree")
  "ol" '(org-toggle-link-display :wk "Display org links")
  "oc" '(org-capture :wk "org campture")
  "oo" '(occur "^*+" :wk "org sidebar"))

(chris/leader-keys
"c" '(:ignore t :wk "code")
"cc" '(compile :wk "Compile")
"cd" '(flymake-show-buffer-diagnostics :wk "show lsp diagnostics")
"cm" '(open-matlab-shell :wk "Open Matlab shell"))

(use-package hydra
  :defer t
  :config
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
  :general
  (chris/leader-keys
    "h" '(:ignore t :wk "hydra")
    "hf" '(hydra-text-scale/body :wk "scale text")
    "hs" '(hydra-split-size/body :wk "split size")))

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

(use-package undo-tree
  :after evil
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :init
  (global-undo-tree-mode))

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

(use-package org
  :config
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
  (setq org-default-notes-file (concat org-directory "/notes.org")))

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

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-construct nil
        modus-themes-subtle-line-numbers nil
        modus-themes-fringes '(subtle)
        modus-themes-hl-line '(intense)
        modus-themes-mode-line '(borderless)
        modus-themes-syntax '(faint green-strings alt-syntax)
        modus-themes-headings
        '((1 . (1.3 rainbow))
          (2 . (1.2 rainbow))
          (3 . (1.1 rainbow))
          (t . (1.0 rainbow)))
        modus-themes-org-blocks 'gray-background))
(define-key global-map (kbd "C-c t") #'modus-themes-toggle)
(modus-themes-load-vivendi)

(use-package doom-modeline
  :config
  (setq doom-modeline-height 30
        doom-modeline-icon nil 
        doom-modeline-lsp t)
  :init
  (doom-modeline-mode 1))

(use-package evil-anzu
  :after evil
  :init
  (global-anzu-mode))

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

(use-package dired
  :straight (:type built-in)
  :general
  (chris/leader-keys
    "d" '(:ignore t :wk "dired")
    "dd" '(dired :wk "Open Dired")
    "dj" '(dired-jump :wk "Jump to current directory in dired")))

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
            (chris/turn-on-hide-mode-line-mode))
          (window-divider-mode 1))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (unless (derived-mode-p 'prog-mode)
        (chris/turn-off-hide-mode-line-mode))
      (window-divider-mode -1)
      )))

;; this piece of code is directly copied from Hlissner
;; I attach a prefix to dinstinguish custom functions
(defvar chris/hide-mode-line-format nil
  "The modeline format to use when `chris/hide-mode-line-mode' is active.")

(defvar chris/hide-mode-line-excluded-modes '(fundamental-mode)
  "List of major modes where `chris/global-hide-mode-line-mode' won't affect.")

(defvar-local chris/hide-mode-line--old-format nil
  "Storage for the old `mode-line-format', so it can be restored when
`chris/hide-mode-line-mode' is disabled.")

(define-minor-mode chris/hide-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if chris/hide-mode-line-mode
      (progn
	(add-hook 'after-change-major-mode-hook #'chris/hide-mode-line-mode nil t)
	(unless chris/hide-mode-line--old-format
	  (setq chris/hide-mode-line--old-format mode-line-format))
	(setq mode-line-format chris/hide-mode-line-format))
    (remove-hook 'after-change-major-mode-hook #'chris/hide-mode-line-mode t)
    (setq mode-line-format chris/hide-mode-line--old-format
	  chris/hide-mode-line--old-format nil))
  (when (called-interactively-p 'any)
    (redraw-display)))

;; Ensure major-mode or theme changes don't overwrite these variables
(put 'chris/hide-mode-line--old-format 'permanent-local t)
(put 'chris/hide-mode-line-mode 'permanent-local-hook t)

(define-globalized-minor-mode chris/global-hide-mode-line-mode
  chris/hide-mode-line-mode chris/turn-on-hide-mode-line-mode
  (redraw-display))

(defun chris/turn-on-hide-mode-line-mode ()
  "Turn on `chris/hide-mode-line-mode'.
Unless in `fundamental-mode' or `chris/hide-mode-line-excluded-modes'."
  (unless (memq major-mode chris/hide-mode-line-excluded-modes)
    (chris/hide-mode-line-mode +1)))

(defun chris/turn-off-hide-mode-line-mode ()
  "Turn off `chris/hide-mode-line-mode'."
  (chris/hide-mode-line-mode -1))

(defun chris/buffers-major-mode (&optional arg)
  "Select buffers that match the current buffer's major mode.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion."
  (interactive "P")
  (let* ((major major-mode)
	 (prompt "Buffers for ")
	 (mode-string (format "%s" major))
	 (mode-string-pretty (propertize mode-string 'face 'success)))
    (if arg
	(ibuffer t (concat "*" prompt mode-string "*")
		 (list (cons 'used-mode major)))
      (switch-to-buffer
       (read-buffer
	(concat prompt mode-string-pretty ": ") nil t
	(lambda (pair) ; pair is (name-string . buffer-object)
	  (with-current-buffer (cdr pair) (derived-mode-p major))))))))

(defun chris/buffers-vc-root (&optional arg)
  "Select buffers that match the present `vc-root-dir'.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion.

When no VC root is available, use standard `switch-to-buffer'."
  (interactive "P")
  (let* ((root (vc-root-dir))
         (prompt "Buffers for VC ")
         (vc-string (format "%s" root))
         (vc-string-pretty (propertize vc-string 'face 'success)))
    (if root
        (if arg
            (ibuffer t (concat "*" prompt vc-string "*")
                     (list (cons 'filename (expand-file-name root))))
          (switch-to-buffer
           (read-buffer
            (concat prompt vc-string-pretty ": ") nil t
            (lambda (pair) ; pair is (name-string . buffer-object)
              (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
      (call-interactively 'switch-to-buffer))))

(defun chris/toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_) 
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

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

(use-package tab-bar
  :straight (:type built-in)
  :general
  (chris/leader-keys
    "i" '(:keymap tab-prefix-map :wk "tab")
    "is" '(chris/tab-bar-select-tab-dwim :wk "tab-select"))
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
  (tab-bar-mode -1)
  (tab-bar-history-mode 1))

(defun chris/tab-bar-select-tab-dwim ()
  "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
  (interactive)
  (let ((tabs (mapcar (lambda (tab)
                        (alist-get 'name tab))
                      (tab-bar--tabs-recent))))
    (cond ((eq tabs nil)
           (tab-new))
          ((eq (length tabs) 1)
           (tab-next))
          (t
           (consult-imenu ()
                          (tab-bar-switch-to-tab
                           (completing-read "Select tab: " tabs nil t)))))))

(use-package company
  :config
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
  :commands
  eglot)

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

(use-package async
  :init
  (dired-async-mode 1))

(use-package rg
  :init
  (rg-enable-default-bindings))

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

(use-package haskell-mode)

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

(use-package vterm
  :hook
  (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :init
  (setq vterm-timer-delay 0.01))

(defun chris/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t)
  (setq tramp-default-method "ssh"))

(use-package eshell
  :straight (:type built-in)
  :hook
  (eshell-first-time-mode . chris/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("ssh" "tail" "htop" "pulsemixer" "top" "nvim" "vim"))))

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
            (define-key eshell-mode-map "\C-l" 'my/eshell/clear)))

(defun eshell/mkdir-and-cd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

(use-package rainbow-mode)

(use-package emms
  :general
  (chris/leader-keys
    "m" '(:ignore t :wk "emms")
    "mm" '(emms :wk "emms")
    "mb" '(emms-smart-browse :wk "EMMS Smart Browse")
    "mi" '(emms-show :wk "EMMS show current song")
    "mn" '(emms-next :wk "EMMS next song")
    "mp" '(emms-previous :wk "EMMS previous song")
    "ml" '(emms-seek-forward :wk "EMMS go 10s forward")
    "mt" '(emms-toggle-repeat-track :wk "EMMS toggle repeat")
    "mh" '(emms-seek-backward :wk "EMMS go 10s backward")))
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
  (org-tree-slide-breadcrumbs nil)
  (org-tree-slide-header nil)
  (org-tree-slide-in-effect nil)
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-heading-emphasis nil)
  (org-tree-slide-cursor-init t)
  (org-tree-slide-never-touch-face t)
  :config
  (defun chris/org-presentation ()
    "Specifies conditions that should apply locally upon activation
of `org-tree-slide-mode'."
    (if (eq org-tree-slide-mode nil)
	(progn
          (chris/olivetti-mode -1)
          (fontaine-set-preset 'regular))
      (chris/olivetti-mode)
      (fontaine-set-preset 'presentation)))
  :hook
  (org-tree-slide-mode . chris/org-presentation)
  )
