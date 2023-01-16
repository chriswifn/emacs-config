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

(use-package emacs
  :straight (:type built-in)
  :config
  (setq user-full-name "Christian Hageloch")
  (setq use-short-answers t)
  (setq indent-tabs-mode nil)
  (setq blink-cursor-mode nil)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq backup-directory-alist
	`((".*" . ,(concat user-emacs-directory "backups")))
	auto-save-file-name-transforms
	`((".*" ,(concat user-emacs-directory "backups") t)))
  (setq create-lockfiles nil)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (setq display-line-numbers-type 'relative)
  (column-number-mode)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (setq find-file-visit-truename t)
  (set-default-coding-systems 'utf-8)
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(defun chris/config-reload ()
  "Reload the configuration file"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(use-package electric
  :straight (:type built-in)
  :config
  (setq electric-pair-pairs '(
			     (?\{ . ?\})
			     (?\( . ?\))
			     (?\[ . ?\])
			     (?\" . ?\")
			     ))
  :init
  (electric-pair-mode t))

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
    "SPC" '(counsel-M-x :wk "M-x"))) ;; an alternative to 'M-x'

(chris/leader-keys
  "f" '(:ignore t :wk "file")
  "ff" '(counsel-find-file :wk "find file")
  "fz" '(counsel-fzf :wk "fuzzy finder")
  "fg" '(counsel-grep :wk "fuzzy finder (grep)")
  "fr" '(counsel-recentf :wk "Recent files")
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
  "bb" '(counsel-switch-buffer :wk "switch buffer")
  "bf" '(chris/toggle-maximize-buffer :wk "Toggle maximize buffer")
  "bc" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer other window")
  "bk" '(kill-current-buffer :wk "Kill current buffer")
  "bv" '(chris/buffers-vc-root :wk "Buffers in project root") 
  "bm" '(chris/buffers-major-mode :wk "Buffers with same major mode")
  "bn" '(next-buffer :wk "Next buffer")
  "bp" '(previous-buffer :wk "Previous buffer")
  "bB" '(ibuffer-list-buffers :wk "Ibuffer list buffers")
  "br" '(revert-buffer :wk "Revert Buffer")
  "bs" '(chris/simple-scratch-buffer :wk "Revert Buffer")
  "bK" '(chris/kill-buffer-and-close-split :wk "Kill buffer"))

(chris/leader-keys
  "t"  '(:ignore t :wk "toggle")
  "tr" '(chris/config-reload :wk "config")
  "tl" '(chris/toggle-line-numbers :wk "linenumbers")
  "tm" '(chris/hide-mode-line-mode :wk "linenumbers")
  "ts" '(chris/tab-status-line :wk "tab-bar-line")
  "tt" '(modus-themes-toggle :wk "theme")
  "tc" '(chris/toggle-code :wk "code"))

(chris/leader-keys
  "o" '(:ignore t :wk "open")
  "ot" '(vterm :wk "vterm")
  "oe" '(eshell :wk "eshell")
  "op" '(list-processes :wk "get a list of processes")
  "os" '(fontaine-set-preset :wk "fontaine")
  "ow" '(woman :wk "woman")
  "of" '(chris/olivetti-mode :wk "olivetti")
  "ol" '(org-toggle-link-display :wk "Display org links")
  "oc" '(org-capture :wk "org campture")
  "oa" '(org-agenda :wk "org campture")
  "oo" '(occur "^*+" :wk "org sidebar")
  "ob" '(bluetooth-list-devices :wk "List bluetooth devices")
  "oi" '(chris/nmcli-wifi-preexist :wk "internet preexisting")
  "oI" '(chris/nmcli-wifi :wk "Connect wifi")
  )

(chris/leader-keys
  "c" '(:ignore t :wk "code-action")
  "cc" '(compile :wk "Compile"))

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
  (setq evil-vsplit-window-right nil)

  ;; cursors
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-motion-state-cursor 'box)
  (setq evil-replace-state-cursor 'box)
  (setq evil-operator-state-cursor 'box)

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
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

(defun chris/scroll-down-and-center ()
"Scroll down and center the text to the screen"
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(define-key evil-motion-state-map "\C-d" 'chris/scroll-down-and-center)

(defun chris/scroll-up-and-center ()
"Scroll up and center the text to the screen"
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(define-key evil-motion-state-map "\C-u" 'chris/scroll-up-and-center)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-minibuffer))

(use-package org
  :straight (:type built-in)
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

  ;; configure babel languages
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)))

    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org")))

(setq org-agenda-files '("~/org/Agenda.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-done 'time)

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
      '((regular
         :default-height 110)
        (medium
         :default-weight semilight
         :default-height 140)
        (large
         :default-weight semilight
         :default-height 180
         :bold-weight extrabold)
        (t ; our shared fallback properties
         :default-family "Iosevka Comfy Wide Fixed"
         :default-weight normal
         ;; :default-height 100
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Iosevka Comfy Duo"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil))))

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-construct nil
        modus-themes-common-palette-overrides
        '(
          ;; (border-mode-line-active unspecified)
          ;; (border-mode-line-inactive unspecified)
	  ;; (bg-mode-line-active bg-blue-subtle)
	  ;; (fg-mode-line-active fg-main)
	  (prose-done green-faint)
	  (prose-todo red-faint)
          (fringe unspecified))
        modus-themes-headings
        '((1 . (1.3))
          (2 . (1.2))
          (3 . (1.1))
          (t . (1.0)))))

;; (defun chris/modus-themes-custom-faces ()
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      ;; Add "padding" to the mode lines
;;      `(mode-line ((,c :underline ,border-mode-line-active
;;                       :overline ,border-mode-line-active
;;                       :box (:line-width 4 :color ,bg-mode-line-active))))
;;      `(mode-line-inactive ((,c :underline ,border-mode-line-inactive
;;                                :overline ,border-mode-line-inactive
;;                                :box (:line-width 4 :color ,bg-mode-line-inactive)))))))

;; ESSENTIAL to make the underline move to the bottom of the box:
;; (setq x-underline-at-descent-line t)

;; (add-hook 'modus-themes-after-load-theme-hook #'chris/modus-themes-custom-faces)

(if (string-match
     "modus-vivendi"
     ;; (shell-command-to-string "cat ~/.config/awesome/theme/local_theme"))
     (shell-command-to-string "cat ~/.config/herbstluftwm/active-theme"))
    (modus-themes-load-theme 'modus-vivendi)
  (modus-themes-load-theme 'modus-operandi))

(use-package minions
  :config
  (setq minions-mode-line-ligher ";")
  (setq minions-prominent-modes
	(list 'flymake-mode
	      'lsp-mode))
  :init 
  (minions-mode 1))
(display-battery-mode)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format 1)
(display-time-mode 1)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package all-the-icons)

(use-package all-the-icons-ibuffer
  :after all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ivy
  :bind
  ("C-s" . swiper)
  :init
  (ivy-mode))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x))

(use-package savehist
  :straight (:type built-in)
  :config
  (setq history-length 25)
  :init
  (savehist-mode))

(use-package dired
  :straight (:type built-in)
  :general
  (chris/leader-keys
    "d" '(:ignore t :wk "dired")
    "dd" '(dired :wk "Open Dired")
    "dj" '(dired-jump :wk "Jump to current directory in dired"))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

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

(use-package sudo-edit)

(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("doc" "xls" "xlsx" "ppt" "odt" "ods" "odg" "odp"))
               "$HOME/.local/share/applications/LibreOffice-still.basic-x86_64.AppImage"
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
          (olivetti-set-width 80)
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

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
	 ("M-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :general
  (chris/leader-keys
    "u" '(:ignore t :wk "popper")
    "ut" '(popper-toggle-latest :wk "toggle latest")
    "uc" '(popper-cycle :wk "cycle")
    "up" '(popper-toggle-type :wk "toggle type (promote)"))
  :init
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  "^\\*MATLAB\\*$"
	  "^\\*Racket REPL.*\\*$"
	  "^\\*lua\\*$"
	  "^\\*Python\\*$"
	  "^\\*Process List\\*$"
	  help-mode
	  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

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

(defun chris/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_) 
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun chris/kill-buffer-and-close-split ()
  "Kill buffer and close split"
  (interactive)
  (kill-current-buffer)
  (evil-window-delete))

(defun chris/kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;;; Generic setup
(defun chris/simple--scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           when (and (functionp sym)
                     (provided-mode-derived-p sym 'prog-mode))
           collect sym))

(defun chris/simple--scratch-buffer-setup (region &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
         (string (format "Scratch buffer for: %s\n\n" major))
         (text (concat string region))
         (buf (format "*Scratch for %s*" major)))
    (with-current-buffer (get-buffer-create buf)
      (funcall major)
      (save-excursion
        (insert text)
        (goto-char (point-min))
        (comment-region (point-at-bol) (point-at-eol))))
    (switch-to-buffer buf)))

(defun chris/simple-scratch-buffer (&optional arg)
  "Produce a bespoke scratch buffer matching current major mode.

If the major-mode is not derived from 'prog-mode, it prompts for
a list of all derived prog-modes AND org-mode

If region is active, copy its contents to the new scratch
buffer."
  (interactive "P")
  (let* ((modes (chris/simple--scratch-list-modes))
         (region (with-current-buffer (current-buffer)
                   (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     "")))
         (m))
    (if (derived-mode-p 'prog-mode)
        (chris/simple--scratch-buffer-setup region)
      (progn
	(setq m (intern (completing-read "Select major mode: " modes nil t)))
	(chris/simple--scratch-buffer-setup region m)))))

(defun chris/toggle-line-numbers ()
  "Toggles the display of line numbers."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode)))

(define-derived-mode chris/nmcli-wifi-preexist-mode tabulated-list-mode
  "nmcli-wifi-preexist"
  "nmcli preexisting WiFi Mode"
  (let ((columns [("NAME" 20 t)
                  ("UUID" 40 t)
                  ("TYPE" 10 t)
                  ("DEVICE" 10 t)])
        (rows (chris/nmcli-wifi-preexist--shell-command)))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun chris/nmcli-wifi-preexist-refresh ()
  "Refresh wifi table."
  (interactive)
  (let ((rows (chris/nmcli-wifi-preexist--shell-command)))
    (setq tabulated-list-entries rows)
    (tabulated-list-print t t)))

(defun chris/nmcli-wifi-preexist-sentinel (process event)
  (cond ((string-match-p "finished" event)
	 (chris/nmcli-wifi-preexist-refresh)
	 (kill-buffer "*async nmcli*"))))

(defun chris/nmcli-wifi-preexist--shell-command ()
  "Shell command to check for preconfigured wifi connections"
  (interactive)
  (mapcar (lambda (x)
	    `(,(car (cdr x))
	      ,(vconcat [] x)))
          (mapcar (lambda (x)
		    x)
		  (cdr (mapcar (lambda (x)
				 (split-string x "  " t " "))
			       (split-string (shell-command-to-string "nmcli connection") "\n" t))))))

(defun chris/nmcli-wifi-preexist ()
  "Menu for (dis)connecting from preexisting wifi connections."
  (interactive)
  (switch-to-buffer "*nmcli-wifi-preexist*")
  (chris/nmcli-wifi-preexist-mode))

(defun chris/nmcli-wifi-preexist-connect ()
  "Connect to wifi."
  (interactive)
  (let* ((ssid (aref (tabulated-list-get-entry) 1))
	 (process (start-process-shell-command "nmcli" "*async nmcli*" (format "nmcli connection up \"%s\"" ssid))))
    (set-process-sentinel process 'chris/nmcli-wifi-preexist-sentinel)))

(defun chris/nmcli-wifi-preexist-disconnect ()
  "Disconnect from wifi."
  (interactive)
  (let* ((ssid (aref (tabulated-list-get-entry) 1))
	 (process (start-process-shell-command "nmcli" "*async nmcli*" (format "nmcli connection down \"%s\"" ssid))))
    (set-process-sentinel process 'chris/nmcli-wifi-preexist-sentinel)))

(general-define-key
 :states 'normal
 :keymaps 'chris/nmcli-wifi-preexist-mode-map
 "c" '(chris/nmcli-wifi-preexist-connect :wk "connect")
 "d" '(chris/nmcli-wifi-preexist-disconnect :wk "disconnect")
 "r" '(chris/nmcli-wifi-preexist-refresh :wk "refresh"))

(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defun chris/bluetooth-sentinel (process event)
  (message (concat "bluetooth: " event)))

(defun chris/bluetooth-connect-soundcore ()
  "Connect to bluetooth in-ears."
  (interactive)
  (let* ((process (start-process-shell-command
		  "bluetoothctl"
		  nil
		  "bluetoothctl power on && bluetoothctl connect E8:EE:CC:00:AD:24")))
    (set-process-sentinel process 'chris/bluetooth-sentinel)))

(defun chris/bluetooth-disconnect-soundcore ()
  "Disconnect from bluetooth in-ears."
  (interactive)
  (let* ((process (start-process-shell-command
		   "bluetoothctl"
		   nil
		   "bluetoothctl disconnect E8:EE:CC:00:AD:24 && bluetoothctl power off")))
    (set-process-sentinel process 'chris/bluetooth-sentinel)))

(chris/leader-keys
  "a" '(:ignore t :wk "audio")
  "ac" '(chris/bluetooth-connect-soundcore :wk "bluetooth connect")
  "ad" '(chris/bluetooth-disconnect-soundcore :wk "bluetooth disconnect"))

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
  (setq company-minium-prefix-length 3)
  :init
  (global-company-mode 1))

(use-package lsp-mode
  :config
  (setq read-process-outpu-max (* 1024 1024))
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-enable-links nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  :init
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . flycheck-mode)
  :commands
  (lsp lsp-deferred))

(use-package flycheck
  :general
  (chris/leader-keys
    "cd" '(list-flycheck-errors :wk "List flycheck errors")))

(use-package lsp-ui
  :commands
  lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp-mode)

(defun chris/toggle-code ()
  "Toggle on line numbers and hl-line-mode for a better code experience"
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1)
    (display-line-numbers-mode))
  (if (bound-and-true-p hl-line-mode)
      (hl-line-mode -1)
    (hl-line-mode)))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

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

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (defun haskell-evil-open-above ()
    (interactive)
    (evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun haskell-evil-open-below ()
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))
  
  (evil-define-key 'normal haskell-mode-map
    "o" 'haskell-evil-open-below
    "O" 'haskell-evil-open-above)
  )

(use-package lua-mode
  :mode ("\\.lua\\'". lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (defun chris/open-lua-repl ()
    "open lua repl in horizontal split"
    (interactive)
    ;; (split-window-horizontally)
    (lua-show-process-buffer))
  :init
  (setq lua-indent-level 4
	lua-indent-string-contents t)
  ;; :hook
  ;; (lua-mode . lsp-deferred)
  :general
  (chris/leader-keys
    "cl" '(chris/open-lua-repl :wk "run-lua"))
  (chris/leader-keys
    :keymaps 'lua-mode-map
    "lr" 'lua-send-buffer))

(use-package python-mode
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq python-indent 4)
  :general
  (chris/leader-keys
    :keymaps 'python-mode-map
    "cp" 'run-python)
  (chris/leader-keys
    "pr" 'python-shell-send-buffer))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(use-package racket-mode
  :interpreter ("racket" . racket-mode)
  :config
  (defun chris/racket-run-and-switch-to-repl ()
    "Call `racket-run-and-switch-to-repl' and enable insert state"
    (interactive)
    (racket-run-and-switch-to-repl)
    (when (buffer-live-p (get-buffer racket-repl-buffer-name))
      (with-current-buffer racket-repl-buffer-name
	(evil-insert-state))))
  :general
  (chris/leader-keys
    "cr" '(racket-repl :wk "run racket and switch to repl"))
  (chris/leader-keys
    :keymaps 'racket-mode-map
    "rs" '(racket-send-last-sexp :wk "racket send last sexp")
    "rd" '(racket-send-definiton :wk "racket send definition")
    "rr" '(chris/racket-run-and-switch-to-repl :wk "run racket and switch to repl")
    ))

;; (add-hook 'sh-mode-hook 'flycheck-mode)
;; (add-hook 'sh-mode-hook 'lsp-deferred)

(straight-use-package 'matlab-mode)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command-switches '("-nosplash" "-nodesktop"))
(setq matlab-shell-command "matlab")

(defun chris/matlab-shell-run-buffer ()
  "Run matlab code"
  (interactive)
  (matlab-shell-run-command (concat "cd " default-directory))
  (matlab-shell-run-region (point-min) (point-max)))

(chris/leader-keys
 :keymaps 'matlab-mode-map
 :states 'normal
 "mr" '(chris/matlab-shell-run-buffer :wk "Run matlab buffer"))

(chris/leader-keys
  "cm" '(matlab-shell :wk "Open matlab shell"))

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
  "Clear `eshell' buffer."
  (interactive)
  (erase-buffer)
  (eshell-send-input))


(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map "\C-l" 'my/eshell/clear)))

(defun eshell/mkdir-and-cd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
           (git-repo (file-name-base (s-trim git-url)))
           (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
           (git-branch (s-trim git-output)))
      (concat " [" git-repo " " git-branch "]"))))

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
				    (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))

(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(defun eshell/eshell-local-prompt-function ()
  "A prompt for eshell that works locally (in that is assumes
that it could run certain commands) in order to make a prettier,
more-helpful local prompt."
  (interactive) 
  (let* ((pwd (eshell/pwd))
         (directory (split-directory-prompt
                     (pwd-shorten-dirs
                      (pwd-replace-home pwd))))
         (parent (car directory))
         (name   (cadr directory))
         (branch (curr-dir-git-branch-string pwd))
	 (for-git '(:foreground "cyan" :weight bold)))
    (concat
     parent
     name
     (when branch (propertize branch 'face for-git))
     " $ ")))
(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)
(setq eshell-hightlight-prompt nil)

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

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))
