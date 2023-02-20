;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 00 Table of contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (occur "^;; [0-9]+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 01 Emacs package management (straight.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bootstrap straight.el
;; in favor of package.el for granular control over version numbers
;; if ever needed
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Ensure use-package is there
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 02 Name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set the full user name
(setq user-full-name "Christian Hageloch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 03 Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no ring bell
(setq ring-bell-function 'ignore)

;; use y-or-n instead of yes-or-no
(setq use-short-answers t)

;; no indent tabs
(setq indent-tabs-mode nil)

;; blinking cursor is distracting
(setq blink-cursor-mode nil)

;; second quit key along side C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; put misc. configuration into another file (custom.el)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10 Global shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reload the configuration file
(defun chris/config-reload ()
  "Reload the configuration file"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c r") 'chris/config-reload)

;; previous and next buffer
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)

;; whichkey to help with keyboard shortcuts
(use-package which-key
  :config
  (which-key-setup-minibuffer)
  :init
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20 Vim Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 21 Vim emulation for text editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evil is the superior way of editing text
(use-package evil
  :init
  (setq evil-search-module 'isearch)

  ;; C-u and C-d are remaped later on
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)

  ;; for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  ;; define direction of splits
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right nil)

  ;; set the cursor to box in every mode
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-motion-state-cursor 'box)
  (setq evil-replace-state-cursor 'box)
  (setq evil-operator-state-cursor 'box)

  ;; C-i jump
  (setq evil-want-C-i-jump nil)

  ;; use the built-in undo-redo system as evil-undo-system
  ;; other options:
  ;; - undo-tree (does a lot, is useful for files that are not under version
  ;;   control
  ;; - undo-fu (never used that thing)
  (setq evil-undo-system 'undo-redo)
  :config
  ;; use evil mode
  (evil-mode t)

  ;; set different evil modes for different emacs major modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 22 Evil collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evil not only when editing text but also for navigating non file buffers
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t)
  :config
  ;; load evil-collection
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil commentary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; quick commenting using gcc and gc
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 24 Misc evil functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-d but with centering the line after jump
(defun chris/scroll-down-and-center ()
"Scroll down and center the text to the screen"
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(define-key evil-motion-state-map "\C-d" 'chris/scroll-down-and-center)

;; C-u but with centering the line after jump
(defun chris/scroll-up-and-center ()
"Scroll up and center the text to the screen"
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(define-key evil-motion-state-map "\C-u" 'chris/scroll-up-and-center)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 30 Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 31 Font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install Monoid font from their website to .local/share/fonts
;; or /usr/share/fonts
;; https://larsenwork.com/monoid/
;; example on how to define fonts without fontaine:
;; (add-to-list 'default-frame-alist '(font . "Monoid-9"))
;; (set-face-attribute 'default t :font "Monoid-9")
;; fontaine for better adjustment of fonts
(use-package fontaine
  :bind
  ("C-c f" . fontaine-set-preset)
  ("C-c F" . fontaine-set-face-font)
  :config
  (setq x-underline-at-descent-line t)
  (setq-default text-scale-remap-header-line t)
  (setq fontaine-presets
      '((regular
         :default-height 90)
        (medium
         :default-weight semilight
         :default-height 160)
        (large
         :default-weight semilight
         :default-height 200 
         :bold-weight extrabold)
        (t
         :default-family "Monoid"
         :default-weight normal
         :fixed-pitch-family nil
         :fixed-pitch-weight nil
         :fixed-pitch-height 1.0
         :variable-pitch-family "Monoid"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil))))

(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 32 Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modus themes for a good dark theme and the best light theme I have ever
;; used (the only usable one)
;; the version that are built-into emacs are outdated to the version
;; installed through straight as has differnt configuration variables
(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-construct nil
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
    (modus-themes-load-theme 'modus-vivendi)
  (modus-themes-load-theme 'modus-operandi))

(global-set-key (kbd "C-c t") 'modus-themes-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 33 Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the only reason to use this in favor of the built-in modeline isearch
;; that it shows the index of the tab when activating tab-bar.el
(use-package doom-modeline
  :config
  (setq doom-modeline-height 28)
  :init
  (doom-modeline-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 40 Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 41 Completion at point extension Cape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cap improvement for corfu
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 42 Corfu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; more minimal and faster than company
;; needs cape/a different provider for completions (like lsp-mode/eglot)
;; to work
(use-package corfu
  :config
  (customize-set-variable 'corfu-cycle t)
  (customize-set-variable 'corfu-auto t)
  (customize-set-variable 'corfu-auto-prefix 2)
  (customize-set-variable 'corfu-auto-delay 0.0)
  (customize-set-variable 'corfu-echo-documentation 0.25)
  :hook
  (eshell-mode . (lambda () (setq-local corfu-quit-at-boundary t
					corfu-quit-no-match t
					corfu-auto nil)))
  :init
  ;; enable corfu-mode globally
  (global-corfu-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 43 Completion frontend Ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ivy and ivy-prescient to remember search candidates
;; (use-package ivy
;;   :bind
;;   ("C-s" . swiper)
;;   :init
;;   (ivy-mode))

;; (use-package ivy-prescient
;;   :after
;;   ivy
;;   :init
;;   (ivy-prescient-mode))

;; enhance ivy with counsel 
;; (use-package counsel
;;   :bind
;;   ;; keybinding for recently edited files
;;   ("C-x C-r" . counsel-recentf)
;;   :config
;;   ;; no preview of buffers in switch-buffer 
;;   (setq counsel-switch-buffer-preview-virtual-buffers nil)
;;   :init
;;   (counsel-mode))
(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1))

(use-package consult
  :bind
  ("C-s" . consult-line)
  ("C-x C-r" . consult-recent-file))
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 50 File management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file settings
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq find-file-visit-truename t)
(set-default-coding-systems 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 51 Sudo edit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; edited files with elevated privileges
(use-package sudo-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 52 Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dired and async to run processes async
(use-package dired
  :ensure nil
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package async
  :init
  (dired-async-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 60 Buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; relative line numbers
(setq display-line-numbers-type 'relative)

;; display line numbers globally
(global-display-line-numbers-mode 1)

;; remove line numbers from list of major modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; highlight the current line
(global-hl-line-mode 1)

;; revert buffers
(global-auto-revert-mode)

;; revert all buffers (also the non-file ones)
(setq global-auto-revert-non-file-buffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 61 Popper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle pop-up buffers
(use-package popper
  ;; keybindings for popper
  :bind (("C-`"   . popper-toggle-latest)
	 ("M-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-mode-line nil)
  :init
  ;; list of buffers to treat as pop-ups
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  "^\\*MATLAB\\*$"
	  "^\\*Racket REPL.*\\*$"
	  "^\\*lua\\*$"
	  "^\\*Python\\*$"
	  "^\\*Process List\\*$"
	  "^\\*Flycheck \\*"
	  help-mode
	  compilation-mode))
  ;; enable popper
  (popper-mode +1)
  (popper-echo-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 62 Winner Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; control window setup and previous states
(use-package winner
  :ensure nil
  :init
  (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 63 Kill all dired buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function to kill all dired buffers because the can clutter up
;; the buffer list
(defun chris/kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 64 Simple scratch buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get all pro-mode derivatives to be able to create custom scratch buffers
(defun chris/simple--scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           when (and (functionp sym)
                     (provided-mode-derived-p sym 'prog-mode))
           collect sym))

;; create custom scratch buffers
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

;; create custom scratch buffers with current major mode as major mode
;; if the current major mode is a prog-mode derivative or a prompt
;; for a list to choose from
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

;; keybinding to create custom scratch buffer
(global-set-key (kbd "C-c s") 'chris/simple-scratch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 65 Get all buffers with same major mode as current buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get all open buffers with current major-mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 66 Get all buffers in project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get all open buffers in project
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
            (lambda (pair)
              (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
      (call-interactively 'switch-to-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 66 Buffer keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybindings to control buffers

(let ((map global-map))
  ;; switch buffers
  (define-key map (kbd "C-x C-b") 'consult-buffer)
  ;; ibuffer
  (define-key map (kbd "C-x b") 'ibuffer)
  ;; kill buffer and close split if it exists
  (define-key map (kbd "C-x K") 'kill-buffer-and-window)
  ;; prompt for buffer to kill (will preserver split/window configuration)
  (define-key map (kbd "C-x k") 'kill-current-buffer)
  ;; get all buffers in current project
  (define-key map (kbd "M-s v") 'chris/buffers-vc-root)
  ;; get all buffers with current major mode
  (define-key map (kbd "M-s b") 'chris/buffers-major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 70 Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; projectile to manage projects daaahhh
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-modes "org-mode")
  (setq projectile-indexing-method 'hybrid)
  :init
  (projectile-mode +1))

;; sort ibuffer according to projects
;; keeps ibuffer organized
(use-package ibuffer-projectile
  :config 
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 80 tab-bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make tab-bar more minimal
(use-package tab-bar
  :ensure nil
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-forward-button-show nil)
  (setq tab-bar-backward-button-show nil)
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
  (setq tab-bar-show nil)
  (tab-bar-history-mode 1))

;; attempt to treat tabs as workspaces because they preserve window layouts
;; and buffers
;; if no other tab exists it will create a new tab and switch to it
;; else use completion to choose from tab list
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
           (tab-bar-switch-to-tab
            (completing-read "Select tab: " tabs nil t))))))

;; keybinding to manage tabs
(global-set-key (kbd "C-x t s") 'chris/tab-bar-select-tab-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 90 IDE Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 91 Lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eglot will be built into emacs 29 so no reason to use lsp-mode
;; if speed is the target.
;; lsp-mode has it's own features and possibilities like dap-mode
;; this is subject to change when I need a debugger
(use-package eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 92 Treesitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-sitter languages for treesitter support
(use-package tree-sitter-langs)

;; tree-sitter (syntax parsing sitting in a tree)
(use-package tree-sitter
  :defer t
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; enable tree-sitter globally
  (global-tree-sitter-mode)
  :custom
  ;; no italics (because italics are for maniacs
  (custom-set-faces
   '(italic ((t nil)))
   '(tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face)))))
  :config
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t))

;; define tree-sitter objects for evil mode
;; instead of vap this allows it to use something like vaf so select around
;; a function,...
(use-package evil-textobj-tree-sitter
  :ensure t
  :init
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-outer-text-objects-map "C"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-outer-text-objects-map "a"
    (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 93 Hl-todo (better comments)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; highlight keywords in comments
(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 100 Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git integration
(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-repository-directories
        '(("~/code"  . 2)
          ("~/" . 2)))
  (setq git-commit-summary-max-length 50)
  :bind
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-list-repositories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 110 Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 111 Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org mode configuration
(use-package org
  :ensure nil
  :config
  ;; org source code block language configuration
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

  (setq org-edit-src-content-indentation 0)
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org")))

;; org-agenda
(setq org-agenda-files '("~/org/Agenda.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-done 'time)

;; denote (notes)
;; simplified version of org-roam with a clever file naming scheme instead
;; of using a database (allows to use notes with anything, not only emacs)
(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq denote-known-keywords '("emacs" "programming" "administration" "linux"))
  :hook
  (dired-mode . denote-dired-mode)
  (dired-mode . dired-hide-details-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 112 Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; haskell configuration
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 113 Lua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lua configuration
(use-package lua-mode
  :mode ("\\.lua\\'". lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (defun chris/open-lua-repl ()
    "open lua repl in horizontal split"
    (interactive)
    (lua-show-process-buffer))
  :init
  (setq lua-indent-level 4
	lua-indent-string-contents t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 114 Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python configuration
(use-package python-mode
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq python-indent 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 115 racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket configuration
(use-package racket-mode
  :interpreter ("racket" . racket-mode)
  :config
  (defun chris/racket-run-and-switch-to-repl ()
    "Call `racket-run-and-switch-to-repl' and enable insert state"
    (interactive)
    (racket-run-and-switch-to-repl)
    (when (buffer-live-p (get-buffer racket-repl-buffer-name))
      (with-current-buffer racket-repl-buffer-name
	(evil-insert-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 116 matlab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; matlab configuration
;; the point of matlab is to not use it because it is a piece of trash,
;; use octave if you can
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 120 Terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 121 Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vterm
;; will need configuration in the shell the be great
;; best terminal emulation for emacs
(use-package vterm
  :ensure t
  :hook
  (vterm-mode .(lambda ()
		 (evil-local-mode -1)))
  (vterm-mode . (lambda ()
		  (setq-local global-hl-line-mode nil)))
  :init
  (setq vterm-timer-delay 0.01))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 122 eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eshell
;; best shell for emacs
(defun chris/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t)
  (setq tramp-default-method "ssh"))

;; configure eshell
(use-package eshell
  :ensure nil
  :hook
  (eshell-first-time-mode . chris/configure-eshell)
  (eshell-mode . (lambda ()
		   (setq-local global-hl-line-mode nil)))
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("ssh" "tail" "htop" "pulsemixer"))))

;; git status
(defun eshell/gst (&rest args)
  "Git status in eshell"
  (magit-status (pop args) nil)
  (eshell/echo))

;; find wrapper for eshell 
(defun eshell/f (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/f (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/f (concat "*" filename) dir 2))
       (t "")))))

;; find wrapper for eshell
(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))

;; clear
(defun eshell/clear ()
  "Clear `eshell' buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; create directory and switch to it immediately
(defun eshell/mkdir-and-cd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

;; history completion with C-c h in eshell-mode
(add-hook 'eshell-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c h")
                               (lambda ()
                                 (interactive)
                                 (insert
                                  (completing-read "Eshell history: "
                                                       (delete-dups
                                                        (ring-elements eshell-history-ring))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 130 Overflow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; color in hex-codes and other color codes
(use-package rainbow-mode
  :ensure t)

;; emms (listen to music)
(use-package emms
  :ensure t)
(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-mode-line 0)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/media/music/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
