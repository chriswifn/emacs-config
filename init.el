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

(use-package emacs
  :init
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

  (global-display-line-numbers-mode t)

  (global-hl-line-mode t)

  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (dolist (mode '(treemacs-mode-hook
                  org-mode-hook
                  vterm-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

  ;; follow symlinks
  (setq find-file-visit-truename t)

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

  ;; open
  (chris/leader-keys
    "o" '(:ignore t :wk "open")
    "ot" '(vterm :wk "Vterm")
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

(use-package link-hint
  :general
  (chris/leader-keys
   "l" '(link-hint-open-link :wk "open link"))
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
  (setq link-hint-avy-style 'pre))

(use-package which-key
  :after evil
  :init (which-key-mode)
  :config
  (which-key-setup-minibuffer))

(defvar chris/default-font-size 90)
(defvar chris/default-variable-font-size 120)

(set-face-attribute 'default nil :font "Monoid" :height chris/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Monoid" :height chris/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height chris/default-variable-font-size :weight 'regular)
(add-to-list 'default-frame-alist '(font . "Monoid-9"))

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
        modus-themes-subtle-line-numbers t
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
(load-theme 'modus-vivendi t)

(use-package doom-modeline
  :init
  (setq doom-modeline-height 26
	doom-modeline-icon t 
	doom-modeline-lsp t)
  (doom-modeline-mode 1))

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
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
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

(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minium-prefix-length 3))

(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 1))
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)

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
(add-hook 'org-mode-hook 'org-indent-mode)

;; configure babel languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package org-appear
  :straight (:type git :host github :repo "awth13/org-appear")
  :after org
  :hook (org-mode . org-appear-mode))

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

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
