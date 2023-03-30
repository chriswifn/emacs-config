;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

(setq straight-use-package-by-default t) ;; have use-package use straight.el by default.
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
(straight-use-package 'use-package) ;; install use-package via straight

(use-package emacs
  :straight (:type built-in)
  :config
  (setq user-full-name "Christian Hageloch")
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (setq indent-tabs-mode nil)
  (setq blink-cursor-mode nil)
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)
  (setq find-file-visit-truename t)
  (set-default-coding-systems 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(setq display-line-numbers-type 'relative)
(global-hl-line-mode 1)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)

(defun chris/config-reload ()
  "Reload the configuration file"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun chris/config-visit ()
  (interactive)
  (find-file "~/.emacs.d/readme.org"))

(let ((map global-map))
  (define-key map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key map (kbd "C-c r") 'chris/config-reload)
  (define-key map (kbd "C-c e") 'chris/config-visit)
  (define-key map (kbd "C-x K") 'kill-buffer-and-window)
  (define-key map (kbd "C-x b") 'ibuffer)
  (define-key map (kbd "C-x C-p") 'previous-buffer)
  (define-key map (kbd "C-x C-n") 'next-buffer)
  (define-key map (kbd "C-x k") 'kill-current-buffer))

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

;; evil not only when editing text but also for navigating non file buffers
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t)
  :config
  ;; load evil-collection
  (evil-collection-init))

;; quick commenting using gcc and gc
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; C-d but with centering the line after jump
(defun chris/scroll-down-and-center ()
"Scroll down and center the text to the screen"
  (interactive)
  (evil-scroll-down 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun chris/scroll-up-and-center ()
"Scroll up and center the text to the screen"
  (interactive)
  (evil-scroll-up 0)
  (evil-scroll-line-to-center (line-number-at-pos)))

(let ((map evil-motion-state-map))
  (define-key map "\C-d" 'chris/scroll-down-and-center)
  (define-key map "\C-u" 'chris/scroll-up-and-center))

;; cap improvement for corfu
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

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

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :straight (:type built-in)
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
  :init
  (setq consult-preview-key nil))

(let ((map global-map))
  (define-key map (kbd "C-s") 'consult-line)
  (define-key map (kbd "C-x C-r") 'consult-recent-file)
  (define-key map (kbd "C-x C-b") 'consult-buffer))

(use-package dired
  :straight (:type built-in)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package async
  :init
  (dired-async-mode 1))

(defun chris/kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(let ((map global-map))
  (define-key map (kbd "C-c d") 'chris/kill-dired-buffers))

(use-package popper
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

(let ((map global-map))
  (define-key map (kbd "C-`") 'popper-toggle-latest)
  (define-key map (kbd "M-`") 'popper-cycle)
  (define-key map (kbd "C-M-`") 'popper-toggle-type))

(use-package winner
  :straight (:type built-in)
  :init
  (winner-mode 1))

(let ((map global-map))
  (define-key map (kbd "C-c h") 'winner-undo)
  (define-key map (kbd "C-c l") 'winner-redo))

;; make tab-bar more minimal
(use-package tab-bar
  :straight (:type built-in)
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

(let ((map global-map))
  (define-key map (kbd "C-x t s") 'chris/tab-bar-select-tab-dwim))

(use-package modus-themes
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
    (modus-themes-load-theme 'modus-vivendi)
  (modus-themes-load-theme 'modus-operandi))

(let ((map global-map))
  (define-key map (kbd "C-c t") 'modus-themes-toggle))

(use-package fontaine
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

(let ((map global-map))
  (define-key map (kbd "C-c f") 'fontaine-set-preset)
  (define-key map (kbd "C-c F") 'fontaine-set-face-font))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package org
  :straight (:type built-in)
  :config
  ;; org source code block language configuration
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (latex . t)
       (shell . t)))
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (push '("conf-unix" . conf-unix) org-src-lang-modes))

  (setq org-edit-src-content-indentation 0)
  (setq org-directory "~/org")
  (setq org-hide-emphasis-markers t)
  (setq org-default-notes-file (concat org-directory "/notes.org")))

;; org-agenda
(setq org-agenda-files '("~/org/Agenda.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-done 'time)

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))

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

(use-package python-mode
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq python-indent 4))

(use-package racket-mode
  :interpreter ("racket" . racket-mode))

(defun chris/racket-run-and-switch-to-repl ()
  "Call `racket-run-and-switch-to-repl' and enable insert state"
  (interactive)
  (racket-run-and-switch-to-repl)
  (when (buffer-live-p (get-buffer racket-repl-buffer-name))
    (with-current-buffer racket-repl-buffer-name
      (evil-insert-state))))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

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

;; vterm
;; will need configuration in the shell the be great
;; best terminal emulation for emacs
(use-package vterm
  :hook
  (vterm-mode .(lambda ()
		 (evil-local-mode -1)))
  (vterm-mode . (lambda ()
		  (setq-local global-hl-line-mode nil)))
  :init
  (setq vterm-timer-delay 0.01))

(let ((map global-map))
  (define-key map (kbd "C-c o v") 'vterm))

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
  :straight (:type built-in)
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
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; create directory and switch to it immediately
(defun eshell/mkdir-and-cd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

(add-hook 'eshell-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c h")
                               (lambda ()
                                 (interactive)
                                 (insert
                                  (completing-read "Eshell history: "
                                                       (delete-dups
                                                        (ring-elements eshell-history-ring))))))))

(let ((map global-map))
  (define-key map (kbd "C-c o e") 'eshell))

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

(let ((map global-map))
  (define-key map (kbd "C-c s") 'chris/simple-scratch-buffer))

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

(let ((map global-map))
  (define-key map (kbd "M-s b") 'chris/buffers-major-mode))

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

(let ((map global-map))
  (define-key map (kbd "M-s v") 'chris/buffers-vc-root))

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  :init
  (which-key-mode))

(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/code/personal-wiki/"))
  (setq denote-known-keywords '("emacs" "programming" "administration" "linux"))
  :hook
  (dired-mode . denote-dired-mode)
  (dired-mode . dired-hide-details-mode))

(let ((map global-map))
  (define-key map (kbd "C-c n j") #'prot/denote-journal) ; our custom command
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n f") #'denote-open-or-create)
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  (define-key map (kbd "C-c n r") #'denote-dired-rename-file))

(use-package sudo-edit)

(let ((map global-map))
  (define-key map (kbd "C-c p") 'sudo-edit-find-file)
  (define-key map (kbd "C-c P") 'sudo-edit))

;; projectile to manage projects
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

(use-package evil-textobj-tree-sitter)

(define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-outer-text-objects-map "C"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-outer-text-objects-map "a"
    (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

(use-package simple-httpd)

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

(let ((map global-map))
  (define-key map (kbd "C-x g") 'magit-status)
  (define-key map (kbd "C-x C-g") 'magit-list-repositories))

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

(add-to-list 'display-buffer-alist
	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(let ((map chris/nmcli-wifi-preexist-mode-map))
  (define-key map (kbd "C-c c") 'chris/nmcli-wifi-preexist-connect)
  (define-key map (kbd "C-c d") 'chris/nmcli-wifi-preexist-disconnect)
  (define-key map (kbd "C-c r") 'chris/nmcli-wifi-preexist-refresh))

(let ((map global-map))
  (define-key map (kbd "C-c o n") 'chris/nmcli-wifi-preexist))

;; color in hex-codes and other color codes
(use-package rainbow-mode)

;; emms (listen to music)
(use-package emms)

(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-mode-line 0)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/media/music/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
