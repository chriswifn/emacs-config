;;; chris-general.el --- General configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; whichkey.el: help with remembering keybindings
(use-package which-key
  :config
  (which-key-setup-minibuffer)
  :init
  (which-key-mode))

;; general.el: better way of doing keybindings
(use-package general
  :config
  ;; integrate general with evil
  (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer chris/leader-keys
			  :states '(normal insert visual emacs)
			  :keymaps 'override
			  :prefix "SPC"
			  :global-prefix "M-SPC")
  ;; setup up ',' as the local leader key
  (general-create-definer chris/local-leader-keys
			  :states '(normal insert visual emacs)
			  :keymaps 'override
			  :prefix ","
			  :global-prefix "M-,")
  ;; unbind some annoying default bindings
  (general-unbind
   "C-x C-r"
   "C-x C-z"
   "C-x C-d")

  (chris/leader-keys
   "SPC" '(execute-extended-command :wk "execute command")
   "TAB" '(:keymap tab-prefix-map :wk "tab"))

  ;; code actions
  (chris/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; help
  (chris/leader-keys
   "h" '(:ignore t :wk "help"))

  ;; file
  (chris/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file")
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  (chris/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bk" '(kill-this-buffer :wk "kill this buffer"))

  ;; open
  (chris/leader-keys
   "o" '(:ignore t :wk "open"))
  )

(provide 'chris-general)
