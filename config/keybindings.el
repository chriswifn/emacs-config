;; general is a great way of defining keybindings
(use-package general
  :straight t
  :after evil
  :config
  (general-create-definer my-leader-def
    :prefix "SPC"))


;; Text scaling
(use-package hydra
  :straight t
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my-leader-def
  :states 'normal
  :keymaps 'override
  "t f" '(hydra-text-scale/body :which-key "scale text"))


;; split size
(defhydra hydra-split-size (:timeout 4)
  "increase/decrease split size"
  ("h" shrink-window-horizontally)
  ("j" enlarge-window)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("n" balance-windows)
  ("f" nil "finished" :exit t))

(my-leader-def
  :states 'normal
  :keymaps 'override
  "t s" '(hydra-split-size/body :which-key "split size"))


;; keybindings for buffers
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_) 
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(my-leader-def
  :states 'normal
  :keymaps 'override
  "b i"   '(persp-ibuffer :whichkey "Ibuffer perspective")
  "b I"   '(ibuffer :whichkey "Ibuffer")
  "b b"   '(counsel-switch-buffer :which-key "Switch Buffers")
  "b f"   '(toggle-maximize-buffer :which-key "Toggle maximize buffer")
  "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
  "b n"   '(next-buffer :which-key "Next buffer")
  "b p"   '(previous-buffer :which-key "Previous buffer")
  "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
  "b r"   '(revert-buffer :which-key "Revert Buffer")
  "b K"   '(kill-buffer :which-key "Kill buffer"))


;; keybindings for files
(my-leader-def
  :states '(normal visual)
  :keymaps 'override
  "."     '(counsel-find-file :which-key "Find file")
  "f f"   '(counsel-fzf :whichkey "FZF")
  "f r"   '(counsel-recentf :which-key "Recent files")
  "f s"   '(save-buffer :which-key "Save file")
  "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
  "f y"   '(dt/show-and-copy-buffer-path :which-key "Yank file path")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "f R"   '(rename-file :which-key "Rename file")
  "f S"   '(write-file :which-key "Save file as...")
  "f U"   '(sudo-edit :which-key "Sudo edit file"))


;; keybindings for managing splits
(my-leader-def
  :states 'normal
  :keymaps 'override
  "w c"   '(evil-window-delete :which-key "Close window")
  "w n"   '(evil-window-new :which-key "New window")
  "w s"   '(evil-window-split :which-key "Horizontal split window")
  "w v"   '(evil-window-vsplit :which-key "Vertical split window")
  ;; Window motions
  "w h"   '(evil-window-left :which-key "Window left")
  "w j"   '(evil-window-down :which-key "Window down")
  "w k"   '(evil-window-up :which-key "Window up")
  "w l"   '(evil-window-right :which-key "Window right")
  "w w"   '(evil-window-next :which-key "Goto next window")
  ;; winner mode
  "w <left>"  '(winner-undo :which-key "Winner undo")
  "w <right>" '(winner-redo :which-key "Winner redo"))


;; keybindings for dired
(my-leader-def
  :states 'normal
  :keymaps 'override
  "d d" '(dired :whichkey "Open Dired")
  "d j" '(dired-jump :whichkey "Jump to current directory in dired"))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "<") 'dired-up-directory
  (kbd ">") 'dired-find-file
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill 
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)


;; keybindings for emms
(my-leader-def
  :states 'normal
  :keymaps 'override
  "m m" '(emms :whichkey "EMMS")
  "m b" '(emms-smart-browse :whichkey "EMMS Smart Browse")
  "m i" '(emms-show :whichkey "EMMS show current song")
  "m n" '(emms-next :whichkey "EMMS next song")
  "m p" '(emms-previous :whichkey "EMMS previous song")
  "m l" '(emms-seek-forward :whichkey "EMMS go 10s forward")
  "m t" '(emms-toggle-repeat-track :whichkey "EMMS toggle repeat")
  "m h" '(emms-seek-backward :whichkey "EMMS go 10s backward"))


;; keybindings for compiling
(my-leader-def
  :states 'normal
  :kaymaps 'override
  "c c" '(compile :whichkey "Compile"))


;; keybindings for matlab
(defun open-matlab-shell ()
  (interactive)
  (split-window-below 40)
  (other-window 1)
  (matlab-shell))

(my-leader-def
  :states 'normal
  :keymaps 'override
  "c m" '(open-matlab-shell :whichkey "Open Matlab shell"))


;; keybindings for vterm
(my-leader-def
  :states 'normal
  :keymaps 'override
  "o t" '(vterm :whichkey "Vterm"))


;; keybindings for lsp
(my-leader-def
  :states 'normal
  :keymaps 'override
  "l d" '(flymake-show-buffer-diagnostics :whichkey "show lsp diagnostics"))

