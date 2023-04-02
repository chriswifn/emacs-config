;;; chris-tabbar.el --- Tabbar configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; make tab-bar more minimal
(use-package tab-bar
  :general
  (chris/leader-keys
    "i" '(:keymap tab-prefix-map :wk "tab bar"))
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

(chris/leader-keys
  "is" '(chris/tab-bar-select-tab-dwim :wk "tab bar select"))

(provide 'chris-tabbar)
