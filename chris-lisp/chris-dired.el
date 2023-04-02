;;; chris-dired.el --- Dired configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; dired: directory editor
(use-package dired
  :straight (:type built-in)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; async:
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

(chris/leader-keys
  "bd" '(chris/kill-dired-buffers :wk "kill all dired buffers"))

(provide 'chris-dired)
