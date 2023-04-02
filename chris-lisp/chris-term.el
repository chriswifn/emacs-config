;;; chris-term.el --- Term configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

;; vterm
;; will need configuration in the shell the be great
;; best terminal emulation for emacs
(use-package vterm
  :general
  (chris/leader-keys
    "ot" '(vterm :wk "vterm"))
  :hook
  (vterm-mode .(lambda ()
		 (evil-local-mode -1)))
  (vterm-mode . (lambda ()
		  (setq-local global-hl-line-mode nil)))
  :init
  (setq vterm-timer-delay 0.01))

(provide 'chris-term)

;; eshell
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
  :general
  (chris/leader-keys
    "oe" '(eshell :wk "eshell"))
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
