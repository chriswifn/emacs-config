(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

(when (native-comp-available-p)
  (setq native-comp-async-report-warning-errors 'silent) ;; Emacs 28
  (setq native-compile-prune-cache t)) ;; Emacs 29

(setq custom-file (make-temp-file "emacs-custom-"))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(bind-key "C-x k" 'kill-buffer-and-window)

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("chris-lisp" "chris-private"))

(require 'chris-emacs-ui)
(require 'chris-emacs-org)
(require 'chris-completion)
(require 'chris-evil)
(require 'chris-programming)
(require 'chris-denote)
(require'chris-misc)
