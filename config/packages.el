;; THIS IS WHERE SOME USEFUL PACKAGES ARE INSTALLED
;; whichkey
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


;; ivy and counsel
(use-package ivy
  :straight t
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; completion
(use-package ivy-rich
  :straight t
  :after ivy
  :init
  (ivy-rich-mode 1))

;; completion engine
(use-package counsel
  :straight t
  :config
  (counsel-mode 1))

;; improved candidate sorting
(use-package ivy-prescient
  :straight t
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))


;; anzu
(use-package evil-anzu
  :straight t
  :after evil
  :init
  (global-anzu-mode))


;; sudo-edit
(use-package sudo-edit
  :straight t)


;; emms - music player
(use-package emms
  :straight t)
(require 'emms-setup)
(emms-all)
(emms-default-players)
(emms-mode-line 0)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)


;; org-tree-slide - presentations
(use-package org-tree-slide
  :straight t
  :custom
  (org-image-actual-width nil))


;; pdf-tools - view pdfs inside emacs
(use-package pdf-tools
  :straight t
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



