;;; chris-buffers.el --- Buffer configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

(chris/leader-keys
  "bp" '(previous-buffer :wk "previous buffer")
  "bi" '(ibuffer :wk "ibuffer")
  "bn" '(next-buffer :wk "next buffer"))

;; popper: handle "pop-up" buffers
(use-package popper
  :general
  (chris/leader-keys
    "bt" '(popper-toggle-latest :wk "toggle latest pop-up buffer")
    "bc" '(popper-cycle :wk "cycle pop-up buffers")
    "bu" '(popper-toggle-type :wk "toggle type of pop-up buffer"))
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

;; custom scratch buffers
;; get all proc-mode derivatives to be able to create custom scratch buffers
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

(chris/leader-keys
  "bs" '(chris/simple-scratch-buffer :wk "simple scratch buffer"))

(provide 'chris-buffers)
