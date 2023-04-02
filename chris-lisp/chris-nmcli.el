;;; chris-nmcli.el --- Nmcli configuration file -*- lexical-binding: t; no-byte-compile: t; -*-

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

(chris/leader-keys
  "on" '(chris/nmcli-wifi-preexist :wk "nmcli"))

(provide 'chris-nmcli)
