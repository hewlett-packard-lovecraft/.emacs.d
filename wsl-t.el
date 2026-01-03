;;; wsl-t.el  -*- lexical-binding: t; -*-
;; wsl and terminal specific options

;; copy and paste from clipboard
(defun wsl-p ()
  "Return t if Emacs is running in WSL2, nil otherwise."
  (and (eq system-type 'gnu/linux)
       (getenv "WSLENV")))

(when (and (wsl-p) (eq (display-graphic-p) nil))
  ;; https://www.rahuljuliato.com/posts/emacs-clipboard-terminal
  (setq interprogram-cut-function
	(lambda (text &optional _)
	  (let ((process-connection-type nil))
	    (let ((proc (start-process "clip.exe" "*Messages*" "clip.exe")))
	      (process-send-string proc text)
	      (process-send-eof proc)))))
  (setq interprogram-paste-function
	(lambda ()
	  (string-trim (shell-command-to-string "powershell.exe -command Get-Clipboard")))))

;; stolen directly from https://github.com/purcell/exec-path-from-shell/blob/master/exec-path-from-shell.el
;; complaining about "exec-path-from-shell-mode not defined in .elc" or smth

(when (and (display-graphic-p) (eq system-type 'gnu/linux))
  )
