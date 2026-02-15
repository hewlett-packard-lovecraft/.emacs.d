;;; wsl-t.el --- wsl and terminal specific options  -*- lexical-binding: t; -*-
;;; Commentary: Enables WSL copy, sets terminal & GUI themes, and sets fonts
;;; Code:
(defun wsl-p ()
  "Return t if Emacs is running in WSL2, nil otherwise."
  (and (eq system-type 'gnu/linux)
       (getenv "WSLENV")))

(defun setup-wsl-t () "Check if running in WSL and not graphically, then configure Emacs to use the Windows clipboard."
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
		 (string-trim (shell-command-to-string "powershell.exe -command Get-Clipboard"))))))

(defun font-exists-p (font)
  "Return t if font exists, nil otherwise"
  (if (null (x-list-fonts font)) nil t))

(defun set-frame-theme ()
  "Set theme based on whether FRAME is a GUI or TTY frame."
  (with-selected-frame frame
    (if (display-graphic-p)
	(progn (disable-theme 'modus-vivendi)
	       (enable-theme 'modus-operandi)
	       (when (window-system)
		 (cond ((font-exists-p "") (set-frame-font "Courier Prime:spacing=100:size=18" nil t))
		       ((font-exists-p "Courier New") (set-frame-font "Courier New:spacing=100:size=18" nil t)))))
      (progn (disable-theme 'modus-operandi) (enable-theme 'modus-vivendi)))))

(when (wsl-p)
  (add-hook 'after-make-frame-functions 'setup-wsl-t))

(add-hook 'face-set-after-frame-functions 'set-frame-theme)

(provide 'wsl-t)
;;; wsl-t.el ends here
