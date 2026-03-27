;;; wsl-t.el --- wsl and terminal specific options  -*- lexical-binding: t; -*-
;;; Commentary: Enables WSL copy, sets terminal & GUI themes, and sets fonts
;;; Code:
(defun wsl-p ()
  "Return t if Emacs is running in WSL2, nil otherwise."
  (and (eq system-type 'gnu/linux)
       (getenv "WSLENV")))

;; (defun setup-wsl-t () "Check if running in WSL and not graphically, then configure Emacs to use the Windows clipboard."
;;        (when (and (wsl-p) (eq (display-graphic-p) nil))
;; 	 ;; https://www.rahuljuliato.com/posts/emacs-clipboard-terminal
;; 	 (setq interprogram-cut-function
;; 	       (lambda (text &optional _)
;; 		 (let ((process-connection-type nil))
;; 		   (let ((proc (start-process "clip.exe" "*Messages*" "clip.exe")))
;; 		     (process-send-string proc text)
;; 		     (process-send-eof proc)))))
;; 	 (setq interprogram-paste-function
;; 	       (lambda ()
;; 		 (string-trim (shell-command-to-string "powershell.exe -command Get-Clipboard"))))))

;; (when (wsl-p)
;;    (add-hook 'after-make-frame-functions 'setup-wsl-t))


(defun my/font-exists-p (font)
  "Return t if font exists, nil otherwise"
  (if (null (x-list-fonts font)) nil t))

(defun my/server-set-font ()
  "Set theme based on whether FRAME is a GUI or TTY frame."
  (when (window-system)
    (cond ((my/font-exists-p "Iosevka NFM") (set-frame-font "Iosevka NFM-12" nil t))
	  ;; ((my/font-exists-p "Courier Prime") (set-frame-font "Courier Prime:spacing=100:size=18" nil t))
	  ;; ((my/font-exists-p "Courier New") (set-frame-font "Courier New:spacing=100:size=18" nil t))
	  )))

(add-hook 'server-after-make-frame-hook 'toggle-frame-maximized t)
(add-hook 'server-after-make-frame-hook 'my/server-set-font)

(defun contextual-menubar (&optional frame)
 "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame)
                           1 0))

  (setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))
  )
(unless (string-match-p "LUCID" system-configuration-features)
  (add-hook 'after-make-frame-functions #'contextual-menubar)
  )
;; 

(provide 'wsl-t)
;;; wsl-t.el ends here
