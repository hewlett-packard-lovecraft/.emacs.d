;; elpaca 
(setq package-enable-at-startup nil)

;; performance
(setq gc-cons-threshold 500000000)
(setq read-process-output-max (* 1024 1024))

;; autostart in ~/.emacs.d
(setq default-directory (file-truename user-emacs-directory))
