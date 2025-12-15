;; ;; built-in emacs features
;;;; User defined customs

;; custom functions
(defun initel()
  "Open the Emacs init file for editing."
  (interactive)
  (find-file user-init-file))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t)
 '(custom-safe-themes
   '("e4d2af79944ea414bd747aae4f773e1dce68b1c0ae50f9835b6c59a37d8e946c"
     "1ec1befd70983146823bb69fa899b1f7d5be4104409c99095e853954cc837335"
     "9edfbcb903ede673182d5cd5e523e8974243535b33d4c52bc4c3c2d74fdaa4d9"
     "1861cf0cbce9420ef61ea2793d72a4141e9d5464bc2761add982520312462d81"
     "6e4910b3e8dedcb45723bcde8d21e0df4f48a25ee72e8b8a97fae137b215dd11"
     "360e3ad3858b2639636e4de3e39f7dd2a1b5bb860a0980349da3392723823e6d"
     default))
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(avy company consult-denote denote-journal denote-silo diminish evil
	 flx-ido flycheck highlight-escape-sequences iedit magit
	 marginalia markdown-mode orderless projectile realgud
	 standard-themes treesit-fold undo-fu yasnippet))
 '(size-indication-mode t)
 '(tab-bar-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka NF" :foundry "outline" :slant normal :weight regular :height 102 :width normal))))
 '(menu ((t (:background "#333" :foreground "#eee"))))
 '(tool-bar ((t (:background "#333" :foreground "#eee")))))
