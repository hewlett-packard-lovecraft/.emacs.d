;; ;; built-in emacs features

;; line numbers
(global-display-line-numbers-mode 1)

;; autoreload buffers
(global-auto-revert-mode t)

;; etc
(tool-bar-mode -1)
(cua-mode 1)

;; remember last opened file
(recentf-mode 1)

;; restore the last cursor location of opened files
(save-place-mode 1)

;; context menu
(context-menu-mode)

;; ;; custom functions below

;; open init file
(defun myinitel()
  "Open the Emacs init file for editing."
  (interactive)
  (find-file user-init-file))

;; stop starting at C:/
(setq-default default-directory "~/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t)
 '(global-display-line-numbers-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka NF" :foundry "outline" :slant normal :weight regular :height 102 :width normal)))))
