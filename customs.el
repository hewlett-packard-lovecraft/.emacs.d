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
(context-menu-mode))

;; ;; custom functions below

;; open init file
(defun my-init-el()
  "Open the Emacs init file for editing."
  (interactive)
  (find-file user-init-file))

;; stop starting at C:/
(setq-default default-directory "~/AppData/Roaming")

(add-to-list 'default-frame-alist
             '(font . "Iosevka Nerd Font Regular-10"))
