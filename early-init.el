;;; early-init.el -- tune performance here  -*- lexical-binding: t; -*-

;; temporarily increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup (e.g. 50MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

(setq read-process-output-max (* 1024 1024))

(setq native-comp-speed 2)
;; (setq native-comp-speed 3) ;; faster but slower compile (?)

;; lsp
(setenv "LSP_USE_PLISTS" "true")

;; Elpaca
(setq package-enable-at-startup nil
      use-package-enable-imenu-support t
      ;; use-package-compute-statistics t
      )

;; Load themes, disable toolbar early to avoid flickering

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-resize-pixelwise nil)
(setq frame-inhibit-implied-resize t)

;; (if (eq system-type 'windows-nt) ;; light theme on Windows to match the bar
;;     (load-theme 'modus-operandi t)
;; (load-theme 'modus-vivendi t))
(load-theme 'modus-operandi t)

(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)
