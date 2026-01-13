;;; performance  -*- lexical-binding: t; -*-
;; temporarily increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup (e.g. 50MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

(setq read-process-output-max (* 1024 1024))

;; (setq native-comp-speed 2)
(setq native-comp-speed 3) ;; faster but slower compile (?)

;; lsp
(setenv "LSP_USE_PLISTS" "true")

;; Elpaca
(setq package-enable-at-startup nil)

;; Load themes, disable toolbar early to avoid flickering

(tool-bar-mode -1)
(scroll-bar-mode -1)

(if (eq system-type 'windows-nt) ;; light theme on Windows to match the bar
    (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t))

(setq inhibit-startup-screen t)
;; (setq initial-buffer-choice 'recentf-open-files)

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path (expand-file-name "C:\\msys64\\usr\\bin"))
  ;; (add-to-list 'exec-path (expand-file-name "C:\\Program Files\\Git\\git.exe"))
  )
