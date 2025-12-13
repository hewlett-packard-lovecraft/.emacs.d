;;; performance
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

;; Load themes early to avoid flickering
(load-theme 'modus-operandi t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path (expand-file-name "C:\\msys64\\usr\\bin"))
  (add-to-list 'exec-path (expand-file-name "C:\\Program Files\\Git\\git.exe"))
  ;; more windows specific stuff
  )

;; Elpaca
(setq package-enable-at-startup nil)
