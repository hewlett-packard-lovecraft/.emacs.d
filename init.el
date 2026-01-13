;;; init.el --- Howard's Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; A basic config for editing Org files and LaTeX. Works on Windows.
;;
;;; Code:
;; (setq use-package-compute-statistics t)

;; define custom functions early on so they still work if something breaks
(defun initel()
  "Open the Emacs init file for editing."
  (interactive)
  (find-file user-init-file))

(defun +elpaca-reload-package (package &optional allp)
  "Reload PACKAGE's features.
If ALLP is non-nil (interactively, with prefix), load all of its
features; otherwise only load ones that were already loaded.

This is useful to reload a package after upgrading it.  Since a
package may provide multiple features, to reload it properly
would require either restarting Emacs or manually unloading and
reloading each loaded feature.  This automates that process.

Note that this unloads all of the package's symbols before
reloading.  Any data stored in those symbols will be lost, so if
the package would normally save that data, e.g. when a mode is
deactivated or when Emacs exits, the user should do so before
using this command."
  (interactive
   (list (let ((elpaca-overriding-prompt "Reload package: "))
           (elpaca--read-queued))
         current-prefix-arg))
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (when (yes-or-no-p (format "Unload all of %s's symbols and reload its features? " package))
    (let* ((package-name (symbol-name package))
           (package-dir (file-name-directory
                         (locate-file package-name load-path (get-load-suffixes))))
           (package-files (directory-files package-dir 'full (rx ".el" eos)))
           (package-features
            (cl-loop for file in package-files
                     when (with-temp-buffer
                            (insert-file-contents file)
                            (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                              (goto-char (match-beginning 0))
                              (cadadr (read (current-buffer)))))
                     collect it)))
      (unless allp
        (setf package-features (seq-intersection package-features features)))
      (dolist (feature package-features)
        (ignore-errors
          ;; Ignore error in case it's not loaded.
          (unload-feature feature 'force)))
      (dolist (feature package-features)
        (require feature))
      (when package-features
        (message "Reloaded: %s" (mapconcat #'symbol-name package-features " "))))))


;; elpaca installer
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca-no-symlink-mode)

(when (eq system-type 'windows-nt)
  (setq elpaca-queue-limit 12))

;; startup files
(setq org-custom-file (expand-file-name "org.el" user-emacs-directory))
(add-hook 'emacs-startup-hook (lambda () (load org-custom-file 'noerror)))

;; load wsl, terminal file unless on Windows
(unless (eq system-type 'windows-nt)
  (setq wsl-t-custom-file (expand-file-name "wsl-t.el" user-emacs-directory))
  (add-hook 'emacs-startup-hook (lambda () (load wsl-t-custom-file 'noerror))))

(when (display-graphic-p) ;; load customs file only when graphic
  (setq customs-file (expand-file-name "customs.el" user-emacs-directory))
  (add-hook 'elpaca-after-init-hook (lambda () (load customs-file 'noerror))))


;; ;; use-package
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.

;; ;; ;; emacs-builtins
(use-package emacs
  :custom
  ;; ;; dape
  (window-sides-vertical t)

  ;; ;; corfu
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 5)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :config
  ;; line numbers
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)

  ;; autoreload buffers
  (global-auto-revert-mode t)

  ;; replace sound with flash
  (setq visible-bell t)

  ;; to disable all sounds including flash
  ;; (setq ring-bell-function 'ignore)

  ;; ;; etc
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; (menu-bar-mode -1)

  (cua-mode 1)

  ;; remember last opened file
  ;; (recentf-mode 1)

  ;; restore the last cursor location of opened files
  (save-place-mode 1)

  :hook (prog-mode . context-menu-mode)
  :hook (conf-mode . context-menu-mode)
  :hook (text-mode . context-menu-mode)
  :config

  ;; other defaults https://www.patrickdelliott.com/emacs.d/
  (setq enable-recursive-minibuffers t)

  (setq backup-by-copying t)

  (setq sentence-end-double-space nil)

  (setq frame-inhibit-implied-resize t) ;; useless for a tiling window manager

  (setq show-trailing-whitespace t) ;; self-explanatory

  (setq user-full-name "Hao Ming Xia ") ;; my details
  (setq user-mail-address "hx2314@nyu.edu")

  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  (setq indent-tabs-mode nil) ;; no tabs

  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
	`((".*" . ,(concat user-emacs-directory "backups")))
	auto-save-file-name-transforms
	`((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; no need to create lockfiles

  (set-charset-priority 'unicode) ;; utf8 everywhere
  (setq locale-coding-system 'utf-8
	coding-system-for-read 'utf-8
	coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  (show-paren-mode t)
  (put 'suspend-frame 'disabled t) ;; disable confusing suspend in GUI mode)
  ;; terminal stuff here

  (setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

  (use-package tab-bar
    :config
    (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
    (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
    ;; (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
    (setq tab-bar-tab-hints t)                 ;; show tab numbers
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))
  )

;; terminal copy
(use-package clipetty :ensure t
  :config
  :hook (after-init . global-clipetty-mode))

;; gui get path from shell
(use-package exec-path-from-shell
  :ensure t
  :if (and (display-graphic-p) (memq window-system '(mac ns x)))
  :hook (after-init . exec-path-from-shell-initialize))
(use-package electric
  :demand t
  :ensure nil
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package diminish :ensure t)

;; tab bar
;;; Vim Bindings
;; (use-package undo-fu :ensure t :demand t)

(use-package evil
  :demand t
  :ensure t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-respect-visual-line-mode t) ;; respect visual lines

  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.
  ;; (setq evil-search-module 'evil-search) ;; allows for using cgn

  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-disable-insert-state-bindings t)

  (setq evil-undo-system 'undo-redo) ;; built-in, C-r for redo and u for undo
  :config
  ;; set the initial state for some kinds of buffers.
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  ;; (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'magit-diff-mode 'insert)

  (with-eval-after-load 'evil-maps ; avoid conflict with corfu tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))

  (evil-mode t))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-want-integration t)
  (with-eval-after-load 'flymake (evil-collection-flymake-setup))
  :config(evil-collection-init))


(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; 'SPC' as the global leader key
  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode)

  ;; set up ',' as the local leader key
  (general-create-definer my/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "," ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  (general-unbind '(normal motion)
    :with 'ignore
    [remap evil-substitute] ;; prevent S and s conflict
    [remap evil-change-whole-line]
    )

  (my/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (my/leader-keys
    "w" '(:keymap evil-window-map :wk "window")) ;; window bindings

  (my/leader-keys
    "c" '(:ignore t :wk "code"))
  (my/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (my/leader-keys
    "w" '(:keymap evil-window-map :wk "window")) ;; window bindings

  (my/leader-keys
    "c" '(:ignore t :wk "code"))
  ;; bookmark
  (my/leader-keys
    "B" '(:ignore t :wk "bookmark")
    "Bs" '(bookmark-set :wk "set bookmark")
    "Bj" '(bookmark-jump :wk "jump to bookmark"))

  ;; universal argument
  (my/leader-keys
    "u" '(universal-argument :wk "universal prefix"))
  ;; code
  ;; see 'flymake'
  (my/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (my/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")
    "op" '(elpaca-log t :wk "elpaca"))


  ;; search
  ;; see 'consult'
  (my/leader-keys
    "s" '(:ignore t :wk "search"))
  )

;; vim-commentary for Emacs
;; (Use gcc to comment out a line, gc to comment out the target of a motion
;; (for example, gcap to comment out a paragraph), gc in visual mode to comment out the selection etc.)

(use-package evil-commentary
  :ensure t
  :after evil
  :diminish
  :config (evil-commentary-mode +1))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; https://howardabrams.com/hamacs/ha-evil.html#orgf9898ca
(use-package evil-surround
  :ensure t
  :config
  (defun evil-surround-elisp ()
    (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))
  (defun evil-surround-org ()
    (push '(?\" . ("“" . "”")) evil-surround-pairs-alist)
    (push '(?\' . ("‘" . "’")) evil-surround-pairs-alist)
    (push '(?b . ("*" . "*")) evil-surround-pairs-alist)
    (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
    (push '(?i . ("/" . "/")) evil-surround-pairs-alist)
    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)
    (push '(?= . ("=" . "=")) evil-surround-pairs-alist)
    (push '(?~ . ("~" . "~")) evil-surround-pairs-alist))

  (global-evil-surround-mode 1)

  :hook
  (org-mode . evil-surround-org)
  (emacs-lisp-mode . evil-surround-elisp))

(use-package evil-snipe :ensure t
  :after evil
  :hook (magit-mode-hook . turn-off-evil-snipe-override-mode)
  :general
  (general-def '(normal motion) ;; needs a special override for some reason
    "s" 'evil-snipe-s
    "S" 'evil-snipe-S)
  ;; (general-def '(normal motion) ;; needs a special override for some reason
  ;;   "s" 'evil-snipe-s
  ;;   "S" 'evil-snipe-S)

  :config
  (setq evil-snipe-scope 'whole-visible)
  (setq evil-snipe-smart-case t)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode 1))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :ensure t
  :hook (prog-mode . hes-mode))

(use-package super-save
  :ensure t
  :config
  ;; automatically save buffers associated with files on buffer switch
  ;; and on windows switch
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1)
  (diminish 'super-save-mode)
  )

(use-package project :ensure nil
  :config
  (setq vc-handled-backends '(Git))
  (setq project-vc-extra-root-markers '(".project" ".vscode"))
  (defun my-vc-off-if-remote ()
    (if (file-remote-p (buffer-file-name))
	(setq-local vc-handled-backends nil)))
  (add-hook 'find-file-hook 'my-vc-off-if-remote)
  :general
  ;; assign built-in project.el bindings a new prefix
  (my/leader-keys "p" '(:keymap project-prefix-map :wk "project")))


(use-package project-x
  :ensure (:host github :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-local-identifier '(".project"))
  (setq project-x-save-interval 600)    ;Save project state every 10 min
  (project-x-mode 1))

;; (use-package ido
;;   :config
;;   (ido-mode +1)
;;   (setq ido-everywhere t
;; 	ido-enable-prefix         nil
;;         ido-enable-flex-matching t
;; 	ido-virtual-buffers t
;; 	ido-use-faces t
;; 	ido-cannot-complete-command 'ido-next-match
;; 	)
;;   )

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :after ido
;;   :config
;;   (ido-vertical-mode +1)
;;   (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

;; (use-package ido-completing-read+
;;   :ensure t
;;   :after ido
;;   :config (ido-ubiquitous-mode +1))

;; (use-package flx-ido
;;   :after ido
;;   :ensure t
;;   :config (flx-ido-mode +1))

;; Corfu for auto-completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom

  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  :config
  (setq corfu-auto t
	corfu-auto-delay 0.1
	corfu-auto-trigger "." ;; Custom trigger characters
	corfu-quit-no-match 'separator) ;; or t
  )

(use-package corfu-terminal :ensure t
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )


;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package recentf :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (global-set-key (kbd "C-x C-r") 'recentf-open)
  (setq recentf-max-saved-items 50)
  (setq recentf-keep '(file-remote-p file-readable-p))

  (defun recentf-open ()
    (interactive)
    (if (find-file (completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
      (message "Aborting")))
  )

;; icomplete: setup icomplete-vertical-mode / fido-mode / fido-vertical-mode
;; replaces ido
(use-package icomplete
  :config
  (defun basic-completion-style ()
    (setq completion-auto-wrap t
          completion-auto-select 'second-tab
          ;; completion-auto-help 'always
          completion-auto-help nil ;; show on ? and not TAB
          completion-show-help nil
          completions-format 'one-column
          completions-max-height 10))

  (defun icomplete-vertical-style ()
    (setq completion-auto-wrap t
          completion-auto-help nil
          completions-max-height 15
          completion-styles '(initials flex)
          ;; icomplete-in-buffer t
          max-mini-window-height 10)

    (icomplete-mode 1)
    (icomplete-vertical-mode 1))


  (defun fido-style ()
    (setq completion-auto-wrap t
          completion-auto-help 'lazy
          completions-max-height 15
          completion-styles '(flex)
          ;; icomplete-in-buffer t
          max-mini-window-height 10)

    (fido-mode 1)
    (fido-vertical-mode 1))

  (defun icomplete-post-command-hook ()
    (when (not (and (eq (icomplete--completion-table) 'read-file-name-internal)
		    (file-remote-p (minibuffer-contents-no-properties))))
      ;; Original function
      (let ((non-essential t))
	(icomplete-exhibit))))

  (add-hook 'icomplete-minibuffer-setup-hook 'basic-completion-style)

  :hook (after-init . fido-style)
  :bind (:map minibuffer-mode-map ("C-r" . minibuffer-complete-history))

  ;; Bind C-r to show minibuffer history entries
  ;; (keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)

  ;; :hook (icomplete-minibuffer-setup . fido-style)
  ;; enable
  ;; (basic-completion-style)
  ;; (icomplete-vertical-style)
  ;; (fido-style)
  )

;; Consult
(use-package consult
  :ensure t
  :config

  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             ;;consult-notes-org-roam-find-node
             ;; consult-notes-org-roam-find-node-relation
	     )
  :bind (
	 :map global-map
	 ("C-x n" . consult-notes)
	 ("C-c n s" . consult-notes-search-in-all-notes)
	 )
  :config
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  ;; (setq consult-notes-org-headings-files '("~/path/to/file1.org"
  ;;                                         "~/path/to/file2.org"))

  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Enable orderless for completions
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Flymake
;; (use-package flymake-collection
;;   :ensure t
;;   :hook (after-init . 'flymake-collection-hook-setup))


(use-package flymake-vale
  :ensure (:host github :repo "tpeacock19/flymake-vale")
  :config
  (add-hook 'text-mode-hook #'flymake-vale-load)
  (add-hook 'tex-mode-hook #'flymake-vale-load)
  (add-hook 'org-mode-hook #'flymake-vale-load)
  (add-hook 'markdown-mode-hook #'flymake-vale-load)
  (add-hook 'message-mode-hook #'flymake-vale-load)
  )


(use-package which-key
  :defer nil
  :config
  (which-key-mode +1)
  (diminish 'which-key-mode))

(use-package iedit
  :ensure t)

;; LaTeX
;; Set up AuCTeX to load with the builtin TeX package
(use-package tex
  :ensure (:repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
		 :pre-build (("make" "elpa"))
		 :build (:not elpaca--compile-info) ;; Make will take care of this step
		 :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
		 :version (lambda (_) (require 'auctex) AUCTeX-version))
  :init
  (setq TeX-parse-self t ; parse on load
        reftex-plug-into-AUCTeX t
        TeX-auto-save t  ; parse on save
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-electric-sub-and-superscript t
        TeX-engine 'luatex ;; use lualatex by default
        TeX-save-query nil
        TeX-electric-math (cons "\\(" "\\)")) ;; '$' inserts an in-line equation '\(...\)'

  (add-hook 'TeX-mode-hook #'reftex-mode)
  (add-hook 'TeX-mode-hook #'olivetti-mode)
  (add-hook 'TeX-mode-hook #'turn-on-auto-fill)
  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook #'outline-minor-mode)

  :general
  (my/local-leader-keys
    :keymaps 'LaTeX-mode-map
    ;; "TAB" 'TeX-complete-symbol ;; FIXME let's 'TAB' do autocompletion (but it's kind of useless to be honest)
    "=" '(reftex-toc :wk "reftex toc")
    "(" '(reftex-latex :wk "reftex label")
    ")" '(reftex-reference :wk "reftex ref")
    "m" '(LaTeX-macro :wk "insert macro")
    "s" '(LaTeX-section :wk "insert section header")
    "e" '(LaTeX-environment :wk "insert environment")
    "p" '(preview-at-point :wk "preview at point")
    "f" '(TeX-font :wk "font")
    "c" '(TeX-command-run-all :wk "compile"))

  ;; :bind (:map LaTeX-mode-map
  ;; 	      ("M-, S-e" . latex-math-from-calc))
  ;; :config
  ;; ;; Format math as a Latex string with Calc
  ;; (defun latex-math-from-calc ()
  ;;   "Evaluate `calc' on the contents of line at point."
  ;;   (interactive)
  ;;   (cond ((region-active-p)
  ;; 	   (let* ((beg (region-beginning))
  ;; 		  (end (region-end))
  ;; 		  (string (buffer-substring-no-properties beg end)))
  ;; 	     (kill-region beg end)
  ;; 	     (insert (calc-eval `(,string calc-language latex
  ;; 					  calc-prefer-frac t
  ;; 					  calc-angle-mode rad)))))
  ;; 	  (t (let ((l (thing-at-point 'line)))
  ;; 	       (end-of-line 1) (kill-line 0)
  ;; 	       (insert (calc-eval `(,l
  ;; 				    calc-language latex
  ;; 				    calc-prefer-frac t
  ;; 				    calc-angle-mode rad)))))))

  :config
  (use-package evil-tex
    :ensure t
    :defer t
    :after (evil)
    :hook (LaTeX-mode . evil-tex-mode)
    :general
    (:keymaps 'evil-tex-mode-map
	      "M-]" 'evil-tex-brace-movement)
    :hook (LaTeX-mode . evil-tex-mode))
  )

(use-package olivetti
  :ensure t
  :init
  (setq olivetti-body-width 120)
  (setq olivetti-style 'nil)
  (setq olivetti-minimum-body-width 80)
  )

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (pdf-tools-install :no-query))


;; Org Mode
(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-src-fontify-natively t)
  (setq org-startup-with-inline-images t)

  ;; syntax highlighting with minted (requires minted in miktex)
  (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  (setq org-latex-src-block-backend 'minted))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-download
  :ensure t
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  ;; (org-image-actual-width 300)
  (org-image-actual-width (truncate (* (display-pixel-width) 0.8)))

  (when (eq system-type 'windows-nt)
    (org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)"))

  :bind (:map org-mode-map
	      ("C-M-Y" . org-download-yank)
	      ("C-M-y" . org-download-screenshot)))

;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote
  :ensure t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter) )

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Documents/Denote/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("cs202"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-keywords-to-not-infer-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (setq denote-rename-buffer-mode 1)
  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t))

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  :commands ( denote-journal-new-entry
	      denote-journal-new-or-existing-entry
	      denote-journal-link-or-create-entry )
  :bind ()
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

;; (use-package denote-silo
;;   :ensure t
;;   ;; Bind these commands to key bindings of your choice.
;;   :commands ( denote-silo-create-note
;;               denote-silo-open-or-create
;;               denote-silo-select-silo-then-command
;;               denote-silo-dired
;;               denote-silo-cd )
;;   :config
;;   ;; Add your silos to this list.  By default, it only includes the
;;   ;; value of the variable `denote-directory'.
;;   (setq denote-silo-directories
;;         (list denote-directory
;; 	      "~/../../OneDrive/Documents/Denote/"
;;               "~/../../OneDrive/Documents/Journal/"))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;; Useful major modes
(use-package markdown-mode
  :ensure t
  :defer t
  :hook (markdown-mode . visual-line-mode))

;; (use-package realgud :ensure t)

;;; dape
(use-package dape
  ;; :unless (eq system-type 'windows-nt)
  :defer t
  :ensure t
  ;; refer to evil-collection binds

  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")

  ;; :general
  ;; assign built-in dape.el bindings a new prefix
  ;; (my/leader-keys "a" '(:keymap dape-prefix-map :wk "dape")))
  ;; Save breakpoints on quit
  ;; :hook
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  ;; (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)

  :hook (dape-display-source . pulse-momentary-highlight-one-line)
  :hook (dape-start . (lambda () (save-some-buffers t t)))
  :hook (dape-compile . kill-buffer)
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook #'kill-buffer)
  )

;; For a more ergonomic Emacs and `dape' experience

(use-package repeat
  :hook (dape-start . repeat-mode)
  ;; :custom
  ;; (repeat-mode +1)
  )


;;; LSP with Eglot
(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package eglot
  :defer t
  ;; refer to evil-collection bindings
  :hook ((python-ts-mode c++-ts-mode c-ts-mode) . eglot-ensure)
  :hook ((python-mode c++-mode c-mode) . eglot-ensure)
  :hook ((LaTeX-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '((c-or-c++-mode c-or-c++-ts-mode) . ("clangd" "-j=24"
						     "--log=error"
						     "--malloc-trim"
						     "--background-index"
						     "--clang-tidy"
						     "--cross-file-rename"
						     "--completion-style=detailed"
						     "--pch-storage=memory"
						     "--header-insertion=never"
						     "--header-insertion-decorators=0")))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  )

(use-package eglot-booster
  :if (eq system-type 'gnu/linux)
  :ensure (:host github
		 :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode t)
  (setq eglot-booster-io-only t))

;; shamelessly stolen from patrick d elliot

(use-package aas :ensure t
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  ;; easy emoji entry in text mode.
  (aas-set-snippets 'text-mode
		    ":-)" "??"
		    "8-)" "??"
		    ":rofl" "??"
		    ":lol" "??"
		    "<3" "??"
		    ":eyes" "??"
		    ":dragon" "??"
		    ":fire" "??"
		    ":hole" "???"
		    ":flush" "??"
		    ":wow" "??"))

(use-package laas :ensure t
  ;; disables accent snippets - things like 'l (which expands to \textsl{}) end up being very disruptive in practice.
  :init (setq laas-accent-snippets nil)
  :hook ((LaTeX-mode . laas-mode)
         (org-mode . laas-mode))
  :config
  (aas-set-snippets 'laas-mode
		    ;; I need to make sure not to accidentally trigger the following, so I should only use impossible (or extremely rare) bigrams/trigrams.
		    ;; "*b" (lambda () (interactive)
		    ;;        (yas-expand-snippet "\\textbf{$1}$0"))
		    ;; "*i" (lambda () (interactive)
		    ;;     (yas-expand-snippet "\\textit{$1}$0"))
		    "mx" (lambda () (interactive)
			   (yas-expand-snippet "\\\\($1\\\\)$0"))
		    "mq" (lambda () (interactive)
			   (yas-expand-snippet "\\[$1\\]$0"))
		    ;; "*I" (lambda () (interactive)
		    ;;      (yas-expand-snippet "\\begin{enumerate}\n$>\\item $0\n\\end{enumerate}"))
		    ;; "*e" (lambda () (interactive)
		    ;;      (yas-expand-snippet "\\begin{exe}\n$>\\ex $0\n\\end{exe}"))
		    ;; "*f" (lambda () (interactive)
		    ;;      (yas-expand-snippet "\\begin{forest}\n[{$1}\n[{$2}]\n[{$0}]\n]\n\\end{forest}"))
		    "*\"" (lambda () (interactive)
			    (yas-expand-snippet "\\enquote{$1}$0"))
		    :cond #'texmathp ; expand only while in math
		    "Olon" "O(n \\log n)"
		    ";:" "\\coloneq"
		    ";;N" "\\mathbb{N}"
		    ";T" "\\top"
		    ";B" "\\bot"
		    ";;x" "\\times"
		    ";;v" "\\veebar"
		    ";;u" "\\cup"
		    ";;{" "\\subseteq"
		    ";D" "\\Diamond"
		    ";;b" "\\Box"
		    ;; bind to functions!
		    "sum" (lambda () (interactive)
			    (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
		    "grandu" (lambda () (interactive)
			       (yas-expand-snippet "\\bigcup\limits_{$1} $0"))
		    "Span" (lambda () (interactive)
			     (yas-expand-snippet "\\Span($1)$0"))
		    "lam" (lambda () (interactive)
			    (yas-expand-snippet "\\lambda $1_{$2}\\,.\\,$0"))
		    ;; "set" (lambda () (interactive)
		    ;;           (yas-expand-snippet "\\set{ $1 | $2} $0"))
		    "txt" (lambda () (interactive)
			    (yas-expand-snippet "\\text{$1} $0"))
		    ";;o" (lambda () (interactive)
			    (yas-expand-snippet "\\oplus"))
		    ;; "ev" (lambda () (interactive)
		    ;;             (yas-expand-snippet "\\left\\llbracket$3\\right\\rrbracket^$1_$2 $3"))
		    ;; clash with event type sigs
		    ;; add accent snippets
		    :cond #'laas-object-on-left-condition
		    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(use-package yasnippet :ensure t
  :hook (prog-mode . yas-minor-mode)
  ;; :hook (yas-minor-mode . yas-reload-all)
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory ))
  ;;(yas-global-mode +1)
  )

(use-package tempel :ensure t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :general
  ("M-p +" 'tempel-complete) ;; M-p completion prefix; see `cape'
  (my/leader-keys
    "ti" '(tempel-insert :wk "tempel insert"))
  (:keymaps 'tempel-map
            "TAB" 'tempel-next) ;; progress through fields via `TAB'
  :init
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand))

  :hook (conf-mode . tempel-setup-capf)
  :hook (prog-mode . tempel-setup-capf)
  :hook (text-mode . tempel-setup-capf)
  )

(use-package tempel-collection :ensure t)

(use-package simple-httpd :ensure t
  :commands httpd-serve-directory)

(use-package htmlize :ensure t)

(use-package doom-snippets :ensure (:host github :repo "doomemacs/snippets" :files ("*.el" "*"))
  :after yasnippet)

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-corfu :ensure t
  :after nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

  ;; ;; Optionally:
  ;; (setq nerd-icons-corfu-mapping
  ;; 	'((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
  ;;         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
  ;;         ;; You can alternatively specify a function to perform the mapping,
  ;;         ;; use this when knowing the exact completion candidate is important.
  ;;         ;; Don't pass `:face' if the function already returns string with the
  ;;         ;; face property, though.
  ;;         (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
  ;;         ;; ...
  ;;         (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; If you add an entry for t, the library uses that as fallback.
  ;; The default fallback (when it's not specified) is the ? symbol.

  ;; The Custom interface is also supported for tuning the variable above.
  )
(use-package nerd-icons-completion :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-mode-line
  :ensure  (:host github :repo "grolongo/nerd-icons-mode-line")
  :custom
  (nerd-icons-mode-line-v-adjust 0.1) ; default value
  (nerd-icons-mode-line-size 1.0) ; default value
  :config (nerd-icons-mode-line-global-mode t))


(use-package transient :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-tramp-pipe-stty-settings 'pty)
  ;; don't show the diff by default in the commit buffer. Use `C-c C-d' to display it
  (setq magit-commit-show-diff nil)
  ;; don't show git variables in magit branch
  (setq magit-branch-direct-configure nil)
  ;; don't automatically refresh the status buffer after running a git command
  (setq magit-refresh-status-buffer nil)

  ;; set global
  (setq magit-define-global-key-bindings 'reccomended )

  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c g" . magit-dispatch)
	 ("C-c f" . magit-file-dispatch)
	 :map project-prefix-map
	 ("g" . magit-project-status))
  )

;; autoformat
(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :bind ("M-F" . format-all-buffer)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

;; autocorrect

(use-package ispell
  :init
  (setenv "DICTIONARY" "en_US")
  (setenv "DICPATH" (file-name-concat (file-truename user-emacs-directory) "hunspell"))

  ;; TODO: figure out why dicpath symbol void
  ;;(setq dicpath '(file-name-concat (file-truename user-emacs-directory) "hunspell")
  ;;enUS '(file-name-concat dicpath "en_US.aff")
  ;;enCA '(file-name-concat dicpath "en_CA.aff"))

  :if (eq system-type 'windows-nt)
  :custom
  (ispell-local-dictionary "en_CA")
  (ispell-hunspell-dict-paths-alist
   '(("en_US" "~/.emacs.d/hunspell/en_US.aff")
     ("en_CA" "~/.emacs.d/hunspell/en_CA.aff")))
  )

;; flyspell
(use-package flyspell
  :after ispell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (setq ispell-local-dictionary "en_US")
  (setq ispell-hunspell-dict-paths-alist
	'(("en_US" "~/.emacs.d/hunspell/en_US.aff")
	  ("en_CA" "~/.emacs.d/hunspell/en_CA.aff")))
  )

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; Replace with flyspell-correct-helm if you are a helm person.
;; (use-package flyspell-correct-ido
;;   :after flyspell-correct)

;; (use-package flyspell-correct-popup
;;   :after flyspell-correct)

;;  /plink:haoming@localhost#2222:/home/haoming/git/labs-25fa-hewlett-packard-lovecraft
;;  /plink:hxia@orangepi5:/home/haoming/git/labs-25fa-hewlett-packard-lovecraft


;;; TRAMP
(use-package tramp
  :ensure nil
  :custom
  (tramp-default-remote-shell "/bin/bash")

  :config
  ;; ;; per-host config
  ;; (connection-local-set-profile-variables 'remote-path-with-bin
  ;;                                         '(tramp-remote-path . ("~/.cargo/bin/" tramp-default-remote-path))
  ;; 					  )
  ;; (connection-local-set-profiles '(:application tramp :user "hxia" :machine "orangepi5")
  ;; 				 'remote-path-with-bin)
  ;; (connection-local-set-profiles '(:application tramp :user "haoming" :machine "localhost")
  ;; 'remote-path-with-bin)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;;; various perf improvements
  (setq remote-file-name-inhibit-locks t
	tramp-use-scp-direct-remote-copying t
	remote-file-name-inhibit-auto-save-visited t)

  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
	tramp-verbose 2)

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "rsync") ;; scp if it breaks things
   'remote-direct-async-process)

  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))
  )

;; wrapper around terminal
(use-package mistty
  :unless (eq system-type 'windows-nt)
  :ensure t
  :bind (("C-c s" . mistty)

         ;; bind here the shortcuts you'd like the
         ;; shell to handle instead of Emacs.
         :map mistty-prompt-map

         ;; fish: directory history
         ("M-<up>" . mistty-send-key)
         ("M-<down>" . mistty-send-key)
         ("M-<left>" . mistty-send-key)
         ("M-<right>" . mistty-send-key))
  )

;; declare linux specific packages here

(use-package eat :ensure t
  :unless (eq system-type 'windows-nt)
  :custom
  (eat-term-name "xterm-256color")
  :hook (eshell-load . eat-eshell-mode)
  :hook (eshell-load . eat-eshell-visual-command-mode)
  :bind (("C-c e" . eat)
	 :map project-prefix-map
	 ("t" . eat-project)
	 ))

;; tree-sitter, at least until it works on Windows
(use-package treesit
  :if (eq system-type 'gnu/linux)
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
	     ;; Note the version numbers. These are the versions that
	     ;; are known to work with Combobulate *and* Emacs.
	     '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
	       (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
	       (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
	       (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
	       (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	       (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
	       (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
	       (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
	       (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
	       (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
	       (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
	       (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
	(treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also

  ;;;;treesit-auto handles this

  ;; (dolist (mapping
  ;;          '((python-mode . python-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (typescript-mode . typescript-ts-mode)
  ;;            (js2-mode . js-ts-mode)
  ;;            (bash-mode . bash-ts-mode)
  ;;            (conf-toml-mode . toml-ts-mode)
  ;;            (go-mode . go-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (json-mode . json-ts-mode)
  ;;            (js-json-mode . json-ts-mode)
  ;; 	     (yaml-mode . yaml-ts-mode)
  ;; 	     ))
  ;;   (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  ;; (use-package combobulate
  ;;   :custom
  ;;   ;; You can customize Combobulate's key prefix here.
  ;;   ;; Note that you may have to restart Emacs for this to take effect!
  ;;   (combobulate-key-prefix "C-c o")
  ;;   :hook ((prog-mode . combobulate-mode))
  ;;   ;; Amend this to the directory where you keep Combobulate's source
  ;;   ;; code.
  ;;   :load-path ("path-to-git-checkout-of-combobulate"))
  )

(use-package treesit-auto
  :if (eq system-type 'gnu/linux)
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(cmake))
  (global-treesit-auto-mode))

(use-package cmake-mode :ensure t)

(use-package nix-mode :ensure t
  :when (eq system-type 'gnu/linux)
  :mode "\\.nix\\'")

(use-package direnv :ensure t
  :when (eq system-type 'gnu/linux)
  :config
  (direnv-mode)
  (add-to-list 'warning-suppress-types '(direnv)))

;;; init.el ends here.
(provide 'init)
