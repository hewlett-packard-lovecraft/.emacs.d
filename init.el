;;; init.el --- Howard's Emacs configuration
;;; Commentary:
;; A basic config for editing Org files and LaTeX. Works on Windows.
;;
;;; Code:

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

;; Custom file
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;; ;; use-package
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.

(use-package emacs :ensure nil
  :custom
  ;; ;; dape
  (window-sides-vertical t)

  ;; ;; corfu
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

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
  (electric-pair-mode 1)
  ;; (add-hook 'latex-mode-hook #'display-line-numbers-mode)

  (global-display-line-numbers-mode 1)

  ;; autoreload buffers
  (global-auto-revert-mode t)

  ;; replace sound with flash
  (setq visible-bell t)

  ;; to disable all sounds including flash
  ;; (setq ring-bell-function 'ignore)

  ;; ;; etc
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (cua-mode 1)

  ;; remember last opened file
  (recentf-mode 1)

  ;; restore the last cursor location of opened files
  (save-place-mode 1)

  ;; context menu
  (context-menu-mode 1))

;; Set up AuCTeX to load with the builtin TeX package
(use-package auctex
  :ensure (:repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
		 :pre-build (("make" "elpa"))
		 :build (:not elpaca--compile-info) ;; Make will take care of this step
		 :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
		 :version (lambda (_) (require 'auctex) AUCTeX-version)))
(use-package latex
  :ensure nil
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
	      ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
	   (let* ((beg (region-beginning))
		  (end (region-end))
		  (string (buffer-substring-no-properties beg end)))
	     (kill-region beg end)
	     (insert (calc-eval `(,string calc-language latex
					  calc-prefer-frac t
					  calc-angle-mode rad)))))
	  (t (let ((l (thing-at-point 'line)))
	       (end-of-line 1) (kill-line 0)
	       (insert (calc-eval `(,l
				    calc-language latex
				    calc-prefer-frac t
				    calc-angle-mode rad))))))))


(use-package diminish :ensure t)

;; tab-bar
(use-package tab-bar
  :bind (
	 ("C-c C-<tab>" . tab-bar-switch-to-next-tab)
	 ("C-c C-S-<tab>" . tab-bar-switch-to-prev-tab))
  :config
  (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  ;; (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
  (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)) ;; elements to include in bar

  ;; look and feel
  ;; modeline settings
  '(mode-line ((t (:underline nil :overline nil :box (:line-width 8 :color "#353644" :style nil) :foreground "white" :background "#353644"))))
  '(mode-line-buffer-id ((t (:weight bold))))
  '(mode-line-emphasis ((t (:weight bold))))
  '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
  '(mode-line-inactive ((t (:weight light :underline nil :overline nil :box (:line-width 8 :color "#565063" :style nil) :foreground "white" :background "#565063" :inherit (mode-line)))))
  ;; tab bar settings
  '(tab-bar ((t (:inherit mode-line))))
  '(tab-bar-tab ((t (:inherit mode-line :foreground "white"))))
  '(tab-bar-tab-inactive ((t (:inherit mode-line-inactive :foreground "black"))))

  (tab-bar-mode 1))

;;; Vim Bindings
(use-package undo-fu :ensure t :demand t)
(use-package evil
  :ensure t
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))

  (defun my/display-set-relative ()
    (setq display-line-numbers 'relative))     ; or 'visual
  (defun my/display-set-absolute ()
    (setq display-line-numbers t))
  (add-hook 'evil-insert-state-entry-hook #'my/display-set-absolute)
  (add-hook 'evil-insert-state-exit-hook #'my/display-set-relative))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :ensure t
  :after evil

  :custom (evil-collection-setup-minibuffer t)
  (evil-want-integration t)
  (with-eval-after-load 'flymake (evil-collection-flymake-setup))
  :config(evil-collection-init))

;; vim-commentary for Emacs
;; (Use gcc to comment out a line, gc to comment out the target of a motion
;; (for example, gcap to comment out a paragraph), gc in visual mode to comment out the selection etc.)

(use-package evil-commentary
  :ensure t
  :after evil
  :diminish
  :config (evil-commentary-mode +1))

(use-package evil-snipe :ensure t
  :after evil
  :config
  ;; add exceptions, otherwise enable everywhere
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (evil-snipe-override-mode 1))

(use-package ag
  :ensure t
  )

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
  (setq project-vc-extra-root-markers '(".project-root"))
  (defun my-vc-off-if-remote ()
    (if (file-remote-p (buffer-file-name))
	(setq-local vc-handled-backends nil)))
  (add-hook 'find-file-hook 'my-vc-off-if-remote))


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

  ;; auto trigger
  ;; :config
  ;; (setq corfu-auto t
  ;;     corfu-auto-delay 0.1
  ;;     corfu-auto-trigger "." ;; Custom trigger characters
  ;; corfu-quit-no-match 'separator) ;; or t
  )
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
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   :config
;;   (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
;;   (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
;;   (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
;;   (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
;;   (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package recentf :ensure nil
  :config
  (global-set-key (kbd "C-x C-r") 'recentf-open)
  (recentf-mode t)
  (setq recentf-max-saved-items 50)

  (defun recentf-open ()
    (interactive)
    (if (find-file (completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
      (message "Aborting")))
  )

;; icomplete: setup icomplete-vertical-mode / fido-mode / fido-vertical-mode
;; replaces ido
(use-package icomplete
  :demand t
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
          icomplete-in-buffer t
          max-mini-window-height 10)

    (icomplete-mode 1)
    (icomplete-vertical-mode 1))


  (defun fido-style ()
    (setq completion-auto-wrap t
          completion-auto-help 'lazy
          completions-max-height 15
          completion-styles '(flex)
          icomplete-in-buffer t
          max-mini-window-height 10)

    (fido-mode 1)
    (fido-vertical-mode 1))

  (defun icomplete-post-command-hook ()
    (when (not (and (eq (icomplete--completion-table) 'read-file-name-internal)
		    (file-remote-p (minibuffer-contents-no-properties))))
      ;; Original function
      (let ((non-essential t))
	(icomplete-exhibit))))

  ;; Bind C-r to show minibuffer history entries
  (keymap-set minibuffer-mode-map "C-r" #'minibuffer-complete-history)

  ;; enable
  (basic-completion-style)
  ;; (icomplete-vertical-style)
  (fido-style)
  )

;; Consult
(use-package consult
  :ensure t
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
  :bind
  ( :map global-map
    ("C-c n s" . consult-notes)
    ("C-c C-n" . consult-notes-search-in-all-notes)
    )
  :config
  (setq consult-notes-file-dir-sources '(
					 ("Denote"  ?o  "C:\\Users\\Haoming Xia\\OneDrive\\Documents\\Denote")
					 )) ;; Set notes dir(s), see below

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
  (add-hook 'latex-mode-hook #'flymake-vale-load)
  (add-hook 'org-mode-hook #'flymake-vale-load)
  (add-hook 'markdown-mode-hook #'flymake-vale-load)
  (add-hook 'message-mode-hook #'flymake-vale-load)
  )


(use-package which-key
  :ensure t
  :config
  (which-key-mode +1)
  (diminish 'which-key-mode))

(use-package iedit
  :ensure t)

;; Org Mode
(use-package org
  :defer t
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-src-fontify-natively t)
  (setq org-startup-with-inline-images t))

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
  (org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")

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
  (setq denote-directory (expand-file-name "~/../../OneDrive/Documents/Denote"))
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

;; tree-sitter

(use-package treesit-langs
  :ensure (:host github :repo "emacs-tree-sitter/treesit-langs")
  :ensure (:wait t)
  :config (treesit-langs-major-mode-setup))

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :ensure (:wait t))

;;; dape
(use-package dape
  :defer t
  :ensure t
  ;; refer to evil-collection binds
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")
  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)

  :config
  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer))

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :custom
  (repeat-mode +1))


;;; LSP with Eglot
(use-package eldoc
  :init
  (global-eldoc-mode))

(use-package eglot
  :defer t
  ;; refer to evil-collection bindings
  :hook ((python-ts-mode c++-ts-mode c-ts-mode) . eglot-ensure)
  :hook ((python-mode c++-mode c-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '((python-ts-mode . ("pyright-langserver" "--stdio"))
                 (c-ts-mode c++-ts-mode . ("clangd" "-j=8"
					   "--log=error"
					   "--malloc-trim"
					   "--background-index"
					   "--clang-tidy"
					   "--cross-file-rename"
					   "--completion-style=detailed"
					   "--pch-storage=memory"
					   "--header-insertion=never"
					   "--header-insertion-decorators=0")
			    ;; (LaTeX-mode tex-mode . (""))
			    ))))

(use-package eglot-booster
  :ensure (:wait t)
  :ensure (:host github
		 :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode t)
  (setq eglot-booster-io-only t))

;; snippets
(use-package yasnippet
  :ensure t
  )

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package all-the-icons
  :ensure t
  :config
  (add-to-list 'all-the-icons-extension-icon-alist '("m" all-the-icons-fileicon "matlab" :face all-the-icons-orange)))

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


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

  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; autoformat
(use-package format-all
  :ensure t
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))

;; autocorrect

(use-package ispell
  :init
  (setenv "DICTIONARY" "en_US")
  (setenv "DICPATH" (file-name-concat (file-truename user-emacs-directory) "hunspell"))

  ;; TODO: figure out why dicpath symbol void
  ;;(setq dicpath '(file-name-concat (file-truename user-emacs-directory) "hunspell")
  ;;enUS '(file-name-concat dicpath "en_US.aff")
  ;;enCA '(file-name-concat dicpath "en_CA.aff"))

  ;;:if (eq system-type 'windows-nt)
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

(use-package shell :ensure nil
  :config
  (setq explicit-shell-file-name "/bin/bash"))


;;; init.el ends here.
(provide 'init)
