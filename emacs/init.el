
(defvar default-font-size 130)
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)
(blink-cursor-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq visible-bell t)
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Place all auto-save files in one directory
(setq my-backup-dir (concat user-emacs-directory "backups"))
(setq backup-directory-alist
      `((".*" . ,my-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my-backup-dir t)))

(set-face-attribute 'default nil :font "Fira Code" :height default-font-size)

(load-theme 'wombat)

;; Make C-g/ESC quit prompts
(global-set-key (kbd "C-g") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
	
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Disable line numbers for some modes
(dolist (hook '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

(use-package diminish)
(use-package command-log-mode
  :diminish)

(use-package swiper)

(use-package ivy
  :diminish
  :bind
  (("C-s" . swiper)
   :map ivy-minibuffer-map
   :map ivy-switch-buffer-map
   :map ivy-reverse-i-search-map)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))


;; Bind custom functions here
(use-package general
  :config
  (general-create-definer vgowda/leader
    :prefix "C-c")
  (vgowda/leader
    "/" 'swiper
    "a" 'org-agenda
    "c" 'goto-code-dir
    "e" 'eshell
    "s" 'zsh-term
    "b" 'eww)
  (general-override-mode 1))

;; Hydra for sticky keybindings
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(vgowda/leader
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package counsel
  :after ivy
  :bind (("C-M-j" . counsel-switch-buffer))
  :config (counsel-mode))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config (ivy-prescient-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         ;; ("C-c m" . vr/mc-mark)
         ))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package csv-mode
  :defer t)

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

(use-package hl-line
  :custom-face (hl-line ((t (:extend t))))
  :hook (after-init . global-hl-line-mode))

;; Don't forget to run:
;; M-x all-the-icons-install-fonts
;; M-x nerd-icons-install-fonts
(use-package all-the-icons)
(use-package nerd-icons)

(use-package doom-modeline
  :custom ((doom-modeline-height 15))
  :config (doom-modeline-mode 1))

(use-package doom-themes
  :config (load-theme 'doom-gruvbox t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.75))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun goto-code-dir ()
  "Navigates to the home directory."
  (interactive)
  (find-file "~/code"))

(defun zsh-term ()
  "Invokes term with zsh shell"
  (interactive)
  (term "/usr/bin/zsh"))

(defhydra winner-nav ()
  "cycle through window layouts"
  ("j" winner-undo "prev")
  ("l" winner-redo "next"))

;; (use-package winner
;;   ;; :bind ("C-c w" . winner-nav/body)
;;   :config
;;   (winner-mode 1))

(winner-mode 1)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; Bind custom functions here
(use-package general
  :config
  (general-create-definer vgowda/leader
    :prefix "C-c")
  (vgowda/leader
    "/" 'swiper
    "a" 'org-agenda
    "c" 'goto-code-dir
    "e" 'eshell
    "s" 'zsh-term
    "b" 'eww
    "w" 'winner-nav/body)
  (general-override-mode 1))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package browse-url
  :custom
  (browse-url-browser-function 'eww-browse-url))

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))
;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  ;; (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
  )

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :custom
  (org-agenda-files '("~/orgs"))
  :config
  (setq org-ellipsis " ▾")
  ;; (setq org-hide-emphasis-markers t)
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package ace-window
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :weight 'bold
   :height 2.0)
  :bind (("M-o" . ace-window))
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ================================IDE Config ====================================
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)
	   (project-switch-project-action 'project-dired))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code"))))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package gitignore-templates
  :defer t)

;; (use-package forge)

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

(use-package avy 
  :ensure t
  :bind ("C-:" . avy-goto-char))

(use-package electric
  :ensure nil
  :config (electric-pair-mode 1))

(use-package company
  :after lsp-mode
  :hook prog-mode
  :bind ((:map company-active-map
               ("<tab>" . company-complete-selection))
	 (:map lsp-mode-map
               ("<tab>" . company-indent-or-complete-common)))
  :custom
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-idle-delay 0.5)
  (company-require-match nil)
  (company-minimum-prefix-length 2))

(use-package company-prescient
  :config (company-prescient-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :after lsp-mode
  :config
  (yas-reload-all)
  :hook ((lsp-mode . yas-minor-mode)))

(use-package yasnippet-snippets)

(use-package flycheck
  :hook prog-mode)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; ================================LSP CONFIG================================
;; (defun efs/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;; (lsp-eldoc-render-all nil)
  :hook python-mode java-mode go-mode
  :bind (:map lsp-mode-map
	      ("C-c l p d" . lsp-ui-peek-find-definitions)
	      ("C-c l d" . lsp-describe-thing-at-point)
	      ("C-c l p r" . lsp-ui-peek-find-references)
	      ("C-c l p i" . lsp-ui-peek-find-implementation)
	      ("C-c l m" . lsp-ui-imenu)
	      ("C-c l s" . lsp-ivy-workspace-symbol))
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook lsp-mode
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-java
  :custom (lsp-java-maven-download-sources t))

(use-package dap-mode)

(use-package go-mode)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
	      ("C-c C-c C-a" . rustic-cargo-add)
	      ("C-c C-c C-s" . cargo-process-search))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))
