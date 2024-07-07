
(defvar default-font-size 120)
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height default-font-size)

(load-theme 'wombat)

;; Make C-g/ESC quit prompts
(global-set-key (kbd "C-g") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Uncomment regions
;; (global-set-key (kbd "C-M-;") 'uncomment-region) 

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

(column-number-mode)
(global-display-line-numbers-mode t)

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
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind (("C-M-j" . counsel-switch-buffer))
  :config (counsel-mode))


;; Don't forget to run:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

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

(use-package ivy-rich
  :init (ivy-rich-mode 1))

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

(use-package general
  :config
  (general-create-definer vgowda/leader
    :prefix "C-c")
  (vgowda/leader
    "/" 'swiper
    "a" 'org-agenda
    "c" 'goto-code-dir)
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
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package forge)

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

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))


(use-package use-package-chords
  :ensure t
  :init 
  :config (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.4)
  (setq key-chord-one-key-delay 0.5))

(use-package avy 
  :ensure t
  :chords
  ("jc" . avy-goto-char)
  ("jw" . avy-goto-word-1)
  ("jl" . avy-goto-line))

(use-package ace-window
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :weight 'bold
   :height 2.0)
  :bind (("M-o" . ace-window))
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :hook ((lsp-mode . efs/lsp-mode-setup)
	 (prog-mode . lsp-deferred))
  :commands(lsp lsp-deferred)
  :custom ((lsp-keymap-prefix "C-c l"))
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package rust-mode
  ;; :hook (rust-mode . lsp-deferred)
  :custom (rust-format-on-save t))

(use-package lsp-java
  :custom (lsp-java-maven-download-sources t)
  ;; :hook (java-mode . lsp-deferred)
  )

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.3))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package quickrun 
  :ensure t
  :bind ("C-c r" . quickrun))

(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets)
(use-package flycheck :init (global-flycheck-mode))

(use-package go-mode
  ;; :hook (go-mode . lsp-deferred)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3d3eef7dd80e89aee44be1ff51d8285045615ea3239358a51818f4deaa5bd558" default))
 '(package-selected-packages
   '(go-mode lsp-ivy lsp-treemacs company-box company company-mode lsp-ui rust-mode lsp-mode smartparens visual-fill-column org-bullets forge magit projectile hydra general all-the-icons helpful ivy-rich counsel swiper which-key rainbow-delimiters ivy doom-themes doom-modeline diminish command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
