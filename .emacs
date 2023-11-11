;;; Package --- Summary:
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t))
(require 'use-package)

(use-package ivy
  :diminish (ivy-mode . "")
  :init (ivy-mode 1) ; globally at startup
  :custom
  ((ivy-use-virtual-buffers t)
   ;; (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
   (ivy-height 20)
   (ivy-count-format "%d/%d ")))

(use-package counsel
  :bind* ; load when pressed
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)  ; search for recently edited
   ("C-c g"   . counsel-git)      ; search for files in git repo
   ("C-c j"   . counsel-git-grep) ; search for regexp in git repo
   ("C-c /"   . counsel-ag)       ; Use ag for regexp
   ("C-x l"   . counsel-locate)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ("C-c C-r" . ivy-resume)))     ; Resume last Ivy-based completion

(use-package flycheck
  :init (global-flycheck-mode))

(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :after yasnippet)


(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.2)
  (global-company-mode t))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.05)
  :diminish which-key-mode)

(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "White")))) ;; Could also have :background "Grey" for example.
  :bind
  (("C-S-<left>" . sp-backward-slurp-sexp)
   ("C-S-<right>" . sp-backward-barf-sexp)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (defvar lsp-keymap-prefix "C-c l")
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode :after lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol :after lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list :after lsp)
(use-package dap-mode :after lsp)
(use-package lsp-java
  :hook ((java-mode . lsp)) :after lsp)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map)
  :custom
  ((projectile-project-search-path '("~/w/"))))

(use-package magit)

(use-package modus-themes
  :demand
  :autoload modus-vivendi
  :config
  (load-theme 'modus-vivendi t))

;;; customizations for in-built packages
(use-package window
  :ensure f
  :bind
  (("M-o" . other-window)))

(use-package warnings
    :ensure nil
    :custom    ((warning-minimum-level :error)))

(provide 'emacs)
;;; .emacs ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("15604b083d03519b0c2ed7b32da6d7b2dc2f6630bef62608def60cdcf9216184" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" default))
 '(package-selected-packages
   '(modus-themes yasnippet-snippets magit yasnippet which-key smartparens projectile lsp-ui lsp-java lsp-ivy flycheck counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
