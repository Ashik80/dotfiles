;;; package -- Summary
;;; Code:
;; Disable signature verification for package archives
(setq package-check-signature nil)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; (package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
;; Download Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
;; Download recentf
(unless (package-installed-p 'recentf)
  (package-install 'recentf))
;; Download company
(unless (package-installed-p 'company)
  (package-install 'company))
;; Download ivy
(unless (package-installed-p 'ivy)
  (package-install 'ivy))
;; Download flycheck
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
;; Download lsp-mode
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
;; Download yasnippet (lsp-mode dependency)
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
;; Download lsp-ui
(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))
;; Download emojify
(unless (package-installed-p 'emojify)
  (package-install 'emojify))

;;; Commentary:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(typescript-mode evil cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; MAIN CONFIG
;; ========================================

;; Disable menus
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Font
(set-face-attribute 'default nil :font "FiraMono Nerd Font" :height 140)
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))
;; Enable line numbers globally
(global-display-line-numbers-mode)
;; Set theme
(load-theme 'wombat t)
;; Disable backup file
(setq make-backup-files nil)

;; Enable emojify
(require 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)
;; Enable Evil - VI mode
(require 'evil)
(evil-mode 1)
;; Enable projectile
(require 'projectile)
(projectile-mode 1)
;; Enable recentf
(require 'recentf)
(recentf-mode 1)
;; Enable company
(require 'company)
(company-mode 1)
;; Enable ivy for autocomplete
(require 'ivy)
(ivy-mode 1)
;; Enable flycheck
(require 'flycheck)
(global-flycheck-mode 1)

;; Define a prefix command for the space key in normal mode
(define-prefix-command 'my-evil-space-prefix)
(evil-define-key 'normal 'global (kbd "SPC") 'my-evil-space-prefix)

;; Projectile keybinds
(evil-define-key 'normal 'global (kbd "SPC p p") 'projectile-switch-project)
(evil-define-key 'normal 'global (kbd "SPC f f") 'projectile-find-file)

;; Recentf keybinds
(evil-define-key 'normal 'global (kbd "SPC f r") 'recentf-open-files)

;; LSP setup
(require 'lsp-mode)
;; (setq lsp-keymap-prefix "SPC g")

(evil-define-key 'normal lsp-mode-map
  (kbd "SPC g d") 'evil-goto-definition
  (kbd "SPC g r") 'xref-find-references)

;; Enable lsp ui
(require 'lsp-ui)
(lsp-ui-mode 1)

;; disable evil key
(define-key evil-motion-state-map (kbd "K") nil)
;; hover information
(evil-define-key 'normal lsp-mode-map (kbd "K") #'lsp-describe-thing-at-point)

(define-key lsp-ui-mode-map [remap evil-goto-definition] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; Disbale annoying highlighting of word under cursor
(setq lsp-enable-symbol-highlighting nil)
;; Disable breadcrumbs
(setq lsp-headerline-breadcrumb-enable nil)

;; Download typescript-mode
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;; Set the path to the Node.js binary
(let ((node-path "/home/ashik/.nvm/versions/node/v20.12.1/bin"))
  (setq exec-path (append exec-path (list node-path)))
  (setenv "PATH" (concat node-path ":" (getenv "PATH"))))

;; Start lsp for typescript
(add-hook 'typescript-mode-hook #'lsp-deferred)
;;; init.el ends here

