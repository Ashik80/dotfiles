(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(python-black prettier-js lsp-pyright magit corfu lsp-ui flycheck lsp-mode typescript-mode projectile git-gutter)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Package archive configurations
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;;; Leader key
(define-prefix-command 'leader)
(global-set-key (kbd "C-c") 'leader)


;;; Packages
;; Typescript mode settings
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))
(require 'typescript-mode)
(add-hook 'typescript-mode-hook
	  (lambda () (setq typescript-indent-level 2)))

;; Ivy settings
(unless (package-installed-p 'ivy)
  (package-install 'ivy))
(ivy-mode)

;; Projectile settings
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(projectile-mode)
;; Keybinds
(define-key 'leader (kbd "p p") 'projectile-switch-project)
(define-key 'leader (kbd "f f") 'projectile-find-file)

;; Recentf settings
(recentf-mode)
(define-key 'leader (kbd "f r") 'recentf)

;; Git gutter settings
(unless (package-installed-p 'git-gutter)
  (package-install 'git-gutter))
(global-git-gutter-mode)

;; Flycheck settings
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(global-flycheck-mode)

;; Magit settings
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Completion settings
(unless (package-installed-p 'corfu)
  (package-install 'corfu))
(require 'corfu)
(setq corfu-auto t
      corfu-auto-prefix 1
      corfu-separator ?\s          ;; Orderless field separator
      corfu-quit-at-boundary nil   ;; Never quit at completion boundary
      corfu-on-exact-match nil     ;; Configure handling of exact matches
      corfu-scroll-margin 5
      completion-styles '(basic))
(global-corfu-mode)
(corfu-popupinfo-mode)

;; Lsp settings
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))
(lsp-ui-mode)

(require 'lsp-mode)
(setq lsp-enable-symbol-highlighting nil) ;; Disbale annoying highlighting of word under cursor

(defvar node-version "20.12.1") ;; Set node version
(let ((node-path (concat (getenv "HOME") "/.nvm/versions/node/v" node-version "/bin")))
  (setq exec-path (append exec-path (list node-path)))
  (setenv "PATH" (concat node-path ":" (getenv "PATH"))))

(add-hook 'typescript-mode-hook 'lsp-deferred) ;; Enable lsp for typescript

(unless (package-installed-p 'lsp-pyright) ;; Install reqruired pyright client
  (package-install 'lsp-pyright))
(add-hook 'python-mode-hook 'lsp-deferred) ;; Enable lsp for python

;; Keybinds
(add-hook 'lsp-mode-hook
	  (lambda ()
	    (define-key 'leader (kbd "c a") 'lsp-execute-code-action)
	    (define-key 'leader (kbd "K") 'lsp-describe-thing-at-point)))

;; Formatters for typescript (LSP required)
(unless (package-installed-p 'prettier-js)
  (package-install 'prettier-js))
(require 'prettier-js)
(add-hook 'typescript-mode-hook
	  (lambda ()
	    (define-key 'leader (kbd "f e") 'lsp-eslint-fix-all)
	    (define-key 'leader (kbd "f p") 'prettier-js)))
;; Formatters for python
(unless (package-installed-p 'python-black)
  (package-install 'python-black))
(add-hook 'python-mode-hook 'python-black-on-save-mode)


;;; General settings
;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set font
(set-face-attribute 'default nil :height 140)

;; Enable line numbers
(global-display-line-numbers-mode)

;; Disable backup files
(setq make-backup-files nil)

;; Enable downcasing and upcasing text
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set theme
(unless (package-installed-p 'kanagawa-theme)
  (package-install 'kanagawa-theme))
(load-theme 'kanagawa t)