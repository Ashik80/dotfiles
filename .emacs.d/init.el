(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-mode treesit-auto python-black prettier-js lsp-pyright magit corfu lsp-ui flycheck lsp-mode typescript-mode git-gutter)))
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
;; Treesitter settings
(unless (package-installed-p 'treesit-auto)
  (package-install 'treesit-auto))
(require 'treesit-auto)
(setq treesit-auto-install 'prompt)
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode)

;; Typescript mode settings
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))
(require 'typescript-mode)
(defvar tsjs-dirs-to-ignore "node_modules,.git,build,.build,.next")
(add-hook 'typescript-mode-hook
	  (lambda ()
	    (setq typescript-indent-level 2)
	    (setq grep-command (concat "grep -Rin --exclude-dir={" tsjs-dirs-to-ignore "} --exclude=tsconfig.tsbuildinfo "))))
(add-hook 'typescript-ts-mode-hook
	  (lambda ()
	    (setq grep-command (concat "grep -Rin --exclude-dir={" tsjs-dirs-to-ignore "} --exclude=tsconfig.tsbuildinfo "))))
(add-hook 'tsx-ts-mode-hook
	  (lambda ()
	    (setq grep-command (concat "grep -Rin --exclude-dir={" tsjs-dirs-to-ignore "} --exclude=tsconfig.tsbuildinfo "))))

;; Javascript mode settings
(defvar js-indent-level)
(add-hook 'js-mode-hook
	  (lambda ()
	    (setq js-indent-level 2)
	    (setq grep-command (concat "grep -Rin --exclude-dir={" tsjs-dirs-to-ignore "} "))))
(add-hook 'js-ts-mode-hook
	  (lambda ()
	    (setq js-indent-level 2)
	    (setq grep-command (concat "grep -Rin --exclude-dir={" tsjs-dirs-to-ignore "} "))))

;; Python mode settings
(defvar python-dirs-to-ignore "__pycache__,.git")
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq grep-command (concat "grep -Rin --exclude-dir={" python-dirs-to-ignore "} "))))
(add-hook 'python-ts-mode-hook
	  (lambda ()
	    (setq grep-command (concat "grep -Rin --exclude-dir={" python-dirs-to-ignore "} "))))

;; Ivy settings
(unless (package-installed-p 'ivy)
  (package-install 'ivy))
(require 'ivy)
(ivy-mode)

;; Recentf settings
(recentf-mode)
(define-key 'leader (kbd "f r") 'recentf)

;; Git gutter settings
(unless (package-installed-p 'git-gutter)
  (package-install 'git-gutter))
(require 'git-gutter)
(global-git-gutter-mode)

;; Flycheck settings
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(require 'flycheck)
(global-flycheck-mode)

;; Magit settings
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
(define-key 'leader (kbd "B") 'magit-blame)

;; Rainbow mode settings
(unless (package-installed-p 'rainbow-mode)
  (package-install 'rainbow-mode))

;; Completion settings
(unless (package-installed-p 'corfu)
  (package-install 'corfu))
(require 'corfu)
(setq corfu-auto t
      corfu-cycle t
      corfu-auto-prefix 1
      corfu-auto-delay 0.8
      corfu-preview-current 'insert
      corfu-on-exact-match 'nil
      corfu-quit-no-match t)
(global-corfu-mode)
(declare-function corfu-popupinfo-mode "corfu")
(corfu-popupinfo-mode)
(setq global-corfu-modes '((not fundamental-mode text-mode) t))
(add-hook 'after-save-hook 'corfu-quit)

;; Lsp settings
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))
(require 'lsp-ui)
(lsp-ui-mode)

(require 'lsp-mode)
(setq lsp-enable-symbol-highlighting nil) ;; Disbale annoying highlighting of word under cursor

;;; To make node work either uncomment this or make soft links for node and npm
;; (defvar node-version "20.15.1") ;; Set node version
;; (let ((node-path (concat (getenv "HOME") "/.nvm/versions/node/v" node-version "/bin")))
;;   (setq exec-path (append exec-path (list node-path)))
;;   (setenv "PATH" (concat node-path ":" (getenv "PATH"))))
;;; Or ...
;; sudo ln -s $(which node) /usr/bin/node
;; sudo ln -s $(which npm) /usr/bin/npm

(add-hook 'typescript-mode-hook 'lsp-deferred) ;; Enable lsp for typescript
(add-hook 'typescript-ts-mode-hook 'lsp-deferred)
(add-hook 'tsx-ts-mode-hook 'lsp-deferred)

(add-hook 'js-mode-hook 'lsp-deferred) ;; Enable lsp for javascript
(add-hook 'js-ts-mode-hook 'lsp-deferred)

(unless (package-installed-p 'lsp-pyright) ;; Install reqruired pyright client
  (package-install 'lsp-pyright))
(add-hook 'python-mode-hook 'lsp-deferred) ;; Enable lsp for python
(add-hook 'python-ts-mode-hook 'lsp-deferred)

(add-hook 'rust-ts-mode-hook 'lsp-deferred) ;; Enable lsp for rust

;; Keybinds
(add-hook 'lsp-mode-hook
	  (lambda ()
	    (define-key 'leader (kbd "c a") 'lsp-execute-code-action)
	    (define-key 'leader (kbd "K") 'lsp-describe-thing-at-point)
	    (define-key 'leader (kbd "r n") 'lsp-rename)))

;; Formatters for typescript (LSP required)
(unless (package-installed-p 'prettier-js)
  (package-install 'prettier-js))
(require 'prettier-js)
(add-hook 'typescript-mode-hook
	  (lambda ()
	    (define-key 'leader (kbd "f e") 'lsp-eslint-fix-all)
	    (define-key 'leader (kbd "f p") 'prettier-js)))
(add-hook 'typescript-ts-mode-hook
	  (lambda ()
	    (define-key 'leader (kbd "f e") 'lsp-eslint-fix-all)
	    (define-key 'leader (kbd "f p") 'prettier-js)))
(add-hook 'tsx-ts-mode-hook
	  (lambda ()
	    (define-key 'leader (kbd "f e") 'lsp-eslint-fix-all)
	    (define-key 'leader (kbd "f p") 'prettier-js)))
;; Formatters for python
(unless (package-installed-p 'python-black)
  (package-install 'python-black))
(add-hook 'python-mode-hook 'python-black-on-save-mode)
(add-hook 'python-ts-mode-hook 'python-black-on-save-mode)

;; Increase GC threshold to improve LSP performace
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1)


;;; General settings
;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Disbable tool bar
(tool-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set font
(defvar my-font "IosevkaTerm Nerd Font")
(defvar my-font-height 160)
(set-face-attribute 'default nil :font my-font :height my-font-height)
(set-face-attribute 'markdown-code-face nil :font my-font :height my-font-height)

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
(require 'kanagawa-theme)
(setq kanagawa-theme-comment-italic nil
      kanagawa-theme-keyword-italic nil)
(load-theme 'kanagawa t)
(set-face-italic 'font-lock-string-face nil)

;; Convert tab to spaces
(setq-default indent-tabs-mode nil)

;; Grep command settings
(setq-default grep-command "grep -Rin ")

;; Stop blinking cursor
(blink-cursor-mode 0)

;; Open Journal
(defun open-journal()
  "Open journal with the current month."
  (interactive)
  (let ((file-of-month (substring (shell-command-to-string "date +%B,%Y 2>/dev/null") 0 -1)))
    (find-file (concat (getenv "HOME") "/Documents/Journal/" file-of-month))))

(define-key 'leader (kbd "J") 'open-journal)
