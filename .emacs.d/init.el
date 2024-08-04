(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell flymake-eslint rainbow-mode treesit-auto python-black prettier-js magit corfu git-gutter)))
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
;; Exec path from shell
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Treesitter settings
(unless (package-installed-p 'treesit-auto)
  (package-install 'treesit-auto))
(require 'treesit-auto)
(setq treesit-auto-install 'prompt)
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode)

(setq treesit-font-lock-level 4)

;; Typescript mode settings
(defvar tsjs-dirs-to-ignore "node_modules,.git,build,.build,.next")
(defun my-ts-config()
  (setq tab-width 2)
  (setq grep-command (concat "grep -Rin --exclude-dir={" tsjs-dirs-to-ignore "} --exclude=tsconfig.tsbuildinfo ")))
(add-hook 'typescript-ts-mode-hook #'my-ts-config)
(add-hook 'tsx-ts-mode-hook #'my-ts-config)

;; Javascript mode settings
(defvar js-indent-level)
(defun my-js-config()
  (setq tab-width 2)
  (setq js-indent-level 2)
  (setq grep-command (concat "grep -Rin --exclude-dir={" tsjs-dirs-to-ignore "} ")))
(add-hook 'js-mode-hook #'my-js-config)
(add-hook 'js-ts-mode-hook #'my-js-config)

;; Python mode settings
(defvar python-dirs-to-ignore "__pycache__,.git")
(defun my-python-config()
  (setq tab-width 4)
  (setq grep-command (concat "grep -Rin --exclude-dir={" python-dirs-to-ignore "} ")))
(add-hook 'python-mode-hook #'my-python-config)
(add-hook 'python-ts-mode-hook #'my-python-config)

;; Rust settings
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (setq tab-width 4)))

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
(add-hook 'after-save-hook 'corfu-quit)

;; Lsp settings
(require 'eglot)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure) ;; Enable lsp for typescript
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(add-hook 'js-mode-hook 'eglot-ensure) ;; Enable lsp for javascript
(add-hook 'js-ts-mode-hook 'eglot-ensure)

(add-hook 'python-mode-hook 'eglot-ensure) ;; Enable lsp for python
(add-hook 'python-ts-mode-hook 'eglot-ensure)

(add-hook 'rust-ts-mode-hook 'eglot-ensure) ;; Enable lsp for rust

;; Keybinds
(add-hook 'eglot-managed-mode-hook
	  (lambda ()
	    (define-key 'leader (kbd "c a") 'eglot-code-actions)
	    (define-key 'leader (kbd "r n") 'eglot-rename)))

;; Increase GC threshold to improve LSP performace
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      eglot-events-buffer-size 0
      eglot-autoshutdown t)

;; Formatters for typescript
(unless (package-installed-p 'prettier-js)
  (package-install 'prettier-js))
(require 'prettier-js)
(unless (package-installed-p 'flymake-eslint)
  (package-install 'flymake-eslint))
(require 'flymake-eslint)
(setq flymake-eslint-executable-name "eslint_d")
(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (when (derived-mode-p 'tsx-ts-mode 'typescript-ts-mode 'js-ts-mode)
                                       (flymake-eslint-enable))))
(defun my-eslint-fix-all()
  (interactive)
  (let ((current-file-name (buffer-file-name)))
    (shell-command (concat "eslint_d --fix " current-file-name))
    (revert-buffer-quick)))
(defun my-typescript-keybindings ()
  (define-key 'leader (kbd "f e") 'my-eslint-fix-all)
  (define-key 'leader (kbd "f p") 'prettier-js))
(add-hook 'typescript-ts-mode-hook #'my-typescript-keybindings)
(add-hook 'tsx-ts-mode-hook #'my-typescript-keybindings)
;; Formatters for python
(unless (package-installed-p 'python-black)
  (package-install 'python-black))
(add-hook 'python-mode-hook 'python-black-on-save-mode)
(add-hook 'python-ts-mode-hook 'python-black-on-save-mode)


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
(defface markdown-code-face
  `((t :font ,my-font :height ,my-font-height))
  "Face for markdown code blocks."
  :group 'markdown-faces)
(set-face-attribute 'markdown-code-face nil :font my-font :height my-font-height)

;; Enable line numbers
(global-display-line-numbers-mode)

;; Disable backup files
(setq make-backup-files nil)

;; Enable downcasing and upcasing text
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Enable narrow region
(put 'narrow-to-region 'disabled nil)

;; Set theme
(unless (package-installed-p 'kanagawa-theme)
  (package-install 'kanagawa-theme))
(require 'kanagawa-theme)
(setq kanagawa-theme-comment-italic nil
      kanagawa-theme-keyword-italic nil)
(load-theme 'kanagawa t)
(set-face-italic 'font-lock-string-face nil)

;; Default tab width
(setq tab-width 4)

;; Convert tab to spaces
(setq-default indent-tabs-mode nil)

;; Grep command settings
(setq-default grep-command "grep -Rin ")

;; Stop blinking cursor
(blink-cursor-mode 0)

;; Keybind for newlines
(global-set-key (kbd "M-o")
                (lambda ()
                  (interactive)
                  (move-end-of-line 1)
                  (newline)))
(global-set-key (kbd "M-O")
                (lambda ()
                  (interactive)
                  (back-to-indentation)
                  (newline)
                  (indent-for-tab-command)
                  (forward-line -1)
                  (indent-for-tab-command)))

;; Copy/Move files to another split dired
(setq dired-dwim-target t)

;; Disable all sounds
(setq ring-bell-function 'ignore)

;; Disable annoying electric indent
(setq electric-indent-mode nil)

;; Open Journal
(defun open-journal()
  "Open journal with the current month."
  (interactive)
  (let ((file-of-month (substring (shell-command-to-string "date +%B,%Y 2>/dev/null") 0 -1)))
    (find-file (concat (getenv "HOME") "/Documents/Journal/" file-of-month))))

(define-key 'leader (kbd "J") 'open-journal)
