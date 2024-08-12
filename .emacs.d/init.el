(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell flymake-eslint rainbow-mode python-black prettier-js magit git-gutter)))
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
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))


;; Treesitter settings
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (sh-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (scss-mode . css-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        (js-json-mode . json-ts-mode)))
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

;; Recentf settings
(use-package recentf
  :hook (after-init . recentf-mode)
  :config (define-key 'leader (kbd "f r") 'recentf))

;; Git gutter settings
(use-package git-gutter
  :ensure t
  :hook (after-init . global-git-gutter-mode))

;; Magit settings
(use-package magit
  :ensure t
  :defer t
  :init (define-key 'leader (kbd "B") 'magit-blame))

;; Rainbow mode settings
(use-package rainbow-mode
  :defer t
  :ensure t)

;; Lsp settings
(require 'eglot)
(use-package eglot
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 (when (derived-mode-p 'js-ts-mode 'js-mode)
                                   (local-set-key (kbd "M-.") 'xref-find-definitions)))))
  :config
  (define-key 'leader (kbd "c a") 'eglot-code-actions)
  (define-key 'leader (kbd "r n") 'eglot-rename))

;; Increase GC threshold to improve LSP performace
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
(setq read-process-output-max (* 256 1024)
      eglot-events-buffer-size 0
      eglot-autoshutdown t)

;; Formatters for typescript
(defun my-eslint-config-file ()
  (or (locate-dominating-file (buffer-file-name) ".eslintrc.json")
      (locate-dominating-file (buffer-file-name) ".eslintrc.js")
      (locate-dominating-file (buffer-file-name) ".eslintrc")))
(use-package prettier-js
  :ensure t)
(use-package flymake-eslint
  :ensure t
  :after eglot
  :init (setq flymake-eslint-executable-name "eslint_d")
  :hook (eglot-managed-mode . (lambda ()
                                (when (derived-mode-p 'tsx-ts-mode 'typescript-ts-mode 'js-ts-mode)
                                  (let ((root (my-eslint-config-file)))
                                    (when root (flymake-eslint-enable)))))))
(defun my-eslint-fix-all()
  (interactive)
  (let ((default-directory (my-eslint-config-file)))
    (shell-command (concat "eslint_d --fix " (file-relative-name (buffer-file-name) default-directory)))
    (revert-buffer-quick)))
(defun my-typescript-keybindings ()
  (define-key 'leader (kbd "f e") 'my-eslint-fix-all)
  (define-key 'leader (kbd "f p") 'prettier-js))
(add-hook 'typescript-ts-mode-hook #'my-typescript-keybindings)
(add-hook 'tsx-ts-mode-hook #'my-typescript-keybindings)
;; Formatters for python
(use-package python-black
  :ensure t
  :hook ((python-mode . python-black-on-save-mode)
         (python-ts-mode . python-black-on-save-mode)))


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
(use-package gruber-darker-theme
  :ensure t
  :config (load-theme 'gruber-darker t))

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
                  (newline)
                  (indent-for-tab-command)))
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

;; Turn on ido mode
(use-package ido
  :defer t
  :hook (after-init . ido-mode))
(add-hook 'after-init-hook 'ido-mode)

;; Allow project to recognize directories
(use-package project
  :defer t
  :init (setq project-vc-extra-root-markers '(".projectel")))
(setq project-vc-extra-root-markers '(".projectel"))

;; Open Journal
(defun open-journal()
  "Open journal with the current month."
  (interactive)
  (let ((file-of-month (substring (shell-command-to-string "date +%B,%Y 2>/dev/null") 0 -1)))
    (find-file (concat (getenv "HOME") "/Documents/Journal/" file-of-month))))

(define-key 'leader (kbd "J") 'open-journal)
