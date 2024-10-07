(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defun ar/require(hook)
  (unless (package-installed-p hook)
    (package-install hook))
  (require hook))

(require 'project)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(ido-mode 1)
(ido-everywhere 1)
(global-display-line-numbers-mode 1)
(recentf-mode 1)

(ar/require 'base16-theme)
(load-theme 'base16-default-dark t)

(setq-default inhibit-splash-screen t)
(setq-default make-backup-files nil)
(setq-default dired-dwim-target t)
(setq-default ring-bell-function 'ignore)
(setq-default electric-indent-mode nil
              indent-tabs-mode nil
              tab-width 4)

(defvar my-font "CommitMono")
(defvar my-font-height 160)
(set-face-attribute 'default nil :font my-font :height my-font-height)

(defun ar/format-on-save(mode cmd)
  (when (eq major-mode mode)
    (shell-command (concat cmd " " (buffer-name))))
  (revert-buffer-quick))

(ar/require 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(defun ar/format-go-on-save()
  (ar/format-on-save 'go-mode "gofmt -w"))
(add-hook 'after-save-hook #'ar/format-go-on-save)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(defun typescript-settings()
  (setq tab-width 2))
(add-hook 'tsx-ts-mode-hook #'typescript-settings)
(add-hook 'typescript-ts-mode-hook #'typescript-settings)
(defun ar/format-with-prettier-on-save()
  (ar/format-on-save 'tsx-ts-mode "prettier -w")
  (ar/format-on-save 'typescript-ts-mode "prettier -w"))
(add-hook 'after-save-hook #'ar/format-with-prettier-on-save)

(defun python-settings()
  (setq tab-width 4))
(setq major-mode-remap-alist '((python-mode . python-ts-mode)))
(add-hook 'python-ts-mode-hook #'python-settings)
(defun ar/format-python-on-save()
  (ar/format-on-save 'python-ts-mode "black"))
(add-hook 'after-save-hook #'ar/format-python-on-save)

(ar/require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
