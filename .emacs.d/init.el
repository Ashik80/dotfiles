;; Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; initialize package
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; font settings
(set-face-attribute 'default nil :font "Iosevka Custom Condensed-17")
(setq-default line-spacing 0.1)

;; hide menu and tool
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; hide line wrap icons
(setq-default fringe-indicator-alist nil)
(let ((table (or standard-display-table (make-display-table))))
  (set-display-table-slot table 'wrap ?\ )
  (set-display-table-slot table 'truncation ?\ )
  (setq standard-display-table table))

;; typescript and tsx highlighting with treesitter
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; lsp
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)

;; this package is need so that emacs can read variables from PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; autocompletion
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(setq corfu-auto t)
(setq corfu-auto-delay 0.2)
(setq corfu-auto-prefix 3)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((eglot (styles basic)))))

;; AI autocomplete
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)))
(setq-default copilot--indent-warning-printed-p t)
(setq copilot-indent-offset-warning-disable t)

;; increase lsp performance
(setq eglot-events-buffer-size 0)
(setq eglot-send-changes-idle-time 0.5)
(setq eglot-ignored-server-capabilities
      '(:documentHighlightProvider
        :inlayHintProvider))

;; completion mode for commands
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; a better terminal that supports interactive programs
(when (and (not (locate-library "eat"))
           (fboundp 'package-vc-install))
  (package-vc-install "https://codeberg.org/akib/emacs-eat.git"))
(require 'eat)

(defun my/project-eat ()
  "Open or switch to an Eat terminal for this project."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (buf-name (format "*%s-shell*" (file-name-nondirectory (directory-file-name root)))))
    (if (get-buffer buf-name)
	(switch-to-buffer buf-name)
      (let ((eat-buf (save-window-excursion (eat))))
        (with-current-buffer eat-buf
          (rename-buffer buf-name))
        (switch-to-buffer buf-name)))))

(keymap-set project-prefix-map "s" #'my/project-eat)

;; auto read changes
(global-auto-revert-mode 1)

;; globally enable line numbers
(global-display-line-numbers-mode 1)
(add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))

;; display git changes in gutter
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

;; git client
(use-package magit
  :ensure t)

;; theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

;; keep changes in specific directory
(setq make-backup-files t)
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups")))
(setq auto-save-default nil)

;; evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-default-state 'emacs)
  :config
  (evil-mode 1))

;; C indentation
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4)))

;; Whitespace characters
(setq whitespace-style '(face trailing tabs tab-mark indentation))
(global-whitespace-mode 1)
