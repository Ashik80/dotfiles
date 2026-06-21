;; -*- lexical-binding: t; -*-
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
(defvar my-font "Iosevka Custom Semi-Extended")
(set-face-attribute 'default nil :font (concat my-font "-17"))
(custom-set-faces
 '(fixed-pitch ((t (:family my-font))))
 '(variable-pitch ((t (:family my-font)))))
(setq-default line-spacing 0.1)

;; turn off sound
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

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

;; Make cursor not blink
(setq blink-cursor-mode nil)

;; Org agenda configuration
(setq org-agenda-files '("~/Documents/todo.org"))
(setq org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %I:%M %p"))
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-c o" #'org-agenda))

;; Open splits vertically
;; (setq split-height-threshold 80)
;; (setq split-width-threshold 160)

;; typescript and tsx highlighting with treesitter
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Go mode
(use-package go-mode
  :ensure t
  :mode "\\.go\\'")
(add-hook 'go-mode-hook #'eglot-ensure)

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
  (let* ((root (project-root (project-current t)))
        (buf-name (format "*%s-shell*" (file-name-nondirectory (directory-file-name root))))
        (eat-buf (or (get-buffer buf-name)
                       (eat))))
      (with-current-buffer eat-buf
        (rename-buffer buf-name))
      (switch-to-buffer buf-name)))

(keymap-set project-prefix-map "s" #'my/project-eat)
(keymap-global-set "C-c s" #'eat)

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
  (setq evil-want-minibuffer t)
  (setq evil-undo-system 'undo-redo)
  ;; (setq evil-default-state 'emacs)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
  (define-key evil-motion-state-map (kbd "C-u") #'evil-scroll-up))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Reselect after indent
(defun my/visual-shift-left ()
  "Reselect after indenting left"
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (call-interactively #'evil-shift-left)
        (evil-visual-restore))
    (call-interactively #'evil-shift-left)))
(defun my/visual-shift-right ()
  "Reselect after indeting right"
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (call-interactively #'evil-shift-right)
        (evil-visual-restore))
    (call-interactively #'evil-shift-right)))
(keymap-set evil-normal-state-map "<" #'my/visual-shift-left)
(keymap-set evil-normal-state-map ">" #'my/visual-shift-right)

;; C indentation
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4)))

;; Whitespace characters
(setq whitespace-style '(face trailing tabs tab-mark indentation))
(global-whitespace-mode 1)

;; eslint
(use-package flymake-eslint
  :ensure t
  :config
  (defun my/enable-flymake-eslint ()
    (when (or (locate-dominating-file default-directory ".eslintrc*")
              (locate-dominating-file default-directory "eslint.config.mjs"))
      (flymake-mode 1)
      (flymake-eslint-enable)))
  (add-hook 'eglot-managed-mode-hook #'my/enable-flymake-eslint))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;; copy file paths
(defun my/copy-absolute-path ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied: %s" (buffer-file-name)))
(keymap-global-set "C-c a p" #'my/copy-absolute-path)

(defun my/copy-project-relative-path ()
  "Copy the current file path relative to the project root."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (rel  (file-relative-name buffer-file-name root)))
    (kill-new rel)
    (message "Copied: %s" rel)))
(keymap-global-set "C-c c p" #'my/copy-project-relative-path)

;; Prettier
(defun my/prettier-format ()
  "Format current buffer using Prettier."
  (interactive)
  (when (and buffer-file-name
             (executable-find "prettier"))
    (let ((p (point)))
      (shell-command-on-region
       (point-min)
       (point-max)
       (format "prettier --stdin-filepath %s"
               (shell-quote-argument buffer-file-name))
       (current-buffer)
       t)
      (goto-char p))))

(add-hook 'typescript-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'my/prettier-format nil t)))
(add-hook 'tsx-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'my/prettier-format nil t)))

;; Indents
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Disable evil in eat buffers
(add-hook 'eat-mode-hook #'evil-emacs-state)

;; Copy buffer name
(defun my/copy-buffer-name ()
  "Copy the current buffer name to the clipboard and display a message."
  (interactive)
  (kill-new (buffer-name))
  (message "Copied buffer name: %s" (buffer-name)))
(keymap-global-set "C-c c b" #'my/copy-buffer-name)

;; Python formatter with black
(defun my/black-format ()
  "Format current buffer using Black via uv."
  (interactive)
  (when (and buffer-file-name
             (executable-find "uv"))
    (let ((p (point)))
      (shell-command-on-region
       (point-min)
       (point-max)
       (format "uv run black --quiet --stdin-filename %s -"
               (shell-quote-argument buffer-file-name))
       (current-buffer)
       t)
      (goto-char p))))
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/black-format nil t)))

;; Rust mode
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

;; Eglot setup
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio"))))
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)

;; increase lsp performance
(setq eglot-events-buffer-size 0)
(setq eglot-send-changes-idle-time 0.5)
(setq eglot-ignored-server-capabilities
      '(:documentHighlightProvider
        :inlayHintProvider))

;; Recognize submodules as separate projects
(setq project-vc-merge-submodules nil)

;; Git blame
(defun my/git-blame-file()
  "Blame current file with colors"
  (interactive)
  (async-shell-command (concat "git-blame-colored " (buffer-name))))

(defun my/git-blame-region()
  "Blame region with colors"
  (interactive)
  (let ((region-start (number-to-string (line-number-at-pos (region-beginning))))
        (region-finish (number-to-string (- (line-number-at-pos (region-end)) 1))))
    (async-shell-command (concat "git-blame-colored " (buffer-name) " -L" region-start "," region-finish))))

(defun my/git-blame-dwim()
  (interactive)
  (if (use-region-p)
      (my/git-blame-region)
    (my/git-blame-file)))

(keymap-global-set "C-c g b" #'my/git-blame-dwim)

;; Run kot in project root
(defun my/project-kot-compile-dwim ()
  "Run kot in project root in compile mode"
  (interactive)
  (let ((compile-command "kot -i "))
    (if-let ((proj (project-current nil)))
        (call-interactively #'project-compile)
      (call-interactively #'compile))))
(keymap-global-set "C-c f g" #'my/project-kot-compile-dwim)

;; Launch project AI shell
(defun my/launch-ai-shell ()
  "Launches a shell named with suffix -shell-AI for a project"
  (interactive)
  (let* ((proj (project-root (project-current t)))
         (buf-name (format "*%s-shell-AI*" (file-name-nondirectory (directory-file-name proj))))
         (eat-buf (or (get-buffer buf-name)
                      (eat))))
    (with-current-buffer eat-buf
      (rename-buffer buf-name))
    (switch-to-buffer buf-name)))

;; SQL functions
(defvar my/pgsql-selected-db nil
  "The PostgreSQL database to run query on")

(defun my/list-pgsql-databases()
  "Interactively select a PostgreSQL to run query on"
  (interactive)
  (let* ((output (shell-command-to-string "PGPASSWORD=postgres psql -U postgres -c 'select datname from pg_database' | head -n -2 | tail -n +3 | sed 's/^\\s\\+//'"))
        (items (split-string output "\n" t)))
    (setq my/pgsql-selected-db (completing-read "Select a database: " items nil t))))

(defun my/compile-sql-file()
  "Compiles the PostgreSQL file to show data in compilation buffer"
  (interactive)
  (let* ((file (buffer-name))
         (compile-command (format "PGPASSWORD=postgres psql -U postgres -d %s -f %s" my/pgsql-selected-db file)))
    (compile compile-command)))
(with-eval-after-load 'sql
  (keymap-set sql-mode-map "C-c l" #'my/list-pgsql-databases)
  (keymap-set sql-mode-map "C-c b" #'my/compile-sql-file))

;; [MANZIL] Launch Platform Be
(defun my/project-run-command-in-eat (name command)
  (let ((eat-buf (or (get-buffer name)
                     (eat)))
        (buf-name name))
    (with-current-buffer eat-buf
      (rename-buffer name))
    (run-with-timer
     0.5 nil
     (lambda ()
       (let ((proc (get-buffer-process buf-name)))
         (eat--send-string proc (concat command "\n")))))))

(defun my/launch-platform-be()
  "Launches necessary commands for platform-be in different shells"
  (interactive)
  (let ((default-directory "~/src/ManzilApp/platform-be"))
    (my/project-run-command-in-eat "*platform-be-shell*" "uv run ./scripts.sh serve")
    (my/project-run-command-in-eat "*platform-be-shell-queue*" "./scripts.sh sls:queue:serve")
    (my/project-run-command-in-eat "*platform-be-shell-sls*" "cd .serverless && nvm use && cd - && ./scripts.sh sls:serve")))

(defun my/launch-manzil-be()
  "Launches necessary commands for manzil-mobile-be in different shells"
  (interactive)
  (let ((default-directory "~/src/ManzilApp/manzil"))
    (my/project-run-command-in-eat "*manzil-shell*" "nvm use 18 && npm run be:serve")
    (my/project-run-command-in-eat "*manzil-shell-local*" "npm run init-local-s3")))
