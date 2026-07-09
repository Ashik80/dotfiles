;; Eat terminal
(when (and (not (locate-library "eat"))
           (fboundp 'package-vc-install))
  (package-vc-install "https://codeberg.org/akib/emacs-eat.git"))
(require 'eat)

(defun my/project-eat ()
  "Open or switch to an Eat terminal for this project."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (buf-name (format "*%s-shell*" (file-name-nondirectory (directory-file-name root))))
         (eat-buf (or (get-buffer buf-name)
                      (eat))))
    (with-current-buffer eat-buf
      (rename-buffer buf-name))
    (switch-to-buffer buf-name)))

(keymap-set project-prefix-map "s" #'my/project-eat)
(keymap-global-set "C-c s" #'eat)

(with-eval-after-load 'eat
  (define-key eat-semi-char-mode-map (kbd "M-<left>")  (kbd "M-b"))
  (define-key eat-semi-char-mode-map (kbd "M-<right>") (kbd "M-f")))

(add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode 0)))

;; evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-default-state 'emacs)
  :config
  ;; (evil-mode 1)
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

;; Disable evil in eat buffers
(add-hook 'eat-mode-hook #'evil-emacs-state)

;; Launch project AI shell
(defun my/launch-ai-shell ()
  "Launches a shell named with suffix -shell-AI for a project"
  (interactive)
  (let* ((proj (project-root (project-current t)))
         (default-directory proj)
         (buf-name (format "*%s-shell-AI*" (file-name-nondirectory (directory-file-name proj))))
         (eat-buf (or (get-buffer buf-name)
                      (eat))))
    (with-current-buffer eat-buf
      (rename-buffer buf-name))
    (switch-to-buffer buf-name)))

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

;; [MANZIL] Launch Platform Be
(defun my/project-run-command-in-ghostel (name command)
  (let* ((ghostel-buffer-name name)
         (buf (or (get-buffer ghostel-buffer-name)
                  (ghostel))))
    (ghostel-send-string (concat command "\n"))))

(defun my/launch-platform-be()
  "Launches necessary commands for platform-be in different shells"
  (interactive)
  (let ((default-directory "~/src/ManzilApp/platform-be"))
    (my/project-run-command-in-ghostel "*platform-be-shell*" "uv run ./scripts.sh serve")
    (my/project-run-command-in-ghostel "*platform-be-shell-queue*" "./scripts.sh sls:queue:serve")
    (my/project-run-command-in-ghostel "*platform-be-shell-sls*" "cd .serverless && nvm use && cd - && ./scripts.sh sls:serve")))

(defun my/launch-manzil-be()
  "Launches necessary commands for manzil-mobile-be in different shells"
  (interactive)
  (let ((default-directory "~/src/ManzilApp/manzil"))
    (my/project-run-command-in-ghostel "*manzil-shell*" "nvm use 18 && npm run be:serve")
    (my/project-run-command-in-ghostel "*manzil-shell-local*" "npm run init-local-s3")))
