;; Evil mode settings
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode)
(setq evil-insert-state-cursor nil)     ; Use block cursor
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
