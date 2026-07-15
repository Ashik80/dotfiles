(defun my/disable-themes-before-load (theme &rest _)
  (mapc #'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'my/disable-themes-before-load)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'gruvbox-light-medium t)
(load-theme 'gruvbox-dark-hard t)
;; (load-theme 'kanagawa-wave t)
;; (load-theme 'kanagawa-lotus t)
