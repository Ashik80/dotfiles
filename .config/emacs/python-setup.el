;;; package --- Summary
;;; Commentary:
;;; Settings for python

;;; Code:
;; Download python
(unless (package-installed-p 'python-mode)
  (package-install 'python-mode))
(unless (package-installed-p 'python-black)
  (package-install 'python-black))

(require 'python-mode)

;; Download lsp-pyright
(unless (package-installed-p 'lsp-pyright)
  (package-install 'lsp-pyright))

(require 'lsp-pyright)
;; Start lsp for python
(add-hook 'python-mode-hook #'lsp-deferred)
;; Hook black formatter
(require 'python-black)
(add-hook 'python-mode-hook #'python-black-on-save-mode)

;;; python-setup.el ends here

