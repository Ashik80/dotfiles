;;; package --- Summary
;;; Commentary:
;;; Settings for python

;;; Code:
;; Download python
(unless (package-installed-p 'python-mode)
  (package-install 'python-mode))
(require 'python-mode)

;; Download lsp-pyright
(unless (package-installed-p 'lsp-pyright)
  (package-install 'lsp-pyright))

(require 'lsp-pyright)
;; Start lsp for python
(add-hook 'python-mode-hook #'lsp-deferred)

;;; python-setup.el ends here

