;;; pacakge --- Summary:
;;; Commentary:
;;; This file is for configuring rust
;;; Code:
;; Download rust-mode
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

(require 'rust-mode)
;; Start lsp for rust
(add-hook 'rust-mode-hook #'lsp-deferred)

;;; rust-setup.el ends here
