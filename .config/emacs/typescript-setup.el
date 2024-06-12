;;; package --- Summary
;;; Commentary:
;;; Typescript configuration

;;; Code:
;; Download typescript-mode
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

(require 'typescript-mode)
;; Start lsp for typescript
(add-hook 'typescript-mode-hook #'lsp-deferred)

;;; typescript-setup.el ends here

