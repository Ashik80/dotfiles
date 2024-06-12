;;; package --- Summary
;;; Commentary:
;;; Typescript configuration

;;; Code:
;; Download js2-mode
(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))

(require 'js2-mode)
(add-hook 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
;; Start lsp for typescript
(add-hook 'js2-mode-hook #'lsp-deferred)

(require 'prettier-js)
;; uncomment this if you want prettier on save
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

(require 'evil)
(evil-define-key 'normal js2-mode-map (kbd "SPC f p") 'prettier-js)
(evil-define-key 'normal js2-mode-map (kbd "SPC f e") 'lsp-eslint-apply-all-fixes)

;;; javascript-setup.el ends here

