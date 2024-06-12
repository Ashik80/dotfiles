;;; package --- Summary
;;; Commentary:
;;; Typescript configuration

;;; Code:
;; Download typescript-mode
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

(require 'typescript-mode)
;; Set indent level
(add-hook 'typescript-mode-hook
	  '(lambda ()
	     (setq typescript-indent-level 2)))
;; Start lsp for typescript
(add-hook 'typescript-mode-hook #'lsp-deferred)

(require 'prettier-js)
;; uncomment this if you want prettier on save
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

(require 'evil)
(evil-define-key 'normal typescript-mode-map (kbd "SPC f p") 'prettier-js)
(evil-define-key 'normal typescript-mode-map (kbd "SPC f e") 'lsp-eslint-apply-all-fixes)

;;; typescript-setup.el ends here

