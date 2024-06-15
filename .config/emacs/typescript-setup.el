;;; package --- Summary
;;; Commentary:
;;; Typescript configuration

;;; Code:
;; Download typescript-mode
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

(require 'typescript-mode)
(require 'prettier-js)
;; Set indent level and start lsp
(add-hook 'typescript-mode-hook
	  #'(lambda ()
              (lsp-deferred)
	     (setq typescript-indent-level 2)))

;; Set keyboard shortcuts
(add-hook 'typescript-mode-hook
          (lambda ()
            (define-key 'c-l (kbd "f p") 'prettier-js)
            (define-key 'c-l (kbd "f e") 'lsp-eslint-apply-all-fixes)))

;; uncomment this if you want prettier on save
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

;;; typescript-setup.el ends here

