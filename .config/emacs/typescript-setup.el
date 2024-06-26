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

(require 'js2-mode)
(add-hook 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))
(add-hook 'js-jsx-mode-hook
	  #'(lambda ()
              (lsp-deferred)
	      (setq js2-basic-offset 2)))

;; Set keyboard shortcuts
(add-hook 'typescript-mode-hook
          (lambda ()
            (define-key 'leader (kbd "f p") 'prettier-js)
            (define-key 'leader (kbd "f e") 'lsp-eslint-apply-all-fixes)))

;; uncomment this if you want prettier on save
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

;;; typescript-setup.el ends here

