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
;; Set indent level and start lsp
(add-hook 'js2-mode-hook
	  #'(lambda ()
              (lsp-deferred)
	      (setq js2-basic-offset 2)))

(require 'evil)
;; Set keyboard shortcuts
(add-hook 'js2-mode-hook
          (lambda ()
            (define-key 'leader (kbd "f p") 'prettier-js)
            (define-key 'leader (kbd "f e") 'lsp-eslint-apply-all-fixes)
            ;; evil bindings
            (define-key evil-normal-state-map (kbd "SPC f p") 'prettier-js)
            (define-key evil-normal-state-map (kbd "SPC f e") 'lsp-eslint-apply-all-fixes)))

(require 'prettier-js)
;; uncomment this if you want prettier on save
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

;;; javascript-setup.el ends here

