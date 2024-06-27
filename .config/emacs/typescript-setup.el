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

(require 'evil)
;; Set keyboard shortcuts
(defun my-formatting-bindings ()
  "Keybindings for typescript/jsx modes."
  ()
  (define-key 'leader (kbd "f p") 'prettier-js)
  (define-key 'leader (kbd "f e") 'lsp-eslint-apply-all-fixes)
  ;; evil bindings
  (define-key evil-normal-state-map (kbd "SPC f p") 'prettier-js)
  (define-key evil-normal-state-map (kbd "SPC f e") 'lsp-eslint-apply-all-fixes))

(add-hook 'typescript-mode-hook 'my-formatting-bindings)
(add-hook 'js-jsx-mode-hook 'my-formatting-bindings)

;; uncomment this if you want prettier on save
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

;;; typescript-setup.el ends here

