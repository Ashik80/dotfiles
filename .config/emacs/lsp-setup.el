;;; package --- Summary
;;; Commentary:
;;; All the lsp related configurations are here

;;; Code:
(require 'lsp-mode)

(add-hook 'lsp-mode-hook
          (lambda ()
            (define-key 'leader (kbd "g d") 'lsp-find-definition)
            (define-key 'leader (kbd "c a") 'lsp-execute-code-action)
            (define-key 'leader (kbd "g r") 'lsp-find-references)
            (define-key 'leader (kbd "K") 'lsp-describe-thing-at-point)))

;; Enable lsp ui
;; (require 'lsp-ui)
;; (lsp-ui-mode 1)

;; (define-key lsp-ui-mode-map [remap evil-goto-definition] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; Disbale annoying highlighting of word under cursor
(setq lsp-enable-symbol-highlighting nil)
;; Disable breadcrumbs
(setq lsp-headerline-breadcrumb-enable nil)

;;; lsp-setup.el ends here

