;;; package --- Summary
;;; Commentary:
;;; All the lsp related configurations are here

;;; Code:
(require 'lsp-mode)
(require 'evil)

(evil-define-key 'normal lsp-mode-map
  (kbd "SPC g d") 'evil-goto-definition
  (kbd "SPC c a") 'lsp-execute-code-action
  (kbd "SPC g r") 'xref-find-references)

;; Enable lsp ui
(require 'lsp-ui)
(lsp-ui-mode 1)

;; hover information
(evil-define-key 'normal lsp-mode-map (kbd "K") #'lsp-describe-thing-at-point)

(define-key lsp-ui-mode-map [remap evil-goto-definition] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; Disbale annoying highlighting of word under cursor
(setq lsp-enable-symbol-highlighting nil)
;; Disable breadcrumbs
(setq lsp-headerline-breadcrumb-enable nil)

;;; lsp-setup.el ends here

