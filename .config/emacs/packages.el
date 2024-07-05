;;; package -- Summary
;;; Commentary:
;;; All the packages required are registered here

;;; Code:
;; Disable signature verification for package archives
(setq package-check-signature nil)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; (package-refresh-contents)

;; Download evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
;; Download undo-fu (evil dependency)
(unless (package-installed-p 'undo-fu)
  (package-install 'undo-fu))
;; Download vterm
(unless (package-installed-p 'vterm)
  (package-install 'vterm))
;; Download Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
;; Download recentf
(unless (package-installed-p 'recentf)
  (package-install 'recentf))
;; Download company
(unless (package-installed-p 'company)
  (package-install 'company))
;; Download ivy
(unless (package-installed-p 'ivy)
  (package-install 'ivy))
;; Download flycheck
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
;; Download lsp-mode
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
;; Download lsp-ui
(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))
;; Download yasnippet (lsp-mode dependency)
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
;; Download emojify
(unless (package-installed-p 'emojify)
  (package-install 'emojify))
;; Download prettier-js
(unless (package-installed-p 'prettier-js)
  (package-install 'prettier-js))
;; Download magit
(unless (package-installed-p 'ghub)
  (package-install 'ghub))
(unless (package-installed-p 'treepy)
  (package-install 'treepy))
(unless (package-installed-p 'magit)
  (package-install 'magit))
;; Download doom modeline
(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))
;; Download toggle term
(unless (package-installed-p 'toggle-term)
  (package-install 'toggle-term))
;; Download git gutter
(unless (package-installed-p 'git-gutter)
  (package-install 'git-gutter))

;;; packages.el ends here

