;;; package --- Summary
;;; Commentary:
;;; Add searching for patterns functionality
;;; Code:
;; Customize grep

(require 'projectile)
(defvar exclude-dirs-grep "{node_modules,.git,__pycache__,.next,build,.build,dist,target}")
(defvar exclude-files-grep "{yarn.lock,package-lock.json}")
(defun my-grep-whole-project(search-term base-directory)
  "Search the whole project for SEARCH-TERM and BASE-DIRECTORY."
  (interactive "sSearch Expression: \nsBase Directory:")
  (projectile-with-default-dir (projectile-project-root)
    (grep (concat "grep --exclude=" exclude-files-grep " --exclude-dir=" exclude-dirs-grep " --color -Ri -nH --null -e \"" search-term "\" " base-directory))))
(defun my-grep-whole-project-full-word(search-term base-directory)
  "Search the whole project for SEARCH-TERM and BASE-DIRECTORY."
  (interactive "sSearch Word: \nsBase Directory:")
  (projectile-with-default-dir (projectile-project-root)
    (grep (concat "grep --exclude=" exclude-files-grep " --exclude-dir=" exclude-dirs-grep " --color -Ri -w -nH --null -e \"" search-term "\" " base-directory))))

;; Grepping shortcut
(define-key 'c-l (kbd "f g") 'my-grep-whole-project)
(define-key 'c-l (kbd "f w") 'my-grep-whole-project-full-word)

;;; grep-project.el ends here

