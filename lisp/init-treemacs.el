;;; init-treemacs.el --- Treemacs
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package treemacs
  :demand t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-project-follow-mode t)
  (treemacs-git-commit-diff-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (setq treemacs-file-follow-delay 0
        treemacs--project-follow-delay 0
        treemacs-user-mode-line-format (list "")
        treemacs-user-header-line-format (list "")))

;; using default treemacs theme for better compatibility

(use-package treemacs-magit
  :demand t
  :after (treemacs magit))

(use-package treemacs-projectile
  :demand t
  :after (treemacs projectile))

;; modern icon theme for treemacs
(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme 'nerd-icons))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
