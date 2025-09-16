;;; init-projectile.el --- Projectile
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'default)
  :config
  (projectile-mode t))

(provide 'init-projectile)
;;; init-projectile.el ends here
