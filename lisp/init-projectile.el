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
  (projectile-mode t)
  (define-key projectile-command-map (kbd "s s") #'consult-ripgrep))

(provide 'init-projectile)
;;; init-projectile.el ends here
