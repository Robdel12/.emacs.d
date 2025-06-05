;;; init-treesit.el --- Tree-sitter integration
;;; Commentary:
;;; Code:
(require 'use-package)

;; Use treesit-auto to enable built-in tree-sitter modes when available
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'init-treesit)
;;; init-treesit.el ends here
