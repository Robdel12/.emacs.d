;;; init-editorconfig.el --- EditorConfig integration
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(provide 'init-editorconfig)
;;; init-editorconfig.el ends here

