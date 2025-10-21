;;; init-editorconfig.el --- EditorConfig integration
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package editorconfig
  :diminish editorconfig-mode
  :custom
  ;; Disable editorconfig for remote files to avoid TRAMP timeouts
  (editorconfig-exclude-regexps '("^/ssh:"))
  :config
  (editorconfig-mode 1))

(provide 'init-editorconfig)
;;; init-editorconfig.el ends here

