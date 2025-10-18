;;; init-editorconfig.el --- EditorConfig integration
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1)
  ;; Disable editorconfig for remote files to avoid TRAMP timeouts
  (add-to-list 'editorconfig-exclude-regexps "^/ssh:"))

(provide 'init-editorconfig)
;;; init-editorconfig.el ends here

