;;; init-ruby.el --- Ruby configuration
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :custom (ruby-indent-level 2)
  :hook (ruby-mode . lsp-deferred))

(use-package bundler :after ruby-mode)

(use-package rbenv
  :after ruby-mode
  :config
  (global-rbenv-mode))

(use-package ruby-end
  :after ruby-mode
  :hook (ruby-mode . ruby-end-mode))

(use-package inf-ruby :after ruby-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
