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

(with-eval-after-load 'flycheck
  (flycheck-define-checker ruby-reek
    "A Ruby smell checker using Reek."
    :command ("bundle" "exec" "reek" "--format" "json" source)
    :error-parser flycheck-parse-json
    :error-filter (lambda (errors)
                    (seq-map (lambda (err)
                               (setf (flycheck-error-level err) 'warning)
                               err)
                             errors))
    :modes ruby-mode)

  (add-to-list 'flycheck-checkers 'ruby-reek)

  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq flycheck-ruby-rubocop-executable "bundle")
              (setq flycheck-ruby-rubocop-command '("bundle" "exec" "rubocop"))
              (flycheck-select-checker 'ruby-rubocop)
              (flycheck-add-next-checker 'ruby-rubocop 'ruby-reek)
              (flycheck-mode))))

(provide 'init-ruby)
;;; init-ruby.el ends here
