;;; init-elixir.el --- Elixir and Phoenix development
;;; Commentary:
;;; Code:
(require 'use-package)

;; Core Elixir mode
(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :hook ((elixir-mode . flymake-mode)))

;; HEEx templates for Phoenix LiveView
(use-package heex-ts-mode
  :mode "\\.heex\\'"
  :hook ((heex-ts-mode . flymake-mode)))

;; Mix build tool integration
(use-package mix
  :hook (elixir-mode . mix-minor-mode)
  :bind (:map elixir-mode-map
         ("C-c m c" . mix-compile)
         ("C-c m t" . mix-test)
         ("C-c m T" . mix-test-current-test)
         ("C-c m f" . mix-test-current-buffer)
         ("C-c m l" . mix-last-command)
         ("C-c m x" . mix-execute-task)))

;; NOTE: inf-elixir and exunit packages were removed from MELPA.
;; The mix package above provides basic test running via mix-test commands.
;; For REPL, use M-x shell and run iex manually, or consider elixir-ts-mode
;; which is built into Emacs 29+ with tree-sitter.

(provide 'init-elixir)
;;; init-elixir.el ends here
