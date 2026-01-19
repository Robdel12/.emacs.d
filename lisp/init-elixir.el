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

;; Interactive Elixir REPL
(use-package inf-elixir
  :bind (:map elixir-mode-map
         ("C-c C-z" . inf-elixir-project)
         ("C-c C-c" . inf-elixir-send-line)
         ("C-c C-r" . inf-elixir-send-region)
         ("C-c C-b" . inf-elixir-send-buffer)))

;; ExUnit test runner with compilation-mode integration
(use-package exunit
  :hook (elixir-mode . exunit-mode)
  :bind (:map elixir-mode-map
         ("C-c e a" . exunit-verify-all)
         ("C-c e s" . exunit-verify-single)
         ("C-c e r" . exunit-rerun)))

(provide 'init-elixir)
;;; init-elixir.el ends here
