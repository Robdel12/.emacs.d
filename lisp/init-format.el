;;; init-format.el --- Unified on-save formatting via Apheleia
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package apheleia
  :defer t
  :init
  ;; Prefer project-local formatters when available
  (setq apheleia-log-only-errors t)
  :config
  ;; Biome for JS/TS/JSON/CSS
  (setf (alist-get 'biome apheleia-formatters)
        '("biome" "format" "--stdin-file-path" filepath))

  ;; Other formatters
  (setf (alist-get 'rubocop apheleia-formatters)
        '("bundle" "exec" "rubocop" "--auto-correct" "--stdin" filepath "--format" "files"))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-q" "-"))
  (setf (alist-get 'mix-format apheleia-formatters)
        '("mix" "format" "-"))

  ;; Mode associations
  (dolist (entry '((web-mode . biome)
                   (json-mode . biome)
                   (css-mode . biome)
                   (scss-mode . biome)
                   (ruby-mode . rubocop)
                   (python-mode . black)
                   (elixir-mode . mix-format)
                   (heex-ts-mode . mix-format)))
    (setf (alist-get (car entry) apheleia-mode-alist)
          (cdr entry)))

  ;; Turn on globally
  (apheleia-global-mode +1))

(provide 'init-format)
;;; init-format.el ends here

