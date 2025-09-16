;;; init-format.el --- Unified on-save formatting via Apheleia
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package apheleia
  :demand t
  :init
  ;; Prefer project-local formatters when available
  (setq apheleia-log-only-errors t)
  :config
  ;; Formatters: prefer prettierd if available, fall back to prettier
  (setf (alist-get 'prettierd apheleia-formatters)
        '("prettierd" filepath))
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'eslint_d apheleia-formatters)
        '("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath))
  (setf (alist-get 'rubocop apheleia-formatters)
        '("bundle" "exec" "rubocop" "--auto-correct" "--stdin" filepath "--format" "files"))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-q" "-"))

  ;; Mode association: try eslint_d for JS/TS if present, else prettierd/prettier
  (dolist (entry '((web-mode . (prettierd prettier))
                   (typescript-ts-mode . (prettierd prettier))
                   (tsx-ts-mode . (prettierd prettier))
                   (js-ts-mode . (eslint_d prettierd prettier))
                   (json-mode . (prettierd prettier))
                   (css-mode . (prettierd prettier))
                   (scss-mode . (prettierd prettier))
                   (yaml-mode . prettier)
                   (markdown-mode . (prettierd prettier))
                   (gfm-mode . (prettierd prettier))
                   (ruby-mode . rubocop)
                   (python-mode . black)))
    (setf (alist-get (car entry) apheleia-mode-alist)
          (cdr entry)))

  ;; Turn on globally
  (apheleia-global-mode +1))

(provide 'init-format)
;;; init-format.el ends here

