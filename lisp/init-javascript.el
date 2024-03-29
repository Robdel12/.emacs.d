;;; init-javascript.el --- JS modes
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package js2-mode
  :custom
  (sgml-basic-offset 2)
  (sgml-attribute-offset 0)
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  (js2-basic-offset 2)
  (js2-strict-missing-semi-warning nil)
  (js2-missing-semi-one-line-override t)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  (js2-mode-strict-inconsistent-return-warning nil)
  (js2-getprop-has-side-effects t))

(use-package js2-refactor
  :hook ((js2-mode . js2-refactor-mode)
         (web-mode . js2-refactor-mode))
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package rjsx-mode
  :pin melpa
  :mode "\\.jsx?\\'")

(use-package js-doc
  :bind (:map js2-refactor-mode-map
         ("C-c C-r i d" . js-doc-insert-function-doc)
         ("@" . js-doc-insert-tag)))

(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
         (web-mode . add-node-modules-path)))

(use-package json-mode)

;; parse node stack traces in compilation buffers
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'node)
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))

(provide 'init-javascript)
;;; init-javascript.el ends here
