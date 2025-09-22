;;; init-javascript.el --- JS modes
;;; Commentary:
;;; Code:
(require 'use-package)

;; required for some shell commands
(setenv "NPM_TOKEN" "")

;; basic formatting
(setq-default
 sgml-basic-offset 2
 sgml-attribute-offset 0
 js-indent-level 2)

(use-package add-node-modules-path
  :hook ((web-mode . add-node-modules-path)
         (js-mode . add-node-modules-path)
         (js-ts-mode . add-node-modules-path)))

;; Enhanced JSON support
(use-package json-mode
  :mode "\\.json\\'"
  :hook (json-mode . flycheck-mode))

;; Prefer Apheleia for formatting; keep prettier-js available for manual use
(use-package prettier-js
  :defer t)

;; NPM script runner and package management
(use-package npm-mode
  :hook ((js-mode . npm-mode)
         (js-ts-mode . npm-mode)
         (web-mode . npm-mode))
  :bind (:map npm-mode-keymap
         ("C-c n r" . npm-mode-npm-run)
         ("C-c n i" . npm-mode-npm-install)
         ("C-c n s" . npm-mode-npm-run-script)
         ("C-c n t" . npm-mode-npm-test)))

;; Node.js REPL integration
(use-package nodejs-repl
  :bind (:map js-mode-map
         ("C-c C-j" . nodejs-repl-send-line)
         ("C-c C-r" . nodejs-repl-send-region)
         ("C-c C-l" . nodejs-repl-load-file)
         ("C-c C-z" . nodejs-repl-switch-to-repl))
  :bind (:map js-ts-mode-map
         ("C-c C-j" . nodejs-repl-send-line)
         ("C-c C-r" . nodejs-repl-send-region)
         ("C-c C-l" . nodejs-repl-load-file)
         ("C-c C-z" . nodejs-repl-switch-to-repl)))

;; parse node stack traces in compilation buffers
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'node)
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))

(provide 'init-javascript)
;;; init-javascript.el ends here
