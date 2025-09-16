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
  :hook ((web-mode . add-node-modules-path)))

(use-package json-mode)
;; Prefer Apheleia for formatting; keep prettier-js available for manual use
(use-package prettier-js
  :defer t)

;; parse node stack traces in compilation buffers
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'node)
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))

(provide 'init-javascript)
;;; init-javascript.el ends here
