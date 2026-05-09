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

;; Prefer built-in tree-sitter modes when grammars are installed, and fall
;; back cleanly when they are not.
(defun rd/treesit-language-ready-p (language)
  "Return non-nil when LANGUAGE can be used by tree-sitter."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p language)))

(defun rd/js-mode ()
  "Open JavaScript with tree-sitter when available."
  (interactive)
  (if (rd/treesit-language-ready-p 'javascript)
      (js-ts-mode)
    (js-mode)))

(defun rd/typescript-mode ()
  "Open TypeScript with tree-sitter when available."
  (interactive)
  (if (rd/treesit-language-ready-p 'typescript)
      (typescript-ts-mode)
    (web-mode)))

(defun rd/tsx-mode ()
  "Open TSX with tree-sitter when available."
  (interactive)
  (if (rd/treesit-language-ready-p 'tsx)
      (tsx-ts-mode)
    (web-mode)))

(defun rd/css-mode ()
  "Open CSS with tree-sitter when available."
  (interactive)
  (if (rd/treesit-language-ready-p 'css)
      (css-ts-mode)
    (css-mode)))

(defun rd/json-mode ()
  "Open JSON with tree-sitter when available."
  (interactive)
  (if (rd/treesit-language-ready-p 'json)
      (json-ts-mode)
    (js-json-mode)))

(dolist (entry '(("\\.[cm]?js\\'" . rd/js-mode)
                 ("\\.jsx\\'" . rd/js-mode)
                 ("\\.ts\\'" . rd/typescript-mode)
                 ("\\.tsx\\'" . rd/tsx-mode)
                 ("\\.css\\'" . rd/css-mode)
                 ("\\.json\\'" . rd/json-mode)))
  (add-to-list 'auto-mode-alist entry))

;; Make project-local executables available to LSP, formatters, and REPL helpers.
(use-package add-node-modules-path
  :hook ((web-mode . add-node-modules-path)
         (js-mode . add-node-modules-path)
         (js-ts-mode . add-node-modules-path)
         (typescript-ts-mode . add-node-modules-path)
         (tsx-ts-mode . add-node-modules-path)))

;; NPM script runner and package management
(use-package npm-mode
  :hook ((js-mode . npm-mode)
         (js-ts-mode . npm-mode)
         (typescript-ts-mode . npm-mode)
         (tsx-ts-mode . npm-mode)
         (web-mode . npm-mode))
  :config
  (define-key npm-mode-keymap (kbd "C-c n") nil)
  :bind (:map npm-mode-keymap
         ("C-c N r" . npm-mode-npm-run)
         ("C-c N i" . npm-mode-npm-install)
         ("C-c N s" . npm-mode-npm-run-script)
         ("C-c N t" . npm-mode-npm-test)))

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
