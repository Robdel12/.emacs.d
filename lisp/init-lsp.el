;;; init-lsp.el --- Language server mode
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((web-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
         ("C-c l f" . lsp-format-buffer))
  :config
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset)
  (add-to-list 'lsp-language-id-configuration '("\\.hbs?\\'" . "html"))
  (setq lsp-keymap-prefix "C-c l"
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-enable nil
        ;; File watching optimizations for web development
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 2000
        lsp-file-watch-ignored-directories
        '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$"
          "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$"
          "[/\\\\]\\.eunit$" "[/\\\\]node_modules" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$"
          "[/\\\\]dist$" "[/\\\\]build$" "[/\\\\]\\.cache$" "[/\\\\]\\.vscode$"
          "[/\\\\]\\.DS_Store$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.venv$" "[/\\\\]__pycache__$"
          "[/\\\\]\\.mypy_cache$" "[/\\\\]\\.pytest_cache$" "[/\\\\]coverage$"
          "[/\\\\]\\.nyc_output$" "[/\\\\]\\.next$" "[/\\\\]\\.nuxt$" "[/\\\\]public$")
        ;; Disable lsp-eslint since we use Biome
        lsp-eslint-enable nil))

;; Biome LSP support
(use-package lsp-biome
  :vc (:url "https://github.com/cxa/lsp-biome" :rev :newest)
  :after lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable nil))

;; consult integration for lsp
(use-package consult-lsp
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
