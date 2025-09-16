;;; init-modeline.el --- Beautiful, modern modeline
;;; Commentary:
;;; Code:
(require 'use-package)

;; Icons support (requires a Nerd Font installed locally)
(use-package nerd-icons :defer t)

(use-package doom-modeline
  :init
  ;; Appearance
  (setq doom-modeline-height 28
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        ;; Density and truncation
        doom-modeline-minor-modes nil
        doom-modeline-window-width-limit 90
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-vcs-max-length 20
        doom-modeline-checker-simple-format t
        ;; Integrations
        doom-modeline-project-detection 'projectile
        doom-modeline-lsp t
        doom-modeline-indent-info t
        ;; Display env versions for common web stacks
        doom-modeline-env-enable t
        doom-modeline-env-version t
        doom-modeline-env-node t
        doom-modeline-env-ruby t)
  :hook (after-init . doom-modeline-mode))

(provide 'init-modeline)
;;; init-modeline.el ends here
