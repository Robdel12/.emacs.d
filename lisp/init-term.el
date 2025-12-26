;;; init-term.el --- Terminal settings and modes
;;; Commentary:
;;; Code:
(require 'use-package)

(setq shell-file-name "/bin/zsh"
      explicit-shell-file-name "/bin/zsh")

(use-package multi-term
  :bind (("C-c T" . multi-term))
  :hook (term-mode . (lambda () (define-key term-raw-map (kbd "C-z") 'self-insert-command)))
  :custom
  (multi-term-program "/bin/zsh")
  (term-suppress-hard-newline t)
  :config
  ;; project-run-term is defined in init-projectile.el
  )

(unless (display-mouse-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(provide 'init-term)
;;; init-term.el ends here
