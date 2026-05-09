;;; init-linum.el --- Modern line numbering
;;; Commentary:
;;; Code:

;; modern line numbering
(setq display-line-numbers-type t
      display-line-numbers-width 3
      display-line-numbers-widen t)

(global-display-line-numbers-mode 1)

(dolist (hook '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                dired-mode-hook
                help-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

(provide 'init-linum)
;;; init-linum.el ends here
