;;; init-linum.el --- Modern line numbering
;;; Commentary:
;;; Code:

;; modern line numbering (replaces deprecated linum)
(when (version<= "26.0.50" emacs-version)
  ;; ensure clean slate
  (global-display-line-numbers-mode 0)
  (global-display-line-numbers-mode 1)

  (setq display-line-numbers-type t)     ; absolute line numbers
  (setq display-line-numbers-width 3)   ; consistent width
  (setq display-line-numbers-widen t)   ; avoid truncation

  ;; disable in specific modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook
                  dired-mode-hook
                  help-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; fallback for older emacs versions
(unless (version<= "26.0.50" emacs-version)
  (add-hook 'text-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'linum-mode))

(provide 'init-linum)
;;; init-linum.el ends here
