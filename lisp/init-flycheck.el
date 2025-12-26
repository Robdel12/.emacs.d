;;; init-flycheck.el --- Syntax checking with built-in Flymake
;;; Commentary:
;; Replaced flycheck with built-in flymake (much improved in Emacs 29+)
;;; Code:
(require 'use-package)
(require 'flymake)

;; Enable flymake globally for prog modes
(add-hook 'prog-mode-hook #'flymake-mode)

;; Flymake configuration
(setq flymake-fringe-indicator-position 'right-fringe
      flymake-no-changes-timeout 0.5)

;; Custom fringe bitmap for errors (exclamation point style)
(define-fringe-bitmap 'flymake-exclamation
  [0 24 24 24 24 24 24 0 0 24 24 0 0 0 0 0 0])

(setq flymake-error-bitmap '(flymake-exclamation compilation-error)
      flymake-warning-bitmap '(flymake-exclamation compilation-warning)
      flymake-note-bitmap '(flymake-exclamation compilation-info))

;; Keybindings for flymake navigation
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") #'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c ! L") #'flymake-show-project-diagnostics))

;; Use consult for flymake diagnostics if available
(with-eval-after-load 'consult
  (define-key flymake-mode-map (kbd "M-g f") #'consult-flymake))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
