;;; init-dired.el --- Dired settings
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'dired)
(require 'ls-lisp)

;; Built-in dired enhancements
(setq dired-listing-switches "-alh"
      dired-dwim-target t
      dired-recursive-copies 'always
      dired-recursive-deletes 'top
      ;; MacOS `ls` does not support `--dired` so always use ls-lisp
      ls-lisp-use-insert-directory-program nil
      ls-lisp-dirs-first t
      ls-lisp-verbose nil)

;; Modern dired enhancements
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)))

;; Hide details by default, toggle with (
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Reuse dired buffers
(setq dired-kill-when-opening-new-buffer t)

(provide 'init-dired)
;;; init-dired.el ends here
