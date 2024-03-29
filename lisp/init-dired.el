;;; init-dired.el --- Dired settings
;;; Commentary:
;;; Code:
(require 'quelpa-use-package)
(require 'ls-lisp)

(use-package dired+
  :ensure nil ; quelpa will take care of this
  :quelpa (dired+ :fetcher wiki)
  :custom
  (diredp-hide-details-initially-flag nil)
  (diredp-hide-details-propagate-flag nil)
  ;; MacOS `ls` does not support `--dired` so always use ls-lisp
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-dirs-first t)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(provide 'init-dired)
;;; init-dired.el ends here
