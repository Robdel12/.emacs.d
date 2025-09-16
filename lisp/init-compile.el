;;; init-compile.el --- Compilation UX tweaks
;;; Commentary:
;;; Code:
(require 'ansi-color)

;; Colorize ANSI escape codes in compilation buffers
(defun rd/ansi-colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'rd/ansi-colorize-compilation-buffer)

(provide 'init-compile)
;;; init-compile.el ends here

