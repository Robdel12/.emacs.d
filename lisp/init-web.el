;;; init-web.el --- Web mode
;;; Commentary:
;;; Code:
(require 'use-package)

;; web-mode
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.hbs?\\'" . web-mode)
         ("\\.astro\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.[mc]?js[x]?\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (css-indent-offset 2)
  :init
  (defun ww/web-mode-insert-space (arg)
    "Insert spaces between brackets and parens."
    (interactive "*P")
    (let ((prev (char-before))
          (next (char-after)))
      (self-insert-command (prefix-numeric-value arg))
      (if (and prev next
               (string-match-p "[[({]" (string prev))
               (string-match-p "[])}]" (string next)))
          (save-excursion (self-insert-command 1)))))
  (defun ww/web-mode-delete-space (arg &optional killp)
    "Delete spaces between brackets and parens."
    (interactive "*p\nP")
    (let ((prev (char-before))
          (next (char-after))
          (pprev (char-before (- (point) 1))))
      (if (and prev next pprev
               (char-equal prev ?\s) (char-equal next ?\s)
               (string-match "[[({]" (string pprev)))
          (delete-char 1))
      (backward-delete-char-untabify arg killp)))
  (defun rd/web-mode-js-family-p ()
    "Return non-nil when the current web-mode buffer is JS-family code."
    (and buffer-file-name
         (string-match-p "\\.\\([cm]?js\\|jsx\\|ts\\|tsx\\)\\'"
                         buffer-file-name)))
  (defun rd/web-mode-setup ()
    "Configure web-mode buffer-local behavior."
    (local-set-key (kbd "SPC") 'ww/web-mode-insert-space)
    (local-set-key (kbd "DEL") 'ww/web-mode-delete-space)
    ;; Disable smartparens in web-mode, use electric-pair instead
    (smartparens-mode -1)
    (electric-pair-local-mode 1)
    (when (rd/web-mode-js-family-p)
      (setq-local apheleia-formatter 'biome)))
  :hook (web-mode . (lambda ()
                      (rd/web-mode-setup))))

;; emmet
(use-package emmet-mode
  :hook (sgml-mode web-mode)
  :custom (emmet-indentation 2))

(provide 'init-web)
;;; init-web.el ends here
