;;; init-dired.el --- Dired settings
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'dired)

;; MacOS `ls` does not support `--dired` so use ls-lisp
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil
      ls-lisp-dirs-first t)

;; Built-in dired improvements
(setq dired-dwim-target t                    ; suggest other dired buffer as target
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-kill-when-opening-new-dired-buffer t  ; reuse dired buffers
      dired-auto-revert-buffer t
      dired-listing-switches "-alh")         ; human-readable sizes

;; Enable dired-x for extra features
(require 'dired-x)

;; Modern dired UI with dirvish
(use-package dirvish
  :config
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("p" "~/Code/" "Projects")))
  :bind
  (:map dirvish-mode-map
        ("a" . dirvish-quick-access)
        ("f" . dirvish-file-info-menu)
        ("y" . dirvish-yank-menu)
        ("N" . dirvish-narrow)
        ("^" . dirvish-history-last)
        ("h" . dirvish-history-jump)
        ("s" . dirvish-quicksort)
        ("v" . dirvish-vc-menu)
        ("TAB" . dirvish-subtree-toggle)
        ("M-f" . dirvish-history-go-forward)
        ("M-b" . dirvish-history-go-backward)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-t" . dirvish-layout-toggle)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump)))

;; nerd-icons integration - dirvish has built-in support
(use-package nerd-icons
  :if (display-graphic-p))

(provide 'init-dired)
;;; init-dired.el ends here
