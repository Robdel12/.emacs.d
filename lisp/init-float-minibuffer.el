;;; init-float-minibuffer.el --- Top-centered minibuffer via mini-frame
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package mini-frame
  :if (display-graphic-p)
  :after (vertico consult)
  :custom
  (mini-frame-show-parameters
   '((top . 80)
     (left . 0.5)
     (width . 0.5)
     (alpha . 0.95)
     (left-fringe . 12)
     (right-fringe . 12)
     (internal-border-width . 0)
     (child-frame-border-width . 1)))
  ;; Always prefer child-frame for minibuffer prompts
  (mini-frame-determine-function (lambda (&rest _) t))
  (mini-frame-resize 'not-set)
  (mini-frame-ignore-commands '("edebug-eval-expression" debugger-eval-expression))
  :init
  (defun ww/minibuffer-setup ()
    (setq line-spacing 0.4)
    (face-remap-add-relative 'fringe 'default))
  :hook
  (minibuffer-setup . ww/minibuffer-setup)
  :config
  ;; Enable once after all packages are loaded
  (add-hook 'emacs-startup-hook (lambda () (mini-frame-mode 1))))

(provide 'init-float-minibuffer)
;;; init-float-minibuffer.el ends here

