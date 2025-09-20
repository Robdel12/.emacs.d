;;; init-float-minibuffer.el --- Top-centered minibuffer via posframe
;;; Commentary:
;;; Code:
(require 'use-package)

;; Use mini-frame but with better configuration
(use-package mini-frame
  :if (display-graphic-p)
  :custom
  (mini-frame-show-parameters
   '((top . 150)
     (width . 0.6)
     (height . 15)
     (left . 0.5)
     (alpha . 0.95)
     (left-fringe . 10)
     (right-fringe . 10)
     (internal-border-width . 1)
     (child-frame-border-width . 0)
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil)
     (tool-bar-lines . 0)
     (menu-bar-lines . 0)))
  (mini-frame-resize 'grow-only)
  (mini-frame-ignore-commands '("edebug-eval-expression" "debugger-eval-expression"))
  :config
  (mini-frame-mode 1))

(provide 'init-float-minibuffer)
;;; init-float-minibuffer.el ends here

