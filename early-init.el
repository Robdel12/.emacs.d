;;; early-init.el --- Early initialization
;;; Commentary:
;;; Code:

;; Optimize startup performance
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB during startup
(setq read-process-output-max (* 1024 1024)) ; 1MB for LSP
(setq frame-inhibit-implied-resize t)
(setq package-enable-at-startup nil)

;; UI optimizations that should happen early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024)))) ; 20MB after startup

;;; early-init.el ends here