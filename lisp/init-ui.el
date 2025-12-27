;;; init-ui.el --- UI settings
;;; Commentary:
;;; Code:
(require 'use-package)

(setq-default tab-width 2)

(setq max-mini-window-height 0.3)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(set-window-margins nil nil)
(fringe-mode '(12 . 12))  ; slightly wider fringes for indent-bars

;; modern GUI frame behavior and smoother scrolling
(setq frame-resize-pixelwise t
      window-resize-pixelwise t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      scroll-conservatively 101
      scroll-margin 4
      scroll-step 1
      cursor-in-non-selected-windows nil)

;; modern coding font with ligature support
(when (display-graphic-p)
  (let ((font-candidates '("JetBrains Mono-14"
                          "Cascadia Code-14"
                          "Fira Code-14"
                          "SF Mono-14"
                          "Monaco-14")))
    (catch 'font-found
      (dolist (font font-candidates)
        (let ((font-family (car (split-string font "-"))))
          (when (member font-family (font-family-list))
            (set-face-attribute 'default nil :font font)
            (throw 'font-found t)))))))

;; subtle highlight of current line in GUI
(when (display-graphic-p)
  (global-hl-line-mode 1))

;; right-click context menu in GUI
(when (display-graphic-p)
  (context-menu-mode 1))

(setq window-divider-default-right-width 24
      window-divider-default-places 'right-only)
(add-to-list 'default-frame-alist '(internal-border-width . 24))
(window-divider-mode 1)

(setq widget-image-enable nil
      org-hide-emphasis-markers t)

;; dashboard - beautiful startup screen
(use-package dashboard
  :vc (:url "https://github.com/emacs-dashboard/emacs-dashboard"
       :rev :newest)
  :demand t
  :config
  (setq dashboard-banner-logo-title nil
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-show-shortcuts nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-icon-type 'nerd-icons
        dashboard-items '((recents . 10))
        dashboard-display-icons-p t
        dashboard-footer-messages '(""))
  (dashboard-setup-startup-hook)
  ;; Ensure dashboard is shown on new frames
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

(provide 'init-ui)
;;; init-ui.el ends here
