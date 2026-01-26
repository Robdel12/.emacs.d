;;; init-ui.el --- UI settings
;;; Commentary:
;;; Code:
(setq-default tab-width 2)

(setq inhibit-startup-screen t
      max-mini-window-height 0.3)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(set-window-margins nil nil)
(fringe-mode `(2 . 8))

;; modern GUI frame behavior and smoother scrolling
(setq frame-resize-pixelwise t
      window-resize-pixelwise t
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      scroll-conservatively 101
      scroll-step 1
      scroll-preserve-screen-position t
      cursor-in-non-selected-windows nil)

;; pixel-scroll-precision settings for macOS trackpad
(when (display-graphic-p)
  (setq pixel-scroll-precision-large-scroll-height 40.0
        pixel-scroll-precision-interpolate-page t))

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

(provide 'init-ui)
;;; init-ui.el ends here
