;;; init-theme.el --- Theme settings
;;; Commentary:
;;; Code:
(require 'use-package)

;; dark mode!
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(border-width . 1))

;; font setup
(setq-default line-spacing 0.6)

(defun ww/activate-operator-mono ()
  "Activate the Operator Mono font if available."
  (when (find-font (font-spec :name "Operator Mono Lig"))
    (add-to-list 'default-frame-alist '(font . "Operator Mono Lig"))
    (set-face-attribute 'default nil :font "Operator Mono Lig" :height 130)

    ;; modern ligatures handled by ligature.el package
    t

    ;; italic faces
    (set-face-italic 'font-lock-builtin-face t)
    (set-face-italic 'font-lock-comment-face t)
    (set-face-italic 'font-lock-constant-face t)
    (set-face-italic 'font-lock-doc-face t)
    (set-face-italic 'font-lock-function-name-face t)
    (set-face-italic 'font-lock-keyword-face t)

    (with-eval-after-load 'web-mode
      (set-face-italic 'web-mode-html-attr-name-face t)
      (set-face-italic 'web-mode-css-property-name-face t)
      (set-face-italic 'web-mode-css-pseudo-class-face t))))

;; rainbow mode for colors
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook css-mode)

;; rainbow delimiters
(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

;; custom base-16 theme
(use-package base16-theme
  :config
  (setq base16-theme-256-color-source 'colors)

  ;; Create custom theme using base16 infrastructure
  (deftheme ww/base16-theme "Custom base16 theme")

  (let ((colors '(:base00 "#292d3e"
                  :base01 "#444267"
                  :base02 "#32374d"
                  :base03 "#676e95"
                  :base04 "#8796b0"
                  :base05 "#959dcb"
                  :base06 "#959dcb"
                  :base07 "#ffffff"
                  :base08 "#f07178"
                  :base09 "#f78c6c"
                  :base0A "#ffcb6b"
                  :base0B "#c3e88d"
                  :base0C "#89ddff"
                  :base0D "#82aaff"
                  :base0E "#c792ea"
                  :base0F "#ff5370")))
    (base16-theme-set-faces 'ww/base16-theme colors '(
     (fringe :background base00)
     (line-number :foreground base03 :background base00)
     (line-number-current-line :foreground base0E :background base00)
     (window-divider :foreground base00)
     (window-divider-first-pixel :foreground base00)
     (window-divider-last-pixel :foreground base00)
     (internal-border :background base00)
     (child-frame-border :background base03)
     (vertico-current :foreground base09)
     (mode-line :foreground base05 :background base02)
     (mode-line-inactive :foreground base03 :background base01)
     (diff-hl-change :background base0E)
     (diff-hl-delete :background base08)
     (diff-hl-insert :background base0B)
     (markdown-header-face :weight bold :inherit default)
     (web-mode-html-tag-face :inherit font-lock-constant-face)
     (web-mode-html-attr-name-face :inherit font-lock-variable-name-face)
     (web-mode-html-attr-value-face :inherit font-lock-preprocessor-face)))
    (base16-theme-define 'ww/base16-theme colors))

  (setq underline-minimum-offset 5)

  ;; Load theme immediately and also set up for daemon frames
  (enable-theme 'ww/base16-theme)
  (ww/activate-operator-mono)

  ;; Also ensure theme loads for daemon frames
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (with-selected-frame frame
        (when (window-system frame)
          (enable-theme 'ww/base16-theme)
          (ww/activate-operator-mono))))))

;; nerd-icons are configured in init-modeline.el

(provide 'init-theme)
;;; init-theme.el ends here
