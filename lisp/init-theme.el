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

    ;; italic faces - Operator Mono has beautiful italics
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

;; catppuccin theme - modern, aesthetic color scheme
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'frappe)
  (setq catppuccin-italic-comments t
        catppuccin-italic-keywords t)
  (setq underline-minimum-offset 5)

  ;; Load theme
  (load-theme 'catppuccin :no-confirm)
  (ww/activate-operator-mono)

  ;; Ensure theme loads for daemon frames
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (with-selected-frame frame
        (when (window-system frame)
          (load-theme 'catppuccin :no-confirm)
          (ww/activate-operator-mono))))))


;; nerd-icons are configured in init-modeline.el

;; modern ligature support for coding fonts
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
  (global-ligature-mode t))

(provide 'init-theme)
;;; init-theme.el ends here
