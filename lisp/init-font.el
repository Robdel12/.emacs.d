;;; init-font.el --- Font settings
;;; Commentary:
;;; Code:
(setq-default line-spacing 0.5)

;; set up "Fira Code" font + ligatures
(when (find-font (font-spec :name "Fira Code"))
  (add-to-list 'default-frame-alist '(font . "Fira Code-14"))
  (set-face-attribute 'default t :font "Fira Code-14")

  ;; "Fira Code" ligatures
  (dolist (char-regexp
           '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
             (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
             (36 . ".\\(?:>\\)")
             (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
             (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
             (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
             (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
             (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
             (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
             (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
             (48 . ".\\(?:x[a-zA-Z]\\)")
             (58 . ".\\(?:::\\|[:=]\\)")
             (59 . ".\\(?:;;\\|;\\)")
             (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
             (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
             (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
             (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
             (91 . ".\\(?:]\\)")
             (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
             (94 . ".\\(?:=\\)")
             (119 . ".\\(?:ww\\)")
             (123 . ".\\(?:-\\)")
             (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
             (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")))
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring])))

  ;; use "Operator Mono" for cursive keywords
  (when (find-font (font-spec :name "Operator Mono"))
    (set-face-attribute 'font-lock-builtin-face nil :family "Operator Mono" :italic t)
    (set-face-attribute 'font-lock-constant-face nil :family "Operator Mono" :italic t)
    (set-face-attribute 'font-lock-keyword-face nil :family "Operator Mono" :italic t)
    (set-face-attribute 'font-lock-type-face nil :family "Operator Mono" :italic t)
    (set-face-attribute 'font-lock-function-name-face nil :family "Operator Mono" :italic t)))

;; set web-mode specific face-attributes
(defun ww/web-mode-face-attributes ()
  "Set font face attributes for web-mode."
  (when (find-font (font-spec :name "Operator Mono"))
    (set-face-attribute 'web-mode-html-attr-name-face nil :family "Operator Mono" :italic t)
    (set-face-attribute 'web-mode-css-property-name-face nil :family "Operator Mono" :italic t)
    (set-face-attribute 'web-mode-css-pseudo-class-face  nil :family "Operator Mono" :italic t)))
(add-hook 'web-mode-hook 'ww/web-mode-face-attributes)

;; set rjsx-mode specific face-attributes
(defun ww/rjsx-mode-face-attributes ()
  "Set font face attributes for rjsx-mode."
  (when (find-font (font-spec :name "Operator Mono"))
    (set-face-attribute 'rjsx-attr nil :family "Operator Mono" :italic t)))
(add-hook 'rjsx-mode-hook 'ww/rjsx-mode-face-attributes)

(provide 'init-font)
;;; init-font.el ends here
