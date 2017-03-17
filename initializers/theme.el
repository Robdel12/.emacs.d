(set-face-attribute 'default nil :family "Operator Mono Medium" :height 160)
(set-face-attribute 'font-lock-comment-face nil :italic t)
;; (set-face-attribute 'font-lock-string-face nil :foreground "#4f004f")
;; (set-face-attribute 'font-lock-constant-face nil :italic t)
(set-face-attribute 'font-lock-keyword-face nil :italic t)
;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#00003f")
(set-face-attribute 'font-lock-type-face nil :italic t)
(set-face-attribute 'font-lock-function-name-face nil :italic t)
(set-face-attribute 'font-lock-variable-name-face nil :italic t)

(let ((font "Operator Mono Medium 14"))
  (set-frame-font font)
  (add-to-list 'default-frame-alist
               `(font . ,font)))
