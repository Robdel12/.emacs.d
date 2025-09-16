;;; init-minibuffer.el --- Initialize minibuffer
;;; Commentary:
;;; Code:
(require 'use-package)

;; modern completion framework
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; fuzzy matching for completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; popup completion UI (replaces company)
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

;; show docs in corfu popup
(use-package corfu-popupinfo
  :ensure nil ; bundled with corfu
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 0.2))

;; extra completion sources
(use-package cape
  :init
  ;; Keep LSP first when present; add useful fallbacks afterward
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-symbol t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t))

;; enhanced minibuffer commands
(use-package consult
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap locate] . consult-locate)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap yank-pop] . consult-yank-pop)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c u" . consult-focus-lines)
         ("C-s" . consult-line)
         ("s-f" . consult-ripgrep)
         ("C-S-s" . consult-line-multi)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Use Projectile to resolve project root for consult- commands
  (setq consult-project-function (lambda (_) (when (fboundp 'projectile-project-root)
                                               (projectile-project-root))))
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; enhanced minibuffer annotations
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; contextual actions
(use-package embark
  :bind
  (("C-;" . embark-act)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; embark-consult integration
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; modern ligature support
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

;; Floating minibuffer is configured in init-float-minibuffer.el (loaded last)

;; enhanced help buffers
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-command] . helpful-command)
         ([remap describe-symbol] . helpful-symbol)))

;; company removed in favor of corfu + cape

;; floating minibuffer (disabled for now - may conflict with vertico)
;; (use-package mini-frame
;;   :defer nil
;;   :custom
;;   (mini-frame-show-parameters `(
;;     (top . 100)
;;     (left . 0.5)
;;     (width . 0.5)
;;     (alpha . 0.9)
;;     (left-fringe . 13)
;;     (right-fringe . 13)
;;     (internal-border-width . 0)
;;     (child-frame-border-width . 1)))
;;   (mini-frame-ignore-commands '("edebug-eval-expression" debugger-eval-expression))
;;   :init
;;   (defun ww/minibuffer-setup ()
;;     (setq line-spacing 0.4)
;;     (face-remap-add-relative 'default 'highlight)
;;     (face-remap-add-relative 'fringe 'default)
;;     (let ((overlay (make-overlay (point-min) (+ (point-min) 1)))
;;           (space (propertize "\n" 'face '(:height 0.5))))
;;       (overlay-put overlay 'before-string space)))
;;   :hook
;;   (minibuffer-setup . ww/minibuffer-setup)
;;   :config
;;   (mini-frame-mode 1))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
