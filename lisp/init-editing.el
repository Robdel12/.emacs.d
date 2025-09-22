;;; init-editing.el --- Editing utils
;;; Commentary:
;;; Code:
(require 'use-package)

;; Make sure `copilot-mode` symbol exists before the real package is loaded
(autoload 'copilot-mode "copilot" nil t)

;; key-chord support
(use-package key-chord
  :ensure t)

(use-package use-package-chords
  :after key-chord
  :ensure t
  :config
  (key-chord-mode 1))

;; spaces > tabs
(setq-default indent-tabs-mode nil
              tab-always-indent 'complete
              tab-width 2)

;; default fill-column
(setq-default fill-column 100)

;; show trailing whitespace in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; missing newlines can mess up diffs
(setq require-final-newline t)

;; silence!
(setq ring-bell-function 'ignore)

;; some packages modify key-bindings themselves and produce noisy messages
(setq ad-redefinition-action 'accept)

;; temp file locations
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; prevent lockfiles
(setq create-lockfiles nil)

;; auto revert
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)

;; y/n prompts (modern approach for Emacs 28+)
(setq use-short-answers t)

;; delete selections
(delete-selection-mode t)

;; show matching parens
(show-paren-mode t)

;; pixel precision scrolling
(pixel-scroll-mode 1)

;; camelCase-aware word motions in code
(add-hook 'prog-mode-hook #'subword-mode)

;; handle pathological long lines gracefully
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode 1))

;; autosave buffers with names
(defun rd/autosave-if-buffer-file (&rest _)
  "Autosave current buffer if it's visiting a file."
  (when buffer-file-name (save-buffer)))

(advice-add 'switch-to-buffer :before #'rd/autosave-if-buffer-file)
(advice-add 'other-window :before #'rd/autosave-if-buffer-file)
(advice-add 'windmove-up :before #'rd/autosave-if-buffer-file)
(advice-add 'windmove-down :before #'rd/autosave-if-buffer-file)
(advice-add 'windmove-left :before #'rd/autosave-if-buffer-file)
(advice-add 'windmove-right :before #'rd/autosave-if-buffer-file)
(add-hook 'focus-out-hook #'rd/autosave-if-buffer-file)

;; xref enter as tab
(use-package xref
  :ensure t
  :bind (:map xref--xref-buffer-mode-map
              ("<return>" . xref-quit-and-goto-xref)))

;; modern jump navigation
(use-package avy
  :chords (("jj" . avy-goto-char)
           ("jk" . avy-goto-word-1)
           ("jl" . avy-goto-line)))

;; ridiculously useful extensions
(use-package crux
  :ensure t
  :bind (([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c r" . crux-rename-file-and-buffer)
         :map window-swap-map
         ("s" . crux-transpose-windows))
  :chords (("JJ" . crux-switch-to-previous-buffer))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(defun rd/toggle-indent-tabs-mode ()
  "Toggle the 'indent-tabs-mode' variable."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message
   (concat
    "'indent-tabs-mode' is "
    (or (and indent-tabs-mode "t") "nil"))))
(bind-key "C-c <tab>" `rd/toggle-indent-tabs-mode)

;; ability to restart emacs quickly
(use-package restart-emacs
  :ensure t)

;; auto open/close pairs
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode 1))

;; useful to browse kill-ring
(use-package browse-kill-ring
  :ensure t
  :chords (("yy" . browse-kill-ring)))

;; vundo - modern undo visualization (replaces undo-tree)
(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo)
         ("C-/" . vundo)
         :map vundo-mode-map
         ("C-n" . vundo-next)
         ("C-p" . vundo-previous)
         ("C-f" . vundo-forward)
         ("C-b" . vundo-backward)
         ("n" . vundo-next)
         ("p" . vundo-previous)
         ("f" . vundo-forward)
         ("b" . vundo-backward))
  :chords (("uu" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

;; enhanced which-key for discoverability
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 0.5)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-max-height 0.25))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

;; highlight changes
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))
(use-package mc-modal-mode
  :ensure nil
  :bind (("C-c ." . mc-modal-mode)))

;; move lines
(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; kill the current line or region
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode))

;; smart trim whitespace
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode))

;; expand region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; manipulate numbers
(defun rd/increment-number-at-point (&optional arg)
  "Increment the number at point by ARG."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun rd/decrement-number-at-point (&optional arg)
  "Decrement the number at point by ARG."
  (interactive "p*")
  (rd/increment-number-at-point (if arg (- arg) -1)))

(bind-key "C-c C-p" `rd/increment-number-at-point)
(bind-key "C-c C-n" `rd/decrement-number-at-point)

(defun rd/comment-line-or-region ()
  "Comment or uncomment the current line or region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

(global-set-key (kbd "s-/") 'rd/comment-line-or-region)

;; copilot for AI assistance
(use-package copilot
  :ensure nil
  :init
  ;; Install via package-vc if not already installed
  (unless (package-installed-p 'copilot)
    (package-vc-install "https://github.com/copilot-emacs/copilot.el"))
  :config
  ;; Disable if language server not found (install with: npm install -g @github/copilot-language-server)
  (when (not (executable-find "copilot-language-server"))
    (message "Copilot language server not found. Install with: npm install -g @github/copilot-language-server")))

(with-eval-after-load 'copilot
  (add-hook 'prog-mode-hook #'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(provide 'init-editing)
;;; init-editing.el ends here
