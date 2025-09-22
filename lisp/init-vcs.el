;;; init-vcs.el --- Version control settings
;;; Commentary:
;;; Code:
(require 'use-package)

;; magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

;; GitHub integration - PRs, issues, code review
;; Disabled due to compilation issues - use browser-based GitHub workflow
;; Alternative: M-x browse-url-at-point on GitHub URLs in Magit
;; (use-package forge
;;   :defer t
;;   :after magit
;;   :custom
;;   (forge-add-default-bindings nil)
;;   :bind (:map magit-mode-map
;;          ("C-c f f" . forge-dispatch)
;;          ("C-c f p" . forge-list-pullreqs)
;;          ("C-c f i" . forge-list-issues)
;;          ("C-c f c" . forge-create-pullreq)
;;          ("C-c f v" . forge-visit-topic)))

;; highlight diff in fringe
(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode))

;; use pinentry for gpg
(use-package pinentry
  :config
  (pinentry-start))

(provide 'init-vcs)
;;; init-vcs.el ends here
