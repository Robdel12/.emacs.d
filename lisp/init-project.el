;;; init-project.el --- Project management with built-in project.el
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'project)

(defun rd/project-magit-status ()
  "Open Magit at the current project root."
  (interactive)
  (magit-status (project-root (project-current t))))

;; Configure project.el
(setq project-switch-commands
      'rd/project-magit-status)

;; Remember projects across sessions
(setq project-list-file (expand-file-name "projects" user-emacs-directory))

(defun rd/project-remember-current ()
  "Remember the current project when `project.el' can identify one.

This is intentionally quiet: it does not prompt, create transient
projects, or probe remote paths."
  (interactive)
  (unless (file-remote-p default-directory)
    (when-let ((project (project-current nil)))
      (project-remember-project project))))

(add-hook 'find-file-hook #'rd/project-remember-current)
(add-hook 'dired-mode-hook #'rd/project-remember-current)

;; Project keybindings
(global-set-key (kbd "s-p") project-prefix-map)
(global-set-key (kbd "C-c p") project-prefix-map)

;; Additional convenient bindings
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "s") #'consult-ripgrep)
  (define-key project-prefix-map (kbd "b") #'consult-project-buffer)
  (define-key project-prefix-map (kbd "a") #'rd/project-remember-current)
  (define-key project-prefix-map (kbd "m") #'rd/project-magit-status))

;; Helper to run multi-term in project root
(defun project-run-term ()
  "Get dedicated multi-term in project root."
  (interactive)
  (let* ((proj (project-current t))
         (root (project-root proj))
         (name (file-name-nondirectory (directory-file-name root)))
         (buf-name (concat "*term<" name ">*")))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
      (let ((default-directory root))
        (multi-term)
        (rename-buffer buf-name)))))

(define-key project-prefix-map (kbd "t") #'project-run-term)

(provide 'init-project)
;;; init-project.el ends here
