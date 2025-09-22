;;; init-docker.el --- Docker integration
;;; Commentary:
;;; Code:
(require 'use-package)

;; Dockerfile editing
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :hook (dockerfile-mode . flycheck-mode))

;; Docker compose support
(use-package docker-compose-mode
  :mode (("docker-compose.*\\.ya?ml\\'" . docker-compose-mode)
         ("compose.*\\.ya?ml\\'" . docker-compose-mode)))

;; Docker management interface
(use-package docker
  :bind ("C-c d" . docker)
  :custom
  (docker-image-run-arguments '("-i" "-t" "--rm"))
  (docker-container-shell-file-name "/bin/bash"))

;; TRAMP integration for editing files in containers (built-in)
(require 'tramp-container)

(provide 'init-docker)
;;; init-docker.el ends here