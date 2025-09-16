;;; init-elpa.el --- Modern package management
;;; Commentary:
;;; Code:
(require 'package)

;; package repositories
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; prioritize repositories
(setq package-archive-priorities
      '(("melpa" . 10)
        ("melpa-stable" . 5)
        ("nongnu" . 3)
        ("gnu" . 0)))

;; initialize packages
(setq package-enable-at-startup nil)
(package-initialize)

;; refresh package contents on first run
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; configure use-package
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t)

;; install diminish for :diminish keyword
(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(require 'diminish)

(provide 'init-elpa)
;;; init-elpa.el ends here