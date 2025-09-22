;;; init-http.el --- HTTP client and API testing
;;; Commentary:
;;; Code:
(require 'use-package)

;; REST client for API testing
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :bind (:map restclient-mode-map
         ("C-c C-c" . restclient-http-send-current)
         ("C-c C-r" . restclient-http-send-current-raw)
         ("C-c C-v" . restclient-http-send-current-stay-in-window)
         ("C-c C-p" . restclient-jump-prev)
         ("C-c C-n" . restclient-jump-next)))

;; Company completion for REST client (optional - only if company is used)
;; Note: We use corfu, so this is commented out
;; (use-package company-restclient
;;   :after (restclient company)
;;   :config
;;   (add-to-list 'company-backends 'company-restclient))

;; HTTP requests from org-mode
(use-package ob-restclient
  :after restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(provide 'init-http)
;;; init-http.el ends here