;;; init-tramp.el --- TRAMP remote file editing
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'tramp)

;; Performance optimizations
(setq tramp-verbose 1                          ; Minimal verbosity
      tramp-default-method "sshx"              ; Use sshx method (simpler, more reliable)
      tramp-auto-save-directory (expand-file-name "tramp-autosave" user-emacs-directory)
      tramp-persistency-file-name (expand-file-name "tramp" user-emacs-directory)
      tramp-default-remote-shell "/bin/bash"   ; Use bash for remote shell
      tramp-completion-reread-directory-timeout nil) ; Don't cache directory contents

;; Don't auto-open shell buffers for TRAMP connections
(setq tramp-terminal-type "dumb"
      tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*"
      tramp-password-prompt-regexp
      (concat
       "^.*"
       (regexp-opt
        '("passphrase" "Passphrase"
          "password" "Password"
          "Enter passphrase" "Enter password")
        t)
       ".*:? *"))

;; Hide TRAMP shell buffers from buffer list
(defun hide-tramp-buffers ()
  "Hide TRAMP connection buffers."
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*tramp/" (buffer-name buf))
      (bury-buffer buf))))

(add-hook 'tramp-unload-hook #'hide-tramp-buffers)

;; Auto-bury TRAMP connection buffers
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (hide-tramp-buffers))))

;; Better backup handling for remote files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; Disable version control for remote files (improves performance)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; Better handling of remote paths
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; project.el already handles remote files gracefully, no special advice needed

;; Helper function to connect to Pi
(defun connect-to-pi ()
  "Connect to Raspberry Pi via TRAMP."
  (interactive)
  (let* ((user (read-string "Username (default: robertdeluca): " nil nil "robertdeluca"))
         (host (read-string "Hostname/IP (default: 192.168.0.6): " nil nil "192.168.0.6"))
         (path (read-string "Remote path (default: ~): " nil nil "~")))
    (dired (format "/ssh:%s@%s:%s" user host path))))

;; Quick function to connect to your Pi
(defun dired-pi ()
  "Open Dired on Pi at 192.168.0.6."
  (interactive)
  (dired "/sshx:robertdeluca@192.168.0.6:~/"))

;; Helper to view TRAMP debug buffer
(defun show-tramp-debug ()
  "Show the TRAMP debug/connection buffer."
  (interactive)
  (let ((tramp-bufs '()))
    ;; Collect all TRAMP-related buffers
    (dolist (buf (buffer-list))
      (when (string-match-p "^\\*\\(tramp\\|debug tramp\\)" (buffer-name buf))
        (push buf tramp-bufs)))
    (if tramp-bufs
        (progn
          (switch-to-buffer (car tramp-bufs))
          (when (cdr tramp-bufs)
            (message "Other TRAMP buffers available: %s"
                     (mapconcat #'buffer-name (cdr tramp-bufs) ", "))))
      (message "No TRAMP buffers found. Check *Messages* buffer for errors.")
      (switch-to-buffer "*Messages*"))))

;; Helper to go back to local home directory
(defun goto-local-home ()
  "Navigate back to local home directory from remote connection."
  (interactive)
  (dired "~/"))

;; Keybindings - using C-c t prefix for "tramp"
(global-set-key (kbd "C-c t p") 'connect-to-pi)
(global-set-key (kbd "C-c t d") 'dired-pi)
(global-set-key (kbd "C-c t b") 'show-tramp-debug)
(global-set-key (kbd "C-c t h") 'goto-local-home)

(provide 'init-tramp)
;;; init-tramp.el ends here
