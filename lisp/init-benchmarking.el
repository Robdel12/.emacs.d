;; difference between two times in ms
(defun ww/time-subtract-ms (b a)
  (* 1000.0 (float-time (time-subtract b a))))

;; list used to store require times
(defvar ww/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

;; wrap require func to measure times
(defadvice require (around ww/build-require-times (feature &optional filename noerror) activate)
  "Note in `ww/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1 ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (ww/time-subtract-ms (current-time) require-start-time)))
          (add-to-list 'ww/require-times (cons feature time) t))))))

;; prints the time taken to require all modules during init
(defun ww/show-init-time ()
  (message "init completed in %.2fms"
           (ww/time-subtract-ms after-init-time before-init-time)))
(add-hook 'after-init-hook 'ww/show-init-time)

;; provide this module
(provide 'init-benchmarking)
