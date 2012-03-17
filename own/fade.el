(provide 'fade)

(defun fade/start ()
  (interactive)
  (when (not (local-variable-p 'fade-color))
    (make-local-variable 'fade-color)
    (make-local-variable 'fade-timer))
  (buffer-face-mode 1)
  (setq fade-color "#ffffff")
  (fade/step))

(defun fade/step ()
  (buffer-face-set (list :foreground fade-color))
  (setq fade-color (fade/color-triple-to-name (mapcar 'fade/step-color-value
                                              (color-values fade-color))))
  (unless (string= fade-color "#000000")
    (run-at-time 0.1 nil 'fade/step)))

(defun fade/step-color-value (cval)
  (max 0 (- cval 3000)))

(defun fade/color-triple-to-name (ctriple)
  (apply 'format "#%02x%02x%02x" (mapcar (lambda (c) (/ c 257)) ctriple)))
