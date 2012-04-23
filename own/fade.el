(provide 'fade)

;; XXX control of fade interval and step

(defun fade/start ()
  (interactive)
  (make-local-variable 'fade-color)
  (make-local-variable 'fade-overlay)
  (setq fade-color "#ffffff")
  (if (and (boundp 'fade-overlay) (overlayp 'fade-overlay))
      (move-overlay fade-overlay (point-min) (point-max))
    (setq fade-overlay (make-overlay (point-min) (point-max))))
  (fade/step fade-color fade-overlay))

(defun fade/clear ()
  (interactive)
  (when (overlayp fade-overlay)
    (delete-overlay fade-overlay)))

(defun fade/step (fade-color fade-overlay)
  (overlay-put fade-overlay 'face (list :foreground fade-color))
  (setq fade-color (fade/color-triple-to-name (mapcar 'fade/step-color-value
                                              (color-values fade-color))))
  (unless (string= fade-color "#000000")
    (run-at-time 0.1 nil 'fade/step fade-color fade-overlay)))

(defun fade/step-color-value (cval)
  (max 0 (- cval 4000)))

(defun fade/color-triple-to-name (ctriple)
  (apply 'format "#%02x%02x%02x" (mapcar (lambda (c) (/ c 257)) ctriple)))
