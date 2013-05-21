;; Flymake handler for python code
;;
;; Author: Menno Smits
;;
;; Inspired by flymake-ruby.el by Steve Purcell (which has since gone
;; away). This has been heavily modified since.
;;
;; Usage:
;; (require 'flymake-pyflakes)

(defun flymake-pyflakes-create-tempfile (filename prefix)
  (make-temp-file (or prefix "flymake-pyflakes")))

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-pyflakes-create-tempfile)))
    (list "pyflakes" (list temp-file))))

(defun flymake-pyflakes-load ()
  (let ((file-name (buffer-file-name (current-buffer))))
    (set (make-local-variable 'flymake-allowed-file-name-masks) '((".+" flymake-pyflakes-init)))
    (flymake-mode t)))

(add-hook 'python-mode-hook 'flymake-pyflakes-load)

(provide 'flymake-pyflakes)
