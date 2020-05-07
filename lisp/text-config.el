(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(defalias 'tt 'toggle-truncate-lines)

(use-package longlines
  :config
  (defalias 'll 'longlines-mode))

(use-package ethan-wspace
  :straight t
  :init
  (setq mode-require-final-newline nil)  ; ethan-wspace doesn't like this
  (setq ethan-wspace-face '(t (:background "#05ff00")))
  (setq ethan-wspace-face-customized t))

(global-ethan-wspace-mode 1)

(put 'narrow-to-region 'disabled nil)

;; enable case-transformers
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; tabs are for babies and Aztecs (and Makefiles, I guess)
(setq-default indent-tabs-mode nil)

(provide 'text-config)
