
;;; text-config.el --- General text manipulation configuration.
;;
;;; Commentary:
;;
;; Bindings, aliases and config for text general text manipulation
;; functionality.
;;
;;; Code:


(global-set-key (kbd "M-Q") 'unfill-paragraph)
(defalias 'tt 'toggle-truncate-lines)

(use-package longlines
  :config
  (defalias 'll 'longlines-mode))


(use-package ethan-wspace
  :init
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
;;; text-config.el ends here
