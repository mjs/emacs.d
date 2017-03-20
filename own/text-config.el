(require 'longlines)
(defalias 'll 'longlines-mode)
(defalias 'tt 'toggle-truncate-lines)

(put 'narrow-to-region 'disabled nil)

;; enable case-transformers
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; tabs are for babies and Aztecs (and Makefiles, I guess)
(setq-default indent-tabs-mode nil)

;; Get OCD about whitespace
(setq ethan-wspace-face '(t (:background "#05ff00")))
(setq ethan-wspace-face-customized t)
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

(provide 'text-config)
