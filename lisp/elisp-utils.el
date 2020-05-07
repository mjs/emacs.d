
(defmacro with-region-or-buffer (args &rest body)
  "Execute BODY with BEG and END bound to the beginning and end of the
current region if one exists or the current buffer if not.
This macro replaces similar code using (interactive \"r\"), which
can fail when there is no mark set.

\(fn (BEG END) BODY...)"
  `(let ((,(car args) (if (use-region-p) (region-beginning) (point-min)))
         (,(cadr args) (if (use-region-p) (region-end) (point-max))))
     ,@body))

(provide 'elisp-utils)
