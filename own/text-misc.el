;;; text-misc.el --- Miscellaenous functions for manipulating text
;;
;;; Commentary:
;;
;; These functions extend the excellent range of text manipulation
;; functions already present in Emacs.  Many of these are my personal
;; "secret weapons".
;;
;;; Code:

(defun plain-thing-at-point (thing-type)
  "Return the thing at the point of type THING-TYPE (like 'thing-at-point'), but strip out any text properties."
  (let ((thing (thing-at-point thing-type)))
      (set-text-properties 0 (length thing) nil thing)
      thing))

(defun filename-near-point ()
  "Get the filename at point with special handling for C/C++ #include lines."
  (save-excursion
    (let ((orig-col (current-column)))
      (beginning-of-line)
      (if (looking-at "#include")
          (re-search-forward "[<\"]")
        (move-to-column orig-col))
      (plain-thing-at-point 'filename))))

(defun replace-symbol-at-point ()
  "Replace the symbol under the cursor."
  (interactive)
  (save-excursion
    (let* ((sym (plain-thing-at-point 'symbol))
           ; Using r-f-mb so that the prompt can be dynamic and the
           ; thing to be replaced can be used as an initial value
           (with (read-from-minibuffer (concat "Replace \"" sym "\" with: ") sym)))
      (goto-char (point-min))
      (query-replace sym with))))

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region between BEGIN and END."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun count-chars (start end)
  "Print the number of characters in region between START and END."
  (interactive "r")
  (message "%d characters" (- end start)))

(provide 'text-misc)
;;; text-misc.el ends here
