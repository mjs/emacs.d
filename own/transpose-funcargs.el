; ISSUES:
; handling inside strings is not quite right, need to pop out of them (moving to end that is closest to point)
; ws handling isn't quite right
; use syntax tables for looking-at expression so this works in more languages (eg. elisp, C++)
; handle forward-sexp returning errors when it doesn't know what to do
; when starting inside a string it's possible to end up at the last quote instead of on the comma
; make it work with square brackets too

; Useful things
; char-after
; char-syntax
; skip-syntax-forward / backward

;; factor forward/backward-one-funcarg into one implementation

(defun forward-one-funcarg ()
  (interactive)
  (unless (eq (car (syntax-ppss)) 0)
    (condition-case nil
        (progn
          (forward-sexp)
          (while (not (looking-at "[,) ]"))
            (forward-sexp)))
      (scan-error nil))))

(defun backward-one-funcarg ()
  (interactive)
  (unless (eq (car (syntax-ppss)) 0)
    (condition-case nil
        (progn
          (backward-sexp)
          (while (not (looking-back "[,( ]"))
            (backward-sexp)))
      (scan-error nil))))

(defun forward-funcarg (arg)
  (interactive "p*")
  (if (>= arg 0)
      (dotimes (_ arg) (forward-one-funcarg))
    (dotimes (_ (abs arg)) (backward-one-funcarg))))

(defun transpose-funcarg (arg)
  (interactive "*p")
  (transpose-subr 'forward-funcarg arg))

(defun transpose-previous-funcarg (arg)
  (interactive "*p")
  (transpose-subr 'forward-funcarg (- arg)))

(provide 'transpose-funcargs)
