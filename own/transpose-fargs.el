; ISSUES:
; tests!
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

(defun forward-one-farg ()
  (interactive)
  (unless (eq (car (syntax-ppss)) 0)
    (forward-sexp)
    (while (not (looking-at "[,) ]"))
      (forward-sexp))))

(defun backward-one-farg ()
  (interactive)
  (unless (eq (car (syntax-ppss)) 0)
    (backward-sexp)
    (while (not (looking-back "[,( ]"))
      (backward-sexp))))

;; (defun peek-char-ahead ()
;;   (save-excursion
;;     (forward-char)
;;     (thing-at-point 'char)))


(defun forward-farg (arg)
  (interactive "p*")
  (if (>= arg 0)
      (dotimes (_ arg) (forward-one-farg))
    (dotimes (_ (abs arg)) (backward-one-farg))))

(defun transpose-fargs (arg)
  (interactive "*p")
  (transpose-subr 'forward-farg arg))

(provide 'transpose-fargs)

;; Python test data
;;
;; foo(abc, def, 12)
;; foo(abc, "def, smell", 12)

;; foo(abc, "def, smell", thing=12, xxx)
;; foo( abc, "def, smell", thing=12, xxx )

;; foo(abc,
;;     "def, smell",
;;     12)
