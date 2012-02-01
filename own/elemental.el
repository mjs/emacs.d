;; elemental.el -- functions for intelligently jumping between and
;; transposing list/tuple/dictionary/function-parameter
;; elements. These functions are primarily useful when editing
;; software source code.
;;
;; Copyright (C) 2012 Menno Smits
;;
;; Author: Menno Smits <menno@freshfoo.com>
;; Version: 0.1
;; URL: https://bitbucket.org/mjs0/elemental/
;;
;; This file is not part of GNU Emacs.

;; Description:
;;
;; It is difficult to efficiently navigate and manipulate list
;; elements and function parameter lists, especially when string
;; literals and nested function calls are involved. The functions in
;; this module make this process easier.
;;
;; The main functions of interest are elem-forward and elem-transpose.
;;
;; Given a source code line, here's how the point moves with successive
;; calls to elem-forward (the | shows the point):
;;
;;     myfunc(|123, "some, string", fn(a, b))
;;     myfunc(123|, "some, string", fn(a, b))
;;     myfunc(123, "some, string"|, fn(a, b))
;;     myfunc(123, "some, string", fn(a, b)|)
;;
;; Similarly, here's how successive calls to elem-transpose work:
;;
;;     myfunc(|123, "some, string", fn(a, b))
;;     myfunc("some, string", 123|, fn(a, b))
;;     myfunc("some, string", fn(a, b), 123|)
;;
;; These functions work correctly over multiple lines and respect
;; whitespace around elemnents. For example, here's how successive
;; elem-transpose calls work on this multi-line list/array:
;;
;;     [  1|23,
;;        "some, string",
;;        fn(a, b)  ]
;;
;;     [  "some, string",
;;        123|,
;;        fn(a, b)  ]
;;
;;     [  "some, string",
;;        fn(a, b),
;;        123|  ]
;;
;; elem-forward and elem-transpose take an optional numerical argument
;; (which can be specified as a prefix argument). Positive numbers
;; cause the action to be applied multiple times in the forward
;; direction. Negative numbers cause the action to be applied
;; backwards.
;;
;; You may also like to use elem-forward-one, elem-backward-one or
;; elem-transpose-backward for keyboard bindings.

;; Installation:
;;
;; Copy elemental.el to somewhere in your load path and add "(require
;; 'elemental)" to your init.el or .emacs file. You'll want to add
;; some bindings.
;;
;; Example config:
;;
;;   (require 'elemental)
;;   (global-set-key (kbd "C-(") 'elem-backward-one)
;;   (global-set-key (kbd "C-)") 'elem-forward)
;;   (global-set-key (kbd "C-*") 'elem-transpose)

;; Limitations:
;;
;; elemental.el assumes that a comma is the separator between
;; elements. This means it currently won't work when editing code for
;; languages where this assumption isn't true. This will be addressed
;; in a future version. For Lisp-like languages the transpose-sexps
;; function might be want you want.

;; Tests:
;;
;; Unit tests can be found in elemental-tests.el. They rely on the ERT
;; framework which is included with Emacs 24 . Older versions are also
;; available here: https://github.com/ohler/ert
;;
;; See the top of elemental-tests.el for details on running the tests.

;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


(defun elem-forward-one ()
  (interactive)
  (unless (elem-outside-parens?)
    (elem-move-out-of-string-if-required)
    (condition-case nil
        (progn
          (forward-sexp)
          (while (not (elem-looking-forward-ignoring-ws ","))
            (forward-sexp)))
      (scan-error nil))))

(defun elem-backward-one ()
  (interactive)
  (unless (elem-outside-parens?)
    (condition-case nil
        (progn
          (unless (elem-move-out-of-string-if-required)
            (backward-sexp))
          (while (not (elem-looking-back-ignoring-ws ","))
            (backward-sexp)))
      (scan-error nil))))

(defun elem-looking-forward-ignoring-ws (regex)
  (save-excursion
    (elem-skip-ws 'char-after 'forward-char)
    (looking-at regex)))

(defun elem-looking-back-ignoring-ws (regex)
  (save-excursion
    (elem-skip-ws 'char-before 'backward-char)
    (looking-back regex)))

(defun elem-skip-ws (look-func move-func)
  (while (= (char-syntax (funcall look-func)) 32)
    (funcall move-func)))

(defun elem-outside-parens? ()
  (<= (car (syntax-ppss)) 0))

(defun elem-move-out-of-string-if-required ()
  (let ((pstate (syntax-ppss)))
    (when (nth 3 pstate)
        (goto-char (nth 8 pstate)))))    ; in a string, move to start

(defun elem-forward (arg)
  (interactive "p*")
  (if (>= arg 0)
      (dotimes (_ arg) (elem-forward-one))
    (dotimes (_ (abs arg)) (elem-backward-one))))

(defun elem-transpose (arg)
  (interactive "*p")
  (transpose-subr 'elem-forward arg))

(defun elem-transpose-backward (arg)
  (interactive "*p")
  (transpose-subr 'elem-forward (- arg)))

(provide 'elemental)
