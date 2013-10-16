;; XXX unfinished

(when
    (and (featurep 'x) window-system)
 (defvar on-blur--saved-window-id 0 "Last known focused window.")
 (defvar on-blur--timer nil "Timer refreshing known focused window.")  ; XXX this should be used but isn't
 (defun on-blur--refresh ()
   "Runs on-blur-hook if emacs has lost focus."
   (let* ((active-window (x-window-property
                          "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
          (active-window-id (if (numberp active-window)
                                active-window
                              (string-to-number
                               (format "%x00%x"
                                       (car active-window)
                                       (cdr active-window)) 16)))
          (emacs-window-id (string-to-number
                            (frame-parameter nil 'outer-window-id))))

     (when (and                                                    ; intent (perhaps buggy):
            (= emacs-window-id on-blur--saved-window-id)           ; last window was Emacs
            (not (= active-window-id on-blur--saved-window-id)))   ; window has changed
       (run-hooks 'on-blur-hook))
     (setq on-blur--saved-window-id active-window-id)
     (run-with-timer 1 nil 'on-blur--refresh)))

 (on-blur--refresh))

(defun kill-minibuffer-if-active ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'on-blur-hook 'kill-minibuffer-if-active)
