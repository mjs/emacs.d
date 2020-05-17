;; Colour theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")
(load-theme 'metalspleen)

(defun disable-all-themes ()
  "Turn off all enabled colour themes."
  (interactive)
  (dolist (sometheme custom-enabled-themes)
    (disable-theme sometheme)))

(defun replace-theme ()
  (interactive)
  (disable-all-themes)
  (call-interactively 'load-theme))

(defun refresh-theme ()
  (interactive)
  (disable-all-themes)
  (load-theme 'metalspleen t))

;; Get rid of useless chrome
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(column-number-mode)

;; Set cursor to blink for a while after input
(blink-cursor-mode 1)
(setq blink-cursor-interval 0.3)
(setq blink-cursor-blinks 50)

(use-package selectrum :straight t)
(selectrum-mode +1)

(use-package selectrum-prescient :straight t)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(use-package spaceline
  :straight t
  :demand t
  :config
  (require 'spaceline-config)
  (setq powerline-height 26)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-python-env-on))


(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "\C-x 9") 'bury-buffer)

(winner-mode)

;; Easier window sizing bindings.
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "M-=") 'enlarge-window)

;; Font scaling
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)

(defun toggle-window-split ()
  "Switch between horizontal and vertical window split config."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
         (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "<f12>") 'toggle-window-split)


;; Default window splitting behaviour to suit wide monitors
(setq split-height-threshold 45)
(setq split-width-threshold 160)

(defun split-window-sensibly-prefering-horizontal (&optional window)
  "Same as 'split-window-sensibly' but try a width-wise split first.
This is better for wide monitors.  Optionally the WINDOW to split can
be passed otherwise the current window is used."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it vertically disregarding
             ;; the value of `split-height-threshold'.
             (let ((split-height-threshold 0))
               (when (window-splittable-p window)
                 (with-selected-window window
                   (split-window-below))))))))

(setq split-window-preferred-function
      'split-window-sensibly-prefering-horizontal)

;; If the compilation buffer is displayed already, keep using that
;; frame and window. If it isn't displayed, pop it up in a new frame.
(add-to-list
 'display-buffer-alist
  '("\\*compilation\\*"
    (display-buffer-reuse-window display-buffer-pop-up-frame)
    (reusable-frames . t)))

(defun run-terminal ()
  "Spawn an interactive terminal."
  (interactive)
  (start-process "terminal" nil "term"))

(global-set-key (kbd "C-c t") 'run-terminal)

(provide 'gui-config)
