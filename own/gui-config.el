
;; Colour theme
; (load-theme 'kosmos)
; (load-theme 'distinguished)
; (load-theme 'danneskjold)
; (load-theme 'gruber-darker)
(load-theme 'metalspleen)

(defun disable-all-themes ()
  "Turn off all enabled colour themes"
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

;; Show size and position scrollbar in modeline
(require 'sml-modeline)
(sml-modeline-mode t)

(column-number-mode)

(require 'idomenu)
(global-set-key (kbd "C-c i") 'idomenu)
(ido-mode 1)
(ido-everywhere 1)     ; make switch buffer and find file much nicer

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)    ;; This is the default M-x.

;; buffer list on crank
(global-set-key (kbd "C-x C-b") 'ibuffer)

(winner-mode)

;; Make window sizing saner
(defun enlarge-window-horiz-quick (arg)
  (interactive "P")
  (enlarge-window-horizontally 3))

(defun shrink-window-horiz-quick (arg)
  (interactive "P")
  (shrink-window-horizontally 3))

(global-set-key (kbd "C-_") 'shrink-window-horiz-quick)
(global-set-key (kbd "C-+") 'enlarge-window-horiz-quick)
(global-set-key (kbd "C--") 'shrink-window)
(global-set-key (kbd "C-=") 'enlarge-window)

(defun toggle-window-split ()
  "Switch between horizontal and vertical window split config"
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
  "Same as split-window-sensibly but tries a width-wise split first
 (better for wide monitors)"
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

(provide 'gui-config)
