;;; gui-config.el --- UI related configuration
;;
;;; Commentary:
;;
;; A random collection of things affecting the Emacs UI.
;;
;;; Code:

;; Colour theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/own/themes")

(load-theme 'metalspleen)
; base16-classic-dark
; santiyinc-tomorrow-night
; flatui-dark

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

;; ido
(ido-mode 1)
(ido-everywhere 1)


(use-package ido-vertical-mode
  :straight t)
(ido-vertical-mode 1)

; Better fuzzy matching
(use-package flx-ido
  :straight t
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)  ; disable ido faces to see flx highlights.
  )

; Smart M-x (uses ido)
(use-package smex
  :straight t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; Amazing status line
(use-package powerline
  :straight t)

(defun powerline-center-evil-mod-theme ()
  "Setup a mode-line with major, evil, and minor modes centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" mode-line 'l)
                                     (powerline-buffer-size mode-line 'l)
                                     (powerline-buffer-id mode-line-buffer-id 'l)
                                     (powerline-raw "%5l" 'l)
                                     (powerline-raw ":" )
                                     (powerline-raw "%3c" 'l)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" mode-line 'l)
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-vc face1)))
                          (rhs (list (funcall separator-right face1 mode-line)))
                          (center (append (list (powerline-raw " " face1)
                                                (funcall separator-left face1 face2)
                                                (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                                  (powerline-raw erc-modified-channels-object face2 'l))
                                                (powerline-major-mode face2 'l)
                                                (powerline-process face2)
                                                (powerline-raw " " face2))
                                          (if (split-string (format-mode-line minor-mode-alist))
                                              (append (if evil-mode
                                                          (list (funcall separator-right face2 face1)
                                                                (powerline-raw evil-mode-line-tag face1 'l)
                                                                (powerline-raw " " face1)
                                                                (funcall separator-left face1 face2)))
                                                      (list (powerline-minor-modes face2 'l)
                                                            (powerline-raw " " face2)
                                                            (funcall separator-right face2 face1)))
                                            (list (powerline-raw evil-mode-line-tag face2)
                                                  (funcall separator-right face2 face1))))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-center-evil-mod-theme)

;; buffer list on crank
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(provide 'gui-config)
;;; gui-config.el ends here
