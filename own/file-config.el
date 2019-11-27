;; remember what I was doing before
(recentf-mode 1)
(defalias 'rf 'recentf-open-files)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(defalias 'rb 'revert-buffer)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c g") 'show-file-name)
(global-set-key (kbd "C-c C-g") 'show-file-name)

(defun ff-find-other-file-other-window ()
  (interactive)
  (ff-find-other-file t))

(defun ff-delete-windows-and-find-other-file-other-window ()
  (interactive)
  (delete-other-windows)
  (ff-find-other-file-other-window))

(global-set-key (kbd "C-c SPC") 'ff-delete-windows-and-find-other-file-other-window)


;; modified from: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-from-minibuffer "Rename to: " filename)))
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file filename new-name t)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)))))))


(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." 
 (interactive "DNew directory: ")
 (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (dir
         (if (string-match dir "\\(?:/\\|\\\\)$")
             (substring dir 0 -1) dir))
        (newname (concat dir "/" name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (progn
       (copy-file filename newname 1)
       (delete-file filename)
       (set-visited-file-name newname)
       (set-buffer-modified-p nil)
       t))))

(defun delete-buffer-file ()
  "Deletes the file associated with the current buffer and kills the buffer"
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (yes-or-no-p (format "Are you sure you wish to delete %s? " filename))
          (progn
            (save-buffer)
            (delete-file filename)
            (kill-buffer)
            (message "%s deleted" filename))))))

(defun copy-buffer-file (new-filename)
 "Copies the file in the current buffer to a new location."
 (interactive "FDestination filename: ")
 (let* ((filename (buffer-file-name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (progn
       (save-buffer)
       (copy-file filename new-filename 1)
       ))))

(defun clone-buffer-file (new-filename)
 "Copies the file in the current buffer to a new location, opens it, and leaves the old buffer open too."
 (interactive "FDestination filename: ")
 (progn
   (copy-buffer-file new-filename)
   (find-file new-filename)
   t))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t))))
    (message "Reverted all unmodified buffers"))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::"
                         (ido-read-file-name "Find file(as root): ")))
    (cond ((string-prefix-p "/sudo:" buffer-file-name)
           (error "can't sudo an already sudo'ed file"))
          ((string-prefix-p "/ssh:" buffer-file-name)
           (error "can't sudo a remote file"))
          (t (progn
               (find-alternate-file (concat "/sudo::" buffer-file-name))
               (rename-buffer (concat (buffer-name) "(root)")))))))

(provide 'file-config)
