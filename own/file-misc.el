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


(provide 'file-misc)
