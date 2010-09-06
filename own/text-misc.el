;; Replace the symbol under the cursor 
(defun replace-symbol-at-point ()
  (interactive)
  (save-excursion
    (let* ((sym (thing-at-point 'symbol))
           ; Using r-f-mb so that the prompt can be dynamic and the
           ; thing to be replaced can be used as an initial value
           (with (read-from-minibuffer (concat "Replace \"" sym "\" with: ") sym)))
      (query-replace sym with))))

;; Add a per-buffer hook to automatically remove trailing whitespace on write
(defun auto-del-trailing-whitespace ()
  (interactive)
  (add-hook 'write-contents-hooks 'delete-trailing-whitespace t)
  (message "Trailing whitespace will be removed on write"))

(global-set-key (kbd "C-c C-w") 'auto-del-trailing-whitespace)

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(provide 'text-misc)
