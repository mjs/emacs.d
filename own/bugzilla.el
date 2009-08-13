(require 'xml)
(require 'url)

;; XXX make the URL a variable

;; XXX accept bug_id as int
(defun bugzilla-dl-bug (bug_id) 
  (let ((xml (url-retrieve-synchronously (concat "http://bugzilla/show_bug.cgi?ctype=xml&id=" bug_id))))
    (save-excursion
      (set-buffer xml)
      (xml-parse-region (point-min) (point-max)))))

;; Bug attributes that we care about
(setq bug-attributes '((id . bug_id)
                       (status . bug_status)
                       (desc . short_desc)
                       (priority . priority)
                       (severity . bug_severity)
                       (component . component)))  

(defun bugzilla-read-bug (bug_id)
  (let* ((root (bugzilla-dl-bug bug_id))
         (bugzilla-tag (car root))
         (bug-tag (car (xml-get-children bugzilla-tag 'bug)))
         (bug-details))
    
    (defun get-bug-keyval (attr-keys)
      (let* ((key (car attr-keys))
             (xml-key (cdr attr-keys))
             (tag (car (xml-get-children bug-tag xml-key)))
             (tag-value (car (xml-node-children tag))))
        (cons key tag-value)))
   
    (mapcar 'get-bug-keyval bug-attributes)))
 
(bugzilla-read-bug "1837")

(cdr (assq 'id (bugzilla-read-bug "1837")))
(cdr (assq 'desc (bugzilla-read-bug "1837")))


(provide 'bugzilla)

