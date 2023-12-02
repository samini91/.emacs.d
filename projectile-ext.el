(defun read-file-as-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

;; Should be looking for the tag origin instead of finding the first url
(defun collect-urls (lines)
  (cl-loop for l in lines
           for parsed = (split-string l "=" t)
           for first = (string-trim (car parsed))
           when (equal first "url")
           collect (string-trim (car (cdr parsed)))
   )
  )

(defun projectile-url-open ()
  (interactive)
  (let* (
         (file (concat (file-name-as-directory (projectile-project-root)) ".git/config"))
         (lines (read-file-as-lines file))
         (urls (collect-urls lines))
         (first-url (car urls))
         (url (build-projectile-url first-url))
         )
    (browse-url url)
    )
  )

(defun build-projectile-url (url)
   (if (string-prefix-p "http" url)
       (build-projectile-url-http url)
       (build-projectile-url-ssh url)    
    )
  )


(defun build-projectile-url-http (url)
  url
  )

(defun build-projectile-url-ssh (url)
  (let* (
         (ssh-domain (car  (split-string url ":" t) ))
         (domain (car ( cdr (split-string ssh-domain "@" t) ) ))
         (path ( car ( cdr (split-string url ":" t) ) ))
         )
    (concat "https://" (file-name-as-directory domain) path)
    )
  )
