(defun read-file-as-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun projectile-url-open ()
  (interactive)
  (let* (
         (file (concat (file-name-as-directory (projectile-project-root)) ".git/config"))
         (lines (read-file-as-lines file))
         (urls (collect-urls lines))
         (url (build-projectile-url (car urls)))
         )
    (browse-url url)
    )
  )

(defun collect-urls (lines)
  (cl-loop for l in lines
           for parsed = (split-string l "=" t)
           for first = (string-trim (car parsed))
           when (equal first "url")
           collect (string-trim (car (cdr parsed)))
   )
  )

(defun build-projectile-url (url)
  (let* (
         (ssh-domain (car  (split-string url ":" t) ))
         (domain (car ( cdr (split-string ssh-domain "@" t) ) ))
         (path ( car ( cdr (split-string url ":" t) ) ))
         )
    (concat "https://" (file-name-as-directory domain) path)
    )
  )
