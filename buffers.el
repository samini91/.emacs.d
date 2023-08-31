;;; -*- lexical-binding: t; -*-
(require 'projectile)
(require 'helm)

(defun helm-buffer-list-by-projectile-root (&optional visibles)
  (cl-loop for b in (buffer-list)
           for bn = (buffer-name b)
           for bu = (get-buffer b)
           for fbn  = (condition-case nil
                          (buffer-file-name bu)
                        (error nil)
                        )
           for root = (if (eq fbn nil) nil (projectile-project-root fbn)) 
           unless (member bn visibles)
           collect (cons root bn)))

(defun helm-buffer-group-by-projectile-root ()
  (seq-group-by (lambda (x) (car x)) (helm-buffer-list-by-projectile-root) )
  )

(defun helm-buffers-group-by-projectile-root ()
  (interactive)
  (let* ((sources (mapcar (lambda (l)
                          (let*
                              (
                               (buffer-title
                                (if (eq (car l) nil)
                                    "No Project"
                                  (concat (file-name-nondirectory (directory-file-name (file-name-directory (car l)))) " --- " (car l))
                                  )
                                )
                               (buffers (seq-map (lambda (x) (cdr x) ) (cdr l)) )
                               (res (helm-make-source buffer-title helm-source-buffers :buffer-list (lambda () buffers )) )
                               )
                            res
                            )
                          ) (helm-buffer-group-by-projectile-root)) 
             ))
    (helm :sources sources
          :buffer "*helm buffers*"
          :truncate-lines helm-buffers-truncate-lines
          :left-margin-width helm-buffers-left-margin-width))
  )

