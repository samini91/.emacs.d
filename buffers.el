(require 'projectile)
(require 'helm)


(defun helm-buffer-list-satodo (&optional visibles)
  (cl-loop for b in (buffer-list)
           for bn = (buffer-name b)
           for bu = (get-buffer b)
           for fbn  = (condition-case nil
                          (buffer-file-name bu)
                        (error nil)
                        )
           for root = (if (eq fbn nil) nil (projectile-project-root fbn)) 
           ;unless (member bn visibles)
           collect (cons root bn)))
           ;;collect alst (root . b) ))

(defun helm-buffer-group-satodo ()
  (seq-group-by (lambda (x) (car x)) (helm-buffer-list-satodo) )
  )


(defclass helm-source-buffers-sa (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'helm-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform 'helm-buffer-map)
   (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))


(defun hmm ()
  (helm-make-source "Buffers" 'helm-source-buffers-sa)
  )


(defun helm-buffers-list-sa ()
  "Preconfigured `helm' to list buffers."
  (interactive)
  ;; satodo

  (setq sources (mapcar (lambda (l)  
                          (setq buffname (car l))
                          (setq vals (cdr l))
                          (message "This is the buffer name")
                          (message buffname)
                          (setq buffers (seq-map (lambda (x) (cdr x) ) (cdr l)) )
                          ;(message buffers)
                          ;      (message (cdr (cdr vals)))
                          ;      (message "Hello")
                                       
                                        ;message ( car l )
                          (setq helm-buffer-list buffers)
                          (helm-make-source "BufferWTF " 'helm-source-buffers-sa)
                          )   (helm-buffer-group-satodo) )
        )

  (setq helm-source-buffers-list
        ;(helm-make-source "Buffeasdfrs" 'helm-source-buffers-sa)
        (car sources)

        )
  (helm :sources '(helm-source-buffers-list
                   helm-source-buffer-not-found)
        :buffer "*helm buffers*"
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width))
