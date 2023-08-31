;;; -*- lexical-binding: t; -*-

(require 'projectile)
(require 'helm)
(require 'cl-lib)

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


(defun helm-buffers-list--init-sa (buffers)
  (print buffers)
  (require 'dired)
  ;; Bug#51 Create the list before `helm-buffer' creation.
  ;; We were using a global cache in the past and 'candidates was
  ;; bound to this cache, this was a problem when using more than one
  ;; source with a different 'buffer-list fn as the same cache was
  ;; reused in each source (Bug#1907), now 'candidates attr is set
  ;; directly so that each list of candidates is local to source.
                                        ;(helm-set-attr 'candidates (funcall (helm-get-attr 'buffer-list)))
  (helm-set-attr 'candidates (funcall (helm-get-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (
                                               helm-get-attr
                                               'filtered-candidate-transformer
                                               buffers))
                         for b in (if allbufs
                                      (helm-get-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-get-attr 'candidates)
                                     buffers))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))



(defclass helm-source-buffers-sa (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
;   (init :initform 'helm-buffers-list--init-sa)
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

(defun helm-buffers-list-sa ()
  "Preconfigured `helm' to list buffers."

  (interactive)
  ;; satodo

  (setq sources (mapcar (lambda (l)
                          ;(setq lexical-binding t)
                          (let*
                              (
                               (buffname (if (eq (car l) nil) "No Project" (car l)) )
                               (buffers (seq-map (lambda (x) (cdr x) ) (cdr l)) )
                               (lm (lambda () (print "WORKS?") (print "Empty?") buffers ))
                               (res (helm-make-source buffname helm-source-buffers-sa :buffer-list lm) )
                               )
                            res
                            )
                          )    (helm-buffer-group-satodo)) 
        )

  (helm :sources sources
        :buffer "*helm buffers*"
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width))
