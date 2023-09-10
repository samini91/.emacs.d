(require 'helm)
(require 'helm-rg)

(defvar helm-source-grep-rg nil)

(setq helm-grep-ag-command "rg --color=always --smart-case --line-number %s -- %s %s")
(defun helm-grep--rg-command ()
  (car (helm-remove-if-match
        "\\`[A-Z]*=" (split-string helm-grep-ag-command))))

(defcustom helm-grep-actions-rg
  (helm-make-actions
   "Find File" #'helm-rg--async-action
   )
  "Actions for helm grep."
  :type '(alist :key-type string :value-type function))


(defclass helm-grep-rg-class (helm-source-async)
  ((nohighlight :initform t)
   (pcre :initarg :pcre :initform t
         :documentation
         "  Backend is using pcre regexp engine when non--nil.")
   (keymap :initform 'helm-grep-map)
   (history :initform 'helm-grep-ag-history)
   (help-message :initform 'helm-grep-help-message)

   (persistent-action :initform #'helm-rg--async-persistent-action)
   (persistent-help :initform "Jump to line (`C-u' Record in mark ring)")

   (filter-one-by-one :initform #'helm-rg--parse-process-output)
   (display-to-real :initform #'helm-rg--display-to-real)

   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 2)
   (nomark :initform t)

   (action :initform 'helm-grep-actions-rg)

   (find-file-target :initform #'helm-grep-quit-an-find-file-fn)
   (group :initform 'helm-grep)))

(defun helm-grep-rg-1 (directory &optional type input)
  "Start helm ag in DIRECTORY maybe searching in files of type TYPE.
If INPUT is provided, use it as the search string."

  (setq helm-source-grep-rg
        (helm-make-source (upcase (helm-grep--rg-command)) 'helm-grep-rg-class
          :header-name (lambda (name)
                         (format "%s [%s]"
                                 name (abbreviate-file-name directory)))
          :candidates-process
          (lambda () (helm-grep-ag-init directory type))))

  (helm-set-local-variable 'helm-input-idle-delay helm-grep-input-idle-delay)

  (helm :sources 'helm-source-grep-rg
        :keymap helm-grep-map
        :history 'helm-grep-ag-history
        :input input
        :truncate-lines helm-grep-truncate-lines
        :buffer (format "*helm %s*" (helm-grep--rg-command))))


(defun helm-grep-rg (directory with-types)
  "Start grep RG in DIRECTORY.
When WITH-TYPES is non-nil provide completion on AG types."
  (require 'helm-adaptive)
  (let ((com (capitalize (helm-grep--rg-command))))
    (helm-grep-rg-1 directory
                    (helm-aif (and with-types
                                   (helm-grep-ag-get-types))
                        (helm-comp-read
                         (format "%s type: " com) it
                         :must-match t
                         :marked-candidates t
                         :fc-transformer 'helm-adaptive-sort
                         :buffer (format "*helm %s types*" com))))))

(defun helm-do-grep-rg (arg)
  "Preconfigured `helm' for grepping with RG in `default-directory'.
With prefix arg prompt for type if available with your AG
version."
  (interactive "P")
  (require 'helm-files)
  (helm-grep-rg (expand-file-name default-directory) arg)
  )

(defun projectile-helm-do-grep-rg (arg)
  "Projectile version of `helm-rg'."
  (interactive "P")
  (require 'helm-files)
  (if (projectile-project-p)
      (helm-grep-rg (projectile-project-root) arg) 
    (error "You're not in a project")
    )

  )
