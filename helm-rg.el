(require 'helm)
(require 'helm-rg)

(defun projectile-helm-do-grep-rg (arg)
  "Projectile version of `helm-rg'."
  (interactive "P")
  (require 'helm-files)
  (if (projectile-project-p)
      (helm-rg (projectile-project-root) arg) 
    (error "You're not in a project")
    )

  )
