;; SATODO add sentinel to make error nicer
(require 'helm)
(require 'helm-files)
(require 'helm-external)

(defvar helm-find-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map (kbd "DEL") 'helm-delete-backward-no-update)
    map))

(defun helm-findutils-transformer (candidates _source)
  (let (non-essential
        (default-directory (helm-default-directory)))
    (cl-loop for i in candidates
             for abs = (expand-file-name
                        (helm-aif (file-remote-p default-directory)
                            (concat it i) i))
             for type = (car (file-attributes abs))
             for disp = (if (and helm-ff-transformer-show-only-basename
                                 (not (string-match "[.]\\{1,2\\}$" i)))
                            (helm-basename abs) abs)
             collect (cond ((eq t type)
                            (cons (propertize disp 'face 'helm-ff-directory)
                                  abs))
                           ((stringp type)
                            (cons (propertize disp 'face 'helm-ff-symlink)
                                  abs))
                           (t (cons (propertize disp 'face 'helm-ff-file)
                                    abs))))))

(defvar helm-source-find-rg
  (helm-build-async-source "Find"
    :header-name (lambda (name)
                   (concat name " in [" (helm-default-directory) "]"))
                                        ;    :candidates-process 'helm-find-shell-command-fn
    :candidates-process (lambda ()
                          (let* (
                                 (pattern (helm-rg--helm-pattern-to-ripgrep-regexp helm-pattern))
                                 )
                            (start-file-process-shell-command "find" helm-buffer (format "rg --files --hidden | rg -S -- \"%s\" " pattern))
                            )
                          )
    :filtered-candidate-transformer 'helm-findutils-transformer
    :action-transformer 'helm-transform-file-load-el
    :persistent-action 'helm-ff-kill-or-find-buffer-fname
    :action 'helm-type-file-actions
    :help-message 'helm-generic-file-help-message
    :keymap helm-find-map
    :candidate-number-limit 9999
    :requires-pattern 1))

(defun helm-find-rg-dir (dir)
  (let ((default-directory (file-name-as-directory dir)))
    (helm :sources 'helm-source-find-rg
          :buffer "*helm find*"
          :ff-transformer-show-only-basename nil
          :case-fold-search helm-file-name-case-fold-search)))

(defun helm-find-rg (arg)
  (interactive "P")
  (let ((directory
         (if arg
             (file-name-as-directory
              (read-directory-name "DefaultDirectory: "))
           default-directory)))
    (helm-find-rg-dir directory)))
