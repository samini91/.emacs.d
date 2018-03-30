;;;;;;;;;;;;;;;;;;;;;Ensime Mode;;;;;;;;;;;;;;;;;;;;;;;

(define-key ensime-mode-map (kbd "M-n") nil)
(define-key ensime-mode-map (kbd "M-p") nil)
(setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)

(defun my-scala-mode-setup ()
  (setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)
)

(define-key ensime-mode-map (kbd "C-c C-c") 'comment-region)
(define-key ensime-mode-map (kbd "C-c C-d") 'uncomment-region)
(add-hook 'scala-mode-hook 'my-scala-mode-setup t)
