;;;;;;;;;;;;;;;;;;;;;Ensime Mode;;;;;;;;;;;;;;;;;;;;;;;

(define-key ensime-mode-map (kbd "M-n") nil)
(define-key ensime-mode-map (kbd "M-p") nil)
(setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-scala-mode-setup ()
  (setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)
  )

(defun ensime-startandevalbuffer()
  (interactive)
  (funcall 'ensime-inf-run-scala)
  ;;(funcall 'switch-)
  (funcall 'ensime-inf-eval-buffer)
  )

(define-key ensime-mode-map (kbd "C-c C-c") 'comment-region)
(define-key ensime-mode-map (kbd "C-c C-d") 'uncomment-region)
(define-key ensime-mode-map  (kbd "C-.") 'ensime-import-type-at-point)
(key-chord-define ensime-mode-map ";e" 'ensime-print-errors-at-point)
(key-chord-define ensime-mode-map ";t" 'ensime-type-at-point)


;;(key-chord-define ensime-mode-map ";r" 'ensime-repl)
;;(key-chord-define ensime-mode-map ";r" 'ensime-inf-run-scala)

;;(key-chord-define ensime-mode-map "[r" 'ensime-inf-eval-buffer)

;;(key-chord-define ensime-mode-map ";r"
;;		  (lambda ()
;;		    (ensime-inf-eval-buffer 1)
;;		    (ensime-inf-run-scala 1)))

;;(key-chord-define ensime-mode-map ";r" 'ensime-inf-eval-buffer)
;;(key-chord-define ensime-mode-map "[s" 'ensime-inf-run-scala)

;;(define-key ensime-mode-map (kbd "C-t") 'ensime-type-at-point)


(add-hook 'scala-mode-hook 'my-scala-mode-setup t)
(add-hook 'scala-mode-hook 'ensime)

(add-hook 'java-mode-hook 'ensime)
