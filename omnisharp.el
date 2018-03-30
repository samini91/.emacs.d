(defun my-csharp-mode-setup ()
  (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation f)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4))

;;;;;;;;;;;;;;;;;;;;;;C Sharp Mode;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
(define-key csharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)
(define-key csharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'flycheck-mode)
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))


;;(setq omnisharp-server-executable-path "C:\\Users\\Mugen\\AppData\\Roaming\\.emacs.d\\omnisharp\\OmniSharp.exe")



