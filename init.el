(package-initialize)
(require 'package)

;;;;;;;;;;;;; Allows specific setting loading ;;;;;;;;;;;;;;;;;;;;;
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


;;;;;;;;;;;;Melpa;;;;;;;;;;;;;;;;
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ;; ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
;; For Stable Packages
;; package-archive-priorities '(("melpa-stable" . 1)))
package-archive-priorities '(("melpa" . 1)))


(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;; Automatically downloads "use-package" packages if missing;;;;;;;;;;
(setq use-package-always-ensure t)
(use-package key-chord)
(use-package company)
(use-package flycheck)
(use-package magit)
(use-package yasnippet)
(use-package async)

(use-package omnisharp
  :ensure t
  :config
  (load-user-file "omnisharp.el")
  (setq omnisharp-auto-complete-want-documentation nil)
  (setq omnisharp-company-match-type (quote company-match-server))
  (setq omnisharp-eldoc-support nil)
  (add-hook 'omnisharp-mode-hook
	    (lambda ()
	      (setq-local company-backends (list 'company-omnisharp))))
  )

(use-package ensime
  :ensure t
  :config
  (load-user-file "ensime.el")
  (setq ensime-typecheck-idle-interval 0)
  (setq ensime-startup-notification nil)
  )

(use-package irony
  :ensure t
  :config
  (load-user-file "irony.el")
  (add-hook 'irony-mode-hook
	    (lambda ()
	      (setq-local company-backends (list 'company-irony))))
  )

(use-package company-irony)
(use-package company-irony-c-headers)
(use-package irony-eldoc)
(use-package flycheck-irony)
(use-package csharp-mode)
(use-package dash)
(use-package popup)
(use-package helm)
(use-package helm-projectile)
(use-package helm-rg)
(use-package swiper-helm)
(use-package smartparens)
(use-package restclient)
(use-package org-jira
  :config
  ;;(setq jiralib-url "https://???.atlassian.net")
  )
(use-package helm-jira)


(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'flycheck-mode t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode t)
    (add-hook 'lsp-mode-hook
	    (lambda ()
	      (setq-local company-backends (list 'company-lsp))))
  )
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-update-mode 'point)
  (setq lsp-ui-sideline-delay 0)
  )

(use-package lsp-javascript-typescript
  :ensure t
  :config
  ;;;;Javascript;;;;;
  (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
  (add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support
  )

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-async t))

(use-package lsp-java
  :ensure t
  :config  
  (add-hook 'java-mode-hook
	    (lambda ()
	      (setq-local company-backends (list 'company-lsp))))

  (add-hook 'java-mode-hook 'lsp-java-enable)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'lsp-ui-mode))

(use-package lsp-html
  :ensure t
  :config  
  (add-hook 'html-mode-hook #'lsp-html-enable)
  (add-hook 'html-mode-hook 'lsp-ui-mode))

;;;;;;;;;;;;;;;;;;;Universal KeyChords;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-n")
            (lambda ()
              (interactive)
              (ignore-errors (next-line 5))))

(global-set-key (kbd "M-p")
		(lambda ()
		  (interactive)
		  (ignore-errors (previous-line 5))))

(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
			       (save-all)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(key-chord-mode 1)
(setq key-chord-two-keys-delay .040)

(key-chord-define-global ";s" 'swiper-helm)
(key-chord-define-global ";a" 'helm-buffers-list)
(key-chord-define-global ";w" 'helm-projectile-rg)
(key-chord-define-global ";q" 'helm-projectile)
(key-chord-define-global ";e" 'helm-semantic-or-imenu)

(global-set-key (kbd "C-x rl") 'helm-bookmarks )

(global-set-key (kbd "C-x g") 'magit-status )

;;(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x l") 'windmove-right)

(global-unset-key (kbd "C-x C-c"))

;;;;;;;;;;;;;;;;;;;;Global-Modes;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'projectile-mode)
(setq company-idle-delay '0)
(setq company-tooltip-idle-delay '0)

;;;;;;;;;;;;; Miscelanous Functions ;;;;;;;;;;;;;;

;; Causes buffer to always have the latest version (if using an external editor)
(global-auto-revert-mode t)

(setq ediff-split-window-function 'split-window-horizontally) ;; gonna try this out

;; Jump to Init File
(defun init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; Removes Splash Screen
(setq inhibit-startup-message t)
;;Set title frame
(setq frame-title-format '("Gorgeous"))
;; remove bars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; No bell
(setq ring-bell-function 'ignore)

;; No Word Wrap
(add-hook 'diff-mode-hook (lambda () (setq truncate-lines t)))

;;;;;;;;;; Moves Backup Files to another directory ;;;;;;;;;;
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms `((".*" "~/" t)))
(setq create-lockfiles nil)

;; Save All Func
 (defun save-all ()
    (interactive)
    (save-some-buffers t))


;;;;;;;;;;;;; Custom Theme ;;;;;;;;;;;;;;;

;;;We can change the size of the text with this function
;;(set-face-attribute 'default nil :height 100)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (helm-rg helm-rp helm-ag helm-jira org-jira lsp-html use-package swiper-helm smartparens restclient omnisharp meghanada magit lsp-ui lsp-javascript-typescript lsp-java kotlin-mode key-chord irony-eldoc helm-projectile flycheck-kotlin flycheck-irony ensime eglot company-lsp company-irony-c-headers company-irony))))

;;(load-user-file "meghanada.el")



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
