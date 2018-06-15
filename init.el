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
(use-package magit)
(use-package omnisharp)
(use-package flycheck)
(use-package ensime)
(use-package yasnippet)
(use-package async)
(use-package auto-complete)
(use-package csharp-mode)
(use-package dash)
(use-package popup)
(use-package helm)
(use-package helm-projectile)
(use-package swiper-helm)
(use-package smartparens)


;;;;;;;;;;;;;;;;;;;Universal KeyChords;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-n")
            (lambda ()
              (interactive)
              (ignore-errors (next-line 5))))

(global-set-key (kbd "M-p")
		(lambda ()
		  (interactive)
		  (ignore-errors (previous-line 5))))

(key-chord-mode 1)
(setq key-chord-two-keys-delay .040)

(key-chord-define-global ";s" 'swiper-helm)
(key-chord-define-global ";a" 'helm-buffers-list)
(key-chord-define-global ";w" 'other-window)
(key-chord-define-global ";q" 'helm-projectile)

(global-set-key (kbd "C-x rl") 'helm-bookmarks )

(global-set-key (kbd "C-x g") 'magit-status )

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;;;;;;;;;;;;;;;;;;;;Global-Modes;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'projectile-mode)
(setq company-idle-delay '0)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(company-backends
   (quote
    (company-omnisharp company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
		       (company-dabbrev-code company-gtags company-etags company-keywords)
		       company-oddmuse company-dabbrev)))
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend)))

 '(custom-enabled-themes (quote (deeper-blue)))
 '(ensime-startup-notification nil)
 '(ensime-typecheck-idle-interval 0.1)
 '(global-company-mode t)
 '(omnisharp-auto-complete-want-documentation nil)
 '(omnisharp-eldoc-support nil)
 '(package-selected-packages (quote (helm omnisharp monokai-theme key-chord company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;Load in specific settings;;;;;;;;;;;;;;;;;;;
(load-user-file "omnisharp.el")
(load-user-file "ensime.el")


