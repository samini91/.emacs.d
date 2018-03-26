(package-initialize)
(require 'package)

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

;;;;;;;;;;;;;;;;;;;KeyChords;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-n")
            (lambda ()
              (interactive)
              (ignore-errors (next-line 5))))

(define-key ensime-mode-map (kbd "M-n") nil)

(global-set-key (kbd "M-p")
             (lambda ()
              (interactive)
              (ignore-errors (previous-line 5))))

(define-key ensime-mode-map (kbd "M-p") nil)

(key-chord-mode 1)
(setq key-chord-two-keys-delay .040)

(key-chord-define-global ";s" 'switch-to-buffer)
(key-chord-define-global ";a" 'helm-buffers-list)
(key-chord-define-global ";w" 'other-window)
(key-chord-define-global ";q" 'helm-projectile)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;;;;;;;;;;;;;;;;;;;;Company-Mode;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'projectile-mode)
  (defun my-csharp-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation f)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    (local-set-key (kbd "C-c C-c") 'recompile))

;;;;;;;;;;;;;;;;;;;;;;C Sharp Mode;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
(define-key csharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)
(define-key csharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'flycheck-mode)
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

;;;;;;;;;;;;; Miscelanous Functions ;;;;;;;;;;;;;;

;; Causes buffer to always have the latest version (if using an external editor)
(global-auto-revert-mode t)

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
;; No Word Wrap
(add-hook 'diff-mode-hook (lambda () (setq truncate-lines t)))

;; No bell
(setq ring-bell-function 'ignore)

;; Moves Backup Files to another directory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms `((".*" "~/" t)))
(setq create-lockfiles nil)

;; Save All Func
 (defun save-all ()
    (interactive)
    (save-some-buffers t))



;; Moves Backup Files to another directory
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
 '(custom-enabled-themes (quote (deeper-blue)))
 '(ensime-startup-notification nil)
 '(omnisharp-auto-complete-want-documentation nil)
 '(omnisharp-eldoc-support nil)
 '(package-selected-packages (quote (helm omnisharp monokai-theme key-chord company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(setq omnisharp-server-executable-path "C:\\Users\\Mugen\\AppData\\Roaming\\.emacs.d\\omnisharp\\OmniSharp.exe")
