(package-initialize)

(require 'package)
(require 'use-package)

;;;;;;;;;;;;Melpa;;;;;;;;;;;;;;;;
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
 ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;;Stable
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;;;;;;;;;;;; Automatically downloads "use-package" packages if missing;;;;;;;;;;
(setq use-package-always-ensure t)
(use-package key-chord)
(use-package company)
(use-package magit)
(use-package omnisharp)
;;;;;;;;;;;;;;;;;;;KeyChords;;;;;;;;;;;;;;;;;;

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

(key-chord-define-global ";s" 'switch-to-buffer)
(key-chord-define-global ";w" 'other-window)

;;;;;;;;;;;;;;;;;;;;Company-Mode;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)
  (defun my-csharp-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    (local-set-key (kbd "C-c C-c") 'recompile))


;;;;;;;;;;;;;;;;;;;;;;C Sharp Mode;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(add-hook 'csharp-mode-hook 'omnisharp-mode)
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

;;;;;;;;;;;;; Custom Theme ;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages (quote (omnisharp monokai-theme key-chord company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(setq omnisharp-server-executable-path "C:\\Users\\Mugen\\AppData\\Roaming\\.emacs.d\\omnisharp\\OmniSharp.exe")
