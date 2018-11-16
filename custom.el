;;;;;;;;;;;;; Custom Theme ;;;;;;;;;;;;;;;
(if (eq system-type 'gnu/linux)
    ;;We can change the size of the text with this function
    (set-face-attribute 'default nil :height 75)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (spacemacs-theme easy-kill helm-rg helm-rp helm-ag helm-jira org-jira lsp-html use-package swiper-helm smartparens restclient omnisharp meghanada magit lsp-ui lsp-javascript-typescript lsp-java kotlin-mode key-chord irony-eldoc helm-projectile flycheck-kotlin flycheck-irony ensime eglot company-lsp company-irony-c-headers company-irony))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
