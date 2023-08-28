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
 '(custom-safe-themes
   '("1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(helm-buffers-sort-fn 'helm-fuzzy-matching-default-sort-fn)
 '(horizontal-scroll-bar-mode nil)
 '(ignored-local-variable-values
   '((checkdoc-package-keywords-flag)
     (highlight-80+-columns . 100)))
 '(package-selected-packages
   '(spacemacs-theme easy-kill helm-rg helm-rp helm-ag helm-jira org-jira lsp-html use-package swiper-helm smartparens restclient omnisharp meghanada magit lsp-ui lsp-javascript-typescript lsp-java kotlin-mode key-chord irony-eldoc helm-projectile flycheck-kotlin flycheck-irony ensime eglot company-lsp company-irony-c-headers company-irony))
 '(scroll-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#b6e3fc")))))
