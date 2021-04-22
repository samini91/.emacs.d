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
   '("ac912f2a75346acdcee4f769435596e0f213599d11e377f58e5c864cf05cd46b" "071cb4e4867144e6288e8cfe3678949359fa88c2b46224ca573858771c6afc9f" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(fci-rule-color "#555556")
 '(helm-rg-default-glob-string "*.{js,cs,sql,html,less,tsx}")
 '(horizontal-scroll-bar-mode nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(lsp-keymap-prefix "C-p")
 '(objed-cursor-color "#e74c3c")
 '(omnisharp-auto-complete-want-documentation nil)
 '(omnisharp-company-match-type 'company-match-server)
 '(omnisharp-eldoc-support nil)
 '(omnisharp-expected-server-version "1.37.5")
 '(omnisharp-imenu-support t)
 '(package-selected-packages
   '(spacemacs-theme easy-kill helm-rg helm-rp helm-ag helm-jira org-jira lsp-html use-package swiper-helm smartparens restclient omnisharp meghanada magit lsp-ui lsp-javascript-typescript lsp-java kotlin-mode key-chord irony-eldoc helm-projectile flycheck-kotlin flycheck-irony ensime eglot company-lsp company-irony-c-headers company-irony))
 '(pdf-view-midnight-colors (cons "#d6d6d4" "#1c1e1f"))
 '(rustic-ansi-faces
   ["#1c1e1f" "#e74c3c" "#b6e63e" "#e2c770" "#268bd2" "#fb2874" "#66d9ef" "#d6d6d4"])
 '(scroll-bar-mode nil)
 '(vc-annotate-background "#1c1e1f")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b6e63e")
    (cons 40 "#c4db4e")
    (cons 60 "#d3d15f")
    (cons 80 "#e2c770")
    (cons 100 "#ebb755")
    (cons 120 "#f3a73a")
    (cons 140 "#fd971f")
    (cons 160 "#fc723b")
    (cons 180 "#fb4d57")
    (cons 200 "#fb2874")
    (cons 220 "#f43461")
    (cons 240 "#ed404e")
    (cons 260 "#e74c3c")
    (cons 280 "#c14d41")
    (cons 300 "#9c4f48")
    (cons 320 "#77504e")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
