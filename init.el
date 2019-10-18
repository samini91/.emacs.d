(package-initialize)
(require 'package)

;;;;;;;;;;;;;;; Allows specific setting loading ;;;;;;;;;;;;;;;
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

;;;;;;;;;;;;;; Load Files ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;;;;;;;;;;;;;;; Melpa ;;;;;;;;;;;;;;;
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ;; ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
;; For Stable Packages
;; package-archive-priorities '(("melpa-stable" . 1)))
package-archive-priorities '(("melpa" . 1)))

;;;;;;;;;;;;;;; Use-Package ;;;;;;;;;;;;;;;
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;; Automatically downloads "use-package" packages if missing ;;;;;;;;;;
(setq use-package-always-ensure t)


;;;;;;;;;;;;;;; Themes ;;;;;;;;;;;;;;;
(use-package spacemacs-theme
  :defer t
  )

(use-package all-the-icons
  )
;;(when (member "Inconsolata" (font-family-list)) (set-frame-font "Inconsolata-8:bold" t t))
(use-package hydra)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay .040)
  ;;;;;;;;;;;;;;; Helm KeyChords ;;;;;;;;;;;;;;;
  (key-chord-define-global ";s" 'swiper-helm)
  (key-chord-define-global ";a" 'helm-buffers-list)
  (key-chord-define-global ";f" 'helm-semantic-or-imenu)
  (key-chord-define-global ";d" 'helm-show-kill-ring)

  (defhydra hydra-global-helm (:color blue :hint nil)
    "Helm"
    ("s" swiper-helm "Swiper-Helm")
    ("a" helm-buffers-list "Buffer-List")
    ("w" helm-projectile-rg "RipGrep-Projectile")
    ("q" helm-projectile "Projectile")
    ("f" helm-semantic-or-imenu "Functions")
    ("d" helm-show-kill-ring "Kill-Ring")
    ("z" helm-rg "RipGrep")

    )
  (key-chord-define-global ";f" 'hydra-global-helm/body)

  (defhydra hydra-global-bookmarks (:color blue :hint nil)
    "Bookmarks"
    ("b" helm-bookmarks "Bookmarks")
    ("s" bookmark-set "Set Bookmark")
    )
  (key-chord-define-global ";b" 'hydra-global-bookmarks/body)

  
  (defhydra hydra-global-execute (:color blue :hint nil)
    "Execute"
    ("j" hydra-jira-menu/body "Hydra-Jira")
    ("e" projectile-run-eshell "Projectile Eshell")
    ("s" (lambda () (interactive) (eshell) (rename-uniquely) ) "Eshell")
    ("i" init-file "Init-File")
    ("r" replace-string "Replace String")
    ("m" helm-make-projectile "Makefile")
    ("w" ace-window "Window Management")
    )
  
  (key-chord-define-global ";x" 'hydra-global-execute/body)

  )

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t )
  (setq doom-themes-treemacs-theme "doom-colors") 
  (doom-themes-treemacs-config)
  (setq column-number-mode t)
  
  )

;;;;; Need to run (all-the-icons-install-fonts) for this to work properly
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes (featurep 'minions))
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 30)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-persp-name-icon nil)
  (setq doom-modeline-lsp t)
  ;; Whether display GitHub notifications or not. Requires `ghub` package.
  ;;(setq doom-modeline-github nil)
  ;;
  ;;  ;; The interval of checking GitHub.
  ;;  (setq doom-modeline-github-interval (* 30 60))
  ;;  ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  ;;  (setq doom-modeline-mu4e t)
  ;;  ;; Whether display irc notifications or not. Requires `circe' package.
  ;;  (setq doom-modeline-irc t)
  ;;  ;; Function to stylize the irc buffer names.
  ;;  (setq doom-modeline-irc-stylize 'identity)
  ;;
  ;;  ;; Whether display environment version or not
  ;;  (setq doom-modeline-env-version t)
  ;;  ;; Or for individual languages
  ;;    ;; Change the executables to use for the language version string
  ;;  (setq doom-modeline-env-load-string "...")

  )

(use-package flycheck)
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  )

;; Rectanlge Edits
(global-set-key (kbd "C-S-R") 'hydra-rectangle-execute/body)

(defhydra hydra-rectangle-execute (:color blue :hint nil)
  "Rectanlge Edits"
  ("w" copy-rectangle-as-kill "Copy")
  ("k" kill-rectangle "Kill")
  ("y" yank-rectangle "Yank")
  )

(use-package dash)
(use-package popup)
(use-package undo-tree
  :config

  (defhydra hydra-undo-tree-menu (:hint nil)
    "Undo Tree"
    ("v" undo-tree-visualize "Visualize" :color blue)
    )
  (key-chord-define-global ";u" 'hydra-undo-tree-menu/body)

  )
(use-package helm
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
)
(use-package helm-projectile)
(use-package helm-rg)
(use-package swiper-helm)
(use-package smartparens)
(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  )
(use-package helm-jira)
(use-package helm-make
  :config
  (setq helm-make-named-buffer t)
  )
(use-package org-jira
  :config
  ;;(setq jiralib-url "https://???.atlassian.net")

    (defhydra hydra-jira-menu (:hint nil)
    "Org Jira"
    ("g" org-jira-get-issues "Get all issues" :color blue)
    ("i" org-jira-get-issue "Get issue" :color blue)
    )
  
    )
(use-package auctex
  :defer t ;; need to defer it
  :config
  (require 'tex-site)
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  
  )

(use-package company-auctex
  :defer t 
  :config
  (push 'company-auctex company-backends)
  )

(use-package pdf-tools)
(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  )
(use-package htmlize)

(use-package magit
  :config
  (key-chord-define-global ";m" 'magit-status)
  )
(use-package forge
  :after magit)

(use-package treemacs
  :config
  (key-chord-define-global ";l" 'treemacs)
  )
(use-package treemacs-projectile)

(use-package yasnippet
  :config
  (yas-global-mode t)
  )
;;(use-package yasnippet-snippets)

(use-package async)

;;;;;;;;;;;; Zoom In-Out;;;;;;;;;;

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


;;;;;;;;;;;; Rainbow ;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )
;;(use-package rainbow-identifiers
;;  :config
;;  (setq rainbow-identifiers-mode t)
;;)

;;;;;;;;;;;;HTML;;;;;;;;;;;;;;;
(add-hook 'html-mode-hook
	  (setq-default indent-tabs-mode nil)
        (lambda ()
          (set (make-local-variable 'sgml-basic-offset) 4)))

;;;;;;;;;;;; Emacs-Lisp ;;;;;;;;;;

(defhydra hydra-emacs-lisp-menu (:hint nil)
  "Emacs Commands"
  ("e" eval-buffer "Eval Buffer" :color blue)
  )

(key-chord-define emacs-lisp-mode-map ";c" 'hydra-emacs-lisp-menu/body)

;;;;;;;;;;;; Haskell ;;;;;;;;;;;;
(use-package haskell-mode)
(use-package intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)

  (define-key haskell-mode-map (kbd "<f12>") 'intero-goto-definition)
  (key-chord-define haskell-mode-map  ";t" 'intero-type-at)
  
  (define-key intero-mode-map (kbd "C-c C-c") nil)
  (define-key haskell-mode-map (kbd "C-c C-c") 'comment-region)
  (define-key haskell-mode-map (kbd "C-c C-d") 'uncomment-region)

  (defhydra hydra-haskell-menu (:hint nil)
    "Haskell Commands"
    ("u" intero-uses-at "Find Usages" :color blue)
    ("l" intero-restart "Reload Intero" :color blue)
    ("c" intero-repl-eval-region "Repl Eval Region" :color blue)
    ("r" intero-repl-load "Repl Load" :color blue)
    ("s" intero-apply-suggestions "Apply Suggestions" :color blue)
    ("a" intero-repl "Repl" :color blue)
    ("t" (intero-type-at 't) "Insert Type At Point" :color blue)
    )
  
  (key-chord-define haskell-mode-map ";c" 'hydra-haskell-menu/body)
  )

(use-package ghc)
(use-package haskell-snippets)
(use-package company-cabal
  :config
  (push 'company-cabal company-backends)
  )
(use-package company-ghci
    :config
  (push 'company-ghci company-backends)
  )
(use-package company-ghc
    :config
  (push 'company-ghc company-backends)
  )
(use-package hindent)
(use-package hlint-refactor)
(use-package helm-hoogle)
(use-package flycheck-haskell)

;;;;;;;;;;; Elm ;;;;;;;;;;;;;;;;;
(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook 'flycheck-mode t)
  )
(use-package flycheck-elm)

;;;;;;;;;;;; PureScript ;;;;;;;;;

(use-package flycheck-purescript)
(use-package psci)
(use-package purescript-mode)
(use-package psc-ide
  :config
  (add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

  (key-chord-define purescript-mode-map  ";t" 'psc-ide-show-type)

  (defhydra hydra-purescript-menu (:hint nil)
    "Purescript Commands"
    ("l" psc-ide-server-start "Start Server" :color blue)
    ("q" psc-ide-server-quit "Quit Server" :color blue)
    ("b" psc-ide-rebuild "Rebuild" :color blue)
    )
  (setq psc-ide-rebuild-on-save t)
  (key-chord-define purescript-mode-map ";c" 'hydra-purescript-menu/body)
  )

;;;;;;;;;;;; C Sharp ;;;;;;;;;;;;
(use-package csharp-mode)
(use-package omnisharp
  :ensure t
  :config
  (defun my-csharp-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation f)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)
    (setq evil-shift-width 4))

  (setq omnisharp-auto-complete-want-documentation nil)
  (setq omnisharp-company-match-type (quote company-match-server))
  (setq omnisharp-eldoc-support nil)
  (setq omnisharp-imenu-support t)
  
  (define-key csharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)
  (define-key csharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)

  (define-key csharp-mode-map (kbd "C-c C-c") 'comment-region)
  (define-key csharp-mode-map (kbd "C-c C-d") 'uncomment-region)
  (key-chord-define csharp-mode-map  ";t" 'omnisharp-current-type-information)

  (defun open-in-visual-studio ()
    "Opens file in visual studios make sure you have the desired version of devenv.exe"
    (interactive)
    ;;(shell-command (concat "devenv.exe /\Edit " (buffer-file-name)))
    (shell-command (concat "devenv.exe /\Edit " (file-name-nondirectory(buffer-file-name))))
    )

  (defhydra hydra-c-sharp-menu (:hint nil)
    "Omnisharp Commands"
    ("u" omnisharp-find-usages "Find Usages" :color blue)
    ("l" omnisharp-reload-solution "Reload Solution" :color blue)
    ("q" omnisharp-stop-server "Stop Server" :color blue)
    ("r" omnisharp-rename "Rename" :color blue)
    ("e" omnisharp-solution-errors "Solution Errors" :color blue)
    ("o" open-in-visual-studio "Open in Visual Studio" :color blue)
    ("f" omnisharp-code-format-entire-file "Format Entire File" :color blue)
    ("g" omnisharp-code-format-region "Format Region" :color blue)    
    )

  (key-chord-define csharp-mode-map ";c" 'hydra-c-sharp-menu/body)

  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'flycheck-mode)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  (add-hook 'omnisharp-mode-hook
	    (lambda ()
	      (setq-local company-backends (list 'company-omnisharp))))
  )

;;;;;;;;;;;; Scala ;;;;;;;;;;;;

(use-package scala-mode)
(use-package sbt-mode)
;;(use-package lsp-scala)

;;(use-package ensime
;;  :ensure t
;;  :config
;;
;;  (defun my-scala-mode-setup ()
;;  (setq comment-start "/* "
;;	  comment-end " */"
;;	  comment-style 'multi-line
;;	  comment-empty-lines t)
;;  )
;;  
;;  (setq ensime-typecheck-idle-interval 0)
;;  (setq ensime-startup-notification nil)
;;
;;  (define-key ensime-mode-map (kbd "M-n") nil)
;;  (define-key ensime-mode-map (kbd "M-p") nil)
;;  (define-key ensime-mode-map (kbd "C-c C-c") 'comment-region)
;;  (define-key ensime-mode-map (kbd "C-c C-d") 'uncomment-region)
;;  (define-key ensime-mode-map  (kbd "C-.") 'ensime-import-type-at-point)
;;  (key-chord-define ensime-mode-map ";e" 'ensime-print-errors-at-point)
;;  (key-chord-define ensime-mode-map ";t" 'ensime-type-at-point)
;;
;;  (key-chord-define ensime-mode-map ";c" 'sbt-hydra)
;;
;;  (add-hook 'scala-mode-hook 'my-scala-mode-setup t)
;;  (add-hook 'scala-mode-hook 'ensime)
;;  )

;;;;;;;;;;;; C/C++ ;;;;;;;;;;;;
(use-package irony
  :ensure t
  :config
  (add-hook 'c-mode-hook
            (lambda ()
	      (unless (file-exists-p "Makefile")
		(set (make-local-variable 'compile-command)
                     ;; emulate make's .c.o implicit pattern rule, but with
                     ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                     ;; variables:
                     ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		     (let ((file (file-name-nondirectory buffer-file-name)))
                       (format "%s -c -o %s.o %s %s %s"
                               (or (getenv "CC") "gcc")
                               (file-name-sans-extension file)
                               (or (getenv "CPPFLAGS") "-DDEBUG=9")
                               (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			       file))))))

  ;; Windows performance tweaks
  ;;
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

  (key-chord-define c-mode-map ";t" 'irony-get-type)

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'irony-eldoc)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook
	    (lambda ()
	      (setq-local company-backends (list 'company-irony))))
  )

(use-package company-irony)
(use-package company-irony-c-headers)
(use-package irony-eldoc)
(use-package flycheck-irony)

;;;;;;;;;;;Html;;;;;;;;;;;;;;;

(add-hook 'html-mode-hook
	  ;;(define-key smgl-mode-map (kbd "C-c C-c") 'comment-region)
	  ;;(define-key smgl-mode-map (kbd "C-c C-d") 'uncomment-region)
;;	  (setq 'web-mode-markup-indent-offset 4)
;;	  )
	  (lambda () (set (make-local-variable 'sgml-basic-offset) 4))
          )

(use-package web-mode
  :config
  (define-key web-mode-map (kbd "C-c C-c") 'comment-region)
  (define-key web-mode-map (kbd "C-c C-d") 'uncomment-region)
  )

;;;;;;;;;;;PowerShell;;;;;;;;;;
(use-package powershell)

;;;;;;;;;;;; LSP ;;;;;;;;;;;;
(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'flycheck-mode t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode t)
    (add-hook 'lsp-mode-hook
	    (lambda ()
	      (setq-local company-backends (list 'company-lsp))))
    (add-hook 'javascript-mode-hook 'lsp)
    (add-hook 'html-mode-hook 'lsp)
  )
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-update-mode 'point)
  (setq lsp-ui-sideline-delay 0)
  )

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-async t)

  (push 'company-lsp company-backends)
  )

;;;;;;;;;;;; Lsp-Java ;;;;;;;;;;;;
(use-package lsp-java
  :ensure t

  :config
  (add-hook 'java-mode-hook 'lsp)
)

;;;;;;;;;;;; Javascript ;;;;;;;;;;;;
(with-eval-after-load 'js
  (setq-default indent-tabs-mode nil)
  (define-key js-mode-map (kbd "C-c C-c") 'comment-region)
  (define-key js-mode-map (kbd "C-c C-d") 'uncomment-region)
)

;;;;;;;;;;;; SQL ;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-c") 'comment-region)
  (define-key sql-mode-map (kbd "C-c C-d") 'uncomment-region)
)

;;;;;;;;;;;; Lsp-Python ;;;;;;;;;;;;
(add-hook 'python-mode-hook 'lsp)

;;;;;;;;;;;;;;;; Lsp-Dap;;;;;;;;;;;;;;;;;; 
(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;;;;;;;;;;;;;;; LeetCode;;;;;;;;;;;;;;;;
(use-package leetcode)

;;;;;;;;;;;;;;; Docker ;;;;;;;;;;;;;;;;;;
(use-package docker)
(use-package docker-api)
(use-package docker-tramp)
(use-package helm-tramp)

;;;;;;;;;;;;;;;; Kubernetes ;;;;;;;;;;;;;;
(use-package k8s-mode)
(use-package kubernetes)
(use-package kubernetes-helm)
(use-package kubernetes-tramp)

;;;;;;;;;;;;;;; Universal KeyChords ;;;;;;;;;;;;;;;

(global-set-key (kbd "M-n")
            (lambda ()
              (interactive)
              (ignore-errors (next-line 5))))

(global-set-key (kbd "M-p")
		(lambda ()
		  (interactive)
		  (ignore-errors (previous-line 5))))


(setq compilation-always-kill t)
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
			       (save-all)
                               (setq-local compilation-read-command nil)
			       (projectile-with-default-dir (projectile-project-root)
				 (call-interactively 'compile))))




;;;;;;;;;;;;;;; Window Navigation ;;;;;;;;;;;;;;;

(use-package ace-window
  :config
  (key-chord-define-global ";w" 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )
(load-user-file "framemove.el")
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
;;(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x l") 'windmove-right)

(global-unset-key (kbd "C-x C-c"))

;;;;;;;;;;;;;;;;;;;; Global-Modes ;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'projectile-mode)
(add-hook 'after-init-hook 'helm-mode)

(setq company-idle-delay '0)
(setq company-tooltip-idle-delay '0)

;;;;;;;;;;;;;;;;;;Org Mode;;;;;;;;;;;;;;;;;;

;;(use-package org-pomodoro)
(defun org-html-header-readtheorg ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/setup/theme-readtheorg.setup")
  )

(defun org-html-header-bigblow ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/setup/theme-bigblow.setup")
  )

(defhydra hydra-org-menu (:hint nil)
  "Org Mode Commands"
  ;;("p" org-pomodoro "Pomodoro" :color blue)
  ("t" hydra-org-html-menu/body "Html" :color blue)
    )

(defhydra hydra-org-html-menu (:hint nil)
    "Html"
    ("r" org-html-header-readtheorg "Read the Org Header" :color blue)
    ("b" org-html-header-bigblow "Big Blow Header" :color blue)
    ("h" org-html-export-to-html "Export to Html" :color blue)
    )

(key-chord-define org-mode-map ";c" 'hydra-org-menu/body)


;;;;;;;;;;;;; Dired Functions ;;;;;;;;;;;;;;
(define-key dired-mode-map (kbd "<DEL>") 'dired-up-directory)

;;;;;;;;;;;;; Buffer Management ;;;;;;;;;;;;;;;;;;;
(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)


;;;;;;;;;;;;; Miscelanous Functions ;;;;;;;;;;;;;;

;; Causes buffer to always have the latest version (if using an external editor)
(global-auto-revert-mode t)

(setq ediff-split-window-function 'split-window-horizontally)

;; Removes Splash Screen
(setq inhibit-startup-message t)
;;Set title frame
(setq frame-title-format '("Gorgeous"))
;; remove bars
(menu-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(tool-bar-mode -1)

;; No bell
(setq ring-bell-function 'ignore)



;;;;;;;;;; Moves Backup Files to another directory ;;;;;;;;;;
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms `((".*" "~/" t)))
(setq create-lockfiles nil)

;; Save All Func
 (defun save-all ()
    (interactive)
    (save-some-buffers t))

;; Jump to Init File
(defun init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

;;(display-time)
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 3
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;;;;;;;;;;;;;;;;;;Custom File;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
