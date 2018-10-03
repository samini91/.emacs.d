(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
