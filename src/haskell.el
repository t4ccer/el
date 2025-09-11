;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs$" "\\.lhs$" "\\.hs-boot$"))

(use-package lsp-haskell
  :ensure t
  :after envrc 
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  (setq lsp-haskell-server-path "haskell-language-server")
  (setq lsp-haskell-plugin-ghcide-type-lenses-global-on nil)
  (setq lsp-haskell-plugin-class-code-lens-on nil)
  (setq lsp-haskell-plugin-import-lens-code-lens-on nil)
  (setq lsp-haskell-plugin-import-lens-code-actions-on nil)
  (setq lsp-haskell-plugin-ghcide-type-lenses-config-mode nil)
  (setq lsp-haskell-plugin-stan-global-on nil)
  :custom
  ; https://github.com/emacs-lsp/lsp-haskell/issues/192
  (lsp-rename-use-prepare nil)
  )
