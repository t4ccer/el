;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs$" "\\.lhs$"))

(use-package lsp-haskell
  :ensure t
  :after envrc 
  :config
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  (add-hook 'haskell-literate-mode-hook #'lsp-deferred)
  (setq lsp-haskell-server-path "haskell-language-server"))
