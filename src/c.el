;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred))
