;; -*- lexical-binding: t; -*-

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook #'lsp-deferred))

(use-package lsp-metals
  :ensure t)
