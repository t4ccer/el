;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-snippet nil)
  ;; (setq lsp-diagnostics-provider :none)
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (purescript-mode . lsp-deferred)
   (haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (use-package yasnippet
    :ensure t)
  (use-package lsp-ui
    :ensure t
    :config
    ;; (setq-local eldoc-documentation-function #'ignore)
    ;; (setq lsp-lens-enable nil)
    ;; (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-doc-show-with-cursor t)
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-ui-doc-max-height 10)
    (setq lsp-ui-sideline-enable t)
    (setq lsp-ui-doc-position 'at-point))
  (use-package dap-mode
    :ensure t))
