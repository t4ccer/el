;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-snippet nil)
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (purescript-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-lens-enable nil)
  (setq lsp-auto-execute-action nil) ; TODO: Make it 't in modes other than haskell
  (setq lsp-file-watch-threshold 10000)
  (use-package yasnippet
    :ensure t)
  (use-package lsp-ui
    :ensure t
    :config
    (setq lsp-ui-doc-show-with-cursor t)
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-ui-doc-max-height 10)
    (setq lsp-ui-sideline-enable t)
    (setq lsp-ui-doc-position 'at-point))
  (use-package dap-mode
    :ensure t))

(require 'lsp-mode)
(lsp-defun t4/lsp-fix-import ((action &as &CodeAction :command? :edit?))
  (interactive (list (lsp--select-action (lsp-code-actions-at-point "quickfix.import.extend.list.topLevel"))))
  (if (and (lsp-feature? "codeAction/resolve")
           (not command?)
           (not edit?))
      (lsp--execute-code-action (lsp-request "codeAction/resolve" action))
    (lsp--execute-code-action action))
  (save-buffer))
(define-key lsp-command-map (kbd "i") #'t4/lsp-fix-import)

(defun t4/nothing (&rest rest))

(defalias 'lsp--client-download-server-fn #'t4/nothing)
(defalias 'lsp-client-download-server-fn #'t4/nothing)
