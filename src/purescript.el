;;; -*- lexical-binding: t -*-

;; FIXME: purescript-mode is just broken, brackets parsing is broken,
;; indentation is just bad in all 3 modes, using haskell-mode instead
;; with a bit of customisation

;; (use-package purescript-mode
;;   :ensure t
;;   :hook
;;   (purescript-mode . purescript-indent-mode))

(define-derived-mode purescript-mode haskell-mode "purescript-hs"
  "haskell-mode pretending to be purescript-mode"
  (t4/load-xref-map)
  (haskell-indent-mode)
  (setq lsp-purescript-formatter "purs-tidy")
  (envrc-mode 1)
  (lsp-mode 1))

(add-to-list 'auto-mode-alist '("\\.purs$" . purescript-mode))

(add-to-list
 'compilation-error-regexp-alist
 '("^\\[[0-9]+/[0-9]+\s[[:alnum:]]+\\]\s+\\(.*?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$"
   1 2 3))
