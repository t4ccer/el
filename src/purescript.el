;;; -*- lexical-binding: t -*-

;; FIXME: purescript-mode has broken brackets parsing, using haskell-mode instead

;; (use-package purescript-mode
;;   :ensure t
;;   :config
;;   (add-hook 'purescript-mode-hook 'purescript-indentation-mode))

(define-derived-mode purescript-hs-mode haskell-mode "purescript-hs"
  "haskell-mode pretending to be purescript-mode"
  (t4/load-xref-map)

  (lsp-mode 1)
  
  ;; LSP support
  (let* ((client (gethash 'pursls lsp-clients))
         (new-modes (append (lsp--client-major-modes client) '(purescript-hs-mode))))
    (setf (lsp--client-major-modes client) new-modes)
    (puthash 'pursls client lsp-clients))
  (setq lsp-purescript-formatter "purs-tidy"))

(add-to-list
 'compilation-error-regexp-alist
 '("^\\[[0-9]+/[0-9]+\s[[:alnum:]]+\\]\s+\\(.*?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$"
   1 2 3))

(add-to-list 'auto-mode-alist '("\\.purs$" . purescript-hs-mode))
