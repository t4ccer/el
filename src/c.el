;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key  (kbd "C-c o") 'ff-find-other-file))))



(use-package cmake-mode
  :ensure t)
