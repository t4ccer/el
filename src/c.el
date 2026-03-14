;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
  (add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)))
)



(use-package cmake-mode
  :ensure t)
