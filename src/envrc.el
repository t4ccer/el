;; -*- lexical-binding: t; -*-

;; TODO: Make it async
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode)
  ;; Fix for not applying envrc correctly
  (add-hook 'eshell-directory-change-hook (lambda () (progn (envrc-mode) (envrc-mode)))))
