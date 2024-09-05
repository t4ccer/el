;; -*- lexical-binding: t; -*-

;; TODO: Make it async
(use-package envrc
  :ensure t
  ;; :hook (agenix-pre-mode . envrc-mode)
  :config
  (envrc-global-mode)
  (add-hook 'agenix-pre-mode-hook #'envrc-mode)
  (add-hook 'haskell-mode-hook #'envrc-mode)
  (add-hook 'rustic-mode-hook #'envrc-mode)
  ;; Fix for not applying envrc correctly
  (add-hook 'eshell-directory-change-hook (lambda () (progn (envrc-mode) (envrc-mode)))))
