;; -*- lexical-binding: t; -*-

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))
