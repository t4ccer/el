;; -*- lexical-binding: t; -*-

(use-package csv-mode
  :ensure t
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))
