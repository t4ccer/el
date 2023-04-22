;; -*- lexical-binding: t; -*-

(use-package hl-todo
  :ensure t
  :config
    (setq hl-todo-keyword-faces
          `(("TODO"  warning bold)
            ("NOTE"  warning bold)
            ("FIXME" error bold)
            ("HACK"  error bold)))
    (define-key hl-todo-mode-map (kbd "C-c t p") 'hl-todo-previous)
    (define-key hl-todo-mode-map (kbd "C-c t n") 'hl-todo-next)
    (define-key hl-todo-mode-map (kbd "C-c t o") 'hl-todo-occur)
    (define-key hl-todo-mode-map (kbd "C-c t i") 'hl-todo-insert)
  :hook ((prog-mode . hl-todo-mode)))
