;; -*- lexical-binding: t; -*-

(define-prefix-command 't4/todo)

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  warning bold)
          ("NOTE"  warning bold)
          ("FIXME" error bold)
	  ("todo!" error bold)
          ("HACK"  error bold)))

  (define-key hl-todo-mode-map (kbd "C-c t") 't4/todo)
  (define-key t4/todo (kbd "p") 'hl-todo-previous)
  (define-key t4/todo (kbd "n") 'hl-todo-next)
  (define-key t4/todo (kbd "o") 'hl-todo-occur)
  (define-key t4/todo (kbd "i") 'hl-todo-insert)
  :hook ((prog-mode . hl-todo-mode)))
