;; -*- lexical-binding: t; -*-

(use-package paredit
  :ensure t
  :hook
  (emacs-lisp-mode . paredit-mode)
  (racket-mode . paredit-mode))

(use-package racket-mode
  :ensure t
  :config
  (add-hook
   'racket-mode-hook
   (lambda ()
     (define-key racket-mode-map (kbd "C-c C-c") 'racket-run))))
