;; -*- lexical-binding: t; -*-

;; Usage:
;; 
;; <C-c C-c> to compile/view in Zathura
;; <C-c {> for envs

(use-package cdlatex
  :ensure t)

(use-package auctex
  :ensure t
  :hook
  (LaTeX-mode . hl-todo-mode)
  :config
  (setq tex-fontify-script nil)
  (setq tex-suscript-height-ratio 1.0)
  (setq tex-subcript-height-ratio 1.0)  
  (setq tex-font-script-display (quote (-0.0 0.0)))
  (setq TeX-fold-mode nil)
  (eval-after-load "tex-mode" '(fset 'tex-font-lock-suscript 'ignore))
  (eval-after-load "tex-mode" '(fset 'tex-font-lock-subcript 'ignore)))

(setq font-latex-fontify-script nil)
(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook (lambda () (push (list 'output-pdf "Zathura") TeX-view-program-selection)))
(add-hook 'LaTeX-mode-hook (lambda () (define-key TeX-mode-map (kbd "C-M-<return>") 'texpresso-move-to-cursor)))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(use-package texpresso
  :ensure t
  :straight
  ( :host github
    :repo "let-def/texpresso"
    :branch "main"
    :files ("emacs/*.el"))
  :config
  (setq texpresso-follow-edition nil))
