;; -*- lexical-binding: t; -*-

;; Usage:
;; 
;; <C-c C-c> to compile/view in Zathura
;; <C-c C-s> for sections
;; <C-c C-e> for envs
;; Open PDF in doc-view-mode to get automatic refresh

(use-package cdlatex
  :ensure t)

(use-package auctex
  :ensure t
  :defer t
  :hook
  (LaTeX-mode . (lambda () (push (list 'output-pdf "Zathura") TeX-view-program-selection)))
  (LaTeX-mode . turn-on-cdlatex)
  (LaTeX-mode . hl-todo-mode))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)