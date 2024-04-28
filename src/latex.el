;; -*- lexical-binding: t; -*-

;; Usage:
;; 
;; <C-c C-c> to compile/view in Zathura
;; <C-c {> for envs

(use-package cdlatex
  :ensure t)

(use-package auctex
  :ensure t
  :defer t
  :hook
  (LaTeX-mode . (lambda () (push (list 'output-pdf "Zathura") TeX-view-program-selection)))
  (LaTeX-mode . turn-on-cdlatex)
  (LaTeX-mode . hl-todo-mode)
  :config
  (setq tex-fontify-script nil)
  (setq tex-suscript-height-ratio 1.0)
  (setq tex-subcript-height-ratio 1.0)  
  (setq tex-font-script-display (quote (-0.0 0.0)))
  (setq TeX-fold-mode nil)
  (eval-after-load "tex-mode" '(fset 'tex-font-lock-suscript 'ignore))
  (eval-after-load "tex-mode" '(fset 'tex-font-lock-subcript 'ignore))
  (setq TeX-command-list (cons TeX-command-list '("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil

                                                  (latex-mode doctex-mode)
                                                  :help "Run LaTeX"))))

(setq font-latex-fontify-script nil)
(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook 'linum-mode)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)


