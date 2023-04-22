;; -*- lexical-binding: t; -*-

(use-package company
  :ensure t
  :hook
  (org-mode . company-mode)
  (lsp-mode . company-mode)
  (haskell-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)

  (define-key company-search-map (kbd "<return>") 'company-complete-selection)
  (define-key company-search-map (kbd "RET") 'company-complete-selection)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
    
  (use-package company-ctags
    :ensure t
    :config
    (global-set-key (kbd "M-?") 'company-ctags)))
