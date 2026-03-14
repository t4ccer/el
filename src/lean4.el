(use-package lean4-mode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  ;; to defer loading the package until required
  :commands (lean4-mode)
  :config
  (add-hook 'lean4-mode-hook #'lsp-deferred)
  (define-key lean4-mode-map (kbd "C-c C-v") 'lean4-toggle-info))
