;; -*- lexical-binding: t; -*-

(defun t4/load-file (file)
  (load-file (concat "~/.emacs.d/src/" file)))

(t4/load-file "package-management.el")

;; Customs config
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; theme is loaded early to minimalize light theme time
(t4/load-file "theme.el")

;; org is loaded early to override the default version
(t4/load-file "org.el")

(setq-default indent-tabs-mode nil)

(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m m") `mc/edit-lines)
  (global-set-key (kbd "C-c m n") `mc/mark-next-like-this)
  (global-set-key (kbd "C-c m p") `mc/mark-previous-like-this))


(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c j") 'ace-jump-mode)
  )

(use-package agenix
  :ensure t
  :straight
  ( :host github
    :repo "t4ccer/agenix.el"
    :branch "main"
    :files ("*.el")))

(use-package horth-mode
  :ensure t
  :straight
  ( :host github
    :repo "t4ccer/horth"
    :branch "main"
    :files ("editor/emacs/*.el")))

;; xref config
;; TODO: Turn into minor mode maybe?
(defun t4/load-xref-map ()
  (local-set-key (kbd "C-c x d") 'xref-find-definitions)
  (local-set-key (kbd "C-c x r") 'xref-find-references)
  (local-set-key (kbd "C-c x p") 'xref-go-back))

;; Starts separate eshell each time, instead of reusing existing one
(defun t4/eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))
(global-set-key (kbd "C-c e n") `t4/eshell-new)

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-immediate-done)
  (ivy-mode))

(use-package counsel
  :ensure t
  :if 'ivy
  :config
  (counsel-mode))

;; TODO: Make it async
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode)
  ;; Fix for not applying envrc correctly
  (add-hook 'eshell-directory-change-hook (lambda () (progn (envrc-mode) (envrc-mode)))))

(use-package auctex
  :ensure t
  :defer t)

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . t4/org-mode-visual-fill))

(use-package adaptive-wrap
  :ensure t)

(use-package gnuplot
  :ensure t)

(use-package sage-shell-mode
  :ensure t)

(defvar disable-tramp-backups '(all))

(eval-after-load "tramp"
  '(progn
     ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
     (setq backup-enable-predicate
           (lambda (name)
             (and (normal-backup-enable-predicate name)
              ;; Disable all tramp backups
              (and disable-tramp-backups
                   (member 'all disable-tramp-backups)
                   (not (file-remote-p name 'method)))
              (not ;; disable backup for tramp with the listed methods
               (let ((method (file-remote-p name 'method)))
                 (when (stringp method)
                   (member method disable-tramp-backups)))))))

     (defun tramp-set-auto-save--check (original)
       (if (funcall backup-enable-predicate (buffer-file-name))
           (funcall original)
         (auto-save-mode -1)))

     (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)

     ;; Use my ~/.ssh/config control master settings according to https://puppet.com/blog/speed-up-ssh-by-reusing-connections
     (setq tramp-default-method "ssh")
     (setq tramp-ssh-controlmaster-options "")))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path
        '( ("~/repos/" . 3)
           "~/.config/"))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line "Projectile")
  (projectile-mode))

(use-package project
  :ensure t
  :config
  (setq project-switch-commands t))

;; TODO: Replace it
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))

(global-eldoc-mode 1)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :after envrc-mode
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-keymap-prefix "C-c l")
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :config
  (setq-local eldoc-documentation-function #'ignore)
  (setq lsp-lens-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-doc-max-height 10)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-position 'at-point))

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

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs$" "\\.lhs$"))

(use-package lsp-haskell
  :ensure t
  :after envrc 
  :config
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  (add-hook 'haskell-literate-mode-hook #'lsp-deferred)
  (setq lsp-haskell-server-path "haskell-language-server"))

;;; Nix

(use-package yaml-mode
  :ensure t)

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package proof-general
  :ensure t)

(use-package company-coq
  :ensure t
  :requires 'proof-general
  :config
  (add-to-list 'auto-mode-alist '("\\.v\\'" . coq-mode))
  (add-hook 'coq-mode-hook #'company-coq-mode))


(use-package typescript-mode
  :ensure t)

(use-package posframe
  :ensure t)

(use-package flymake-posframe
  :ensure t
  :straight (:host github :repo "Ladicle/flymake-posframe")
  :hook (flymake-mode . flymake-posframe-mode))


;; auto-complete setup, sequence is important

(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-modes 'latex-mode) ; beware of using 'LaTeX-mode instead
  (add-to-list 'ac-modes 'org-mode)
  (use-package ac-math
    :ensure t
    :config
    (defun my-ac-latex-mode () ; add ac-sources for latex
      (setq ac-sources
         (append '(ac-source-math-unicode
           ac-source-math-latex
           ac-source-latex-commands)
                 ac-sources)))
    (add-hook 'LaTeX-mode-hook 'my-ac-latex-mode)
    (setq ac-math-unicode-in-math-p t)
    (ac-flyspell-workaround))) ; fixes a known bug of delay due to flyspell (if it is there)

(define-key grep-mode-map (kbd "C-c C-o") 'find-file-at-point)

;;; todo highlighting
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

(use-package csv-mode
  :ensure t
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))

(use-package elm-mode
  :ensure t)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(use-package copilot
  :ensure t
  :straight (copilot :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (setq copilot-node-executable "copilot-node")
  (define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-c a n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "C-c a p") 'copilot-next-completion)
  (add-hook 'prog-mode-hook 'copilot-mode)
  ;; nixpkgs lags with copilot
  ;; TODO: Disable on in nixpkgs repo
  (add-hook 'nix-mode-hook (lambda () (copilot-mode 0))))

(use-package restclient
  :ensure t)

(use-package ledger-mode
  :ensure t)

(use-package symbol-overlay
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode))

(use-package protobuf-mode
  :ensure t)

(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred))

(t4/load-file "lisps.el")
(t4/load-file "rust.el")
(t4/load-file "nix.el")
(t4/load-file "compilation-mode.el")
(t4/load-file "git.el")
(t4/load-file "purescript.el")

