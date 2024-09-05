;; -*- lexical-binding: t; -*-

(defun t4/load-file (file)
  (load-file (concat "~/.emacs.d/src/" file)))

(t4/load-file "package-management.el")

;; Customs config
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defun apply-macro (macro arg-list)
  (eval
   `(,macro ,@(loop for arg in arg-list
                 collect `(quote ,arg)))))


;; NOTE: theme is loaded early to minimalize light theme time
(t4/load-file "theme.el")

;; NOTE: org is loaded early to override the default version
(t4/load-file "org.el")

(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)

(add-hook 'python-mode-hook
          (lambda ()
            (progn
	      (setq indent-tabs-mode nil)
	      (setq standard-indent 4))))
(add-to-list 'auto-mode-alist '("poetry\\.lock" . conf-toml-mode))

(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c j") 'ace-jump-mode))

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
(defun t4/etags-c ()
  (interactive)
  (shell-command "etags $(fd -ag '*.{c,h,cpp,hpp}')")
  (visit-tags-table "TAGS"))

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
  (ido-mode -1)
  (ivy-mode))

(use-package counsel
  :ensure t
  :if 'ivy
  :config
  (counsel-mode))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . t4/org-mode-visual-fill))

(use-package adaptive-wrap
  :ensure t)

(use-package gnuplot
  :ensure t)

(use-package sage-shell-mode
  :ensure t)

;; TODO: Replace it
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))

(global-eldoc-mode 1)

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

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

(use-package elm-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package ledger-mode
  :ensure t
  :config
  (setq ledger-report-use-native-highlighting t))

(use-package symbol-overlay
  :ensure t)

;; (use-package wakatime-mode
;;   :ensure t
;;   :config
;;   (global-wakatime-mode))

(use-package protobuf-mode
  :ensure t)

;; (setq image-dired-external-viewer "meh")
;; (setq image-dired-cmd-create-thumbnail-program "meh")
(setq dired-listing-switches "-lah")

(setq auto-revert-verbose nil)

(defun t4/new-repo ()
    "Create new directory & repository"
    (interactive)
    (let* ((repo-name (read-string "Name: "))
           (dir (concat "~/repos/github/t4ccer/" repo-name)))
      (dired-create-directory dir)
      (dired dir)
      (magit-init dir)))

(use-package fasm-mode
  :ensure t
  :straight (:host github :repo "the-little-language-designer/fasm-mode"))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . asm-mode))

(use-package string-inflection
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.psql\\'" . sql-mode))

(add-to-list 'auto-mode-alist '("\\.golden\\'" . compilation-mode))

(use-package aiken-mode
  :ensure t
  :straight
  ( :host github
    :repo "aiken-lang/aiken-mode"
    :branch "master"))

;; (t4/load-file "exwm.el")
(t4/load-file "projectile.el")
(t4/load-file "tramp.el")
(t4/load-file "lsp.el")
(t4/load-file "haskell.el")
(t4/load-file "polymode.el")
(t4/load-file "typescript.el")
(t4/load-file "purescript.el")
(t4/load-file "lean4.el")
(t4/load-file "company.el")
(t4/load-file "cobol.el")
(t4/load-file "lisps.el")
(t4/load-file "rust.el")
(t4/load-file "nix.el")
(t4/load-file "compilation-mode.el")
(t4/load-file "git.el")
(t4/load-file "llvm.el")
(t4/load-file "cgsuite.el")
(t4/load-file "scala.el")
(t4/load-file "unison.el")
(t4/load-file "tree-sitter.el")
(t4/load-file "agenix.el")
(t4/load-file "todo.el")
(t4/load-file "csv.el")
(t4/load-file "latex.el")
(t4/load-file "multiple-cursors.el")
(t4/load-file "markdown.el")
(t4/load-file "dired.el")
(t4/load-file "lambda-buffers.el")
(t4/load-file "piece.el")
(t4/load-file "stage0.el")

;; NOTE: Keep it at the end
(t4/load-file "envrc.el")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-c d r") `t4/new-repo)
