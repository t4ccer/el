;; -*- lexical-binding: t; -*-

;;; Package management setup  

;; (require 'package)
;; (setq package-archives
;;       '(("melpa"     . "https://melpa.org/packages/")
;;         ("org"       . "https://orgmode.org/elpa/")
;;         ("elpa"      . "https://elpa.gnu.org/packages/")
;;         ("marmalade" . "http://marmalade-repo.org/packages/")))
;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; Customs config
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable noise
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(blink-cursor-mode 0)

;; Silence non important "errors" 
(defun t4/command-error-function (data context caller)
  (when (not (memq (car data) '(beginning-of-line
                                end-of-line
                                beginning-of-buffer
                                end-of-buffer)))
  (command-error-default-function data context caller)))
  (setq command-error-function #'t4/command-error-function)
  (setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq vc-follow-symlinks t)

;;; Theme
(use-package autothemer
  :ensure t)

(use-package monokaish-theme
  :straight
  ( :host github
    :repo "t4ccer/monokaish.el"
    :branch "main"
    :files ("*.el")))

(load-theme 'monokaish t)

;;; Font
(setq t4/font "FiraCode 10")
(add-to-list 'default-frame-alist '(font . "FiraCode 10"))
(set-frame-font t4/font nil t)
(set-face-attribute 'default t :font t4/font)


;; Get face under the cursor
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
	          (get-char-property (point) 'face))))
    (if face (message "(%s (:foreground monokaish-))" face) (message "No face at %d" pos))))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (add-hook 'prog-mode-hook #'linum-relative-mode))

;; FIXME
;; Disable line numbers for some modes
;; (dolist (mode '(org-mode-hook
;;                 term-mode-hook
;;                 shell-mode-hook
;;                 ivy-mode-hook
;;                 eshell-mode-hook))
;;   (add-hook mode (lambda () (linum-relative-mode nil))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq-default indent-tabs-mode nil)

(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)

(use-package mode-line-idle
  :ensure t
  :commands (mode-line-idle)
  :config
   (defun t4/face-mode ()
    (interactive)
    ""
    (defvar my-word '(:eval (what-face)))
    (setq-default mode-line-format
                  (list "Example " '(:eval (list
                                            "Face: "
                                            (mode-line-idle 0.1 my-word "?" :interrupt t)))))
    )
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-c m m") `mc/edit-lines)
  (global-set-key (kbd "C-c m n") `mc/mark-next-like-this)
  (global-set-key (kbd "C-c m p") `mc/mark-previous-like-this)
  )

(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'haskell-mode-hook #'display-fill-column-indicator-mode)

(column-number-mode)

(use-package rainbow-mode)

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c j") 'ace-jump-mode)
  )

(use-package agenix
  :straight
  ( :host github
    :repo "t4ccer/agenix.el"
    :branch "main"
    :files ("*.el"))
  :config
  (define-key agenix-encrypted-mode-map (kbd "C-c a d") 'agenix-decrypt-buffer)
  (add-to-list 'auto-mode-alist '("\\.age" . agenix-encrypted-mode))
  (define-key agenix-decrypted-mode-map (kbd "C-c a e") 'agenix-encrypt-buffer))

(use-package horth-mode
  :straight
  ( :host github
    :repo "t4ccer/horth"
    :branch "main"
    :files ("editor/emacs/*.el")))

;; xref config
(defun t4/load-xref-map ()
  (local-set-key (kbd "C-c x d") 'xref-find-definitions)
  (local-set-key (kbd "C-c x r") 'xref-find-references)
  (local-set-key (kbd "C-c x p") 'xref-go-back))
(add-hook 'purescript-mode-hook 't4/load-xref-map)

;; Starts separate eshell each time, instead of reusing existing one
(defun t4/eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(global-set-key (kbd "C-c e n") `t4/eshell-new)

(use-package ivy
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
  :if 'ivy
  :config
  (counsel-mode))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode)
  ;; Fix for not applying envrc correctly
  (add-hook 'eshell-directory-change-hook (lambda () (progn (envrc-mode) (envrc-mode)))))

(defun t4/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.05))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.05))))
  '(org-level-6 ((t (:inherit outline-6 :height 1.05))))
  '(org-level-7 ((t (:inherit outline-7 :height 1.05))))
  '(org-level-8 ((t (:inherit outline-8 :height 1.05))))))

(defvar my-org-hidden-keywords
  '(title))

(defun t4/org-hide-keywords ()
  (save-excursion
    (let (beg end ov)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\(^[ \t]*#\\+\\)\\("
                      (mapconcat (lambda (kw)
                                   (format "%s:\s"(symbol-name kw)))
                                 my-org-hidden-keywords "\\|")
                      "\\)")
              nil t)
        (setq beg (match-beginning 1)
              end (match-end 2)
              ov  (make-overlay beg end))
    (overlay-put ov 'invisible t)))))

(defun t4/org-mode-setup ()
  (t4/org-hide-keywords)
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :ensure t
  :hook (org-mode . t4/org-mode-setup)
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-ellipsis " ▾")
  (setq org-agneda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq calendar-week-start-day 1) ;; Monday as first day of the week
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d)")
          (sequence "PLANNED(u)" "|" "DONE(d)")))
  (setq org-agenda-files '("~/org/tasks.org"))
  (plist-put org-format-latex-options :scale 1.25)
  (setq org-hide-emphasis-markers t)
  (defun t4/org-insert-image () (interactive)
         (org-insert-link)
         (org-display-inline-images))
  (t4/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●")))

;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

(use-package auctex
  :ensure t
  :defer t)

(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(defun t4/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . t4/org-mode-visual-fill))

(use-package adaptive-wrap
  :ensure t)

(use-package gnuplot
  :ensure t)

(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate nil)
(setq org-startup-with-inline-images t)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(use-package sage-shell-mode
  :ensure t)

(use-package ob-sagemath
  :ensure t)

(org-babel-do-load-languages 'org-babel-load-languages
     '((emacs-lisp . t)
       (shell      . t)
       (gnuplot    . t)
       (python     . t)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle)))

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

;;; elisp emacs lisp
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-region)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m b") `magit-blame))

;; (use-package forge
;;   :requires 'magit)

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

(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :after envrc-mode
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-keymap-prefix "C-c l")
  (lsp-enable-which-key-integration t))

(global-eldoc-mode 1)

(use-package lsp-ui
  :ensure t
  :straight (:local-repo "~/projects/third-party/lsp-ui")
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
  :config
  (setq company-minimum-prefix-length 3)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-search-map (kbd "<return>") nil)
    (define-key company-search-map (kbd "RET") nil)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)))

(use-package dhall-mode
  :mode "\\.dhall\\'")

;; (use-package purescript-mode
;;   :config
;;   (add-hook 'purescript-mode-hook 'purescript-indentation-mode))

(use-package haskell-mode
  :mode ("\\.hs$" "\\.purs$")
  :config)

(use-package lsp-haskell
  :after envrc 
  :config
  ;; (add-hook 'haskell-mode-hook #'lsp-deferred)
  ;; (add-hook 'haskell-literate-mode-hook #'lsp-deferred)
  (setq lsp-haskell-server-path "haskell-language-server"))

;; (use-package lsp-dart
;;   :config
;;   (add-hook 'dart-mode-hook #'lsp))

;; (use-package flutter
;;   :after dart-mode
;;   :bind (:map dart-mode-map
;;             ("C-M-x" . #'flutter-run-or-hot-reload))
;;   :custom
;;   (flutter-sdk-path "~/projects/third-party/flutter/"))

;;; Nix

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (defun t4/get-nix-hash (cmd url)
    (with-temp-buffer
      (shell-command (concat cmd " " url) (current-buffer))
      (goto-char (point-min))
      (next-line)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    
  (defun t4/nix-fetchurl ()
    (interactive)
    (let* ((start-position (point))
           (url (read-string "URL: "))
           (hash (t4/get-nix-hash "nix-prefetch-url" url)))
      (insert (concat
               "fetchurl {\n"
               "  url = \"" url "\";\n"
               "  sha256 = \"" hash "\";\n"
               "};"
               ))
      (indent-region start-position (line-end-position))
      (goto-char start-position)))

  (defun t4/nix-fetchFromGitHub ()
    (interactive)
    (let* ((start-position (point))
           (owner (read-string "Owner: "))
           (repo (read-string "Repo: "))
           (rev (read-string "Rev: "))
           (hash (t4/get-nix-hash "nix-prefetch-url --unpack"
                                  (concat "https://github.com/" owner "/" repo "/archive/" rev ".tar.gz"))))
      (insert (concat
               "fetchFromGitHub {\n"
               "  owner = \"" owner "\";\n"
               "  repo = \"" repo "\";\n"
               "  rev = \"" rev "\";\n"
               "  sha256 = \"" hash "\";\n"
               "};"
               ))
      (indent-region start-position (line-end-position))
      (goto-char start-position)))

  (global-set-key (kbd "C-c n f u") 't4/nix-fetchurl)
  (global-set-key (kbd "C-c n f h") 't4/nix-fetchFromGitHub)
  ;; (add-hook 'nix-mode-hook 'lsp-mode)
  (copilot-mode 0))


(use-package yaml-mode)

(use-package adoc-mode
  :mode "\\.adoc\\'")

(use-package proof-general)

(use-package company-coq
  :requires 'proof-general
  :config
  (add-to-list 'auto-mode-alist '("\\.coq\\'" . coq-mode))
  (add-hook 'coq-mode-hook #'company-coq-mode))

;;; Compilation mode config

;; Ignore warnings
(setq compilation-skip-threshold 2)
(with-eval-after-load 'compile
;; set cursor to follow compilation output
(setq compilation-scroll-output t))

;; FIXME: Haskell specific regex
;; ^\s*in the import of .*\s\([^:]+:\d+:\d+-\d+\).
;; (setq compilation-error-regexp-alist-alist ('('haskell ".*in the import of .* (\\([^:]+\\):\\([1-9]+\\):\\([1-9]+\\)-\\([1-9]+\\))\\." 1)))
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(
;;                ".*in the import of .* (\\([^:]+\\):\\([1-9]+\\):\\([1-9]+\\)-\\([1-9]+\\))\\."
;;              1 2 3))

;; ;; (comp
;; ilation-build-compilation-error-regexp-alist)

;; .*in the import of .* (\([^:]+\):\([1-9]+\):\([1-9]+\)-\([1-9]+\))\.


;; (add-list-to-list 'compilation-error-regexp-alist-alist 'haskell-compilation-error-regexp-alist)


;; (require 'ansi-color)

(defun t4/cd-and-compile ()
  (interactive)
  (call-interactively 'cd)
  (call-interactively 'compile))

(global-set-key (kbd "C-c c c") `t4/cd-and-compile)
(global-set-key (kbd "C-c c g") `recompile)

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
  (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (setq compilation-error-regexp-alist '(
  ;; (bash "(?:[a-zA-Z0-9]\\/?)+\\.hs:[0-9]+:[0-9]+: error:.*" 1 2 3 nil)))

(use-package typescript-mode)

(use-package posframe)
(use-package flymake-posframe
  :straight (:host github :repo "Ladicle/flymake-posframe")
  :hook (flymake-mode . flymake-posframe-mode))

;; FIXME

;; (use-package idris2-mode
;;   :straight (:host github :repo "redfish64/idris2-mode")
;;   :ensure t)

;; (use-package vterm
;;   :ensure t
;;   :config
;;   (use-package multi-vterm
;;     :ensure t
;;     :config
;;     (global-set-key (kbd "C-c v v") `multi-vterm)
;;     (global-set-key (kbd "C-c v n") `multi-vterm-next)
;;     (global-set-key (kbd "C-c v p") `multi-vterm-prev)
;;     (global-set-key (kbd "C-c v r") `multi-vterm-rename-buffer)
;;     )
;;   )


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

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(setq plain-assets-highlights
      '((";;.*" . font-lock-comment-face)
        (":BASE.*" . font-lock-doc-face)
        (":DATAFILE.*" . font-lock-doc-face)
        ("^[0-9]+-[0-9]+-[0-9]+" . font-lock-comment-face)
        ("@" . font-lock-keyword-face)
        ("?" . font-lock-keyword-face)
        ("\s[A-Z]+\s" . font-lock-constant-face)
        ("\s[A-Z]+$" . font-lock-constant-face)
        ("\s.+$" . font-lock-function-name-face)))

(define-derived-mode plain-assets-mode fundamental-mode
  (setq font-lock-defaults '(plain-assets-highlights))
  (setq mode-name "plain-assets"))

(add-to-list 'auto-mode-alist '("\\.pa\\'" . plain-assets-mode))

;;; todo highlighting
(use-package hl-todo
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
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))


;; (use-package js2-mode)
;; (use-package lsp-volar :straight (:host github :repo "jadestrong/lsp-volar"))
;; (use-package vue-mode
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp))


(use-package elm-mode)

(use-package tree-sitter)
(use-package tree-sitter-langs)

;;; 

(define-minor-mode se-mode
  "se-mode"
  :lighter " se"
  :keymap (let ((map (make-sparse-keymap))) map))


(add-hook 'python-mode-hook 'se-mode)
(add-hook 'se-mode-hook 'tree-sitter-mode)
(add-hook 'se-mode-hook 'se/init)

(define-key se-mode-map (kbd "C-c e e") `se/edit)
(define-key se-mode-map (kbd "C-c e r") `se/replace)

(defun se/init ()
  (interactive)
  (tree-sitter-mode)
  (setq se/overlay (make-overlay 0 0))
  (overlay-put se/overlay 'face '(:background "grey20")))

(defun se/stop ()
  (interactive)
  (delete-overlay se/overlay))

(defun se/edit ()
  (interactive)
  (let* ((node-start (tsc-node-start-position (tree-sitter-node-at-pos)))
         (node-end (tsc-node-end-position (tree-sitter-node-at-pos)))
         (node-old-text (buffer-substring node-start node-end))
         (prompt "Edit: "))
    (move-overlay se/overlay node-start node-end)
    (let ((inp (read-string prompt node-old-text)))
      (replace-region-contents node-start node-end (lambda () "" inp)))
    (move-overlay se/overlay 0 0)))

(defun se/replace ()
  (interactive)
  (let* ((node-start (tsc-node-start-position (tree-sitter-node-at-pos)))
         (node-end (tsc-node-end-position (tree-sitter-node-at-pos)))
         (node-old-text (buffer-substring node-start node-end))
         (prompt (concat "Replace " node-old-text " -> ")))
    (move-overlay se/overlay node-start node-end)
    (let ((inp (read-string prompt)))
      (replace-region-contents node-start node-end (lambda () "" inp)))
    (move-overlay se/overlay 0 0)))

(use-package copilot
  :straight (copilot :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (setq copilot-node-executable "copilot-node")
  (define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-c a n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "C-c a p") 'copilot-next-completion)
  (add-hook 'prog-mode-hook 'copilot-mode)
  (add-hook 'nix-mode-hook (lambda () (copilot-mode 0))))


;;; Rust

(use-package rustic
  :ensure t
  :straight t
  :defer t
  :config
  ; Envrc support sucks out of the box
  (setq rustic-lsp-server 'nil)
  (setq rustic-lsp-client 'nil)
  (add-hook 'rustic-mode-hook #'lsp-deferred)

  ; formatting
  (setq rustic-format-trigger 'on-save)
  (setq rustic-use-rust-save-some-buffers t)

  ; Fix compilation mode
  (setq rustic-compile 'compile)
  (setq rustic-recompile 'recompile)
  (defvar rustc-compilation-location
    (let ((file "\\([^\n]+\\)")
          (start-line "\\([0-9]+\\)")
          (start-col "\\([0-9]+\\)"))
      (concat "\\(" file ":" start-line ":" start-col "\\)")))

  (defvar rustc-compilation-regexps
    (let ((re (concat "^\\(?:error\\|\\(warning\\)\\|\\(note\\)\\)[^\0]+?--> "
                      rustc-compilation-location)))
      (cons re '(4 5 6 (1 . 2) 3)))
    "Specifications for matching errors in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")
  
  (defvar rustc-colon-compilation-regexps
    (let ((re (concat "^ *::: " rustc-compilation-location)))
      (cons re '(2 3 4 0 1)))
    "Specifications for matching `:::` hints in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")
  
  (defvar rustc-refs-compilation-regexps
    (let ((re "^\\([0-9]+\\)[[:space:]]*|"))
      (cons re '(nil 1 nil 0 1)))
    "Specifications for matching code references in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")

  ;; Match test run failures and panics during compilation as
  ;; compilation warnings
  (defvar cargo-compilation-regexps
    '("', \\(\\([^:]+\\):\\([0-9]+\\)\\)"
      2 3 nil nil 1)
    "Specifications for matching panics in cargo test invocations.
See `compilation-error-regexp-alist' for help on their format.")
  
  (defun rustc-scroll-down-after-next-error ()
    "In the new style error messages, the regular expression
matches on the file name (which appears after `-->`), but the
start of the error appears a few lines earlier.  This hook runs
after `next-error' (\\[next-error]); it simply scrolls down a few lines in
the compilation window until the top of the error is visible."
    (save-selected-window
      (when (eq major-mode 'rust-mode)
        (select-window (get-buffer-window next-error-last-buffer 'visible))
        (when (save-excursion
                (beginning-of-line)
                (looking-at " *-->"))
          (let ((start-of-error
                 (save-excursion
                   (beginning-of-line)
                   (while (not (looking-at "^[a-z]+:\\|^[a-z]+\\[E[0-9]+\\]:"))
                     (forward-line -1))
                   (point))))
            (set-window-start (selected-window) start-of-error))))))
  
  (eval-after-load 'compile
    '(progn
       (add-to-list 'compilation-error-regexp-alist-alist
                    (cons 'rustc-refs rustc-refs-compilation-regexps))
       (add-to-list 'compilation-error-regexp-alist 'rustc-refs)
       (add-to-list 'compilation-error-regexp-alist-alist
                    (cons 'rustc rustc-compilation-regexps))
       (add-to-list 'compilation-error-regexp-alist 'rustc)
       (add-to-list 'compilation-error-regexp-alist-alist
                    (cons 'rustc-colon rustc-colon-compilation-regexps))
       (add-to-list 'compilation-error-regexp-alist 'rustc-colon)
       (add-to-list 'compilation-error-regexp-alist-alist
                    (cons 'cargo cargo-compilation-regexps))
       (add-to-list 'compilation-error-regexp-alist 'cargo)
       (add-hook 'next-error-hook #'rustc-scroll-down-after-next-error))))


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

