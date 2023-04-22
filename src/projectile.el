;; -*- lexical-binding: t; -*-

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
