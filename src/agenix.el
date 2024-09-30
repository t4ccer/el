;; -*- lexical-binding: t; -*-

(require 'elrage)

(use-package agenix
  :ensure t
  :straight
  ( :host github
    :repo "t4ccer/agenix.el"
    :branch "main"
    :files ("*.el"))
  :config
  (setq agenix-age-decrypt-function 'elrage-decrypt-file-interactive)
  (setq agenix-age-encrypt-function 'elrage-encrypt-file))
