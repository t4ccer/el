;;; -*- lexical-binding: t -*-

(use-package lambda-buffers-mode
  :straight
  ( :host github
    :repo "mlabs-haskell/lambda-buffers"
    :branch "t4/add-simple-emacs" ;; remove after merge
    :files ("extras/editor/emacs/lambda-buffers-mode.el")))
