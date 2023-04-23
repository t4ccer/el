;; -*- lexical-binding: t; -*-

(define-prefix-command 't4/cursors-map)
(global-set-key (kbd "C-c m") 't4/cursors-map)

(use-package multiple-cursors
  :ensure t
  :config
  (define-key t4/cursors-map (kbd "m") `mc/edit-lines)
  (define-key t4/cursors-map (kbd "n") `mc/mark-next-like-this)
  (define-key t4/cursors-map (kbd "p") `mc/mark-previous-like-this))
