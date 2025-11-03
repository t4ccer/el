;;; -*- lexical-binding: t -*-

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
  :ensure t
  :straight
  ( :host github
    :repo "t4ccer/monokaish.el"
    :branch "main"
    :files ("*.el")))

(load-theme 'monokaish t)

;;; Font
(setq t4/font "FiraCode Nerd Font 10")
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font 10"))
(set-frame-font t4/font nil t)
(set-face-attribute 'default t :font t4/font)


;; Get face under the cursor
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
	          (get-char-property (point) 'face))))
    (if face (message "(%s (:foreground monokaish-))" face) (message "No face at %d" pos))))

(setq display-line-numbers-type 'relative)

(defun t4/display-line-numbers--turn-on ()
  "Turn on `display-line-numbers-mode'."
  (unless (or (minibufferp) (derived-mode-p 'pdf-view-mode))
    (display-line-numbers-mode)))

(define-globalized-minor-mode t4/global-display-line-numbers-mode
  display-line-numbers-mode t4/display-line-numbers--turn-on)

(t4/global-display-line-numbers-mode)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq-default fill-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'haskell-mode-hook #'display-fill-column-indicator-mode)

(column-number-mode)

(use-package rainbow-mode
  :ensure t)
