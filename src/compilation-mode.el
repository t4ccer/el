;;; -*- lexical-binding: t -*-

(require 'projectile)
(require 'projectile-variable)

(define-prefix-command 't4/compilation-global-map)
(global-set-key (kbd "C-c c") 't4/compilation-global-map)

;; to ignore warnings set 2
(setq compilation-skip-thresholdcompilation-skip-threshold 1)

(with-eval-after-load 'compile
  ;; set cursor to follow compilation output
  (setq compilation-scroll-output t))

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
  (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (defun t4/cd-and-compile ()
;;   (interactive)
;;   (let* ((comp-buf (generate-new-buffer "*compilation*")))
;;     (projectile-variable-put "t4/comp-buf" comp-buf)
;;     (let ((comp-dir (read-directory-name "Compilation directory: "
;; 					 default-directory default-directory
;; 					 t))
;; 	  (comp-cmd (read-string "Compile command: " nil '(compile-history . nil))))
;;       (switch-to-buffer comp-buf)
;;       (display-buffer comp-buf '(display-buffer-pop-up-window . nil))
;;       (cd comp-dir)
;;       (envrc-mode 1)
;;       (compilation-mode 1)
;;       (compilation-start comp-cmd))))

;; (defun t4/recompile ()
;;   (interactive)
;;   )

;; TODO: Write better projectile compilation function to support multiple projects
(defun t4/cd-and-compile ()
  (interactive)
  (call-interactively 'cd)
  (call-interactively 'compile))

(defun t4/compile-buffer (cmd)
  "Compile current buffer with given command"
  (compile (concat cmd " " (buffer-file-name))))

(defun t4/compile-org ()
  "Compile current buffer with org2pdf."
  (interactive)
  (t4/compile-buffer "nix run -L github:t4ccer/nix-org-export"))

(defun t4/compile-tex ()
  "Compile current buffer with pdflatex"
  (interactive)
  (t4/compile-buffer "pdflatex"))

;; Keys
(define-key t4/compilation-global-map (kbd "c") `t4/cd-and-compile)
(define-key t4/compilation-global-map (kbd "g") `recompile)
(define-key t4/compilation-global-map (kbd "o") `t4/compile-org)
(define-key t4/compilation-global-map (kbd "t") `t4/compile-tex)
