;;; -*- lexical-binding: t -*-


;; to ignore warnings set 2
(setq compilation-skip-thresholdcompilation-skip-threshold 1)
(with-eval-after-load 'compile
;; set cursor to follow compilation output
(setq compilation-scroll-output t))

;; TODO: Write better projectile compilation function to support multiple projects
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

(defun t4/compile-buffer (cmd)
  "Compile current buffer with given command"
  (compile (concat cmd " " (buffer-file-name))))

(defun t4/compile-org ()
  "Compile current buffer with org2pdf."
  (interactive)
  (t4/compile-buffer "nix run -L github:t4ccer/nix-org-export"))

(global-set-key (kbd "C-c c o") `t4/compile-org)

(defun t4/compile-tex ()
  "Compile current buffer with pdflatex"
  (interactive)
  (t4/compile-buffer "pdflatex"))

(global-set-key (kbd "C-c c t") `t4/compile-tex)
