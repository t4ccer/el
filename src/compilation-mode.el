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

(defun t4/set-compilation-env ()
 (add-to-list 'compilation-environment "CARGO_TERM_COLOR=always"))

(add-hook 'compilation-mode-hook 't4/set-compilation-env)

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

(defun t4/compile-runhaskell ()
  "runhaskell"
  (interactive)
  (t4/compile-buffer "runhaskell -Wall -Wextra -fforce-recomp"))

(setq compilation-max-output-line-length 1000000000000)

;; Keys
(define-key t4/compilation-global-map (kbd "c") `t4/cd-and-compile)
(define-key t4/compilation-global-map (kbd "g") `recompile)
(define-key t4/compilation-global-map (kbd "o") `t4/compile-org)
(define-key t4/compilation-global-map (kbd "t") `t4/compile-tex)
(define-key t4/compilation-global-map (kbd "r") `t4/compile-runhaskell)


(setq compilation-error-regexp-alist nil)

;;; Nix errors
;; Handles: at /nix/store/hash-sourcefoo.nix:42:42:
;; Handles: whose name attribute is located at /nix/store/hash-sourcefoo.nix:42:42
;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("^\s*\\(whose name attribute is located\\)? at \\(?1:[^:]+\\):\\(?2:[[:digit:]]+\\):\\(?3:[[:digit:]]+\\):?$"
;;    1 2 3))

;;; Purescript errors
;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("^\\[[0-9]+/[0-9]+\s[[:alnum:]]+\\]\s+\\(.*?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$"
;;    1 2 3))

;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("at \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - \\([[:digit:]]+\\):\\([[:digit:]]+\\)"
;;    1 (2 . 4) (3 . 5)))

;; llvm llc
;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("^llc: error: llc: \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):"
;;    1 2 3))

;; ;; haskell's 'bound at' locations
;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("^\s*(bound at \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))"
;;    1 2 3 0))

;; ;; rust panic
(add-to-list
 'compilation-error-regexp-alist
 '("^thread '[^']*' panicked at \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):$"
   1 2 3)
 t)

;; GHC (old)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^\s\n:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error:$"
   1 2 3)
 t)

;; GHC (new)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^\s\n:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error: \\[[^\n]+\\]$"
   1 2 3)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^\s\n:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error:[^\n]*$"
   1 2 3)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^\s\n:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning: \\[[^\n]+\\]$"
   1 2 3 1)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error: .*$"
   1 2 3)
 t)

;; Rust
(add-to-list
 'compilation-error-regexp-alist
 '("^error[^\n]*\n\s*--> \\([^\s\n:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$"
   1 2 3)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^warning[^\n]*\n\s*--> \\([^\s\n:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$"
   1 2 3 1)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^\s\n:]+\\):\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\)-\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\): error:$"
   1 (2 . 4) (3 . 5))
 t)

;; miranda
(add-to-list
 'compilation-error-regexp-alist
 '("^error found near line \\([[:digit:]]+\\) of file \"\\([^\"]+\\)\"$"
   2 1)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^undeclared typename \"[^\"]+\" \(line\s*\\([[:digit:]]+\\) of \"\\([^\"]+\\)\"\)$"
   2 1)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("\(line\s*\\([[:digit:]]+\\) of \"\\([^\"]+\\)\"\)[^\n]*$"
   2 1)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^:]+\\):\\([^:]+\\): error:[^\n]*$"
   1 2)
 t)

(add-to-list
 'compilation-error-regexp-alist
 '("^  File \"\\([^\"]+\\)\", line \\([[:digit:]]+\\)$"
   1 2)
 t)


(add-to-list
 'compilation-error-regexp-alist
 '("^CMake Error at \\([^:]+\\):\\([[:digit:]]+\\)[^\n]*$"
   1 2)
 t)


;; ;; Grep match
;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("\\(.*\\):\\([[:digit:]]+\\):[^\n]*$"
;;    1 2 3)
;;  t)

;; ;; Grep context
;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("\\(.*\\)-\\([[:digit:]]+\\)-[^\n]*$"
;;    1 2 3 1)
;;  t)



;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("^\\([^:]+\\):\\([^:]+\\):\\([^:]+\\): error:"
;;    1 2 3)
;;  t)


(add-to-list 'auto-mode-alist '("\\.stderr\\'" . compilation-mode))
