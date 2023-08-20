;;; -*- lexical-binding: t -*-

(use-package rustic
  :ensure t
  :straight t
  :defer t
  :config
                                        ; Envrc support sucks out of the box
  (setq rustic-lsp-server 'nil)
  (setq rustic-lsp-client 'nil)
  (require 'dap-cpptools)
  (add-hook 'rustic-mode-hook #'lsp-deferred)
  (add-hook 'rustic-mode-hook (lambda ()
                                (progn
				  (setq indent-tabs-mode nil)
				  (setq standard-indent 2))))

  ;; (add-hook 'before-save-hook
  ;;           (lambda ()
  ;;             (when (eq major-mode 'rustic-mode) (untabify (point-min) (point-max)))))

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

;; (define-hostmode poly-rust-hostmode
;;   :mode 'rustic-mode)

;; (define-innermode poly-rust-html!-innermode
;;   :mode 'html-mode
;;   :head-matcher "html! {\n"
;;   :tail-matcher "^[\t]*}\n"
;;   :head-mode 'host
;;   :tail-mode 'host)

;; (define-polymode poly-rust-mode
;;   :hostmode 'poly-rust-hostmode
;;   :innermodes '(poly-rust-html!-innermode))

;; (add-hook 'rustic-mode 'poly-rust-mode)

;; (add-to-list
;;  'compilation-error-regexp-alist
;;  '("^\\[\\(.*\\)+:\\([[:digit:]]+\\)].*$"
;;    1 2))
