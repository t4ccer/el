;; -*- lexical-binding: t; -*-

(use-package unison-mode
  :mode "\\.u\\'"
  :ensure t)

(defun t4/start-ucm-terminal ()
  ""
  (interactive)
  (let ((buf-name "*ucm*")
	(dir default-directory))
    (get-buffer-create buf-name)
    (pop-to-buffer buf-name)
    (cd dir)
    (unison-ucm-mode)))


;; (global-set-key (kbd "C-c u u") 't4/start-ucm-terminal)

(define-derived-mode unison-ucm-mode comint-mode "unison-ucm"
  ""
  :interactive nil
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp
	      (rx
	       "."
	       (zero-or-more (not (any ">")))
	       "> "))
  ;; (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
  (unless (comint-check-proc (current-buffer))
    (let ((ucm-proc (start-process "ucm" (current-buffer) "ucm" "--codebase" ".")))
      (set-process-query-on-exit-flag ucm-proc t)
      (set-marker (process-mark ucm-proc) (point)))
    ))

