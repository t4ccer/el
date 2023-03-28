;; -*- lexical-binding: t; -*-

(defun t4/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.05))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.05))))
  '(org-level-6 ((t (:inherit outline-6 :height 1.05))))
  '(org-level-7 ((t (:inherit outline-7 :height 1.05))))
  '(org-level-8 ((t (:inherit outline-8 :height 1.05))))))

(defvar my-org-hidden-keywords
  '(title))

(defun t4/org-hide-keywords ()
  (save-excursion
    (let (beg end ov)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\(^[ \t]*#\\+\\)\\("
                      (mapconcat (lambda (kw)
                                   (format "%s:\s"(symbol-name kw)))
                                 my-org-hidden-keywords "\\|")
                      "\\)")
              nil t)
        (setq beg (match-beginning 1)
              end (match-end 2)
              ov  (make-overlay beg end))
    (overlay-put ov 'invisible t)))))

(defun t4/org-mode-setup ()
  (t4/org-hide-keywords)
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :ensure t
  :hook (org-mode . t4/org-mode-setup)
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-ellipsis " ▾")
  (setq org-agneda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq calendar-week-start-day 1) ;; Monday as first day of the week
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d)")
          (sequence "PLANNED(u)" "|" "DONE(d)")))
  (setq org-agenda-files '("~/org/tasks.org"))
  (plist-put org-format-latex-options :scale 1.25)
  (setq org-hide-emphasis-markers t)
  (defun t4/org-insert-image () (interactive)
         (org-insert-link)
         (org-display-inline-images))
  (t4/org-font-setup))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●")))

;; Render latex in org-mode
;; (use-package org-fragtog
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'org-fragtog-mode))

(defun t4/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate nil)
(setq org-startup-with-inline-images t)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; NOTE: Should this be in sage?
(use-package ob-sagemath
  :ensure t
  :after org)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (shell      . t)
                               (gnuplot    . t)
                               (python     . t)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle)))
