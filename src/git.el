;;; -*- lexical-binding: t -*-

(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk 'all))
;; FIXME: Diff color theme is not respected 
;; (use-package magit-delta
;;   :ensure t
;;   ;; :hook (magit-mode . magit-delta-mode)
;;   :custom
;;   (magit-delta-delta-args '("--true-color" "never" "--color-only" "--dark")))

;; FIXME
;; (use-package forge
;;   :ensure t
;;   :requires 'magit)

(defun t4/clone-repo (url)
  (interactive "sURL: ")
  (if (cl-reduce (lambda (acc rgx) (or acc (string-match rgx url)))
                 '("https?://\\(github\\).com/\\([^/]+\\)/\\([^/\n]+\\)"
                   "git@\\(github\\).com:\\([^/]+\\)/\\(.+\\).git"
                   "https?://\\(codeberg\\).org/\\([^/]+\\)/\\([^/\n]+\\)")
                 :initial-value nil)
      (let* ((forge (match-string 1 url))
             (user (match-string 2 url))
             (repo (match-string 3 url))
             (user-dir (concat "~/repos/" forge "/" user))
             (repo-dir (concat user-dir "/" repo)))
        (make-directory user-dir :parents)
        (magit-clone-regular url repo-dir (transient-args 'magit-clone))
        (projectile-discover-projects-in-search-path)
        (find-file repo-dir))
    (error "Could not parse URL")))

(define-prefix-command 't4/magit-map)
(global-set-key (kbd "C-c g") 't4/magit-map)
(define-key t4/magit-map (kbd "c") 't4/clone-repo)
(define-key t4/magit-map (kbd "b") 'magit-blame)
(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
