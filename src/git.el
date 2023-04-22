;;; -*- lexical-binding: t -*-

(use-package magit
  :ensure t)

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
  (let ((github-http-rgx "https?://github.com/\\([^/]+\\)/\\([^/\n]+\\)")
        (github-ssh-rgx "git@github.com:\\([^/]+\\)/\\(.+\\).git"))
    (cond ((or (string-match github-http-rgx url) (string-match github-ssh-rgx url))
           (let* ((user (match-string 1 url))
                  (repo (match-string 2 url))
                  (clean-url (concat "git@github.com:" user "/" repo ".git"))
                  (user-dir (concat "~/repos/github/" user))
                  (repo-dir (concat user-dir "/" repo)))
             (make-directory user-dir :parents)
             (magit-clone-regular clean-url repo-dir (transient-args 'magit-clone))
             (projectile-discover-projects-in-search-path)
             (find-file repo-dir)))
          ((error "Could not parse URL")))))

(define-prefix-command 't4/magit-map)
(global-set-key (kbd "C-c g") 't4/magit-map)
(define-key t4/magit-map (kbd "c") 't4/clone-repo)
(define-key t4/magit-map (kbd "b") 'magit-blame)
