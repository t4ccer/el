;;; -*- lexical-binding: t -*-

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m b") `magit-blame))

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
(global-set-key (kbd "C-c g c") 't4/clone-repo)
