;; -*- lexical-binding: t; -*-

(defvar disable-tramp-backups '(all))

(eval-after-load "tramp"
  '(progn
     ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
     (setq backup-enable-predicate
           (lambda (name)
             (and (normal-backup-enable-predicate name)
		  ;; Disable all tramp backups
		  (and disable-tramp-backups
                       (member 'all disable-tramp-backups)
                       (not (file-remote-p name 'method)))
		  (not ;; disable backup for tramp with the listed methods
		   (let ((method (file-remote-p name 'method)))
                     (when (stringp method)
                       (member method disable-tramp-backups)))))))

     (defun tramp-set-auto-save--check (original)
       (if (funcall backup-enable-predicate (buffer-file-name))
           (funcall original)
         (auto-save-mode -1)))

     (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)

     ;; Use my ~/.ssh/config control master settings according to https://puppet.com/blog/speed-up-ssh-by-reusing-connections
     (setq tramp-default-method "ssh")
     (setq tramp-ssh-controlmaster-options "")))
