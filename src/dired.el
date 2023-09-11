;; -*- lexical-binding: t; -*-

(defun t4/dired-to-pdf (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (dolist (file files)
       (let ((command (concat "pandoc -t pdf -o \"" (file-name-base file) ".pdf\" \"" file "\"")))
         (message "Converting \"%s\" to pdf" file)
         (shell-command command)))
     (message "Converting done!")
     (revert-buffer))))

(defun t4/dired-normalize-file-name (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (dolist (file files)
       (rename-file
        file
        (replace-regexp-in-string "[[:space:]]+" "-" (downcase file))
        nil))
     (revert-buffer))))

(defun t4/dired-to-logseq (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (let ((dired-dwim-target (lambda () '("/home/t4ccer/logseq-zettelkasten/assets/"))))
       (dired-do-rename))
     (revert-buffer)
     (dolist (file files)
       (message (concat "![" file "](../assets/" file ")"))))))

(define-prefix-command 't4/dired-map)
;; FIXME: Add only in dired mode
(global-set-key (kbd "C-c d") 't4/dired-map)
(define-key t4/dired-map (kbd "p") `t4/dired-to-pdf)
(define-key t4/dired-map (kbd "n") `t4/dired-normalize-file-name)
(define-key t4/dired-map (kbd "l") `t4/dired-to-logseq)
