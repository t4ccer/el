;; -*- lexical-binding: t; -*-

(setq dired-dwim-target t)

(defun t4/dired-to-pdf (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (dolist (file files)
       (let ((command (concat "pandoc -t pdf -o \"" (file-name-base file) ".pdf\" \"" file "\"")))
         (message "Converting \"%s\" to pdf" file)
         (async-shell-command command)))
     (message "Converting done!")
     (revert-buffer))))

(defun t4/dired-normalize-file-name (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (dolist (file files)
       (rename-file
        file
        (replace-regexp-in-string
         "-+" "-"
         (replace-regexp-in-string "[[:space:]]+" "-" (downcase file)))
        nil))
     (revert-buffer))))

(defun t4/dired-to-logseq (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (let ((dired-dwim-target (lambda () '("/home/t4ccer/logseq-zettelkasten/assets/"))))
       (dired-do-rename))
     (revert-buffer)
     (kill-new "")
     (dolist (file files)
       (let ((include (concat "![" file "](../assets/" file ")")))
         (kill-append (concat include "\n")  nil)
         (message include))))))

(setq t4/logseq-base "/home/t4ccer/logseq-zettelkasten")

(defun t4/dired-to-logseq-book (&optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (let ((dired-dwim-target (lambda () (list (concat t4/logseq-base "/assets")))))
       (dired-do-rename))
     (revert-buffer)
     (dolist (file files)
       (let ((entry-file (concat t4/logseq-base "/pages/books___" (file-name-base file) ".md")))
         (write-region "type:: book\n" nil entry-file 'append)
         (write-region (concat "file:: ![" file "](../assets/" file ")\n") nil entry-file 'append)
         (write-region "author:: Author\n" nil entry-file 'append)
         (write-region "book-title:: Book Title\n" nil entry-file 'append)
         (write-region "topics:: topics\n\n" nil entry-file 'append))))))

(define-prefix-command 't4/dired-map)
;; FIXME: Add only in dired mode
(global-set-key (kbd "C-c d") 't4/dired-map)
(define-key t4/dired-map (kbd "p") `t4/dired-to-pdf)
(define-key t4/dired-map (kbd "n") `t4/dired-normalize-file-name)

(define-prefix-command 't4/dired-logseq-map)
(define-key t4/dired-map (kbd "l") `t4/dired-logseq-map)
(define-key t4/dired-logseq-map (kbd "r") `t4/dired-to-logseq)
(define-key t4/dired-logseq-map (kbd "b") `t4/dired-to-logseq-book)

