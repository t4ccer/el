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
         (replace-regexp-in-string "_+" "-" (replace-regexp-in-string "[[:space:]]+" "-" (downcase file))))
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

(defmacro t4/dired-to-logseq-collection (name collection type props)
  `(defun ,name (&optional arg file-list)
     (interactive)
     (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
       (let ((dired-dwim-target (lambda () (list (concat t4/logseq-base "/assets")))))
         (dired-do-rename))
       (revert-buffer)
       (dolist (file files)
         (let ((entry-file (concat t4/logseq-base "/pages/" ,collection  "___" (file-name-base file) ".md")))
           (write-region (concat "type:: " ,type "\n") nil entry-file 'append)
           (write-region (concat "file:: ![" file "](../assets/" file ")\n") nil entry-file 'append)
           (dolist (prop ,props)
             (write-region (concat prop ":: " prop "\n") nil entry-file 'append)))
         (kill-new (concat ,collection "/" (file-name-base file)))))))

(t4/dired-to-logseq-collection t4/dired-to-logseq-book "books" "book" '("author" "book-title" "topics"))
(t4/dired-to-logseq-collection t4/dired-to-logseq-paper "papers" "paper" '("author" "paper-title" "year" "doi" "topics"))

(defun t4/is-video (fp) ""
       (apply-macro
        'or
        (mapcar (lambda (ext) (s-suffix? ext fp t))
                '("mp4" "m4v" "mkv" "avi" "mov"))))

(defun t4/dired-open () ""
    (interactive)
    (let ((fp (dired-get-file-for-visit)))
      (if (t4/is-video fp)
          (call-process-shell-command (concat "vlc \"" fp "\" &") nil 0)
        (dired--find-possibly-alternative-file fp))))

(define-key dired-mode-map (kbd "RET") 't4/dired-open)

(define-prefix-command 't4/dired-map)
;; FIXME: Add only in dired mode
(global-set-key (kbd "C-c d") 't4/dired-map)
(define-key t4/dired-map (kbd "p") `t4/dired-to-pdf)
(define-key t4/dired-map (kbd "n") `t4/dired-normalize-file-name)

(define-prefix-command 't4/dired-logseq-map)
(define-key t4/dired-map (kbd "l") `t4/dired-logseq-map)
(define-key t4/dired-logseq-map (kbd "r") `t4/dired-to-logseq)
(define-key t4/dired-logseq-map (kbd "b") `t4/dired-to-logseq-book)
(define-key t4/dired-logseq-map (kbd "p") `t4/dired-to-logseq-paper)

(defun t4/dired-compress-zip ()
  (let* ((fp (dired-get-filename))
         (out (concat (file-name-base fp) ".zip"))
         (res (dired-shell-command (format "zip -r %s %s" out fp))))
    (if (eql res 0)
        nil
      fp)))

(defun t4/dired-do-compress-zip (&optional arg)
  "Like `dired-do-compress' but create .zip archive rather than .tar.gz"
  (interactive "P")
  (dired-map-over-marks-check #'t4/dired-compress-zip arg 'compress-zip t)
  (revert-buffer))

(define-key dired-mode-map (kbd "z") #'t4/dired-do-compress-zip)
