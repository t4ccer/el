(require 'mu4e) ; From NixOS

(setq mu4e-change-filenames-when-moving t)
(setq mu4e-update-interval (* 5 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/email")
(setq mu4e-completing-read-function 'ivy-completing-read)
(setq mu4e-confirm-quit nil)
(setq mu4e-headers-date-format "%F")
(setq mu4e-headers-time-format "%H:%M:%S")
(setq mu4e-use-fancy-chars nil)
(setq mu4e-compose-format-flowed t)
(setq mu4e-headers-thread-last-child-prefix '("╰" . "┗ "))
(setq mu4e-bookmarks
      '((:name "Unread"
         :query "flag:unread AND NOT flag:trashed"
         :key ?u)
        (:name "Today"
         :query "date:today..now"
         :key ?t)
        (:name "This month"
         :query "date:30d..now"
         :key ?m)))

(setq message-send-mail-function 'smtpmail-send-it)

(setq mm-discouraged-alternatives
      '("text/html" "text/richtext" "multipart/related"))
(setq mml-secure-openpgp-signers '("6866981C49924D64D154E1AC19E5A2D8B1E43F19"))
;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

(defmacro t4/make-mu4e-gmail-context (ctx address name)
  (let
      ((drafts (concat "/" address "/[Gmail]/Drafts"))
       (sent (concat "/" address "/[Gmail]/Sent Mail"))
       (refile (concat "/" address "/[Gmail]/All Mail"))
       (trash (concat "/" address "/[Gmail]/Trash")))
    (make-mu4e-context
     :name ctx
     :match-func `(lambda (msg)
                    (when msg (string-match-p (concat "^/" ,address) (mu4e-message-field msg :maildir))))
     :vars `((user-mail-address . ,address)
             (user-full-name . ,name)
             (mu4e-drafts-folder . ,drafts)
             (mu4e-sent-folder . ,sent)
             (mu4e-refile-folder . ,refile)
             (mu4e-trash-folder . ,trash)
             (smtpmail-smtp-user . ,address)
             (smtpmail-smtp-server . "smtp.gmail.com")
             (smtpmail-smtp-service . 587)
             (smtpmail-smtp-type . 'ssl)))))

(setq mu4e-contexts
      (list
       (t4/make-mu4e-gmail-context "MUN" "tmaciosowski@mun.ca" "Tomasz Maciosowski")
       (t4/make-mu4e-gmail-context "t4ccer" "t4ccer@gmail.com" "t4ccer")))

(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
