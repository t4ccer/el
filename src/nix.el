;;; -*- lexical-binding: t -*-

(define-prefix-command 't4/nix-global-map)
(define-prefix-command 't4/nix-templates-map)
(global-set-key (kbd "C-c n") 't4/nix-global-map)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :config
  (defun t4/get-nix-hash (cmd url)
    (with-temp-buffer
      (shell-command (concat cmd " " url) (current-buffer))
      (goto-char (point-min))
      (next-line)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

  (defun t4/nix-fetchurl ()
    (interactive)
    (let* ((start-position (point))
           (url (read-string "URL: "))
           (hash (t4/get-nix-hash "nix-prefetch-url" url)))
      (insert (concat
               "fetchurl {\n"
               "  url = \"" url "\";\n"
               "  sha256 = \"" hash "\";\n"
               "};"
               ))
      (indent-region start-position (line-end-position))
      (goto-char start-position)))
  (define-key nix-mode-map (kbd "C-c n f u") 't4/nix-fetchurl)

  (defun t4/nix-fetchFromGitHub ()
    (interactive)
    (let* ((start-position (point))
           (owner (read-string "Owner: "))
           (repo (read-string "Repo: "))
           (rev (read-string "Rev: "))
           (hash (t4/get-nix-hash "nix-prefetch-url --unpack"
                                  (concat "https://github.com/" owner "/" repo "/archive/" rev ".tar.gz"))))
      (insert (concat
               "fetchFromGitHub {\n"
               "  owner = \"" owner "\";\n"
               "  repo = \"" repo "\";\n"
               "  rev = \"" rev "\";\n"
               "  sha256 = \"" hash "\";\n"
               "};"
               ))
      (indent-region start-position (line-end-position))
      (goto-char start-position)))

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection "nixd")
  ;;   :major-modes '(nix-mode)
  ;;   :server-id 'nixd))

  (define-key nix-mode-map (kbd "C-c n f h") 't4/nix-fetchFromGitHub))

;; (add-hook 'nix-mode-hook (lambda ()
;;   (when
;;       (> 1000 (count-lines (point-min) (point-max)))
;;     (lsp-mode 1))))

;; M-x nix-flake is really slow, evaluates part of flake which is not needed
;; and fails if flake uses IFD
(defun t4/nix-bump-flake-input ()
  (interactive)
  (let* ((root-path (projectile-project-root))
         (flake-file (concat root-path "flake.nix"))
         (inputs-string (shell-command-to-string
                         (concat
                          "nix-instantiate --eval --json -E '"
                          "builtins.concatStringsSep \" \" (builtins.attrNames (import "
                          flake-file
                          ").inputs)'")))
         (inputs-list (split-string (nth 1 (split-string inputs-string "\"")) " "))
         (input-to-bump (completing-read "Input to bump: " inputs-list)))
    (shell-command (concat "nix flake update " input-to-bump))))


(defun t4/nix-template-init (url)
  "run 'nix flake init --refresh -t URL'"
  (shell-command-to-string (concat "nix flake init --refresh -t " url))
  (revert-buffer))

(defun t4/nix-template-init-general ()
  "Create a general template in current directory"
  (interactive)
  (t4/nix-template-init "github:t4ccer/t4.nix#general"))

(defun t4/nix-template-init-rust-yew ()
  "Create a rust + yew template in current directory"
  (interactive)
  (t4/nix-template-init "github:t4ccer/t4.nix#rust-yew")
  (envrc-allow)
  (magit-init default-directory))

(defun t4/nix-template-init-rust ()
  "Create a rust template in current directory"
  (interactive)
  (t4/nix-template-init "github:t4ccer/t4.nix#rust")
  (envrc-allow)
  (magit-init default-directory))

;; general
(define-key t4/nix-global-map (kbd "b") 't4/nix-bump-flake-input)
(define-key t4/nix-global-map (kbd "t") 't4/nix-templates-map)

;; templates
(define-key t4/nix-templates-map (kbd "y") 't4/nix-template-init-rust-yew)
(define-key t4/nix-templates-map (kbd "g") 't4/nix-template-init-general)
(define-key t4/nix-templates-map (kbd "r") 't4/nix-template-init-rust)

;; (defun t4/store-path (dir)
;;   "Get current nix-store path, handles only git repos and no VC cases."
;;   (with-existing-directory
;;     dir
;;     (with-temp-buffer
;;       (let* ((err-buf (generate-new-buffer "*nix-stderr*"))
;;              (cmd (if (file-exists-p (concat dir ".git"))
;;                       (concat
;;                        "nix-instantiate --eval -E '(builtins.fetchTree {type = \"git\"; url = "
;;                        (substring dir 0 -1)
;;                        ";}).outPath' --json")
;;                     (concat
;;                      "nix eval --expr '"
;;                      (substring dir 0 -1)
;;                      "' --raw")))
;;              (json-path (shell-command cmd (current-buffer) err-buf)))
;;         (kill-buffer err-buf)
;;         (json-parse-string (buffer-string))))))

;; (defun t4/parent-directory (dir)
;;   "Get partent directory, return `nil' if reached root"
;;   (unless (equal "/" dir)
;;     (file-name-directory (directory-file-name dir))))

;; (defun t4/get-flake-dir (dir)
;;   "Emulate nix searching up for `flake.nix' file if it doesn't exist.
;; Returns directory with flake if found, `nil' otherwise.
;; `dir' must have trailing slash"
;;   (when dir
;;     (if (file-exists-p (concat dir "flake.nix")) dir
;;       (t4/get-flake-dir (t4/parent-directory dir)))))

;; (defun t4/add-trailing-slash (path)
;;   "Append slash to path if it is missing"
;;   (concat path (if (equal "/" (substring path -1 nil)) "" "/")))

;; (defun t4/replace-curr-store-path ()
;;   "Replace nix store path with local file path"
;;   (when (and compilation-filter-start t4/flake-dir)
;;     (let* ((store-path (t4/store-path t4/flake-dir))
;;            (inhibit-read-only t))
;;       (replace-string-in-region
;;        (t4/add-trailing-slash store-path)
;;        (t4/add-trailing-slash default-directory)
;;        compilation-filter-start
;;        (point-max)))))

;; (defun t4/set-flake-path ()
;;   (setq t4/flake-dir (t4/get-flake-dir (t4/add-trailing-slash default-directory))))

;; (add-hook 'compilation-mode-hook 't4/set-flake-path)
;; (add-hook 'compilation-filter-hook 't4/replace-curr-store-path)

;; (setq compilation-mode-hook nil)
;; (setq compilation-filter-hook nil)
