;;; -*- lexical-binding: t -*-

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
  (global-set-key (kbd "C-c n f u") 't4/nix-fetchurl)

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
  ;; (add-hook 'nix-mode-hook 'lsp-mode)
  (global-set-key (kbd "C-c n f h") 't4/nix-fetchFromGitHub))


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
    (shell-command (concat "nix flake lock --update-input " input-to-bump))))
(global-set-key (kbd "C-c n b") 't4/nix-bump-flake-input)

(defun t4/nix-template-init (url)
  "run 'nix flake init --refresh -t URL'"
  (shell-command-to-string (concat "nix flake init --refresh -t " url))
  (revert-buffer))

(defun t4/nix-template-init-general ()
  "Create a general template in current directory"
  (interactive)
  (t4/nix-template-init "github:t4ccer/t4.nix#general"))
(global-set-key (kbd "C-c n t g") 't4/nix-template-init-general)


(defun t4/nix-template-init-rust-yew ()
  "Create a general template in current directory"
  (interactive)
  (t4/nix-template-init "github:t4ccer/t4.nix#rust-yew")
  (envrc-allow)
  (magit-init default-directory))
(global-set-key (kbd "C-c n t y") 't4/nix-template-init-rust-yew)
