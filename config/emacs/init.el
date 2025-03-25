(setq vc-follow-symlinks t)
(setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))
(setq exec-path (split-string (getenv "PATH") path-separator))
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))
