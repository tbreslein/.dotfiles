;;; config -- Summary
;;; Commentary:
;; TODO:
;; - ido (/ orderless?)
;; - flymake
;; - lspce
;; - corfu? with corfu/terminal?

;;; Code:
;; PACKAGE MANAGEMENT
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;(defvar elpaca-installer-version 0.7)
;(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
;(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
;                              :ref nil :depth 1
;                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
;                              :build (:not elpaca--activate-package)))
;(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
;       (build (expand-file-name "elpaca/" elpaca-builds-directory))
;       (order (cdr elpaca-order))
;       (default-directory repo))
;  (add-to-list 'load-path (if (file-exists-p build) build repo))
;  (unless (file-exists-p repo)
;    (make-directory repo t)
;    (when (< emacs-major-version 28) (require 'subr-x))
;    (condition-case-unless-debug err
;        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
;                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
;                                                 ,@(when-let ((depth (plist-get order :depth)))
;                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
;                                                 ,(plist-get order :repo) ,repo))))
;                 ((zerop (call-process "git" nil buffer t "checkout"
;                                       (or (plist-get order :ref) "--"))))
;                 (emacs (concat invocation-directory invocation-name))
;                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
;                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
;                 ((require 'elpaca))
;                 ((elpaca-generate-autoloads "elpaca" repo)))
;            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
;          (error "%s" (with-current-buffer buffer (buffer-string))))
;      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
;  (unless (require 'elpaca-autoloads nil t)
;    (require 'elpaca)
;    (elpaca-generate-autoloads "elpaca" repo)
;    (load "./elpaca-autoloads")))
;(add-hook 'after-init-hook #'elpaca-process-queues)
;(elpaca `(,@elpaca-order))
;(elpaca elpaca-use-package (elpaca-use-package-mode))
;(setq use-package-always-ensure t)

;; EMACS SETTINGS
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.
(use-package emacs
  :ensure nil

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (electric-indent-mode -1)
  (save-place-mode 1)
  (when scroll-bar-mode (scroll-bar-mode -1))
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  :config
  ;; Performance Hacks
  ;; Emacs is an Elisp interpreter, and when running programs or packages,
  ;; it can occasionally experience pauses due to garbage collection.
  ;; By increasing the garbage collection threshold, we reduce these pauses
  ;; during heavy operations, leading to smoother performance.
  (setq gc-cons-threshold #x40000000)

  ;; Set the maximum output size for reading process output, allowing for larger data transfers.
  (setq read-process-output-max (* 1024 1024 4))
  (setq ring-bell-function #'ignore)
  (setq inhibit-startup-screen t)
  (setq display-line-numbers-type 'visual)
  (setq make-backup-files nil)
  ;;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq use-dialog-box nil)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq text-mode-ispell-word-completion nil) ;; use cape-dict instead
  (setq frame-title-format nil)
  (setq delete-old-versions t)
  (setq kept-old-versions 1000)
  (setq vc-make-backup-files t)
  (setq version-control t)
  (setq line-height (if (eq system-type 'darwin) 180 110))
  (set-face-attribute 'default nil :family "Iosevka Nerd Font" :height line-height)
  (set-frame-parameter nil 'alpha 96)
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers
        ring-bell-function #'ignore))



;; EXEC-PATH / ENVRC / NIX
(use-package exec-path-from-shell
  :straight t
  :demand t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package envrc
  :straight t
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :hook (after-init . envrc-global-mode))

;; EVIL MODE
(use-package evil
  :straight t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-insert-state-cursor 'box)
  (evil-set-leader nil (kbd "SPC"))
  (evil-global-set-key 'normal (kbd "C-d") (lambda () (interactive) (evil-scroll-down 0) (recenter)))
  (evil-global-set-key 'normal (kbd "C-u") (lambda () (interactive) (evil-scroll-up 0) (recenter)))
  (evil-global-set-key 'visual (kbd "C-d") (lambda () (interactive) (evil-scroll-down 0) (recenter)))
  (evil-global-set-key 'visual (kbd "C-u") (lambda () (interactive) (evil-scroll-up 0) (recenter)))
  (evil-global-set-key 'normal (kbd "n") (lambda () (interactive) (evil-search-next) (recenter)))
  (evil-global-set-key 'normal (kbd "N") (lambda () (interactive) (evil-search-previous) (recenter)))
  ;; (evil-global-set-key 'visual (kbd "J") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  ;; (evil-global-set-key 'visual (kbd "K") (concat ":m '<-2" (kbd "RET") "gv=gv"))
  ;; (evil-global-set-key 'normal (kbd "J") (concat ":m +1" (kbd "RET") "=="))
  ;; (evil-global-set-key 'normal (kbd "K") (concat ":m -2" (kbd "RET") "=="))
  (evil-global-set-key 'motion (kbd "j") 'evil-next-visual-line)
  (evil-global-set-key 'motion (kbd "k") 'evil-previous-visual-line)
  (evil-global-set-key 'normal (kbd "C-m") 'compile)
  (evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
  (evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)
  (evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)
  (evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
  (evil-global-set-key 'normal (kbd "<leader>sj") 'evil-window-new)
  (evil-global-set-key 'normal (kbd "<leader>sl") 'evil-window-vnew)
  (evil-global-set-key 'normal (kbd "<leader>st") (lambda () (interactive) (evil-window-new 20 "") (vterm)))
  (evil-mode))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

;; NAVIGATION
(use-package projectile
  :straight t
  :custom
  (projectile-project-search-path '(("~/code" . 1) ("~/.dotfiles" . 0) ("~/notes" . 0) ("~/work" . 1) ("~/work/repos" . 1)))
  (projectile-require-project-root nil)
  (projectile-sort-order 'recentf)
  :config
  (defcustom projectile-project-root-functions
    '(projectile-root-local
      projectile-root-marked
      projectile-root-top-down
      projectile-root-top-down-recurring
      projectile-root-bottom-up)
    "A list of functions for finding project roots."
    :group 'projectile
    :type '(repeat function))
  (evil-global-set-key 'normal (kbd "<leader>f") 'projectile-command-map)
  (projectile-mode +1))

;; NOTE:
;; keybinds:
;;  m - mark file
;;  u - unmark
;;  U - remove all marks
;;  t - invert all marks
;;  %m - enter regex mode for marking files
;;  C - copy marked/hovered file(s)
;;  R - rename(/move) marked/hovered file(s)
;;  D - delete marked/hovered file(s)
;;  d - mark file for deletiong
;;  x - execute all buffered deletions
;;  z - compress file or folder to (.tar.gz)
;;  c - compress selection to a specific file
;;  gr - refresh buffer
;;  T - touch
;;  M - change file mode
;;  O - change file owner
;;  G - change file group
;;  S - create symlink to hovered file
;;  ! - run command on file (sync)
;;  & - run command on file (async)
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  (evil-global-set-key 'normal (kbd "-") 'dired-jump))


;; EGLOT SOMEHOW NEEDS THIS TO CORRECTLY DETERMINE THE PROJECT ROOT
;; ;; This SHOULD take care of the problem that project-root-override tries to solve,
;; ;; but for some reason it does not work. I have no idea why, but I don't seem to
;; ;; be the only one.
;; (setq project-vc-extra-root-markers
;;       '("Cargo.toml" "pyproject.toml"))

;; (defun project-root-override (dir)
;;   "Find DIR's project root by searching for a '.project.el' file.
;;
;; If this file exists, it marks the project root. For convenient compatibility
;; with Projectile, '.projectile' is also considered a project root marker.
;;
;; https://blog.jmthornton.net/p/emacs-project-override"
;;   (let ((root (or (locate-dominating-file dir ".project.el")
;;                   (locate-dominating-file dir ".projectile")
;;                   (locate-dominating-file dir "Cargo.toml")
;;                   (locate-dominating-file dir "setup.py")
;;                   (locate-dominating-file dir "requirements.txt")
;;                   (locate-dominating-file dir "pyproject.toml")))
;;         (backend (ignore-errors (vc-responsible-backend dir))))
;;     (when root (if (version<= emacs-version "28")
;;                    (cons 'vc root)
;;                  (list 'vc backend root)))))
;;
;; ;; Note that we cannot use :hook here because `project-find-functions' doesn't
;; ;; end in "-hook", and we can't use this in :init because it won't be defined
;; ;; yet.
;; (use-package project
;;   :config
;;   (add-hook 'project-find-functions #'project-root-override))


;; LANGUAGE MAJOR MODES
(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")
(use-package go-mode
  :straight t
  :mode "\\.go\\'")
(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'")
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :custom
  (rust-mode-treesitter-derive t))
(use-package cargo
  :straight t
  :hook (rust-ts-mode . cargo-minor-mode)
  :config (evil-define-key 'normal 'cargo-mode-map (kbd "C-c") 'cargo-minor-mode-command-map))
(use-package zig-mode
  :straight t
  :mode "\\.zig\\'")


;; UI
(use-package hl-todo
  :straight t
  :hook
  (after-init . hl-todo-mode))

;(elpaca (magit :branch "main" :pre-build ("make" "info")))
;(elpaca (forge :branch "main"))
;(elpaca (ghub :branch "main"))
;(elpaca (transient :branch "main"))
;(elpaca (with-editor :branch "main"))
(use-package magit
  :straight t
  ;:ensure nil
  :init
  (when (eq system-type 'darwin)
    (setq with-editor-emacsclient-executable "/run/current-system/sw/bin/emacsclient"))
  :config
  (evil-global-set-key 'normal (kbd "<leader>gg") 'magit))

(use-package vterm
  :straight t
  :custom
  (vterm-max-scrollback 20000)
  :config
  (evil-global-set-key 'normal (kbd "<leader>tt") 'vterm))

(use-package writeroom-mode :straight t)
;(use-package gruber-darker-theme
;  :config
;  (load-theme 'gruber-darker nil))
(use-package kanagawa-themes
  :straight t
  :config
  (load-theme 'kanagawa-dragon t))

(use-package treesit
  :ensure nil
  :defer t
  ;; :straight (:type built-in)
  ;; :hook ((bash-ts-mode c-ts-mode c++-ts-mode
  ;;                      html-ts-mode js-ts-mode typescript-ts-mode
  ;;                      json-ts-mode rust-ts-mode tsx-ts-mode python-ts-mode
  ;;                      css-ts-mode yaml-ts-mode) . lsp-deferred)
  :init
  (setq treesit-font-lock-level 4
        treesit-language-source-alist
        '(;; shell / config langs / text
          (awk "https://github.com/Beaglefoot/tree-sitter-awk")
          (csv "https://github.com/tree-sitter-grammars/tree-sitter-csv")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (editorconfig "https://github.com/ValdezFOmar/tree-sitter-editorconfig")
          ;; (git-config "https://github.com/the-mikedavis/tree-sitter-git-config")
          (git-rebase "https://github.com/the-mikedavis/tree-sitter-git-rebase")
          (gitattributes "https://github.com/tree-sitter-grammars/tree-sitter-gitattributes")
          ;; (gitcommit "https://github.com/the-mikedavis/tree-sitter-git-commit")
          (hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (jq "https://github.com/flurie/tree-sitter-jq")
          (json5 "https://github.com/Joakker/tree-sitter-json5")

          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (readline "https://github.com/tree-sitter-grammars/tree-sitter-readline")
          (requirements "https://github.com/tree-sitter-grammars/tree-sitter-requirements")
          (sql "https://github.com/DerekStride/tree-sitter-sql")
          (ssh-config "https://github.com/tree-sitter-grammars/tree-sitter-ssh-config")
          (tmux "https://github.com/Freed-Wu/tree-sitter-tmux")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (vim "https://github.com/tree-sitter-grammars/tree-sitter-vim")
          (vimdoc "https://github.com/neovim/tree-sitter-vimdoc")
          (xml "https://github.com/tree-sitter-grammars/tree-sitter-xml")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
          (zsh "https://github.com/tree-sitter-grammars/tree-sitter-zsh")

          ;; prog langs
          (asm "https://github.com/RubixDev/tree-sitter-asm")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cuda "https://github.com/tree-sitter-grammars/tree-sitter-cuda")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (meson "https://github.com/tree-sitter-grammars/tree-sitter-meson")
          (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")
          ;; (common-lisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (fortran "https://github.com/stadelmanma/tree-sitter-fortran")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (go-sum "https://github.com/tree-sitter-grammars/tree-sitter-go-sum")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (julia "https://github.com/tree-sitter/tree-sitter-julia")
          (just "https://github.com/IndianBoy42/tree-sitter-just")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (luadoc "https://github.com/tree-sitter-grammars/tree-sitter-luadoc")
          (llvm "https://github.com/benwilliamgraham/tree-sitter-llvm")
          (llvm-mir "https://github.com/Flakebi/tree-sitter-llvm-mir")
          ;; might not need this one as it's covered by asm
          ;; (x86-asm "https://github.com/bearcove/tree-sitter-x86asm")
          (nasm "https://github.com/naclsn/tree-sitter-nasm")
          (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
          (ninja "https://github.com/alemuller/tree-sitter-ninja")
          (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src")
          (ocaml-interace "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/interace/src")
          (ocaml-type "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/type/src")
          (odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (scheme "https://github.com/6cdh/tree-sitter-scheme")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")

          ;; web
          (astro "https://github.com/virchau13/tree-sitter-astro")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (scss "https://github.com/tree-sitter-grammars/tree-sitter-scss")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (svelte "https://github.com/tree-sitter-grammars/tree-sitter-svelte")
          ))
  ;; :config
  ;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  ;; (global-tree-sitter-mode)
  )

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;;; init.el ends here
