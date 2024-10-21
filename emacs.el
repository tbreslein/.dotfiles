;; TODO:
;; - try out ido
;; - stop using treesit-auto
;; - is there something like harpoon?
;;   - apparently bookmarks already solve this?
;; - modes for these languages:
;;   - go
;;   - haskell
;;   - zig
;;   - html
;;   - markdown
;;   - css/scss
;;   - toml
;;   - json
;;   - docker
;; - configure org
;; - use org-babel
;; - fix redo (maybe get an undo plugin?)
;; - try to fix visual line move keybinds
;; - dap stuff
;; - configure mode-line
;; - eww
;; - email
;; - dashboard? probably not though
;; - try out eshell
;; - try out dirvish (with nerd-dirvish)

(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.

;; Performance Hacks
;; Emacs is an Elisp interpreter, and when running programs or packages,
;; it can occasionally experience pauses due to garbage collection.
;; By increasing the garbage collection threshold, we reduce these pauses
;; during heavy operations, leading to smoother performance.
(setq gc-cons-threshold #x40000000)

;; Set the maximum output size for reading process output, allowing for larger data transfers.
(setq read-process-output-max (* 1024 1024 4))

(require 'package)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; Actually get “package” to work.
(package-initialize)
;; (package-refresh-contents)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell :demand t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package envrc
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :hook (after-init . envrc-global-mode))

(use-package evil
  :demand t
  :custom
  (evil-want-C-d-scroll t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  :config
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
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package emacs
  :ensure nil

  :custom
  (backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (inhibit-startup-screen t)
  (display-line-numbers-type 'visual)
  (make-backup-files nil)
  (tab-width 4)
  (indent-tabs-mode nil)
  (use-dialog-box nil)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (text-mode-ispell-word-completion nil) ;; use cape-dict instead

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
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq line-height (if (eq system-type 'darwin) 150 110))
  (set-face-attribute 'default nil :family "Hack Nerd Font" :height line-height)
  (setq alpha-val (if (eq system-type 'darwin) 100 92))
  (set-frame-parameter nil 'alpha alpha-val)
  (setq switch-to-prev-buffer-skip 'skip-these-buffers
        ring-bell-function #'ignore))

(use-package flymake
  :ensure nil
  :config
  (add-hook 'emacs-lisp-mode-hook 'flymake-mode)
  (evil-define-key 'normal 'flymake-mode-map (kbd "]d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'flymake-mode-map (kbd "[d") 'flymake-goto-prev-error)
  (evil-define-key 'normal 'flymake-mode-map (kbd "gd") 'flymake-show-project-diagnostics)
  (flymake-mode 1))

(defun add-node-modules ()
  "Foo."
  (add-to-list 'exec-path (expand-file-name "node_modules/.bin/" (locate-dominating-file (buffer-file-name) "node_modules"))))
(defun add-python-venv ()
  "Foo."
  (add-to-list 'exec-path (expand-file-name ".venv/bin/" (locate-dominating-file (buffer-file-name) ".venv"))))

(use-package apheleia
  :config
  (setf (alist-get 'black apheleia-formatters)
        '("poetry" "run" "black" "-"))
  (setf (alist-get 'alejandra apheleia-formatters)
        '("alejandra"))
  (setf (alist-get 'nix-mode apheleia-mode-alist)
        '(alejandra))
  (apheleia-global-mode +1))

;; ;; (cl-flet ((add-node-modules (lambda () (interactive) (add-to-list 'exec-path (expand-file-name "node_modules/.bin" (locate-dominating-file (buffer-file-name) "node_modules")))))
;; ;;           (add-python-venv (lambda () (interactive) (message "foo") (add-to-list 'exec-path (expand-file-name ".venv/bin" (locate-dominating-file (buffer-file-name) ".venv"))))))
;; (use-package format-all
;;   :hook (prog-mode-hook . format-all-mode)
;;   :config
;;   (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
;;   (add-hook 'typescript-ts-mode 'add-node-modules)
;;   (add-hook 'javascript-mode 'add-node-modules)
;;   ;; (add-hook 'python-ts-mode 'add-python-venv)
;;   (add-hook
;;    'python-ts-mode-hook
;;    'add-python-venv
;;    ;; (lambda ()
;;    ;;   (message "foo")
;;    ;;   (add-to-list 'exec-path (expand-file-name ".venv/bin" (locate-dominating-file (buffer-file-name) ".venv"))))
;;    )
;;   (setq-default format-all-formatters
;;                 '(("Haskell" (stylish-haskell))
;;                   ("Lua" (stylua))
;;                   ("Nix" (alejandra))
;;                   ;; ("Python" (ruff))
;;                   ("Shell" (shfmt "-i" "4" "-ci")))))

(use-package vertico
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle nil))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(use-package orderless
  :after vertico
  :custom (completion-styles '(flex basic)))
;; ^ this defines which completion styles to use.
;; flex is fuzzy search, and basic is the regular built-in and it's used as a fallback

(use-package project
  :ensure nil
  :config
  (evil-global-set-key 'normal (kbd "<leader>ff") 'project-find-file)
  (evil-global-set-key 'normal (kbd "<leader>fs") 'project-find-regexp)
  (evil-global-set-key 'normal (kbd "<leader>fp") 'project-switch-project))

;; (use-package projectile
;;   :custom
;;   (projectile-project-search-path '(("~/code" . 1) ("~/.dotfiles" . 0) ("~/notes" . 0) ("~/work" . 1) ("~/work/repos" . 1)))
;;   (projectile-require-project-root nil)
;;   (projectile-sort-order 'recentf)
;;   :config
;;   (defcustom projectile-project-root-functions
;;     '(projectile-root-local
;;       projectile-root-marked
;;       projectile-root-top-down
;;       projectile-root-top-down-recurring
;;       projectile-root-bottom-up)
;;     "A list of functions for finding project roots."
;;     :group 'projectile
;;     :type '(repeat function))
;;   (evil-global-set-key 'normal (kbd "<leader>f") 'projectile-command-map)
;;   (projectile-mode +1))

;; gonna try just using projectile with fd and ripgrep
;; (use-package consult
;;   :config
;;   (evil-global-set-key 'normal (kbd "<leader>psr") 'consult-ripgrep))

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

(use-package hl-todo
  :hook
  (after-init . hl-todo-mode))

(use-package magit
  :init
  (when (eq system-type 'darwin)
    (setq with-editor-emacsclient-executable "/run/current-system/sw/bin/emacsclient"))
  :config
  (evil-global-set-key 'normal (kbd "<leader>gg") 'magit))

(use-package vterm
  :custom
  (vterm-max-scrollback 20000)
  :config
  (evil-global-set-key 'normal (kbd "<leader>tt") 'vterm))

(use-package corfu
  :after vertico
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-echo-delay 0.2)
  (corfu-popupinfo-delay 0.2)
  (corfu-preview-current nil)
  :bind (:map corfu-map ("RET" . nil))
  :config
  (evil-define-key 'insert 'corfu-map (kbd "C-j") 'corfu-next)
  (evil-define-key 'insert 'corfu-map (kbd "C-k") 'corfu-previous)
  (evil-define-key 'insert 'corfu-map (kbd "C-l") 'corfu-insert)
  (evil-define-key 'insert 'corfu-map (kbd "C-h") 'corfu-insert-separator)
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; (use-package

(use-package treesit-auto
  :hook
  (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

;; ;; This SHOULD take care of the problem that project-root-override tries to solve,
;; ;; but for some reason it does not work. I have no idea why, but I don't seem to
;; ;; be the only one.
;; (setq project-vc-extra-root-markers
;;       '("Cargo.toml" "pyproject.toml"))

(defun project-root-override (dir)
  "Find DIR's project root by searching for a '.project.el' file.

If this file exists, it marks the project root. For convenient compatibility
with Projectile, '.projectile' is also considered a project root marker.

https://blog.jmthornton.net/p/emacs-project-override"
  (let ((root (or (locate-dominating-file dir ".project.el")
                  (locate-dominating-file dir ".projectile")
                  (locate-dominating-file dir "Cargo.toml")
                  (locate-dominating-file dir "setup.py")
                  (locate-dominating-file dir "requirements.txt")
                  (locate-dominating-file dir "pyproject.toml")))
        (backend (ignore-errors (vc-responsible-backend dir))))
    (when root (if (version<= emacs-version "28")
                   (cons 'vc root)
                 (list 'vc backend root)))))

;; Note that we cannot use :hook here because `project-find-functions' doesn't
;; end in "-hook", and we can't use this in :init because it won't be defined
;; yet.
(use-package project
  :config
  (add-hook 'project-find-functions #'project-root-override))

(defun run-command-in-directory (dir cmd &rest args)
  "Run a command in the specified directory. If the directory is nil, the directory of the file is used. The stdout result is trimmed of whitespace and returned."
  (let (
        (default-directory (or dir default-directory))
        (stdout-buffer (generate-new-buffer "tmp-stdout" t))
        (full-cmd (append '(call-process cmd nil (list stdout-buffer nil) nil) args))
        )
    (unwind-protect
        (let ((exit-status (condition-case nil (eval full-cmd) (file-missing nil))))
          (if (eq exit-status 0)
              (progn
                (with-current-buffer stdout-buffer
                  (string-trim (buffer-string))
                  )
                )
            )
          )
      (kill-buffer stdout-buffer)
      )
    )
  )

(defun locate-venv-poetry ()
  "Find a poetry venv."
  (run-command-in-directory nil "poetry" "env" "info" "-p")
  )

(use-package eglot
  :ensure nil
  :hook
  ((go-ts-mode
    python-base-mode-hook
    rust-ts-mode) . eglot-ensure))

;; (add-hook
;;  'python-ts-mode
;;  (lambda ()
;;    (when (executable-find "poetry")
;;      (let
;;          ((venv (locate-venv-poetry)))
;;        (when venv
;;          (setq eglot-workspace-configuration
;;                (list (cons ':python (list ':venvPath venv ':pythonPath (concat venv "/bin/python")))))
;;          ))
;;      )
;;    (eglot-ensure)))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config	(eglot-booster-mode))


(use-package nix-mode
  :mode "\\.nix\\'")
(use-package rust-mode
  :custom
  (rust-mode-treesitter-derive t))
(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode)
  :config (evil-define-key 'normal 'cargo-mode-map (kbd "C-c") 'cargo-minor-mode-command-map))

(use-package nerd-icons
  :custom (nerd-icons-font-family "Hack Nerd Font"))
(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package gruber-darker-theme)
;; (use-package sourcerer-theme)
;; (use-package gruvbox-theme)
(load-theme 'gruber-darker t)
;; (load-theme 'sourcerer t)
;; (load-theme 'gruvbox-dark-soft t)
