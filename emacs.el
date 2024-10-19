;; TODO:
;; - is there something like harpoon?
;;   - apparently bookmarks already solve this?
;; - fix consult icon size (on linux at least)
;; - modes for these languages:
;;   - c/cpp
;;   - go
;;   - zig
;;   - lua
;;   - haskell
;;   - nix
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
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

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

(use-package auto-package-update
  :defer 30
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

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
  (setq line-height (if (eq system-type 'darwin) 150 130))
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

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode-hook . format-all-mode)
  :config
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (let ((add-node-modules (lambda () (add-to-list 'exec-path (expand-file-name "node_modules/.bin" (locate-dominating-file (buffer-file-name) "node_modules")))))
        (add-python-venv (lambda () (add-to-list 'exec-path (expand-file-name ".venv/bin" (locate-dominating-file (buffer-file-name) "node_modules"))))))
    (add-hook 'typescript-ts-mode add-node-modules)
    (add-hook 'javascript-mode add-node-modules)
    (add-hook 'python-ts-mode-hook add-python-venv)))
;; (setq-default format-all-formatters
;;               '(("C" (clang-format))
;;                 ("Angular" (astyle "--mode=c"))
;;                 ("Shell" (shfmt "-i" "4" "-ci")))))

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

(use-package projectile
  :custom
  (projectile-project-search-path '(("~/code" . 1) ("~/.dotfiles" . 0) ("~/notes" . 0) ("~/work" . 1) ("~/work/repos" . 1)))
  (projectile-require-project-root nil)
  (projectile-sort-order 'recentf)
  :config
  (evil-global-set-key 'normal (kbd "<leader>p") 'projectile-command-map)
  (projectile-mode +1))

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
  :commands
  (magit)
  :init
  (setq with-editor-emacsclient-executable "/run/current-system/sw/bin/emacsclient")
  :config
  (evil-global-set-key 'normal (kbd "<leader>gg") 'magit))

(use-package vterm
  :commands
  (vterm)
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

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package treesit-auto
  :hook
  (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package eglot
  :ensure nil)
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config	(eglot-booster-mode))

(use-package rust-mode
  :custom
  (rust-mode-treesitter-derive t)
  :hook
  (rust-ts-mode . eglot-ensure))
(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode)
  :config (evil-define-key 'normal 'cargo-mode-map (kbd "C-c") 'cargo-minor-mode-command-map))

(use-package gruber-darker-theme)
;; (use-package sourcerer-theme)
;; (use-package gruvbox-theme)
(load-theme 'gruber-darker t)
;; (load-theme 'sourcerer t)
;; (load-theme 'gruvbox-dark-soft t)
