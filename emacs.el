;; TODO:
;; - configure vterm
;; - is there something like harpoon?
;;   - apparently bookmarks already solve this?
;; - direnv / https://github.com/purcell/envrc
;; - autocompletion (https://github.com/minad/corfu)
;; - treesitter // treesit-auto (https://github.com/renzmann/treesit-auto)
;; - lsp stuff // lsp-mode ++ lsp-booster (https://github.com/blahgeek/emacs-lsp-booster)
;; - flycheck
;; - some autoformatting package
;; - modes for these languages:
;;   - c/cpp
;;   - rust
;;   - go
;;   - zig
;;   - python
;;   - lua
;;   - haskell
;;   - nix
;;   - bash
;;   - js/ts
;;   - html
;;   - markdown
;;   - css/scss
;;   - yaml
;;   - toml
;;   - json
;;   - docker
;; - dap stuff
;; - configure org
;; - configure mode-line
;; - try out kuronami-theme (https://github.com/inj0h/kuronami?tab=readme-ov-file)

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
;(package-refresh-contents)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
    auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package evil
    :init
    (setq evil-want-C-d-scroll t
          evil-want-C-u-scroll t
	  evil-insert-state-cursor 'evil-normal-state-cursor
          evil-want-keybinding nil)
    :config
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
    (evil-mode 1))

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

  ;:hook
  ;(prog-mode . display-line-numbers-mode)

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (electric-indent-mode -1)
  (save-place-mode 1)
  (when scroll-bar-mode
    (scroll-bar-mode -1))
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)
  (dolist (mode '(term-mode-hook shell-mode-hook eshell-mode-hook))
    (add-hook mode (lambda () (global-display-line-numbers-mode 0))))
  (setq
   inhibit-startup-screen t
   display-line-numbers-type 'visual
   make-backup-files nil
   tab-width 4
   indent-tabs-mode nil
   use-dialog-box nil
   scroll-step 1
   scroll-conservatively 10000)
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
  (setq line-height (if (eq system-type 'darwin) 150 150))
  (set-face-attribute 'default nil :family "Hack Nerd Font" :height line-height)
  (setq switch-to-prev-buffer-skip 'skip-these-buffers
    ring-bell-function #'ignore))

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker))

(use-package vertico
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle nil))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :custom
  ;; this defines which completion styles to use.
  ;; flex is fuzzy search, and basic is the regular built-in and it's used as a fallback
  (completion-styles '(flex basic)))

(use-package projectile
  :init
  (setq
    projectile-project-search-path '(("~/code" . 1) ("~/.dotfiles" . 0) ("~/notes" . 0) ("~/work" . 1) ("~/work/repos" . 1))
    projectile-require-project-root nil
    projectile-switch-project-action #'projectile-dired
    projectile-sort-order 'recentf)
  :config
  (evil-global-set-key 'normal (kbd "<leader>p") 'projectile-command-map)
  (projectile-mode +1))

;; gonna try just using projectile with fd and ripgrep
;; (use-package consult
;;   :config
;;   (evil-global-set-key 'normal (kbd "<leader>psr") 'consult-ripgrep))

(use-package dired
  :ensure nil
  :config
  (evil-global-set-key 'normal (kbd "-") 'dired-jump))

(use-package hl-todo
  :config
  (hl-todo-mode))

(use-package magit
  :init
  (setq with-editor-emacsclient-executable "/run/current-system/sw/bin/emacsclient")
  :config
  (evil-global-set-key 'normal (kbd "<leader>gg") 'magit))

(use-package vterm)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
