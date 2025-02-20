;;; config -- Summary
;;; Commentary:
;; TODO:
;; - try to get lspce to work
;;   - otherwise try eglot with lsp-booster to work again
;; - projectile with some sort session management
;; - autoformatting
;; - linting
;; - doom modeline
;; - treesit
;; - dap
;; - vc vs magit
;; - vterm or eat
;; - org mode
;; - try out meow
;; - eww
;;
;; links:
;;   https://github.com/MiniApollo/kickstart.emacs
;;

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
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

;; EMACS SETTINGS
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.
(use-package emacs
  :ensure nil

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  ;; (electric-indent-mode -1)
  (save-place-mode 1)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; Prompt indicator for `completing-read-multiple'.  Available out of the box
  ;; on Emacs 31, see `crm-prompt'.
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args)))))

  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  :config
  ;; Performance Hacks
  (setq gc-cons-threshold (* 2 1000 1000))
  (setq read-process-output-max (* 1024 1024 4))

  (setq ring-bell-function #'ignore)
  (setq inhibit-startup-screen t)
  (setq display-line-numbers-type 'visual)
  (setq make-backup-files nil)
  (setq backup-directory-alist '(("." . "~/.local/emacs/backups")))
  (setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
  (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosave/" t)))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq tab-always-indent 'complete)
  (setq use-dialog-box nil)
  (setq scroll-step 1)
  (setq scroll-margin 5)
  (setq scroll-conservatively 10000)
  (setq text-mode-ispell-word-completion nil) ;; use cape-dict instead
  (setq frame-title-format nil)
  (setq delete-old-versions t)
  (setq kept-old-versions 10)
  (setq vc-make-backup-files t)
  (setq version-control t)
  (setq auto-window-vscroll nil)
  (setopt display-fill-column-indicator-column 80)
  (global-display-fill-column-indicator-mode +1)
  (blink-cursor-mode -1)
  (set-face-attribute 'default nil :family "Hack Nerd Font" :height (if (eq system-type 'darwin) 170 110))
  (set-frame-parameter nil 'alpha 96)
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers
        ring-bell-function #'ignore))

(use-package compile
  :ensure nil
  :config
  (setq compilation-scroll-output t))

(use-package eat
  :straight t
  :hook ('eshell-load-hook #'eat-eshell-mode))

(use-package magit
  :straight t
  :commands magit-status)

;; EXEC-PATH / ENVRC / NIX
(use-package exec-path-from-shell :straight t :demand t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package envrc
  :straight t
  :custom
  (envrc-show-summary-in-minibuffer nil)
  :hook (after-init . envrc-global-mode))

;; EVIL MODE
(use-package undo-fu :straight t)
(use-package drag-stuff :straight t)
(use-package evil
  :straight t
  :demand t
  :after undo-fu
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
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
  (evil-global-set-key 'visual (kbd "J") (lambda () (interactive) (drag-stuff-down 1) (evil-indent)))
  (evil-global-set-key 'visual (kbd "K") (lambda () (interactive) (drag-stuff-up 1) (evil-indent)))
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
  (evil-define-operator +evil-join-a (beg end)
    "Join the selected lines.
This advice improves on `evil-join' by removing comment delimiters when joining
commented lines, by using `fill-region-as-paragraph'.
From https://github.com/emacs-evil/evil/issues/606"
    :motion evil-line
    (let* ((count (count-lines beg end))
	   (count (if (> count 1) (1- count) count))
	   (fixup-mark (make-marker)))
      (dotimes (var count)
	(if (and (bolp) (eolp))
	    (join-line 1)
	  (let* ((end (line-beginning-position 3))
		 (fill-column (1+ (- end beg))))
	    (set-marker fixup-mark (line-end-position))
	    (fill-region-as-paragraph beg end nil t)
	    (goto-char fixup-mark)
	    (fixup-whitespace))))
      (set-marker fixup-mark nil)))
  (evil-global-set-key 'normal (kbd "J") '+evil-join-a)
  (evil-commentary-mode))

(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker nil))

(use-package doom-modeline
  :straight t
  :defer t
  :init
  (doom-modeline-mode 1)
  (setq find-file-visit-truename t)
  :config
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-height 15)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-vcs-max-length 50)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))
;; (use-package mood-line :straight t :config (mood-line-mode))

;; NAVIGATION
(use-package projectile
  :straight t
  :custom
  (projectile-project-search-path
   '(("~/code" . 1)
     ("~/.dotfiles" . 0)
     ("~/notes" . 0)
     ("~/work" . 1)
     ("~/work/repos" . 1)))
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

;; LSP / COMPLETION
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless-flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :after orderless
  :hook
  (after-init . vertico-mode)
  :custom
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  (completion-styles '(flex basic))
  :config
  (evil-define-key 'normal 'vertico-map (kbd "M-h") 'vertico-next-group)
  (evil-define-key 'normal 'vertico-map (kbd "M-j") 'vertico-next)
  (evil-define-key 'normal 'vertico-map (kbd "M-k") 'vertico-previous)
  (evil-define-key 'normal 'vertico-map (kbd "M-;") 'vertico-previous-group))

(use-package marginalia
  :straight t
  :after vertico
  :config
  (marginalia-mode 1))

(use-package flymake
  :ensure nil
  :config
  (add-hook 'emacs-lisp-mode-hook 'flymake-mode)
  (evil-define-key 'normal 'flymake-mode-map (kbd "]d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'flymake-mode-map (kbd "[d") 'flymake-goto-prev-error)
  (evil-define-key 'normal 'flymake-mode-map (kbd "gd") 'flymake-show-project-diagnostics)
  (flymake-mode 1))

(use-package apheleia
  :straight t
  :config
  (setf (alist-get 'black apheleia-formatters)
        '("poetry" "run" "black" "-"))
  (setf (alist-get 'alejandra apheleia-formatters)
        '("alejandra"))
  (setf (alist-get 'nix-mode apheleia-mode-alist)
        '(alejandra))
  (apheleia-global-mode +1))

(use-package markdown-mode :straight t)
(use-package yasnippet :straight t :config (yas-global-mode 1))
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :custom
  (rust-mode-treesitter-derive t))

;; EGLOT SOMEHOW NEEDS THIS TO CORRECTLY DETERMINE THE PROJECT ROOT
;; This SHOULD take care of the problem that project-root-override tries to solve,
;; but for some reason it does not work. I have no idea why, but I don't seem to
;; be the only one.
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
  :ensure nil
  :config
  (add-hook 'project-find-functions #'project-root-override))

(use-package corfu
  :straight t
  ;; :after vertico
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-echo-delay 0.1)
  (corfu-popupinfo-delay 0.1)
  (corfu-preview-current nil)
  :bind (:map corfu-map ("RET" . nil))
  :config
  (evil-define-key 'insert 'corfu-map (kbd "C-j") 'corfu-next)
  (evil-define-key 'insert 'corfu-map (kbd "C-k") 'corfu-previous)
  (evil-define-key 'insert 'corfu-map (kbd "C-l") 'corfu-insert)
  (evil-define-key 'insert 'corfu-map (kbd "C-h") 'corfu-insert-separator)
  (corfu-popupinfo-mode)
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
	 (cons 'orderless-literal-prefix word)))
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  (setq corfu-auto        t
	corfu-auto-delay  0  ;; TOO SMALL - NOT RECOMMENDED
	corfu-auto-prefix 0) ;; TOO SMALL - NOT RECOMMENDED
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local completion-styles '(orderless-fast basic)
                          completion-category-overrides nil
                          completion-category-defaults nil)))
  (global-corfu-mode))

(use-package cape
  :straight t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package eglot
  :ensure nil
  :hook
  ;; ((go-ts-mode
  ;;   haskell-ts-mode
  ;;   python-ts-mode
  ;;   rust-ts-mode
  ;;   zig-ts-mode
  ;;   ) . eglot-ensure)
  ((python-ts-mode
    rust-ts-mode
    zig-ts-mode
    haskell-ts-mode
    go-ts-mode
    ) . eglot-ensure)
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider :colorProvider))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
		 "basedpyright-langserver" "--stdio"))
  (eglot-inlay-hints-mode -1))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; init.el ends here
