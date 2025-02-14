;;; config -- Summary
;;; Commentary:
;; TODO:
;; - configure regular emacs stuff, evil mode, theme
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
  (setq backup-directory-alist '(("." . "~/.local/emacs/backups")))
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
  (setq kept-old-versions 1000)
  (setq vc-make-backup-files t)
  (setq version-control t)
  (setopt display-fill-column-indicator-column 80)
  (global-display-fill-column-indicator-mode +1)
  (set-face-attribute 'default nil :family "Hack Nerd Font" :height (if (eq system-type 'darwin) 150 110))
  (set-frame-parameter nil 'alpha 96)
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers
        ring-bell-function #'ignore))

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
  (setq doom-modeline-height 15)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-vcs-max-length 50)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))

;(require 'package)
;(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
;                         ("gnu"   . "http://elpa.gnu.org/packages/")
;			                   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;                         ("melpa" . "http://melpa.org/packages/")))
;
;;; Actually get “package” to work.
;(package-initialize)
;;; (package-refresh-contents)
;
;(require 'use-package)
;(setq use-package-always-ensure t)
;
;;; EMACS SETTINGS
;(setq custom-file "~/.emacs.d/custom.el")
;(ignore-errors (load custom-file)) ;; It may not yet exist.
;(use-package emacs
;  :ensure nil
;
;  :init
;  (tool-bar-mode -1)
;  (menu-bar-mode -1)
;  (tooltip-mode -1)
;  (electric-indent-mode -1)
;  (save-place-mode 1)
;  (when scroll-bar-mode (scroll-bar-mode -1))
;  (global-hl-line-mode 1)
;  (global-display-line-numbers-mode 1)
;  (global-auto-revert-mode 1)
;  (indent-tabs-mode -1)
;  ;; Set the default coding system for files to UTF-8.
;  (modify-coding-system-alist 'file "" 'utf-8)
;  (global-set-key (kbd "C-=") 'text-scale-increase)
;  (global-set-key (kbd "C--") 'text-scale-decrease)
;  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;
;  :config
;  ;; Performance Hacks
;  ;; Emacs is an Elisp interpreter, and when running programs or packages,
;  ;; it can occasionally experience pauses due to garbage collection.
;  ;; By increasing the garbage collection threshold, we reduce these pauses
;  ;; during heavy operations, leading to smoother performance.
;  (setq gc-cons-threshold #x40000000)
;
;  ;; Set the maximum output size for reading process output, allowing for larger data transfers.
;  (setq read-process-output-max (* 1024 1024 4))
;  (setq ring-bell-function #'ignore)
;  (setq inhibit-startup-screen t)
;  (setq display-line-numbers-type 'visual)
;  (setq make-backup-files nil)
;  (setq backup-directory-alist '(("." . "~/.local/emacs/backups")))
;  (setq tab-width 4)
;  (setq indent-tabs-mode nil)
;  (setq tab-always-indent 'complete)
;  (setq use-dialog-box nil)
;  (setq scroll-step 1)
;  (setq scroll-margin 5)
;  (setq scroll-conservatively 10000)
;  (setq text-mode-ispell-word-completion nil) ;; use cape-dict instead
;  (setq frame-title-format nil)
;  (setq delete-old-versions t)
;  (setq kept-old-versions 1000)
;  (setq vc-make-backup-files t)
;  (setq version-control t)
;  (setopt display-fill-column-indicator-column 80)
;  (global-display-fill-column-indicator-mode +1)
;  (set-face-attribute 'default nil :family "Hack Nerd Font" :height (if (eq system-type 'darwin) 150 110))
;  (set-frame-parameter nil 'alpha 96)
;  (defun skip-these-buffers (_window buffer _bury-or-kill)
;    "Function for `switch-to-prev-buffer-skip'."
;    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
;  (setq switch-to-prev-buffer-skip 'skip-these-buffers
;        ring-bell-function #'ignore))
;
;;; EVIL MODE
;(use-package undo-fu)
;(use-package drag-stuff)
;(use-package evil
;  :demand t
;  :after undo-fu
;  :init
;  (setq evil-want-keybinding nil)
;  (setq evil-undo-system 'undo-fu)
;  :config
;  (setq evil-want-C-d-scroll t)
;  (setq evil-want-C-u-scroll t)
;  (setq evil-split-window-below t)
;  (setq evil-vsplit-window-right t)
;  (setq evil-insert-state-cursor 'box)
;  (evil-set-leader nil (kbd "SPC"))
;  (evil-global-set-key 'normal (kbd "C-d") (lambda () (interactive) (evil-scroll-down 0) (recenter)))
;  (evil-global-set-key 'normal (kbd "C-u") (lambda () (interactive) (evil-scroll-up 0) (recenter)))
;  (evil-global-set-key 'visual (kbd "C-d") (lambda () (interactive) (evil-scroll-down 0) (recenter)))
;  (evil-global-set-key 'visual (kbd "C-u") (lambda () (interactive) (evil-scroll-up 0) (recenter)))
;  (evil-global-set-key 'normal (kbd "n") (lambda () (interactive) (evil-search-next) (recenter)))
;  (evil-global-set-key 'normal (kbd "N") (lambda () (interactive) (evil-search-previous) (recenter)))
;  (evil-global-set-key 'visual (kbd "J") (lambda () (interactive) (drag-stuff-down 1) (evil-indent)))
;  (evil-global-set-key 'visual (kbd "K") (lambda () (interactive) (drag-stuff-up 1) (evil-indent)))
;  ;; (evil-global-set-key 'visual (kbd "J") (concat ":m '>+1" (kbd "RET") "gv=gv"))
;  ;; (evil-global-set-key 'visual (kbd "K") (concat ":m '<-2" (kbd "RET") "gv=gv"))
;  ;; (evil-global-set-key 'normal (kbd "J") (concat ":m +1" (kbd "RET") "=="))
;  ;; (evil-global-set-key 'normal (kbd "K") (concat ":m -2" (kbd "RET") "=="))
;  (evil-global-set-key 'motion (kbd "j") 'evil-next-visual-line)
;  (evil-global-set-key 'motion (kbd "k") 'evil-previous-visual-line)
;  (evil-global-set-key 'normal (kbd "C-m") 'compile)
;  (evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
;  (evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)
;  (evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)
;  (evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
;  (evil-global-set-key 'normal (kbd "<leader>sj") 'evil-window-new)
;  (evil-global-set-key 'normal (kbd "<leader>sl") 'evil-window-vnew)
;  (evil-global-set-key 'normal (kbd "<leader>st") (lambda () (interactive) (evil-window-new 20 "") (vterm)))
;  (evil-mode))
;
;(use-package evil-collection
;  :after evil
;  :config
;  (evil-collection-init))
;
;(use-package evil-commentary
;  :after evil
;  :config
;  (evil-define-operator +evil-join-a (beg end)
;    "Join the selected lines.
;This advice improves on `evil-join' by removing comment delimiters when joining
;commented lines, by using `fill-region-as-paragraph'.
;From https://github.com/emacs-evil/evil/issues/606"
;    :motion evil-line
;    (let* ((count (count-lines beg end))
;	   (count (if (> count 1) (1- count) count))
;	   (fixup-mark (make-marker)))
;      (dotimes (var count)
;	(if (and (bolp) (eolp))
;	    (join-line 1)
;	  (let* ((end (line-beginning-position 3))
;		 (fill-column (1+ (- end beg))))
;	    (set-marker fixup-mark (line-end-position))
;	    (fill-region-as-paragraph beg end nil t)
;	    (goto-char fixup-mark)
;	    (fixup-whitespace))))
;      (set-marker fixup-mark nil)))
;
;  (evil-global-set-key 'normal (kbd "J") '+evil-join-a)
;  (evil-commentary-mode))
;
;(use-package gruber-darker-theme
;  :config
;  (load-theme 'gruber-darker nil))
;
;(use-package doom-modeline
;  :defer t
;  :init
;  (doom-modeline-mode 1)
;  (setq find-file-visit-truename t)
;  :config
;  (setq doom-modeline-height 15)
;  (setq doom-modeline-env-version t)
;  (setq doom-modeline-vcs-max-length 50)
;  (setq doom-modeline-env-version nil)
;  (setq doom-modeline-buffer-encoding nil)
;  (setq doom-modeline-buffer-file-name-style 'relative-from-project))
