;;; default.el --- rhuibjr's emacs configurations    -*- lexical-binding: t; -*-
;;
;; Author: rhuibjr
;; Maintainer: rhuibjr <rhuibjr.business@gmail.com>
;;
;; Copyright (C) 2022, rhuibjr, all rights reserved.
;;
;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;;; Code:
;;
;; Default
(use-package emacs
  :hook
  ((prog-mode . show-paren-mode))
  :config
  (setq-default

    ; Garbage
    initial-scratch-message   nil
    initial-major-mode        'org-mode
    inhibit-startup-message   t
    create-lockfiles          nil
    auto-save-default         nil
    make-backup-files         nil
    use-dialog-box            nil
    truncate-lines            t
    frame-title-format        " "
    gc-cons-threshold         100000000
    tab-width                 10
    ring-bell-function        'ignore
    split-width-threshold     1

    select-enable-clipboard   t

    ; Editor
    custom-safe-themes        t
    custom-theme-load-path
     '("~/.config/emacs/themes/")

    ; Package
    use-package-always-ensure t

    show-paren-delay          0
    compilation-scroll-output t
    compile-command           "make"

    display-line-numbers-type 'relative
    display-fill-column-indicator-column 80
    display-fill-column-indicator-character '32
    fill-column 86

    ; GDB
    gdb-show-main             t
    gdb-many-windows          t

    ; Scrolling
    scroll-margin             3
    scroll-conservatively     101
    scroll-up-aggressively    0.01
    scroll-down-aggressively  0.01
    scroll-preserve-screen-position t
    auto-window-vscroll       nil

    ; Backups
    backup-directory-alist '(("." . "~/.emacs.d/emacs_saves"))
    backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

  ; Appearance
  (load-theme 'rhuibjr-minimal t)

  ; Hooks
  (add-hook 'minibuffer-setup-hook
	   (lambda () (setq truncate-lines t)))
  ; Modes
  (global-display-line-numbers-mode 1)
  (save-place-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1)
  (tool-bar-mode   -1))

(if (eq system-type 'darwin)
 (set-face-attribute 'default nil
  :family "Roboto Mono Medium"
  :height 160 
  :weight 'normal
  :width 'normal)
 (set-face-attribute 'default nil
  :family "Roboto Mono Medium"
  :height 130 
  :weight 'normal
  :width 'normal))

;;
;;; File and directory management
;;
(use-package ido ; smex
  :init
  (setq default-directory "~/")
  :bind
  (( "M-x" . 'smex ))
  :config
  (ido-everywhere 1)
  (ido-mode 1))

;; NOTE: Org-roam nodes with spaces require `C-q SPC`.
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(require 'ido-complete-space-or-hyphen)
(ido-complete-space-or-hyphen-mode 1)

(use-package dired-x
  :ensure nil
  :config
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dired-omit-files
    (rx (or
          (seq bol (? ".") "#")
          (seq bol "." eol)
          (seq bol ".cache" eol)
          (seq bol ".dir-locals" eol)
          (seq bol ".paper.log" eol)
          (seq bol ".DS_Store" eol)
          (seq bol ".ipynb_checkpoints" eol)
          (seq bol "compile_commands.json" eol)
	        (seq bol ".git" eol)))))

(if (eq system-type 'darwin)
 (setq
  dired-dwim-target t
  insert-directory-program "gls" dired-use-ls-dired t
  dired-listing-switches "-laGh1v --group-directories-first")
 (setq
  dired-use-ls-dired t
  insert-directory-program "/usr/bin/ls"
  dired-recursive-copies 'always
  dired-recursive-deletes 'always
  dired-listing-switches "-laGh1v --group-directories-first"))

;;
;;; Writing
;;
(use-package evil
  :ensure t
  :init
  (setq
     evil-want-integration t
     evil-want-keybinding nil
     evil-respect-visual-line-mode t)
  :init
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1))

;;
;;; Terminal/Compilation
;;
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;;
;;; default.el ends here
