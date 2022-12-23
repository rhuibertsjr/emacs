;;; default.el --- rhuibjr's emacs configurations         -*- lexical-binding: t; -*-
;;
;; Author: rhuibjr
;; Maintainer: rhuibjr <rhuibjr.business@gmail.com>
;;
;; Copyright (C) 2022, rhuibjr, all rights reserved.
;;
;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
  ((prog-mode . show-paren-mode)
   (prog-mode . visual-line-mode))
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

    select-enable-clipboard   t

    ; Editor
    custom-safe-themes        t

    ; Package
    use-package-always-ensure t

    show-paren-delay          0
    compilation-scroll-output t
    compile-command           "make"

    display-line-numbers-type 'relative
    display-fill-column-indicator-column 85
    display-fill-column-indicator-character '32 

    ; Scrolling
    scroll-margin             3
    scroll-conservatively   101
    scroll-up-aggressively    0.01
    scroll-down-aggressively  0.01
    scroll-preserve-screen-position t
    auto-window-vscroll       nil

    ; Backups
    backup-directory-alist '(("." . "~/.emacs.d/emacs_saves"))
    backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

  ; Appearance 
  (set-face-attribute 'default nil
   :family "Roboto Mono Medium"
   :height 130
   :weight 'normal
   :width 'normal)
  (set-face-background 'fill-column-indicator
   "#3c3836")

  ; Hooks
  (add-hook 'minibuffer-setup-hook
	   (lambda () (setq truncate-lines t)))
  ; Modes
  (global-display-line-numbers-mode 1)
  (save-place-mode  1)
  (scroll-bar-mode -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1)
  (tool-bar-mode   -1))

;;
;;; File and directory management
;;
(use-package ido ; smex
  :init 
  (setq default-directory "~/")
  :bind 
  (( "M-x" . 'smex ))
  :config
  (ido-mode 1))

(use-package dired-x
  :config
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dired-omit-files
    (rx (or
          (seq bol (? ".") "#")
          (seq bol "." eol)
          (seq bol ".cache" eol)
          (seq bol "compile_commands.json" eol)
	        (seq bol ".git" eol))))
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
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;;
;;; Important functions on startup
;;
(defun rhjr/bookmark-jump ()
  "Jump to custom bookmark via C-x C-g."
  (lambda ()
    (interactive)
    (bookmark-jump
      (ido-completing-read "Jump to bookmark: " (bookmark-all-names)))))
;;
;;; default.el ends here
