;;; init.el --- rhuibjr's emacs configurations            -*- lexical-binding: t; -*-
;;
;; Author: Rhuibjr
;; Maintainer: Rhuibjr <rhuibjr.business@gmail.com>
;;
;; Copyright (C) 2022, Rhuibjr, all rights reserved.
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
;; Packages
(rhjr/require
; Package management
  'use-package
; File and directory management 
  'smex
  'magit
; Keybindings management
  'evil
  'evil-collection
; Editor configurations
  'editorconfig
  'exec-path-from-shell
  'eglot
  'corfu
; Appearance
  'ansi-color
  'olivetti)

;; Keybindings 
(global-set-key (kbd "C-x C-r") 'recompile)
(global-set-key (kbd "C-x C-d") 'ido-dired)

;;
;;; Package configurations
;;
;; Auto-complete and editing text 
(use-package eglot
  :ensure t
  :hook
  (( c-mode   . eglot-ensure)
   ( c++-mode . eglot-ensure))
  :config
  (setq
    eglot-ignored-server-capabilites
    '(:documentHighlightProvider :hoverProvider))
  (add-to-list 'eglot-server-programs
    '((c-mode c++-mode ) . ("clangd-11"))))
 
(use-package corfu
  :ensure t
  :hook
  ((prog-mode . corfu-mode))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  :init
  (global-corfu-mode))

;;
;;; Organisational configurations 
;;
(use-package org
  :ensure t
  :hook
  (( org-mode . olivetti-mode   )
   ( org-mode . org-indent-mode )
   ( org-mode . display-fill-column-indicator-mode ))
  :config
  (setq
    org-hide-emphasis-markers t))

;;
;;; Custom functions
;;
(add-hook 'compilation-mode-hook
  (lambda () (rhjr/semi-mini-buffer-mode "*compilation*")))

(add-hook 'olivetti-mode-on-hook
  (lambda () (progn
               (olivetti-set-width 80)
               (set-variable 'display-fill-column-indicator-column 69))))

(add-hook 'prog-mode-hook
  (lambda () (progn
               (set-variable 'display-fill-column-indicator-column 84)
               (display-fill-column-indicator-mode))))
;;
;;; init.el ends here
