;;; init.el --- rhuibjr's emacs configurations       -*- lexical-binding: t; -*-
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
; Writing and editing
  'hungry-delete
  'visual-fill-column
; Editor confiurations
  'editorconfig
  'exec-path-from-shell
  'eglot
  'corfu
  'cape
; Documents
  'pdf-tools
; Appearance
  'ansi-color)

;; Unset keybindings
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x o"))

;; Set keybindings 
(global-set-key (kbd "C-x C-r") 'recompile)
(global-set-key (kbd "C-x C-d") 'ido-dired)
(global-set-key (kbd "C-x 3 d") (rhuibjr/open-bookmark-window))
(global-set-key (kbd "C-x o")   'previous-buffer)


;;
;;; Language servers and auto-completion 
;;
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
    ;'((c-mode c++-mode ) . ("clangd-11"))))
    '((c-mode c++-mode ) . ("clangd"))))

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
    ("TAB"     . corfu-next)
    ([tab]     . corfu-next)
    ("S-TAB"   . corfu-previous)
    ([backtab] . corfu-previous))
  :hook
  ((prog-mode . corfu-mode)
   (org-mode  . corfu-mode))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions 'cape-file))

;;
;;; Writing and editing text 
;;
(use-package org
  :ensure t
  :hook
  (( org-mode . org-indent-mode )
   ( org-mode . visual-line-mode)
   ( org-mode . display-fill-column-indicator-mode ))
  :config
  (setq
    org-hide-emphasis-markers t))

(use-package hungry-delete
  :init (global-hungry-delete-mode))

(use-package pdf-tools
  :pin manual
  :init
  (pdf-tools-install)
  :config
  (setq
    ; LaTeX font facing
    font-latex-fontify-script      nil
    font-latex-fontify-sectioning 'color
    ; LaTeX pdf-viewer
    TeX-master                     nil 
    TeX-view-program-selection         '((output-pdf "PDF Tools"))
    TeX-source-correlate-start-server  t
    ; PDF viewer
    pdf-view-display-size 'fit-page))

;;
;;; Enviroment variables
;;
(setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig") ;; TODO Debian version
;;
;;; init.el ends here
