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
;;; Start-up
;;
(defun rhuibjr/bookmark-jump ()
  "Jump to custom bookmark via C-x C-g."
  (lambda ()
    (interactive)
    (bookmark-jump
      (ido-completing-read "Jump to bookmark: " (bookmark-all-names)))))

(global-set-key (kbd "C-x C-g") (rhuibjr/bookmark-jump))

;;
;;; Prerequisite
;;
(load "~/.emacs.d/functions/packages.el")
(load "~/.emacs.d/default.el")
(load "~/.emacs.d/appearance/layout.el")

(load "~/.emacs.d/functions/custom.el")

;;; Packages
;;

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
  ;; Completion frameworks
  'corfu
  'cape
  'tempel
; Documents
  'pdf-tools
; Appearance
  'ansi-color)

;; Unset keybindings
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x e"))
(global-unset-key (kbd "M-="))
(global-unset-key (kbd "M-["))
(global-unset-key (kbd "M-]"))

;; Set keybindings 
(global-set-key (kbd "C-x C-r") 'recompile)
(global-set-key (kbd "C-x C-d") 'ido-dired)
(global-set-key (kbd "C-x 4 g") (rhuibjr/open-bookmark-window))
(global-set-key (kbd "C-x o")   'previous-buffer)
(global-set-key (kbd "C-x e")   'org-emphasize)
(global-set-key (kbd "M-[")     'tempel-previous)
(global-set-key (kbd "M-]")     'tempel-next)

;;
;;; Completion frameworks
;;;  ~ @Minad is a beautiful man.
;; 
(use-package corfu
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

(use-package tempel
  :bind
  (("M-=" . tempel-complete)) 
  :config
  (setq
    tempel-path "~/.emacs.d/templates/template")
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook  'tempel-setup-capf))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions 'cape-file))

;;
;;; Writing and editing text 
;;
(use-package org
  :ensure t
  :hook
  (( org-mode . org-indent-mode )
   ( org-mode . visual-line-mode))
  :config
  (setq
    org-hide-emphasis-markers t))

(use-package org-cliplink
  :ensure t
  :defer  t)

(use-package hungry-delete
  :init (global-hungry-delete-mode))

(use-package pdf-tools
  :pin manual
  :defer t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :init                 ; Disabeling saves 1s in startup time.
  ;(pdf-tools-install)  ;
  :config
  (setq
    ; LaTeX font facing
    font-latex-fontify-script          nil
    font-latex-fontify-sectioning      'color
    ; LaTeX pdf-viewer
    TeX-PDF-mode                       t 
    TeX-Master                         nil 
    TeX-view-program-selection         '((output-pdf "PDF Tools"))
    TeX-source-correlate-start-server  t
    ; PDF viewer
    pdf-view-display-size 'fit-page))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(load "~/.emacs.d/functions/modes/org-mode.el" 'nomessage)
(load "~/.emacs.d/functions/hooks.el" 'nomessage)

;;
;;; Ending
;;
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-cliplink pdf-tools tempel cape corfu exec-path-from-shell visual-fill-column hungry-delete magit smex use-package ido-completing-read+ ido-complete-space-or-hyphen evil-collection esup editorconfig)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
