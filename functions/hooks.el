;;; hooks.el --- rhuibjr's emacs configurations      -*- lexical-binding: t; -*-
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
(add-hook 'compilation-mode-hook
  (lambda () (rhuibjr/semi-mini-buffer-mode "*compilation*")))

(add-hook 'prog-mode-hook
  (lambda () (progn
               (show-paren-mode 1)
               (electric-pair-mode -1)
               (setq
                 visual-fill-column-center-text nil)
               (visual-line-mode 1)
               (display-fill-column-indicator-mode +1)
               (display-line-numbers-mode +1))))

;;
;;; Organizational files
;;
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook
  (lambda () (progn
               (display-line-numbers-mode -1)
               (display-fill-column-indicator-mode -1)
               (auto-fill-mode +1)
               (setq
                 visual-fill-column-center-text t))))

;;
;;; LaTeX & PDF-viewing
;;
(add-hook 'pdf-view-mode-hook
  (lambda () (progn
               (pdf-view-fit-page-to-window)
               (display-line-numbers-mode -1))))

(add-hook 'doc-view-mode-hook
  (lambda () (progn
               (display-line-numbers-mode -1))))

(add-hook 'LaTeX-mode-hook
  (lambda () (progn
               (display-fill-column-indicator-mode)
               (visual-line-mode 1))))

(add-hook 'TeX-after-compilation-finished-functions
  #'TeX-revert-document-buffer)

;;
;;; Language settings 
;;
(add-hook 'c-mode-common-hook 'rhuibjr/gnuish-c-hook)

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

;;
;;; Orgmode 
;;
(add-hook 'org-mode-hook
  (lambda ()
    "Remove ugly symbols"
    (push '("#+title: "        . "") prettify-symbols-alist)
    (push '("#+filetags: "     . "") prettify-symbols-alist)
    (push '("#+category: "     . "") prettify-symbols-alist)
    (prettify-symbols-mode)
    (rhuibjr/org-hide-properties)))

;;
;;; Others
;;
(add-hook 'emacs-startup-hook
  #'rhuibjr/startup-time-metric)

(add-hook 'minibuffer-setup-hook
	(lambda () (setq truncate-lines t)))

;;
;;; hook.el ends here
