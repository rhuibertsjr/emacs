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
  (lambda () (rhjr/semi-mini-buffer-mode "*compilation*")))

(add-hook 'prog-mode-hook
  (lambda () (progn
               (electric-pair-mode)
               (setq
                 visual-fill-column-center-text nil)
               (visual-line-mode 1)
               (display-fill-column-indicator-mode))))

;;
;;; Organizational files
;;
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook
  (lambda () (progn
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


;; IDO allow `SPC'
(define-key minibuffer-local-completion-map " " 'self-insert-command);;

;;; hook.el ends here
