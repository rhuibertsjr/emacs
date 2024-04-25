;;; early-init.el --- wow, you are early aren't you? -*- lexical-binding: t; -*-
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
;;; Early bird
;;
(setq gc-cons-threshold most-positive-fixnum)

(advice-add 'load-file :override
            (lambda (file) (load (expand-file-name file) nil t t)))

;;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Only available on Emacs v29.1
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Correct size on startup
(add-to-list 'default-frame-alist '(internal-border-width . 24))
(setq initial-frame-alist
      '((top . 5) (left . 1) (width . 190) (height . 49)))

(setenv "PATH"
	(concat "C:\\ProgramData\\chocolatey\\bin\\grep.exe;"
		(getenv "PATH")))

(add-to-list 'load-path "~\\.emacs.d\\themes")
(add-to-list 'load-path "~\\.emacs.d\\themes\\themes")

(require 'rhjr-faces)
(require 'rhjr-theme)
(require 'rhjr-light-theme)
(require 'rhjr-dark-theme)

(rhjr-faces)
(rhjr-theme)
(rhjr-set-dark-theme)
(rhjr/refresh-theme)
