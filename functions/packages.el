;;; packages.el --- rhuibjr's emacs configurations        -*- lexical-binding: t; -*-
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
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(defvar rhjr/package-contents-refreshed nil)

(defun rhjr/package-refresh-contents-once ()
  (when (not rhjr/package-contents-refreshed)
    (setq rhjr/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rhjr/require-one-package (package)
  (when (not (package-installed-p package))
    (rhjr/package-refresh-contents-once)
    (package-install package)))

(defun rhjr/require (&rest packages)
  (dolist (package packages)
    (rhjr/require-one-package package)))

(defun rhjr/require-theme (theme)
  (rhjr/require theme)
  (load-theme theme t))
;;
;;; packages.el ends here.
