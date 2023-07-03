;;; layout.el --- rhuibjr's emacs configurations     -*- lexical-binding: t; -*-
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
;; Layout 
(add-to-list 'load-path "/Users/renehuiberts/.config/emacs/themes")
(add-to-list 'load-path "/Users/renehuiberts/.config/emacs/themes/themes")

(require 'rhjr-faces)
(require 'rhjr-theme)
(require 'rhjr-light-theme)
(require 'rhjr-dark-theme)

(setq-default
  ;; Window divider margins
  window-divider-default-right-width 24
  window-divider-default-places 'right-only)

;; Internal border
(add-to-list 'default-frame-alist '(internal-border-width . 24)) 
(setq-default left-margin-width 2 right-margin-width 2)
(set-window-buffer nil (current-buffer))      

;; Remove toolbar
(push '(tool-bar-lines . 0) default-frame-alist)

;; Turn on window divider
(window-divider-mode 1)

(rhjr-faces)
(rhjr-theme)
(rhjr-set-dark-theme)
(rhjr/refresh-theme) 

(global-display-line-numbers-mode 1)
(save-place-mode -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;;
;;; Helper
;;
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "./" output)))
    output))

;;
;;; Modeline
;;
(setq-default header-line-format nil)

(setq-default mode-line-format
  '(
     ;; Mode
     " "
     (:propertize "("  face default)
     (:propertize "%m" face rhjr-face-const)
     (:propertize ") " face default)

     ;; Directory
     (:propertize (:eval (shorten-directory default-directory 20))
       face 'rhjr-face-modeline-folder)
     (:propertize "%b"
       face 'rhjr-face-modeline-file)

     ;; Cursor position
     (:propertize "::%l" face default)
  )
)

;;
;;; layout.el ends here
