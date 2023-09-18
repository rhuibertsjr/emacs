;;; rhjr-dark-theme.el --- rhjr's dark theme.        -*- lexical-binding: t; -*-
;;
;; Author: Rhuibertsjr 
;; Maintainer: Rhuibertsjr <rhuibjr.business@gmail.com>
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
;;;

(require 'rhjr-faces)

(defun rhjr-set-dark-theme ()
  "(rhjr) Custom Emacs dark theme."
  (setq ; Emacs variables 
    frame-background-mode  'dark)
  (setq ; Custom variables
    rhjr-theme-variant     "dark"
    rhjr-colour-foreground "#f5f5f7"
    rhjr-colour-background "#000000"
    rhjr-colour-main       "#fabd2f"
    rhjr-colour-accent     "#fe8019"
    rhjr-colour-region     "#1e1e1e"
    rhjr-colour-mute       "#8e8e93"
    rhjr-colour-doc        "#b8bb26"
    rhjr-colour-complete   "#b8bb26"
    rhjr-colour-error      "#ff453a"
    rhjr-colour-border     "#1e1e1e"))

(provide 'rhjr-dark-theme)

;;
;;; rhjr-dark-theme.el ends here.  
