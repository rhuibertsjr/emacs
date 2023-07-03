;;; rhjr-dark-theme.el --- Rhuibertsjr dark theme.   -*- lexical-binding: t; -*-
;;
;; Author: Rhuibertsjr 
;; Maintainer: Rhuibertsjr <rhuibjr.business@gmail.com>
;;
;; Copyright (C) 2022, Rhuibertsjr 
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

(require 'rhjr-faces)

(defun rhjr-set-dark-theme ()
  "(RHJR) Dark theme colours"
  (setq ; Emacs variables 
    frame-background-mode  'dark)
  (setq ; Custom variables
    rhjr-theme-variant     "dark"
    rhjr-colour-foreground "#f5f5f7"
    rhjr-colour-background "#1c1c1e"
    rhjr-colour-mute       "#8e8e93"
    rhjr-colour-const      "#b8bb26"
    rhjr-colour-fill       "#3a3a3c"
    rhjr-colour-succeed    "#3bcf5e"
    rhjr-colour-error      "#ff453a"
    rhjr-colour-highlight  "#fabd2f"
    rhjr-colour-strong     "#fe8019"))

(provide 'rhjr-dark-theme)

;;
;;; rhjr-light-theme.el ends here.  
