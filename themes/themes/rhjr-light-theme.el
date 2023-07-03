;;; rhjr-light-theme.el --- Rhuibertsjr light theme. -*- lexical-binding: t; -*-
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

(defun rhjr-set-light-theme ()
  ""
  (setq ; Emacs variables 
    frame-background-mode  'light)
  (setq ; Custom variables
    rhjr-theme-variant     "light"
    rhjr-colour-foreground "#37474F"
    rhjr-colour-background "#FFFFFF"
    rhjr-colour-highlight  "#FAFAFA"
    nano-color-subtle      "#ECEFF1" ; 
    nano-color-faded       "#B0BEC5" ; (vendor) writer-mode
    rhjr-colour-strong     "#37474F"))

(provide 'rhjr-light-theme)

;;
;;; rhjr-light-theme.el ends here.  
