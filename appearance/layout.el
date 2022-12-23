;;; layout.el --- rhuibjr's emacs configurations          -*- lexical-binding: t; -*-
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
(setq-default
  ;; Window divider margins
  window-divider-default-right-width 24
  window-divider-default-places 'right-only)

;; Internal border
(add-to-list 'default-frame-alist '(internal-border-width . 24)) 
(setq-default left-margin-width 2 right-margin-width 2)
(set-window-buffer nil (current-buffer))      

;; Turn on window divider
(window-divider-mode 1)
;;
;;; layout.el ends here
