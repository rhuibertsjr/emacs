;;; modeline.el --- rhuibjr's emacs configurations        -*- lexical-binding: t; -*-
;;
;; author: rhuibjr
;; maintainer: rhuibjr <rhuibjr.business@gmail.com>
;;
;; copyright (c) 2022, rhuibjr, all rights reserved.
;;
;;;
;;
;; this program is free software; you can redistribute it and/or
;; modify it under the terms of the gnu general public license as
;; published by the free software foundation; either version 3, or
;; (at your option) any later version.
;; 
;; this program is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the gnu
;; general public license for more details.
;;
;;; code:
;;
;; Mode- and headerline 
(setq-default
  mode-line-format
  '(
     ;; point position
     (8
       (:propertize " %l:" face font-lock-defaults)
       (:eval (propertize "%c" 'face (if (>= (current-column) 80)
                                       'font-lock-warning-face
                                       'font-lock-defaults))))

     ;; major modes
     (:propertize "%m: " face font-lock-defaults
       help-echo buffer-file-coding-system)

     ;; shortened directory (if buffer have a corresponding file)
     (:eval
       (when (buffer-file-name)
         (propertize (shorten-directory default-directory 25)
           'face 'font-lock-comment-face)))

     ;; buffer name
     (:propertize "%b" face font-lock-defaults)))
;;
;;; modeline.el ends here
