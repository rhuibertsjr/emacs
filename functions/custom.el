;;; custom.el --- rhuibjr's emacs configurations     -*- lexical-binding: t; -*-
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
;; On startup 
(defun rhjr/startup-message ()
  "Welcome message on startup."
  (message " "))

(defun display-startup-echo-area-message ()
  (rhjr/startup-message))

;; Buffers
(defun rhuibjr/open-bookmark-window ()
  "Jump to custom bookmarks in other frame."
  (lambda ()
    (interactive)
    (bookmark-jump
      (ido-completing-read "Jump to bookmark: " (bookmark-all-names))
      'switch-to-buffer-other-window)))

;; Mini buffers
(defun rhjr/semi-mini-buffer-mode (buffer-name)
 "Compile window always at the bottom."
 (when (not (get-buffer-window buffer-name))
  (save-selected-window
   (save-excursion
    (let* ((w (split-window-vertically))
           (h (window-height w)))
     (select-window w)
     (switch-to-buffer buffer-name)
     (delete-other-windows))))))
;;
;;; custom.el ends here
