;;; rhjr-faces.el --- Rhuibertsjr theme faces.       -*- lexical-binding: t; -*-
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
;;; Faces & Colours
;;

(defgroup rhjr '()
  "Rhuibertsjr emacs theme, colours and faces.")

(defcustom rhjr-colour-foreground (face-foreground 'default nil t) 
  "" :type 'color :group 'rhjr)

(defcustom rhjr-colour-background (face-background 'default nil t) 
  "" :type 'color :group 'rhjr)

;;
;;; Colours
;;
(defcustom rhjr-colour-mute nil 
  "(rhjr) Colour ment for informative- or sub-texts."
  :type 'color :group 'rhjr)

(defcustom rhjr-colour-const nil 
  "(rhjr) Constants like strings and texts."
  :type 'color :group 'rhjr)

(defcustom rhjr-colour-fill nil 
  "(rhjr) Fill / Regions"
  :type 'color :group 'rhjr)

;;
;;; Statusses
;;
(defcustom rhjr-colour-succeed nil 
  "(rhjr) Colour to show a completion or succes."
  :type 'color :group 'rhjr)

(defcustom rhjr-colour-error nil 
  "(rhjr) Colour to show an error / danger ."
  :type 'color :group 'rhjr)


(defcustom rhjr-colour-main nil
  "(RHJR) The main colour of the theme."
  :type 'color :group 'rhjr)

(defcustom rhjr-colour-strong nil
  "(RHJR) Comments, line-numbers, etc."
  :type 'color :group 'rhjr)

(defcustom rhjr-colour-highlight nil
  "(RHJR) Comments, line-numbers, etc."
  :type 'color :group 'rhjr)

(defcustom rhjr-colour-indistinct nil
  "(RHJR) A (main) side colour of the theme."
  :type 'color :group 'rhjr)

;;
;;; Fonts
;;

(defcustom rhjr-font-family "Roboto Mono"
  "" :type 'string :group 'rhjr)

(defcustom rhjr-font-size 16 
  "" :type 'integer :group 'rhjr) 

;;
;;; Faces
;;

(defface rhjr-face-default nil
  "(rhjr) The default inherit face."
  :group 'rhjr)

(defface rhjr-face-mute nil
  "(rhjr) A default face with the mute colour."
  :group 'rhjr)

(defface rhjr-face-const nil
  "(rhjr) A default face with the const colour."
  :group 'rhjr)

(defface rhjr-face-fill nil
  "(rhjr) Selection, region, 'highlight'."
  :group 'rhjr)

;; Modeline
(defface rhjr-face-modeline-folder nil
  ""
  :group 'rhjr)

(defface rhjr-face-modeline-file   nil
  ""
  :group 'rhjr)


;;
;;; Statusses
;;
(defface rhjr-face-succeed nil
  "(rhjr) Succesion or completion."
  :group 'rhjr)

(defface rhjr-face-error nil
  "(rhjr) Error, danger and failed."
  :group 'rhjr)


(defface rhjr-face-highlight nil
  "(RHJR) Default face for unspecified faces."
  :group 'rhjr)

(defface rhjr-face-strong nil
  "(RHJR) Default face for unspecified faces."
  :group 'rhjr)

;; Documents
(defface rhjr-face-document-title nil
  "(RHJR) Document title for org-mode."
  :group 'rhjr)

(defface rhjr-face-document-info nil
  "(RHJR) Document information like tags, category, etc. for org-mode."
  :group 'rhjr)

(defface rhjr-face-document-section nil
  "(RHJR) All document sections for org-mode."
  :group 'rhjr)

;;
;;; Initialization
;;

(defun rhjr-faces ()
  "(RHJR) Initialize all the faces for the rhjr theme."

  (set-face-attribute 'rhjr-face-default nil
    :foreground  rhjr-colour-foreground
    :background  rhjr-colour-background
    :family      rhjr-font-family
    :height      (* rhjr-font-size 10))

  (set-face-attribute 'rhjr-face-mute nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-mute)

  (set-face-attribute 'rhjr-face-const nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-const)

  (set-face-attribute 'rhjr-face-fill nil
    :background  rhjr-colour-fill)


  ;; Modeline
  (set-face-attribute 'rhjr-face-modeline-folder nil
    :inherit    'rhjr-face-default)

  (set-face-attribute 'rhjr-face-modeline-file nil
    :foreground rhjr-colour-highlight
    :inherit    'rhjr-face-default)

  ;; Status
  (set-face-attribute 'rhjr-face-succeed nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-succeed)

  (set-face-attribute 'rhjr-face-error nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-error)


  (set-face-attribute 'rhjr-face-highlight nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-highlight)

  (set-face-attribute 'rhjr-face-strong nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-strong)

  ;; Documents
  (set-face-attribute 'rhjr-face-document-title nil
    :foreground  rhjr-colour-foreground
    :background  rhjr-colour-background
    :family      rhjr-font-family
    :height      (* 20 10))

  (set-face-attribute 'rhjr-face-document-info nil
    :foreground  rhjr-colour-foreground
    :background  rhjr-colour-background
    :family      rhjr-font-family
    :height      (* rhjr-font-size 10))

  (set-face-attribute 'rhjr-face-document-section nil
    :foreground  rhjr-colour-foreground
    :background  rhjr-colour-background
    :bold        t)
  )

(provide 'rhjr-faces)

;;
;;; rhjr-faces.el ends here.
