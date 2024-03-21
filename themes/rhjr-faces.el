;;; rhjr-faces.el --- rhjr's theme faces.            -*- lexical-binding: t; -*-
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

(defgroup rhjr '()
  "(rhjr) Customizable Emacs themes.")

;;defaults
(defcustom rhjr-colour-foreground (face-foreground 'default nil t) 
  "" :type 'color :group 'rhjr)

(defcustom rhjr-colour-background (face-background 'default nil t) 
  "" :type 'color :group 'rhjr)


(defcustom rhjr-colour-main 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

(defcustom rhjr-colour-accent 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

(defcustom rhjr-colour-region 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

(defcustom rhjr-colour-mute 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

(defcustom rhjr-colour-doc 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

(defcustom rhjr-colour-complete 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

(defcustom rhjr-colour-error 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

(defcustom rhjr-colour-border 'unspecified 
  "(rhjr)" :type 'color :group 'rhjr)

;;fonts
(defcustom rhjr-font-family "Roboto Mono"
  "(rhjr)" :type 'string :group 'rhjr)

(defcustom rhjr-font-size 12
  "(rhjr)" :type 'integer :group 'rhjr) 

;;faces
(defface rhjr-face-default nil
  "(rhjr) The default inherit face."
  :group 'rhjr)

(defface rhjr-face-main nil
  "(rhjr) The default coloured face."
  :group 'rhjr)

(defface rhjr-face-accent nil
  "(rhjr) The sub-default coloured face."
  :group 'rhjr)

(defface rhjr-face-region nil
  "(rhjr) Background colour for regions."
  :group 'rhjr)

(defface rhjr-face-mute nil
  "(rhjr) Comments, disabled, unchecked, etc."
  :group 'rhjr)

(defface rhjr-face-doc nil
  "(rhjr) Comments, disabled, unchecked, etc."
  :group 'rhjr)

(defface rhjr-face-complete nil
  "(rhjr) Complete, succes, done."
  :group 'rhjr)

(defface rhjr-face-error nil
  "(rhjr) Error, bug, watch out!"
  :group 'rhjr)

(defface rhjr-face-border nil
  "(rhjr) Vertical-border, dashes, etc."
  :group 'rhjr)

(defface rhjr-face-flycheck-error '((t :extend t))
  "(rhjr) Face for errors that are marked by flycheck."
  :group 'rhjr)

;;- rhjr: treesit
(defface rhjr-ts-preprocess nil
  "" :group 'rhjr)

(defface rhjr-ts-preprocess-id nil
  "" :group 'rhjr)

(defface rhjr-ts-preprocess-func nil
  "" :group 'rhjr)

(defface rhjr-ts-preprocess-include-system nil
  "" :group 'rhjr)

(defface rhjr-ts-preprocess-include-literal nil
  "" :group 'rhjr)

(defface rhjr-ts-keywords nil
  "" :group 'rhjr)

(defface rhjr-ts-statement nil
  "" :group 'rhjr)

(defface rhjr-ts-punctuation nil
  "" :group 'rhjr)

;;org-mode
(defface rhjr-face-org-levels nil
  "" :group 'rhjr)

;;show-parens
(defface rhjr-face-show-parens-match nil
  "(rhjr) Show Parens mode matching parenthesis."
  :group 'rhjr)

;;- rhjr: initialize all the faces. 
(defun rhjr-faces ()
  "(rhjr) Initialize all the faces for the 'rhjr' theme."

  (set-face-attribute 'rhjr-face-default nil
    :foreground  rhjr-colour-foreground
    :background  rhjr-colour-background
    :family      rhjr-font-family
    :height      (* rhjr-font-size 10))

  (set-face-attribute 'rhjr-face-main nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-main
    :background 'unspecified)

  (set-face-attribute 'rhjr-face-accent nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-accent)

  (set-face-attribute 'rhjr-face-region nil
    :background  rhjr-colour-region)

  (set-face-attribute 'rhjr-face-border nil
    :foreground  rhjr-colour-border
    :background  rhjr-colour-background)

  (set-face-attribute 'rhjr-face-mute nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-mute)

  (set-face-attribute 'rhjr-face-complete nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-complete)

  (set-face-attribute 'rhjr-face-error nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-error)

  (set-face-attribute 'rhjr-face-doc nil
    :inherit    'rhjr-face-default
    :foreground  rhjr-colour-doc)

  (set-face-attribute 'rhjr-face-flycheck-error nil
    :background  "#310201")

  ;; treesit
  ;; rhjr: preprocessor
  (set-face-attribute 'rhjr-ts-preprocess nil
    :inherit 'rhjr-face-default
    :foreground  rhjr-colour-accent)

  (set-face-attribute 'rhjr-ts-preprocess-id nil
    :inherit 'rhjr-face-default
    :foreground  rhjr-colour-main)

  (set-face-attribute 'rhjr-ts-preprocess-include-system nil
    :inherit 'rhjr-face-default
    :foreground  rhjr-colour-doc)

  (set-face-attribute 'rhjr-ts-preprocess-include-literal nil
    :inherit 'rhjr-face-default
    :foreground  rhjr-colour-doc)

  (set-face-attribute 'rhjr-ts-preprocess-func nil
    :inherit 'rhjr-face-default
    :foreground  rhjr-colour-doc)

  (set-face-attribute 'rhjr-ts-keywords nil
    :inherit 'rhjr-face-default
    :foreground  rhjr-colour-main)

  (set-face-attribute 'rhjr-ts-statement nil
    :inherit 'rhjr-face-default
    :foreground "#fb4934")

  (set-face-attribute 'rhjr-ts-punctuation nil
    :inherit 'rhjr-face-default
    :foreground rhjr-colour-accent)

  (set-face-attribute 'rhjr-face-org-levels nil
    :inherit 'rhjr-face-default
    :bold t)

  (set-face-attribute 'rhjr-face-show-parens-match nil
    :inherit 'rhjr-face-default
    :foreground rhjr-colour-accent)
  )

(provide 'rhjr-faces)

;;
;;; rhjr-faces.el ends here.
