;; rhjr-theme.el --- rhjr's emacs theme.            -*- lexical-binding: t; -*-
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

(defcustom rhjr-theme-variant nil
  "(rhjr)" :group 'rhjr :type 'string)

(defun rhjr-set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
    :foreground 'unspecified :background 'unspecified
    :family     'unspecified :slant      'unspecified
    :weight     'unspecified :height     'unspecified
    :underline  'unspecified :overline   'unspecified
    :box        'unspecified :inherit     style))

(defun rhjr-theme-basics ()
  ""
  (set-foreground-color rhjr-colour-foreground)
  (set-background-color rhjr-colour-background)

  ;; text
  (set-face-attribute 'default nil
    :foreground (face-foreground 'default)
    :background (face-background 'default)
    :weight     'light
    :family     (face-attribute 'rhjr-face-default :family)
    :height     (face-attribute 'rhjr-face-default :height))

  (rhjr-set-face 'region                 'rhjr-face-region)
  (rhjr-set-face 'highlight              'rhjr-face-region)

  (rhjr-set-face 'success                'rhjr-face-complete)

  (with-eval-after-load 'orderless 
    (set-face-attribute 'orderless-match-face-0 nil
      :foreground (face-foreground 'rhjr-face-accent))

    (set-face-attribute 'orderless-match-face-1 nil
      :foreground (face-foreground 'rhjr-face-main))

    (set-face-attribute 'orderless-match-face-2 nil
      :foreground (face-foreground 'rhjr-face-doc))

    (set-face-attribute 'orderless-match-face-3 nil
      :foreground (face-foreground 'rhjr-face-error)))

  (set-face-attribute 'fill-column-indicator nil
    :foreground (face-background 'rhjr-face-region)
    :background (face-background 'rhjr-face-region))

  ;; frame
  (set-face-attribute 'fringe nil
    :background (face-background 'default))

  (set-face-attribute 'cursor nil
    :background (face-foreground 'rhjr-face-default))

  (set-face-attribute 'window-divider nil
    :foreground (face-background 'rhjr-face-default))

  (set-face-attribute 'window-divider-first-pixel nil
    :foreground rhjr-colour-background)

  (set-face-attribute 'window-divider-last-pixel nil
    :foreground rhjr-colour-background)

  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
      :background "#090909"))

  (rhjr-set-face 'vertical-border 'rhjr-face-border)

  ;; Emacs GUI
  (rhjr-set-face 'button                'rhjr-face-main)
  (rhjr-set-face 'minibuffer-prompt     'rhjr-face-main))

(defun rhjr-theme-navigation ()
  "(rhjr) Initialize faces of the navigation category, like: dired, ido, etc."

  ;; Dired
  (rhjr-set-face 'dired-header                'rhjr-face-accent)
  (rhjr-set-face 'dired-directory             'rhjr-face-main)

  ;; Parentheses
  (rhjr-set-face 'show-paren-match            'rhjr-face-accent)
  (rhjr-set-face 'show-paren-mismatch         'rhjr-face-error)

  ;; Searching
  (rhjr-set-face 'isearch                     'rhjr-face-region)
  (rhjr-set-face 'isearch-fail                'rhjr-face-error)
  (rhjr-set-face 'match                       'rhjr-face-complete)
  (rhjr-set-face 'lazy-highlight              'rhjr-face-region))

(defun rhjr-theme-fontlock ()
  (rhjr-set-face 'font-lock-comment-face       'rhjr-face-mute)
  (rhjr-set-face 'font-lock-doc-face           'rhjr-face-doc)
  (rhjr-set-face 'font-lock-string-face        'rhjr-face-doc)
  (rhjr-set-face 'font-lock-constant-face      'rhjr-face-accent)
  (rhjr-set-face 'font-lock-warning-face       'rhjr-face-default)
  (rhjr-set-face 'font-lock-function-name-face 'rhjr-face-default)
  (rhjr-set-face 'font-lock-variable-name-face 'rhjr-face-default)
  (rhjr-set-face 'font-lock-builtin-face       'rhjr-face-accent)
  (rhjr-set-face 'font-lock-type-face          'rhjr-face-main)
  (rhjr-set-face 'font-lock-keyword-face       'rhjr-face-main)

  (rhjr-set-face 'font-lock-keyword-face       'rhjr-face-main)

  (with-eval-after-load 'treesit
    (rhjr-set-face 'font-lock-bracket-face 'rhjr-face-accent)
    (rhjr-set-face 'font-lock-delimiter-face 'rhjr-face-accent)

    ;;function
    (set-face-attribute 'font-lock-function-name-face nil
      :foreground "#b8bb26")
    (set-face-attribute 'font-lock-function-call-face nil
      :foreground "#b8bb26")

    ;;variables
    (set-face-attribute 'font-lock-variable-name-face nil
      :foreground (face-foreground 'default))

    ;;numbers
    (set-face-attribute 'font-lock-number-face nil
      :foreground "#d3869b")
    (set-face-attribute 'font-lock-escape-face nil
      :foreground (face-foreground 'rhjr-face-accent))

    ))

(defun rhjr-theme-languages ()
  ;;flycheck
  (set-face-attribute 'flycheck-info nil
    :underline nil)
  (set-face-attribute 'flycheck-warning nil
    :underline nil)
  (set-face-attribute 'flycheck-error nil
    :extend t
    :background "#510907" 
    :underline nil)

  (with-eval-after-load 'corfu
    (rhjr-set-face 'corfu-default 'rhjr-face-default)
    (rhjr-set-face 'corfu-current 'rhjr-face-accent))

  (with-eval-after-load 'corfu-candidate-overlay
    (set-face-attribute 'corfu-candidate-overlay-face nil
      :foreground (face-foreground 'rhjr-face-mute))
    (set-face-attribute 'corfu-candidate-overlay-face-exact-match nil
      :foreground (face-foreground 'rhjr-face-mute)
      :underline nil)))

(defun rhjr-theme-modeline ()
  (set-face-attribute 'header-line nil
    :background (face-background 'default)

    :overline nil
    :underline nil
    :box nil
    :box `(:line-width 10
            :color ,(face-background 'default)
            :style nil)
    :inherit nil)
  (set-face-attribute 'mode-line nil
    :background (face-background 'default)
    :foreground (face-foreground 'rhjr-face-main)

    :overline nil
    :underline nil
    :box nil
    :box `(:line-width 10
            :color ,(face-background 'default)
            :style nil)
    :inherit nil)
  (set-face-attribute 'mode-line-inactive nil
    :background (face-background 'default)
    :foreground (face-foreground 'rhjr-face-main)

    :overline nil
    :underline nil
    :box nil
    :box `(:line-width 10
            :color ,(face-background 'default)
            :style nil)
    :inherit nil))

(defun rhjr-theme ()
  "The core of the rhuibertsjr theme."
  (rhjr-theme-basics)
  (rhjr-theme-modeline)
  (rhjr-theme-navigation)
  (rhjr-theme-fontlock)
  (rhjr-theme-languages))

(defun rhjr/refresh-theme ()
  ""
  (interactive)
  (progn
    (rhjr-faces)
    (rhjr-theme)))

(defun rhjr/toggle-theme ()
  ""
  (interactive)
  (cond ((string= rhjr-theme-variant "light")
          (rhjr-set-dark-theme))
    ((string= rhjr-theme-variant "dark")
      (rhjr-set-light-theme))
    (t nil))
  (rhjr/refresh-theme))

;; package
(provide 'rhjr-theme)

;;
;;; rhjr-theme.el ends here.
