;;; rhjr-theme.el --- Rhuibertsjr emacs theme.       -*- lexical-binding: t; -*-
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

(defcustom rhjr-theme-variant nil
  "" :group 'rhjr :type 'string)

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

  (rhjr-set-face 'region                'rhjr-face-fill)
  (rhjr-set-face 'highlight             'rhjr-face-fill)
  (rhjr-set-face 'fill-column-indicator 'rhjr-face-highlight)

  (rhjr-set-face 'error                 'rhjr-face-error)
  (rhjr-set-face 'success               'rhjr-face-succeed)

  ;; frame
  (set-face-attribute 'fringe nil
    :foreground (face-background 'rhjr-face-default)
    :background (face-background 'default))

  (set-face-attribute 'cursor nil
    :background (face-foreground 'rhjr-face-default))

  (set-face-attribute 'window-divider nil
    :foreground (face-background 'rhjr-face-default))

  (set-face-attribute 'window-divider-first-pixel nil
    :foreground rhjr-colour-background)

  (set-face-attribute 'window-divider-last-pixel nil
    :foreground rhjr-colour-background)

  ;; Emacs GUI
  (rhjr-set-face 'button                'rhjr-face-highlight)
  (rhjr-set-face 'minibuffer-prompt     'rhjr-face-highlight))

(defun rhjr-theme-navigation ()
  "(rhjr) Initialize faces of the navigation category, like: dired, ido, etc."

  ;; Ido
  (with-eval-after-load 'ido
    (rhjr-set-face 'ido-only-match            'rhjr-face-strong)
    (rhjr-set-face 'ido-first-match           'rhjr-face-strong)
    (rhjr-set-face 'ido-subdir                'rhjr-face-highlight))

  ;; Dired
  (rhjr-set-face 'dired-header                'rhjr-face-strong)
  (rhjr-set-face 'dired-directory             'rhjr-face-highlight)

  ;; Line numbering
  (with-eval-after-load 'display-line-numbers
    (rhjr-set-face 'line-number               'rhjr-face-mute)
    (rhjr-set-face 'line-number-current-line  'rhjr-face-strong))

  ;; Parentheses
  (rhjr-set-face 'show-paren-match               'rhjr-face-strong)
  (rhjr-set-face 'show-paren-mismatch            'rhjr-face-error)

  ;; Searching
  (rhjr-set-face 'isearch                        'rhjr-face-fill)
  (rhjr-set-face 'isearch-fail                   'rhjr-face-error)
  (rhjr-set-face 'match                          'rhjr-face-succeed)
  (rhjr-set-face 'lazy-highlight                 'rhjr-face-fill))

(defun rhjr-theme-org ()
  "(RHJR) Org-mode settings"
  (with-eval-after-load 'org
    (rhjr-set-face 'org-document-title        'rhjr-face-document-title)
    (rhjr-set-face 'org-document-info         'rhjr-face-mute)
    (rhjr-set-face 'org-meta-line             'rhjr-face-document-info)

    (rhjr-set-face 'org-level-1               'rhjr-face-document-section)
    (rhjr-set-face 'org-level-2               'rhjr-face-document-section)
    (rhjr-set-face 'org-level-3               'rhjr-face-document-section)

    ;; Sections 
    (face-remap-add-relative 'org-level-1
      ;:overline nano-color-subtle
      :family "Roboto" :height 180)

    (face-remap-add-relative 'org-level-2
      ;:overline nano-color-subtle
      :family "Roboto" :height 160)

    (face-remap-add-relative 'org-level-3
      ;:overline nano-color-subtle
      :family "Roboto" :height 150)

    ;; Status
    (rhjr-set-face 'org-headline-done         'rhjr-face-default)
    (rhjr-set-face 'org-done                  'rhjr-face-succeed)
    (rhjr-set-face 'org-headline-todo         'rhjr-face-default)
    (rhjr-set-face 'org-todo                  'rhjr-face-error)

    (rhjr-set-face 'org-priority              'rhjr-face-error)

    ;; Text
    (rhjr-set-face 'org-link                  'rhjr-face-highlight)
    (rhjr-set-face 'org-verbatim              'rhjr-face-strong)

    ;; Date
    (rhjr-set-face 'org-date                  'rhjr-face-mute)

    ;; Agenda
    (rhjr-set-face 'org-agenda-structure      'rhjr-face-strong)
    (rhjr-set-face 'org-agenda-date           'rhjr-face-highlight)

    (set-face-attribute 'org-agenda-date-weekend nil
      :inherit 'rhjr-face-highlight :bold t)

  ))

(defun rhjr-theme-fontlock ()
  (rhjr-set-face 'font-lock-comment-face       'rhjr-face-mute)
  (rhjr-set-face 'font-lock-doc-face           'rhjr-face-const)
  (rhjr-set-face 'font-lock-string-face        'rhjr-face-const)
  (rhjr-set-face 'font-lock-constant-face      'rhjr-face-strong)
  (rhjr-set-face 'font-lock-warning-face       'rhjr-face-default)
  (rhjr-set-face 'font-lock-function-name-face 'rhjr-face-default)
  (rhjr-set-face 'font-lock-variable-name-face 'rhjr-face-default)
  (rhjr-set-face 'font-lock-builtin-face       'rhjr-face-strong)
  (rhjr-set-face 'font-lock-type-face          'rhjr-face-highlight)
  (rhjr-set-face 'font-lock-keyword-face       'rhjr-face-highlight))

(defun rhjr-theme-languages ()
  (with-eval-after-load 'makefile
    (rhjr-set-face 'makefile-targets           'rhjr-face-highlight)))

(defun rhjr-theme-modeline ()
  (set-face-attribute 'mode-line nil
    :foreground  rhjr-colour-highlight
    :background  rhjr-colour-background
    :box         `(:line-width 4
                   :color      ,(face-background 'rhjr-face-default)
                   :style      nil)
    :overline    `(:line-width 4
                   :color      ,(face-background 'rhjr-face-default)
                   :style      nil)
    :underline   nil)
  (set-face-attribute 'mode-line-inactive nil
    :foreground  rhjr-colour-highlight
    :background  rhjr-colour-background
    :box         `(:line-width 4
                   :color      ,(face-background 'rhjr-face-default)
                   :style      nil)
    :overline    nil
    :underline   nil))

(defun rhjr-theme ()
  "The core of the rhuibertsjr theme."
  (rhjr-theme-basics)
  (rhjr-theme-modeline)
  (rhjr-theme-navigation)
  (rhjr-theme-fontlock)
  (rhjr-theme-languages)
  (rhjr-theme-org))

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
