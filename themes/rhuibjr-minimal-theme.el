;;; rhuibjr-minimal-theme.el --- rhuibjr's theme     -*- lexical-binding: t; -*-
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
;;; Commentary:
;;
;; A minimalistic emacs theme, based on one of the best themes ever
;; made: morethz/gruvbox. You want more colours then check out
;; rhuibjr/gruverboxer.
;;
;;; Code:
;;

(deftheme rhuibjr-minimal
  "A minimalistic emacs theme, based on morhetz/gruvbox.")

(defvar rhuibjr/default-colours-alist
  '(("foreground"  . "#fbf1c7")
    ("background"  . "#101012")
    ("modeline"    . "#1c1c1e")
    ("dark-grey"   . "#282828")
    ("grey"        . "#928374")
    ("yellow"      . "#fabd2f")
    ("orange"      . "#fe8019")
    ("red"         . "#fb4933")
    ("green"       . "#b8bb26")
   ))

(defmacro rhuibjr/assign-colour-variables (&rest body)
  "`let' bind all colors defined in `github-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
          ,@(mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
              rhuibjr/default-colours-alist))
     ,@body))


(rhuibjr/assign-colour-variables
  (custom-theme-set-faces
    'rhuibjr-minimal

;; 
    `(cursor
       ((t (:foreground ,yellow :background ,yellow))))
    `(region
       ((t (:foreground ,foreground :background ,dark-grey))))
    `(button
       ((t (:foreground ,yellow))))
    `(highlight
       ((t (:background ,background :foreground ,foreground))))
    `(lazy-highlight
       ((t (:background ,yellow :foreground ,foreground))))
    `(minibuffer-prompt
       ((t (:foreground ,yellow))))
  
;; Frame
    `(default
       ((t (:foreground ,foreground :background ,background))))
    `(window-divider
       ((t (:foreground ,background))))
    `(window-divider-first-pixel
       ((t (:foreground ,background))))
    `(window-divider-last-pixel
       ((t (:foreground ,background))))
    `(fringe
       ((t (:background ,background))))
    `(fill-column-indicator
       ((t (:background ,modeline))))

;; Modeline
    `(mode-line
       ((t (:foreground ,yellow :background ,modeline))))
    `(mode-line-inactive
       ((t (:background ,modeline))))

;; line numbers
    `(line-number
       ((t (:foreground ,grey :background ,background))))
    `(line-number-current-line
       ((t (:foreground ,orange :background ,background))))

;; Searching
    `(isearch
       ((t (:foreground ,foreground :background ,orange))))
    `(isearch-fail
       ((t (:background ,red))))
    `(match
       ((t (:background ,yellow))))

;; Font-lock
    `(font-lock-comment-face
       ((t (:foreground ,grey))))
    `(font-lock-type-face
       ((t (:foreground ,yellow))))
    `(font-lock-keyword-face
       ((t (:foreground ,yellow))))
    `(font-lock-function-name-face
       ((t (:foreground ,foreground))))
    `(font-lock-variable-name-face
       ((t (:foreground ,foreground))))
    `(font-lock-preprocessor-face
       ((t (:foreground ,orange))))
    `(font-lock-string-face
       ((t (:foreground ,green))))
    `(font-lock-constant-face
       ((t (:foreground ,foreground))))
    `(font-lock-builtin-face
       ((t (:foreground ,orange))))
    `(font-lock-warning-face
       ((t (:foreground ,orange))))

;;; Compilation
    `(compilation-info
       ((t (:foreground ,yellow))))
    `(compilation-info
       ((t (:foreground ,orange))))
    `(compilation-error
       ((t (:foreground ,red))))

    ;; Makefile
    `(makefile-targets
       ((t (:foreground ,yellow))))

;;; LaTeX

    `(font-latex-sedate-face
       ((t (:foreground ,orange))))
    `(font-latex-string-face
       ((t (:foreground ,green))))
    `(font-latex-bold-face
       ((t (:foreground ,green))))
    `(font-latex-italic-face
       ((t (:foreground ,green))))
    `(font-latex-warning-face
       ((t (:foreground ,orange))))

;;; Packages

    ;; IDO
    `(ido-only-match
       ((t (:foreground ,orange ))))
    `(ido-first-match
       ((t (:foreground ,orange))))
    `(ido-subdir
       ((t (:foreground ,foreground))))
    
     ;; Dired   
    `(dired-header
       ((t (:foreground ,orange))))
    `(dired-directory
       ((t (:foreground ,yellow))))

    ;; Show-paren mode
    `(show-paren-match
       ((t (:foreground ,orange))))
    `(show-paren-mismatch
       ((t (:foreground ,red))))

    ;; Org mode
    `(org-level-1
       ((t (:foreground ,foreground))))
    `(org-level-2
       ((t (:foreground ,foreground))))
    `(org-level-3
       ((t (:foreground ,foreground))))
    `(org-level-4
       ((t (:foreground ,foreground))))
    `(org-date
       ((t (:foreground ,grey))))
    `(org-table
       ((t (:foreground ,grey))))
    `(org-link
       ((t (:foreground ,foreground))))
    `(org-todo
       ((t (:foreground ,red))))
    `(org-drawer
       ((t (:foreground ,yellow))))
    `(org-document-title
       ((t (:foreground ,grey))))
    `(org-special-keyword
       ((t (:foreground ,orange))))
    `(org-document-info-keyword
       ((t (:foreground ,grey))))

    `(org-agenda-structure
       ((t (:foreground ,yellow))))
    `(org-agenda-date-today
       ((t (:foreground ,orange :italic t))))
    `(org-agenda-date
       ((t (:foreground ,foreground))))
    `(org-agenda-calendar-event
       ((t (:foreground ,grey))))

    ;; Eglot / flymake
    `(flymake-note
       ((t (:underline nil))))
    `(flymake-warning
       ((t (:background ,background :underline nil))))
    `(flymake-error
       ((t (:background ,background :underline nil))))

  )) 

;;
;;; Footer:
;;

;;;###autoload
(when load-file-name
  (add-to-list
    'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))
    
(provide-theme 'rhuibjr-minimal)

;;
;;; rhuibjr-minimal-theme.el ends here
