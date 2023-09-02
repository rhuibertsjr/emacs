;;; agenda.el --- rhuibjr's agenda configurations    -*- lexical-binding: t; -*-
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
;;; Org
;;
(setq
  org-image-actual-width         (list 300)
  org-agenda-span                10
  org-agenda-start-on-weekday    nil
  org-agenda-start-day           "d"
  org-agenda-use-time-grid       nil
  org-capture-bookmark           nil ; Thank god 
  org-bookmark-names-plist       nil
  org-hide-block-startup         t
  org-startup-with-inline-images t
  org-agenda-prefix-format
    '((agenda . "%i %-12:c%?-12t% s")
      (todo . " %i %-12:c")
      (tags . " %i %-12:c")
      (search . " %i %-12:c"))

  org-refile-targets
    '(("W:\\agenda\\archive.org" :maxlevel . 1))

  org-blank-before-new-entry '((heading . always) (plain-list-item . auto))
  org-hidden-keywords '(title author date startup)

  line-spacing                 1
  org-startup-folded         nil
  org-level-color-stars-only nil
  org-hide-leading-stars     nil
)

(org-indent-mode)

;; Numbering
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;;
;;; Org bindings
;;
(global-set-key (kbd "C-c a")   #'org-agenda)
(global-set-key (kbd "C-c c")   #'org-capture)

;;
;;; Org TODO states
;;
(setq
  org-todo-keywords
  '((sequence "TODO"       "WAITING"  "|" "CANCELLED" "DONE")
    (sequence "ASSIGNMENT" "WAITING"  "|" "CANCELLED" "DONE")
    (sequence "MEETING"               "|" "CANCELLED" "SKIPPED" "ATTENDED")
    (sequence "DELIVERY"   "EXTENDED"
                           "RETURN"  "|" "CANCELLED" "DELIVERED")
    (sequence "UNASSIGNED" "|")
))

;;
;;; Org tags
;;
(setq org-agenda-hide-tags-regexp (regexp-opt '("Schedule")))

;;
;;; Org capture  
;;
(setq org-capture-templates
  '(
     ("t" "Task"
       entry (file+headline
               "w:\\agenda\\agenda.org" "Tasks")
       "\n** TODO %? ")
     ("m" "Meeting"
       entry (file+headline
               "w:\\agenda\\agenda.org" "Meetings")
       "\n** MEETING %? ")
     ("u" "Unassigned"
       entry (file+headline
               "w:\\agenda\\agenda.org" "Unassigned")
       "\n** UNASSIGNED %? ")
   )
)

;;
;;; Org note-taking
;;
(setq
  org-roam-v2-ack                t
  org-roam-completion-everywhere t)

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory
    (file-truename "w:\\entries"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (setq org-roam-completion-system 'ido)
  (org-roam-db-autosync-mode))

(defun rhuibjr/org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if
   it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:PROPERTIES:\n\\( *:.+?:.*\n\\)+ *:END:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun rhuibjr/org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

;;
;;; Org journaling
;;   -> https://systemcrafters.net/build-a-second-brain-in-emacs/keep-a-journal/
;(setq
;  org-roam-dailies-directory "journal/")


;;
;;; Others
;; 
(require 'holidays)

(defvar holiday-netherlands-holidays
  '((holiday-fixed 1 1     "Nieuwjaarsdag")
    (holiday-easter-etc -2 "Goede Vrijdag")
    (holiday-easter-etc 0  "Eerste paasdag")
    (holiday-easter-etc 1  "Tweede paasdag")
    (holiday-fixed 4 27    "Koningsdag")
    (holiday-fixed 5 5     "Bevrijdingsdag")
    (holiday-easter-etc 39 "Hemelvaartsdag")
    (holiday-easter-etc 49 "Eerste pinksterdag")
    (holiday-easter-etc 50 "Tweede pinksterdag")
    (holiday-fixed 12 5    "Sinterklaasavond")
    (holiday-fixed 12 25   "Eerste kerstdag")
    (holiday-fixed 12 26   "Tweede kerstdag"))
  "Netherlands holidays.")

(setq calendar-holidays
  (append holiday-netherlands-holidays))

;;
;;; org-mode.el ends here
