
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
  org-agenda-files '("~/Documentations/agenda")

  org-agenda-span 10
  org-agenda-start-on-weekday nil
  org-agenda-start-day "-2d")

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
  '((sequence "TODO"       "WAITING" "|" "CANCELLED" "DONE")
    (sequence "ASSIGNMENT" "WAITING" "|" "CANCELLED" "DONE")
    (sequence "MEETING"              "|" "CANCELLED" "SKIPPED" "ATTENDED")
))

;;
;;; Org capture  
;;
(setq
  org-capture-templates
  ;; Overall
  '(("t" "Task"
      entry (file+headline "~/Documentations/agenda/agenda.org" "Task")
      "*** TODO %? ")
    ("m" "Meeting"
      entry (file+headline "~/Documentations/agenda/agenda.org" "Meeting")
      "*** MEETING %?\n")
  ;; Study
    ("a" "Assignments"
      entry (file+headline "~/Documentations/agenda/study.org" "Assignments")
      "*** ASSIGNMENT %?\nDEADLINE: ")
    ("c" "Course: Appl. Artificial Intelligence"
      entry (file+headline "~/Documentations/agenda/study.org" "Course")
      "*** Appl. Artificial Intelligence %?\n")

))
