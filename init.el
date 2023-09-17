;;; init.el --- rhjr's emacs configurations          -*- lexical-binding: t; -*-

;;packages
(package-initialize)
(setq package-user-dir "~/.emacs.d/elpa/"
  package-archives '(("melpa" . "https://melpa.org/packages/")
                     ("gnu" . "http://elpa.gnu.org/packages/")))
;; defaults
(setq-default
  ;;frame
  frame-title-format "%b"

  ;;initial
  initial-major-mode 'lisp-mode
  initial-scratch-message ""
  inhibit-startup-message ""

  ;;file-generation
  create-lockfiles nil
  make-backup-files nil
  auto-save-default nil

  ;;prompts
  use-dialog-box nil

  ;;quality-of-life
  select-enable-clipboard t
  grep-program "C:\\ProgramData\\chocolatey\\bin\\grep.exe"

  scroll-margin             3
  scroll-conservatively     101
  scroll-up-aggressively    0.01
  scroll-down-aggressively  0.01
  scroll-preserve-screen-position t
  auto-window-vscroll       nil

  ;;compilation
  compile-command "build"
  compilation-scroll-output t

  ;;annoyances
  ring-bell-function 'ignore
  bookmark-set-fringe-mark nil

  ;; fill
  display-fill-column-indicator-column 80
  display-fill-column-indicator-character '24
  visual-fill-column-width 80 
  fill-column 80)

;;appearance
(add-to-list 'default-frame-alist '(internal-border-width . 24)) 

(setq-default header-line-format
  '(;;mode
     (:propertize "%m" face rhjr-face-const)
     (:propertize " - " face default)

     ;; Directory
     (:propertize (:eval (shorten-directory default-directory 20))
       face rhjr-face-modeline-folder)
     (:propertize "%b"
       face rhjr-face-modeline-file)

     ;; Cursor position
     (:propertize "::%l " face default)

     (:propertize "%-" face rhjr-face-new)
     ))

(setq-default mode-line-format
  '(;;mode
     (:propertize "%-" face rhjr-face-new)
     ))

;(setq-default mode-line-format nil)

;;functions
(defun rhjr/profile-startup ()
  "(rhjr-func) startup profiler."
  (message
    "Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time (time-subtract after-init-time before-init-time)))
    gcs-done))

(defun rhjr/close-compilation-buffer ()
  "Close the compilation buffer and current split window if they exist."
  (interactive)
  (let ((compilation-buffer (get-buffer "*compilation*")))
    (when compilation-buffer
      (delete-window (get-buffer-window compilation-buffer))
      (kill-buffer compilation-buffer))))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "./" output)))
    output))

;;language
(defconst rhjr/gnuish-c-style
  '((c-basic-offset . 2)
    (c-indent-level . 2)

     (c-offsets-alist .
       ((statement-cont . +)
         (substatement . +)
         (substatement-open . 0)
         (brace-list-open . 0)

         ;;functions 
         (defun-open             . 0)
         (defun-block-intro      . +)
         (arglist-intro          . +)
         (arglist-close          . 0)

         ;;switch-case
         (case-label             . +)

      )))
  "rhjr/gnuish-c-style")
(c-add-style "rhjr/gnuish-c-style" rhjr/gnuish-c-style)

(require 'esup)

(setq-default
  indent-tabs-mode nil
  tab-width 2
  c-default-style "rhjr/gnuish-c-style"
  lisp-indent-offset 2)

(use-package dired-x
  :ensure nil)

(setq-default
  default-directory "~"
  ;; dired
  dired-omit-files
  (rx (or
        (seq bol "."    eol)
        (seq bol ".git" eol)
        (seq bol "desktop.ini" eol)))
  dired-use-ls-dired t
  insert-directory-program "/usr/bin/ls"
  dired-recursive-copies 'always
  dired-recursive-deletes 'always
  dired-listing-switches "-laGh1v --group-directories-first")

;;evil
(use-package evil
  :ensure t
  :init
  (setq
     evil-want-integration t
     evil-want-keybinding nil
     evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

;;completion
;; @Minad you beautiful man.
(use-package corfu
  :bind
  (:map corfu-map
    ("TAB"     . corfu-next)
    ([tab]     . corfu-next)
    ("S-TAB"   . corfu-previous)
    ([backtab] . corfu-previous))
  :hook
  ((prog-mode . corfu-mode)
   (org-mode  . corfu-mode))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  :init
  (global-corfu-mode))

(use-package tempel
  :bind
  (("M-=" . tempel-complete))
  :config
  (setq
    tempel-path "~/.emacs.d/templates/template")
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook  'tempel-setup-capf))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions 'cape-abbrev)
  (add-to-list 'completion-at-point-functions 'cape-dabbrev)
  (add-to-list 'completion-at-point-functions 'cape-file))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :init
  (vertico-mode)
  (vertico-buffer-mode)
  (setq
    vertico-cycle t
    vertico-count 10))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq
    flycheck-highlighting-mode 'lines
    flycheck-indication-mode nil))

;;misc 
(use-package org-cliplink
  :ensure t)

(use-package hungry-delete
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :init
  (setq
    hl-todo-highlight-punctuation ":"
    hl-todo-keyword-faces
    `(("rhjr"  font-lock-builtin-face   bold))))

;;keybindings
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x e"))
(global-unset-key (kbd "C-x C-q"))
(global-unset-key (kbd "M-="))
(global-unset-key (kbd "M-["))
(global-unset-key (kbd "M-]"))

(global-set-key (kbd "C-x C-r") 'recompile)
(global-set-key (kbd "C-x C-q") 'rhjr/close-compilation-buffer)

(global-set-key (kbd "M-[")     'tempel-previous)
(global-set-key (kbd "M-]")     'tempel-next)

(global-set-key (kbd "C-x C-g") 'bookmark-jump)

(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'consult-ripgrep)

(global-unset-key (kbd "C-x b"))
(global-set-key (kbd "C-x b") 'consult-buffer)

;;modes
(tool-bar-mode   0)
(menu-bar-mode   0)
(scroll-bar-mode 0)

;;theme
(add-to-list 'load-path "~\\.emacs.d\\themes")
(add-to-list 'load-path "~\\.emacs.d\\themes\\themes")

(require 'rhjr-faces)
(require 'rhjr-theme)
(require 'rhjr-light-theme)
(require 'rhjr-dark-theme)

(rhjr-faces)
(rhjr-theme)
(rhjr-set-dark-theme)
(rhjr/refresh-theme)

;;hooks
(add-hook 'emacs-startup-hook
  (lambda ()
    (rhjr/profile-startup)
    (setq gc-cons-threshold (expt 2 23))))

(add-hook 'prog-mode-hook
  (lambda ()
    (electric-pair-mode 1)
    (show-paren-mode 1)
    (visual-line-mode 1)
    (flycheck-mode 1)
    (display-fill-column-indicator-mode 1)))

(add-hook 'dired-mode-hook
  'dired-omit-mode)

(add-hook 'minibuffer-setup-hook
  (lambda ()
    (evil-local-mode -1)
    (setq truncate-lines t)))

;;fixes
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode)) ;; fix lisp mode on .el

(setq minibuffer-prompt-properties ;; cursor in minibuffer-prompt
  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; init.el ends here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
    '(vertico orderless consult visual-fill-column use-package tempel pdf-tools org-roam org-cliplink magit hungry-delete hl-todo goto-chg flycheck exec-path-from-shell evil-collection esup corfu cape)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
