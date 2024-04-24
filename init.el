;;; init.el --- rhjr's emacs configurations          -*- lexical-binding: t; -*-

;;packages
(package-initialize)
(setq package-user-dir "~/.emacs.d/elpa/"
  package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'no-littering)

;;important!
(global-set-key (kbd "C-x C-g") 'bookmark-jump)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

  ;;gdb-debugger
  gdb-many-windows t

  ;;text
  truncate-lines t
  fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist)

  ;;prompts
  use-dialog-box nil

  ;;quality-of-life
  select-enable-clipboard t

  scroll-margin             3
  scroll-conservatively     101
  scroll-up-aggressively    0.01
  scroll-down-aggressively  0.01
  scroll-preserve-screen-position t
  auto-window-vscroll       nil

  ;; tramp
  tramp-default-method "plink"

  ;;compilation
  compile-command "build"
  compilation-scroll-output t

  ;;corfu
  completion-cycle-threshold 3
  tab-always-indent t
  dabbrev-case-fold-search nil
  dabbrev-case-replace nil

  ;;annoyances
  ring-bell-function 'ignore
  bookmark-set-fringe-mark nil
  esup-depth 0

  ;; fill
  display-fill-column-indicator-column 80
  display-fill-column-indicator-character '24
  fill-column 80)

;;tags
(setq tags-table-list (list (expand-file-name "~/devel/gazoo-testing/TAGS")))

;;appearance
(add-to-list 'default-frame-alist '(internal-border-width . 24)) 

(setq-default header-line-format
  '(;;mode
     (:propertize "%m" face rhjr-face-doc)
     (:propertize " - " face rhjr-face-border)

     ;;directory path
     (:eval
       (if (eq major-mode 'dired-mode)
         (if (string-match-p "\\`\\*.*\\*\\'" (buffer-name))
           ;; Buffer is a special buffer
           ""
           ;; Buffer is not a special buffer, display directory
           (concat " " (shorten-directory default-directory 20)))))

     ;;buffer
     (:propertize "%b" face rhjr-face-main)

     ;;position
     (:propertize " - " face rhjr-face-border)
     (:propertize "Row: %l" face default)
     (:propertize " - " face rhjr-face-border)
     (:propertize "Col: %C " face default)

     ;;etc
     (:propertize "%-" face rhjr-face-border)))

(setq-default mode-line-format
  '(;;mode
     (:propertize "%-" face rhjr-face-border)))

;;rhjr/functions
(defvar rhjr/previous-buffer nil
  "Variable to store the previous buffer.")

(defun rhjr/is-compilation-buffer ()
  "Check if the current buffer is the compilation buffer."
  (string= (buffer-name) "*compilation*"))

(defun rhjr/change-compilation-buffer-size (delta)
  (let ((current-height (window-total-height)))
    (if (and (> (+ current-height delta) 0) (< (+ current-height delta) 20))
      (enlarge-window delta))))

(defun rhjr/compilation-buffer-peek ()
  "Check if the current buffer is the compilation buffer after a buffer switch."
  (let ((current-buffer-name (buffer-name)))
    (if (and rhjr/previous-buffer (string= rhjr/previous-buffer "*compilation*")
          (not (rhjr/is-compilation-buffer)))
      (enlarge-window 10)
      (if (rhjr/is-compilation-buffer)
        (rhjr/change-compilation-buffer-size 10)))
    (setq rhjr/previous-buffer current-buffer-name)))

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

(setq display-buffer-alist
  '(("\\*compilation\\*" . ((display-buffer-reuse-window
                             display-buffer-in-side-window)
                             (reusable-frames . visible)
                             (side . bottom)
                             (window-height . 0.1)))))

(defun rhjr/build-executable ()
  (interactive)
  (let ((root (project-root (project-current))))
    (if root
        (progn
          (compile (concat root "build.sh"))
          (rhjr/resize-compilation-buffer))
      (message "(rhjr) Currently not in a project."))))

(defun rhjr/run-executable ()
  (interactive)
  (let ((root (project-root (project-current))))
    (if root
        (progn
          (compile (concat root "start.sh"))
          (rhjr/resize-compilation-buffer))
      (message "(rhjr) Currently not in a project."))))

(defun rhjr/resize-compilation-buffer ()
  "Resize the compilation buffer."
  (with-current-buffer (get-buffer "*compilation*")
    (when (derived-mode-p 'compilation-mode)
      (enlarge-window 10))))

(defun rhjr/programmable-enviroment-mode ()
  (interactive)
  (progn
    (hl-line-mode)
    (hl-todo-mode)
    (indentinator-mode)
    (show-paren-mode 1)
    ;;(visual-line-mode 1)
    (display-fill-column-indicator-mode 1)))

;;rhjr/overlays
(defun rhjr/comment-dividers ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (delete-overlay overlay))
    (while (re-search-forward "//-.*" nil t)
      (let* ((start (match-beginning 0))
              (end (match-end 0)))
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'after-string
            (concat " " (propertize
                          (make-string (- 79 (current-column)) ?-)
                          'face 'rhjr-face-mute))))))
    (goto-char (point-min))
    (while (re-search-forward "//=.*" nil t)
      (let* ((start (match-beginning 0))
              (end (match-end 0)))
          (let ((overlay (make-overlay start end)))
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'after-string
              (concat " " (propertize
                            (make-string (- 79 (current-column)) ?=)
                            'face 'rhjr-face-mute))))))))

;;language
(defun rhjr/indentation ()
  `( ;; custom rules
     ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
     ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)  
     ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
     ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)

     ;; bsd rules
     ,@(alist-get 'bsd (c-ts-mode--indent-styles 'c))
     ))       

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq
    c-ts-mode-indent-offset 4
    c-ts-mode-indent-style #'rhjr/indentation
    treesit-language-source-alist
    '((c   "https://github.com/tree-sitter/tree-sitter-c")
       (cpp "https://github.com/tree-sitter/tree-sitter-cpp"))
    font-lock-maximum-decoration t))

(defconst rhjr/gnuish-c-style
  '((c-basic-offset . 4)
     (c-indent-level . 4)

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

(setq-default
  indent-tabs-mode nil
  tab-width 4
  c-default-style "rhjr/gnuish-c-style"
  lisp-indent-offset 2)

;;rhjr/projects
(require 'project)
(defun rhjr/project-switch-on-gazoo ()
  "This function will be called after visiting the `gazoo` bookmark."
  (when (string-equal (bookmark-name) "gazoo")
    (project-switch-project "~/devel/gazoo-testing/")))

(advice-add 'bookmark-bmenu-this-window :after #'rhjr/project-switch-on-gazoo)

;;files
(use-package dired-x
  :ensure nil
  :config
  (setq-default
    dired-free-space nil
    default-directory "~/"
    dired-omit-files
    (rx (or
          (seq bol "."    eol)
          (seq bol ".git" eol)
          (seq bol ".dir-locals.el" eol)
          (seq bol "auto" eol)
          (seq bol "TAGS" eol)
          (seq bol "documentation-paper.log" eol)
          (seq bol "documentation-paper.out" eol)
          (seq bol "documentation-paper.aux" eol)
          (seq bol "rhjr-portfolio.log" eol)
          (seq bol "rhjr-portfolio.out" eol)
          (seq bol "rhjr-portfolio.aux" eol)
          (seq bol "rhjr-thesis.log" eol)
          (seq bol "rhjr-thesis.out" eol)
          (seq bol "rhjr-thesis.aux" eol)
          (seq bol "research-paper.log" eol)
          (seq bol "research-paper.log" eol)
          (seq bol "research-paper.aux" eol)
          (seq bol "research-paper.toc" eol)
          (seq bol "research-paper.out" eol)
          (seq bol "desktop.ini" eol)))
    dired-use-ls-dired t
    insert-directory-program "/usr/bin/ls"
    dired-recursive-copies 'always
    dired-recursive-deletes 'always
    dired-listing-switches "-laGh1v --group-directories-first"))

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
  :ensure t
  :hook
  ((prog-mode . corfu-mode)
    (org-mode  . corfu-mode))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  :config
  (global-corfu-mode)
  (corfu-history-mode))

(use-package tempel
  :after corfu
  :ensure t
  :bind (("M-=" . tempel-complete)
          ("M-*" . tempel-insert))
  :config
  (setq tempel-path
	"~\\.emacs.d\\templates\\template")
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
	  (cons #'tempel-expand
		completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook  'tempel-setup-capf))

(use-package cape
  :ensure t
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
  (setq
    vertico-cycle t
    vertico-count 10))

(use-package orderless
  :ensure t
  :init
  (setq
    completion-styles '(orderless basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :ensure t)

;;rhjr/misc 
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

(add-to-list 'load-path "~/.emacs.d/thirdparty")
(require 'indentinator)

(use-package highlight-parentheses
  :ensure t
  :custom
  (highlight-parentheses-colors
    '("#8ffff2" "#8ffff2" "#8ffff2" "#8ffff2" "#8ffff2")))

;;rhjr/writing
(use-package org
  :ensure t
  :hook
  (( org-mode . org-indent-mode )
    ( org-mode . olivetti-mode ))
  :config
  (setq
    org-hide-emphasis-markers t))

(setq
  Tex-master nil
  TeX-PDF-mode t
  TeX-auto-save 1
  TeX-parse-self t
  TeX-source-correlate-start-server t)

(setq-default
  TeX-view-program-selection '((output-pdf "PDF Tools")))

;;do not forget to actually install 'auctex' you dummy

(use-package pdf-tools
  :ensure t)

;;rhjr/plots
(use-package gnuplot
  :ensure t
  :mode ("\\.gp\\'" . gnuplot-mode))

;;rhjr/theme
(add-to-list 'load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes/themes")

(require 'rhjr-faces)
(require 'rhjr-theme)
(require 'rhjr-light-theme)
(require 'rhjr-dark-theme)

(rhjr-faces)
(rhjr-theme)
(rhjr-set-dark-theme)
(rhjr/refresh-theme)

;;rhjr/keybindings
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

;;movement
(global-unset-key (kbd "C-d"))
(global-set-key   (kbd "C-d") 'evil-scroll-down)

(global-unset-key (kbd "C-u"))
(global-set-key   (kbd "C-u") 'evil-scroll-up)

;;consult
(global-unset-key (kbd "C-s"))
(global-set-key   (kbd "C-s") 'consult-ripgrep)

(global-unset-key (kbd "C-x b"))
(global-set-key   (kbd "C-x b") 'consult-buffer)

(global-unset-key (kbd "C-x n"))
(global-set-key   (kbd "C-x n") 'consult-imenu-multi)

(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                  evil-insert-state-map
                  evil-emacs-state-map))
    (define-key (eval map) "\C-z" nil)
    (define-key (eval map) "\C-f" nil)))
(global-set-key (kbd "C-f") 'consult-find)

(global-unset-key (kbd "C-c m"))
(global-set-key   (kbd "C-c m") 'consult-imenu-multi)

(global-unset-key (kbd "C-x 4 g"))
(global-set-key (kbd "C-x 4 g") 'bookmark-jump-other-window)

(global-set-key (kbd "<f1>") 'rhjr/build-executable)
(global-set-key (kbd "<f2>") 'rhjr/run-executable)

;;rhjr/mode
(tool-bar-mode   0)
(menu-bar-mode   0)
(scroll-bar-mode 0)
(pixel-scroll-precision-mode)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'"
  :hook
  ((csv-mode . csv-align-mode)))

;;rhjr/hooks
(add-hook 'emacs-startup-hook
  (lambda ()
	(rhjr/profile-startup)
	(setq gc-cons-threshold (expt 2 23))))

;;replace c-mode with c-ts-mode
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
  '(c-or-c++-mode . c-or-c++-ts-mode))

(add-hook 'c-ts-mode-hook 'rhjr/comment-dividers)
(add-hook 'c++-ts-mode-hook 'rhjr/comment-dividers)
(add-hook 'after-save-hook 'rhjr/comment-dividers)

(add-hook 'buffer-list-update-hook #'rhjr/compilation-buffer-peek)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-hook 'yaml-ts-mode #'rhjr/programmable-enviroment-mode)

(add-hook 'minibuffer-setup-hook
  (lambda ()
    (if (fboundp 'evil-local-mode)
	  (evil-local-mode -1))
	(setq truncate-lines t)))

(add-hook 'pdf-view-mode-hook
  (lambda ()
    (setq
      pdf-view-display-size 'fit-page)))

(add-hook 'org-mode-hook
  (lambda ()
    (visual-line-mode)))

(add-hook 'dired-mode-hook       #'dired-omit-mode)

(add-hook 'prog-mode-hook        #'rhjr/programmable-enviroment-mode)
(add-hook 'TeX-mode-hook         #'rhjr/programmable-enviroment-mode)

(add-hook 'prog-mode-hook        #'highlight-parentheses-mode)

(add-hook 'TeX-after-compilation-finished-functions
  #'TeX-revert-document-buffer)

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode)) 
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)) 

;;rhjr/fix
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
     '(csv-mode visual-fill olivetti gnuplot auctex flycheck-inline flymake-easy aggressive-indent esup magit evil corfu-candidate-overlay vertico orderless consult use-package tempel pdf-tools org-roam org-cliplink hungry-delete hl-todo goto-chg flycheck exec-path-from-shell corfu cape))
  '(safe-local-variable-values 'nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
