(defconst rhjr/base-config-dir
  (if (eq system-type 'windows-nt)
      "C:\\Users\\Rhjr\\AppData\\Roaming\\.emacs.d\\"
      "~/.config/emacs/"))

(defconst rhjr/paths/config rhjr/base-config-dir)
(defconst rhjr/paths/packages (concat rhjr/paths/config "packages/"))
(defconst rhjr/paths/thirdparty (concat rhjr/paths/config "thirdparty/"))

(defconst rhjr/paths/no-littering/etc (concat rhjr/paths/config "config/"))
(defconst rhjr/paths/no-littering/var (concat rhjr/paths/config "data/"))

(defconst rhjr/paths/eln-cache (concat rhjr/paths/config "data/eln-cache"))

(defconst rhjr/paths/home
  (if (eq system-type 'windows-nt)
      "C:\\Users\\Rhjr\\Home\\"
      "~"))


(setq tramp-ssh-program "C:\\Windows\\System32\\openSSH\\ssh.exe")
(setq tramp-default-method "ssh"
      tramp-use-ssh-controlmaster-options nil)

(when (eq system-type 'windows-nt)
  (require 'cl-lib)
  (with-eval-after-load 'tramp
    (cl-pushnew '("-tt")
                (car (alist-get 'tramp-login-args
                                (cdr (assoc "ssh" tramp-methods))))
                :test #'equal)))

;; rhjr/archives
(require 'package)
(setq package-user-dir rhjr/paths/packages
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
  (eval-and-compile
    (setq use-package-always-ensure t
          use-package-expand-minimally t))

(require 'use-package)

;;; rhjr/config-folder
(setq user-emacs-directory rhjr/paths/config)

(use-package compat
  :ensure t) ; no-littering dependency
(add-to-list'load-path rhjr/paths/thirdparty)
(eval-and-compile
  (setq no-littering-etc-directory rhjr/paths/no-littering/etc)
  (setq no-littering-var-directory rhjr/paths/no-littering/var)
  (require 'no-littering))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
    (convert-standard-filename rhjr/paths/eln-cache)))

;;; rhjr/defaults
(setq-default
  initial-major-mode 'text-mode
  initial-scratch-message ""

  use-dialog-box nil

  inhibit-startup-message ""
  inhibit-splash-screen t
  inhibit-startup-echo-area-message t

  display-line-numbers-width 4

  ring-bell-function 'ignore

  tab-width 2
  indent-tabs-mode nil

  create-lockfiles nil
  make-backup-files nil
  auto-save-default nil)

;;; rhjr/standard-packages
(use-package dired-x
  :ensure nil
  :config
  (setq-default
   dired-free-space nil
   default-directory rhjr/paths/home
   dired-omit-files
   (rx (or
        (seq bol "."    eol)
        (seq bol ".git" eol)
        (seq bol ".dir-locals.el" eol)))
   dired-use-ls-dired t
   insert-directory-program "/usr/bin/ls"
   dired-recursive-copies 'always
   dired-recursive-deletes 'always
   dired-listing-switches "-laGh1v --group-directories-first"))

;;; rhjr/win32
(when (eq system-type 'windows-nt)
  (setq select-enable-clipboard t
        grep-program "C:\\ProgramData\\chocolatey\\bin\\grep.exe"
        find-program "C:\\Windows\\System32\\find.exe"))

;;; rhjr/functions
;; private functions
(defun display-startup-echo-area-message ()
  (message ""))

;; public functions

;;; rhjr/mode
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode '(0 . 0))
(pixel-scroll-precision-mode)
(global-display-line-numbers-mode 1)

;;; rhjr/packages
(use-package magit
  :defer t)

(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)

  :config
  (keymap-unset corfu-map "RET")

  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; M-s bindings in `search-map'
         ("C-x f" . consult-find)
         ("C-x s" . consult-ripgrep))

  :config
  (setq consult-find-args
      "find . -not -path \"*/.git/*\"")

  :init
  ;; use consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(defun rhjr/load-all-project-files ()
  "Open all files in the current project."
  (interactive)
  (let ((files (project-files (project-current t))))
    (dolist (file files)
      (find-file-noselect file))))

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package savehist ;; store history when emacs closes.
  :init
  (savehist-mode))

(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  )

(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :custom
  (highlight-parentheses-colors
   '("#8ffff2" "#8ffff2" "#8ffff2" "#8ffff2" "#8ffff2")))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg)
  ;; use completion-read instead of a separate buffer with candidates
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

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

(add-hook 'c-mode-hook 'rhjr/comment-dividers)
(add-hook 'after-save-hook 'rhjr/comment-dividers)

(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;;; rhjr/keybindings
(global-set-key (kbd "C-x C-g") 'bookmark-jump)

;;; rhjr/programming
(defconst rhjr/c-style
  `((c-basic-offset  . 2)
    (c-hanging-braces-alist . ((defun-open)))
    (c-offsets-alist .((statement . 0)
		       (statement-cont . 2)
		       (substatement-open . 0)
		       (arglist-intro . 2)
		       (case-label . 2)
		       (label . 0)

		       ;; functions
		       (func-decl-cont . 0)))))

(defun rhjr/set-c-style ()
  ""
  (interactive)
  (setq tab-width 2
	indent-tabs-mode nil)

  (c-add-style "rhjr/c-style" rhjr/c-style t)
  (c-set-style "rhjr/c-style"))

(add-hook 'c-mode-common-hook #'rhjr/set-c-style)

(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;rhjr/comp
(setq compilation-directory-locked nil)

(defvar project-command-arg nil
  "Stored argument used by project commands.")

(defun set-project-command-arg (arg)
  "Set and remember ARG for project commands."
  (interactive "sSet project argument: ")
  (setq project-command-arg arg)
  (message "Project argument set to: %s" project-command-arg))

(defun find-project-directory-recursive (command)
  "Recursively search upward until COMMAND exists."
  (if (file-exists-p command)
      t
    (let ((parent (file-name-directory (directory-file-name default-directory))))
      (unless (or (null parent) (string= parent default-directory))
        (cd parent)
        (find-project-directory-recursive command)))))

(defun lock-compilation-directory ()
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory (command)
  (let ((start-dir default-directory))
    (switch-to-buffer-other-window "*compilation*")
    (if compilation-directory-locked
        (cd last-compilation-directory)
      (cd start-dir)
      (find-project-directory-recursive command)
      (setq last-compilation-directory default-directory))))

(defun run-project-command (command)
  "Run COMMAND from the project root using stored argument."
  (interactive)
  (find-project-directory command)
  (compile
   (if project-command-arg
       (concat command " " project-command-arg)
     command))
  (other-window 1))

(global-set-key [f1] (lambda () (interactive)
                        (run-project-command "build.bat")))

(global-set-key [f2] (lambda () (interactive)
                        (run-project-command "start.bat")))

(global-set-key [f3] #'set-project-command-arg)

;;rhjr/fix
(setq minibuffer-prompt-properties ;; cursor in minibuffer-prompt
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq indent-line-function (quote insert-tab))))

;;;rhjr/styling
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 150
                    :weight 'normal)

(add-to-list 'exec-path "C:\\Program Files\\Git\\usr\\bin")
(setenv "PATH"
        (concat "C:\\Program Files\\Git\\usr\\bin;"
                (getenv "PATH")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dumb-jump cape consult corfu corfu-candidate-overlay gruber-darker-theme highlight-parentheses magit orderless vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
