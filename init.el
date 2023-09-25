;;; init.el --- rhjr's emacs configurations          -*- lexical-binding: t; -*-

;;packages
(package-initialize)
(setq package-user-dir "~/.emacs.d/elpa/"
  package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'no-littering)

;;important!
(global-set-key (kbd "C-x C-g") 'bookmark-jump)

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

  ;;corfu
  completion-cycle-threshold 3
  tab-always-indent 'complete
  dabbrev-case-fold-search nil
  dabbrev-case-replace nil

  ;;annoyances
  ring-bell-function 'ignore
  bookmark-set-fringe-mark nil
  esup-depth 0

  ;; fill
  display-fill-column-indicator-column 80
  display-fill-column-indicator-character '24
  visual-fill-column-width 80 
  visual-fill-column-enable-sensible-window-split t
  fill-column 80)

;;appearance
(add-to-list 'default-frame-alist '(internal-border-width . 24)) 

(setq-default header-line-format
  '(;;mode
     (:propertize "%m" face rhjr-face-doc)
     (:propertize " - " face rhjr-face-border)

                                        ;directory path
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
     (:propertize "Col: %C" face default)

     ;;etc
     (:propertize "%-" face rhjr-face-border)))

(setq-default mode-line-format
  '(;;mode
     (:propertize "%-" face rhjr-face-border)))

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

(defun rhjr/compilation-buffer-bottom ()
  "Compile window always at the bottom."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
                (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h 15)))))))

(defun rhjr/programmable-enviroment-mode ()
  (progn
    ;;(hl-line-mode)
	  (indentinator-mode)
	  (show-paren-mode 1)
	  (visual-line-mode 1)
	  (display-fill-column-indicator-mode 1)))

;;rhjr/overlays
(defun rhjr/comment-dividers ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "//-.*" nil t)
      (let* ((start (match-beginning 0))
             (end (match-end 0)))
        (remove-overlays start end)
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'evaporate t)
          ;;(overlay-put overlay 'face 'rhjr-face-mute)
          (overlay-put overlay 'after-string
            (concat " " (propertize
                          (make-string (- 79 (current-column)) ?-)
                          'face 'rhjr-face-mute))))))
    (forward-line)))

;;inspired by 'flycheck-inline-mode' by @fmdkdd.
(defvar-local rhjr/error-overlays nil
  "(rhjr) Currently active error overlay.")

(defun rhjr/contains-error (overlay &optional pt)
  (let* ((pos (or pt (point)))
          (err (overlay-get overlay 'error))
          (region (flycheck-error-region-for-mode err 'symbols)))
    (and overlay 
      (overlay-get overlay 'rhjr)
      err
      (memq err flycheck-current-errors)
      region
      (>= pos (car region))
      (<= pos (cdr region)))))

(defun rhjr/remove-overlay ()
  (setq rhjr/error-overlays 
    (seq-remove #'rhjr/delete-overlay rhjr/error-overlays)))

(defun rhjr/check-overlay (err)
  (seq-find (lambda (p) (eq err (overlay-get p 'error)))
    rhjr/error-overlays))

(defun rhjr/add-error-overlay (msg &optional pos err)
  (unless (rhjr/check-overlay err)
    (push (rhjr/create-overlay msg pos err) rhjr/error-overlays)))

(defun rhjr/delete-overlay (overlay)
  (if (rhjr/contains-error overlay)
    nil
    (progn (delete-overlay overlay) t)))

(defun rhjr/create-overlay (msg &optional pos err)
  (pcase-let*
    ((overlay (make-overlay
                (line-beginning-position) (+ (line-end-position) 1))))
    (overlay-put overlay 'face 'rhjr-face-flycheck-error)
    (overlay-put overlay 'priority 10)
    (overlay-put overlay 'extend t)
    (overlay-put overlay 'rhjr t)
    (overlay-put overlay 'error err)
    overlay))

(defun rhjr/display-flycheck-error (error)
  (let* ((pos (flycheck-error-pos error))
          (msg (propertize (flycheck-error-message error))))
    (rhjr/add-error-overlay msg pos error)))

(defun rhjr/display-flycheck-errors (errors)
  (rhjr/remove-overlay)
  (mapc #'rhjr/display-flycheck-error
    (seq-uniq (seq-mapcat #'flycheck-related-errors errors))))

;;language
(defun rhjr/indentation ()
  `(;;custom rules
    ;;base rules
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'c))))   

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq
    c-ts-mode-indent-offset 2
    c-ts-mode-indent-style #'rhjr/indentation
    treesit-language-source-alist
    '((c   "https://github.com/tree-sitter/tree-sitter-c")
       (cpp "https://github.com/tree-sitter/tree-sitter-cpp"))
    font-lock-maximum-decoration t))

;;rhjr/c-mode
(defvar rhjr/c-ts-mode-font-lock-settings 
  (treesit-font-lock-rules
    :language 'c :feature 'rhjr-ts-preprocess
    :override t
    '(["#if" "#ifdef" "#ifndef" "#else" "#elif" "#endif" "#elifdef" "#elifndef"
        "#include" "#define" (preproc_directive)] @rhjr-ts-preprocess

       (preproc_def name: (identifier) @rhjr-ts-preprocess-id)

       (preproc_function_def name: (identifier) @rhjr-ts-preprocess-func)

       (preproc_include path: (system_lib_string)
         @rhjr-ts-preprocess-include-system)

       (preproc_include path: (string_literal)
         @rhjr-ts-preprocess-include-literal)
       )

    :language 'c :feature 'rhjr-ts-keywords
    :override t
    '(["default" "enum" "struct" "typedef" "union" "goto" "asm" "__asm__"
        (primitive_type) (type_identifier) (type_descriptor) ]
       @rhjr-ts-keywords

       ["while" "for" "do" "continue" "break" "if" "else" "case" "switch"
        "return"] @rhjr-ts-statement
       )

    :language 'c :feature 'rhjr-ts-punctuation
    :override t
    '([ ";" ":" "," "::" "..." "(" ")" "[" "]" "{" "}" ] @rhjr-ts-punctuation)

    :language 'c :feature 'rhjr-ts-literals
    :override t
    '((string_literal) @font-lock-string-face
       (number_literal) @font-lock-number-face
       (null) @font-lock-constant-face
       )

    )
  )

(define-derived-mode rhjr/c-mode c-mode "rhjrc"
  (cond
    ((treesit-ready-p 'c)
      (treesit-parser-create 'c)
      (setq-local treesit-font-lock-settings rhjr/c-ts-mode-font-lock-settings)
      (setq-local treesit-font-lock-feature-list
        '((rhjr-ts-preprocess rhjr-ts-punctuation rhjr-ts-keywords
            rhjr-ts-literals)
           () ()))
      (treesit-major-mode-setup))
    (t)))

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

(setq-default
  indent-tabs-mode nil
  tab-width 2
  c-default-style "rhjr/gnuish-c-style"
  lisp-indent-offset 2)

;;files
(use-package dired-x
  :ensure nil
  :config
  (setq-default
    dired-free-space nil
    default-directory "c:\\Users\\Rhjr"
    dired-omit-files
    (rx (or
          (seq bol "."    eol)
          (seq bol ".git" eol)
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
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.3)
  :config
  (global-corfu-mode)
  (corfu-history-mode))

(use-package corfu-candidate-overlay
  :ensure t
  :after corfu
  :config
  (corfu-candidate-overlay-mode +1))

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
  (vertico-buffer-mode)
  (setq
    vertico-cycle t
    vertico-count 10))

(use-package marginalia
  :bind (:map minibuffer-local-map
	        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

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

(use-package flycheck
  :ensure t
  :config
  (setq
    flycheck-highlighting-mode 'lines
    flycheck-check-syntax-automatically '(save)
    flycheck-indication-mode nil
    flycheck-display-errors-function #'rhjr/display-flycheck-errors)
  :init
  (global-flycheck-mode))

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

(add-to-list 'load-path "~\\.emacs.d\\thirdparty")
(require 'indentinator)

;;rhjr/writing
(setq
  Tex-master t
  TeX-PDF-mode t
  TeX-auto-save 1
  TeX-parse-self t
  TeX-source-correlate-start-server t)

(setq-default
  TeX-view-program-selection '((output-pdf "PDF Tools")))

;;do not forget to actually install 'auctex' you dummy

(use-package pdf-tools
  :ensure t)

;;rhjr/theme
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

(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'consult-ripgrep)

(global-unset-key (kbd "C-x b"))
(global-set-key (kbd "C-x b") 'consult-buffer)

(global-unset-key (kbd "C-x 4 g"))
(global-set-key (kbd "C-x 4 g") 'bookmark-jump-other-window)


(global-set-key (kbd "C-u") 'evil-scroll-up)
(global-set-key (kbd "C-d") 'evil-scroll-down)

;;rhjr/mode
(tool-bar-mode   0)
(menu-bar-mode   0)
(scroll-bar-mode 0)

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

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

(add-hook 'c-ts-mode-hook
  (lambda ()
	  (rhjr/comment-dividers)
	  (add-hook 'before-save-hook 'rhjr/comment-dividers nil 'local)))

(add-hook 'minibuffer-setup-hook
  (lambda ()
	  (evil-local-mode -1)
	  (setq truncate-lines t)))

(add-hook 'pdf-view-mode-hook
  (lambda ()
    (setq
      pdf-view-display-size 'fit-page)))

(add-hook 'dired-mode-hook       #'dired-omit-mode)

(add-hook 'compilation-mode-hook #'rhjr/compilation-buffer-bottom)

(add-hook 'post-command-hook     #'rhjr/remove-overlay)

(add-hook 'prog-mode-hook        #'rhjr/programmable-enviroment-mode)
(add-hook 'text-mode-hook        #'rhjr/programmable-enviroment-mode)

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
     '(auctex flycheck-inline flymake-easy marginalia aggressive-indent esup magit evil corfu-candidate-overlay vertico orderless consult visual-fill-column use-package tempel pdf-tools org-roam org-cliplink hungry-delete hl-todo goto-chg flycheck exec-path-from-shell corfu cape)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
