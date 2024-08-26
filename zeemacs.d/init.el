;;; init.el --- Zak's customs emacs config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Zak Soliman
;;
;; Author: Zak Soliman <zakaria.soliman1@gmail.com>
;; Maintainer: Zak Soliman <zakaria.soliman1@gmail.com>
;; Created: August 23, 2024
;; Modified: August 23, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/zakaria/init
;; Package-Requires: ((emacs "29"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  My Awesome Minimal Emacs config
;;
;;
;;; Code:

(when (version< emacs-version "29")
  (error "This requires Emacs 29 and above!"))

;; VARIABLES
(defvar zeds/library-path "~/Documents/Library of Alexandria/"
  "Directory where my documents collection lives.")

(defvar zeds/notes-path "~/Documents/Library of Alexandria/notes/"
  "General Notes.")

(defvar zeds/notes-path (concat zeds/notes-path "org-roam/zettels/")
  "Org-Roam Zettlekasten")

(defvar zeds/journal-path (concat zeds/notes-path "org-roam/daily/")
  "Journal entries.")

(defvar zeds/org-path (concat zeds/notes-path "org")
  "Org path.")
;; END VARIABLES

;; FUNCTION DEFINITIONS
(defun zeds/window-maximize-buffer (&optional arg)
  "Close other windows to focus on this one.
Use `winner-undo' to undo this. Alternatively, use `doom/window-enlargen'."
  (interactive "P")
  (when (and (bound-and-true-p +popup-mode)
             (+popup-window-p))
    (+popup/raise (selected-window)))
  (delete-other-windows))

(defun zeds/window-maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-up)) (delete-window))
    (while (ignore-errors (windmove-down)) (delete-window))))

(defun zeds/window-maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-left)) (delete-window))
    (while (ignore-errors (windmove-right)) (delete-window))))
;; END FUNCTION DEFINITIONS


;; Package Manager - ELPACA SETUP
;;  When installing a package used in the init file itself,
;;  e.g. a package which adds a use-package key word,
;;  use the :wait recipe keyword to block until that package is installed/configured.
;;   Expands to: (elpaca evil (use-package evil :demand t))
;;   (use-package evil :ensure t :demand t)
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;;(setq custom-file (location-user-emacs-file "custom-vars.el"))
;;(load custom-file 'noerror 'nomessage)

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
;; (use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;;END ELPACA SETUP


;; CHANGE BAD EMACS DEFAULTS
(use-package emacs
  :demand t
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t)
  (setq backup-by-copying t)
  (setq sentence-end-double-space nil)
  (setq frame-inhibit-implied-resize t) ;; useless for a tiling window manager
  (setq global-auto-revert-non-file-buffers t) ;; Revert Dired and other buffers:
  (setq show-trailing-whitespace t)            ;; self-explanatory
  (setq user-full-name "Zak Soliman")          ;; my details
  (setq user-mail-address "zakaria.soliman1@gmail.com")
  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short
  (setq indent-tabs-mode nil)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  ;; (setq indent-line-function 'insert-tab)
  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

  (setq create-lockfiles nil) ;; no need to create lockfiles

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  (set-charset-priority 'unicode) ;; utf8 everywhere
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything

  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t) ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)    ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)

  (show-paren-mode t)

  (global-display-line-numbers-mode 1)
  (global-visual-line-mode t)
  (setq display-line-numbers-type 'relative)

  ;; Persist history over Emacs restarts.
  (savehist-mode 1)
  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;;FONTS
  ;; This sets the default font on all graphical frames created after restarting Emacs.
  ;; (add-to-list 'default-frame-alist '(font . "Fira Code-12"))

  ;; Makes commented text and keywords italics.
  ;; This is working in emacsclient but not emacs.
  ;; Your font must have an italic face available.
  ;; (set-face-attribute 'font-lock-comment-face nil
  ;;                     :slant 'italic)
  ;; (set-face-attribute 'font-lock-keyword-face nil
  ;;                     :slant 'italic)
  ;; Uncomment the following line if line spacing needs adjusting.
  ;; (setq-default line-spacing 0.12)

  )
;; END EMACS DEFAULTS

;; THEME
;; https://protesilaos.com/emacs/standard-themes
(use-package ef-themes
  :ensure t
  :demand t
  :config
  (load-theme 'ef-winter)
  )

;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; ICONS
(use-package all-the-icons
  :ensure t
  :demand t)


;; prettify dired with icons
(use-package all-the-icons-dired
  :ensure t
  :demand t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :demand t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;;(use-package solaire-mode
;;  :config
;;  (solaire-global-mode 1))
;; END UI/DX

;; EVIL
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t) ;; For evil-collection
  (setq evil-want-keybinding nil) ;; For evil-collection
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-respect-visual-line-mode t) ;; respect visual lines
  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'
  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour
  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  (evil-mode)
  ;; set the initial state for some kinds of buffers.
  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal)
  ;; (evil-set-initial-state 'magit-diff-mode 'insert)
  )

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  (setq evil-collection-mode-list nil) ;; I don't like surprises
  (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

;; https://github.com/linktohack/evil-commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1)) ;; globally enable evil-commentary

;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :ensure t
  :after evil
  :hook ((org-mode . (lambda () (push '(?~ . ("~" . "~")) evil-surround-pairs-alist)))
         (org-mode . (lambda () (push '(?$ . ("\\(" . "\\)")) evil-surround-pairs-alist)))
         (LaTeX-mode . (lambda () (push '(?$ . ("\\(" . "\\)"))))))

  :config
  (global-evil-surround-mode 1)) ;; globally enable evil-surround


;; https://github.com/edkolev/evil-goggles
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; END EVIL

;; KEY BINDINGS - GENERAL.EL
(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer zeds/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"           ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; set up ',' as the local leader key
  (general-create-definer zeds/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","           ;; set local leader
    :global-prefix "M-,") ;; access local leader in insert mode

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state) ;; don't stretch for ESC

  ;; unbind some annoying default bindings
  (general-unbind
    "C-x C-r"    ;; unbind find file read only
    "C-x C-z"    ;; unbind suspend frame
    "C-x C-d"    ;; unbind list directory
    "<mouse-2>") ;; pasting with mouse wheel click

  ;; (zeds/leader-keys
  ;;   "z" (:ignore t :wk "Zeeds Emacs")
  ;;   "zr" ( :wk "Reload emacs configs"))

  (zeds/leader-keys
    "SPC" '(execute-extended-command :wk "execute command") ;; an alternative to 'M-x'
    "TAB" '(:keymap tab-prefix-map :wk "tab")) ;; remap tab bindings

  (zeds/leader-keys
    "w" '(:keymap evil-window-map :wk "window")
    "wmm" '(zeds/window-maximize-buffer :wk "maximize")
    "wms" '(zeds/window-maximize-horizontally :wk "maximize horizontally")
    "wmv" '(zeds/window-maximize-vertically :wk "maximize vertically")) ;; window bindings

  ;; help
  ;; namespace mostly used by 'helpful'
  (zeds/leader-keys
    "h" '(:ignore t :wk "help"))

  ;; file
  (zeds/leader-keys
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find file") ;; gets overridden by consult
    "fs" '(save-buffer :wk "save file"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (zeds/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer")
    "bp" '(previous-buffer :wk "previous buffer")
    "bn" '(next-buffer :wk "next buffer"))

  ;; universal argument
  (zeds/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; ;; notes
  ;; ;; see 'citar' and 'org-roam'
  ;; (zeds/leader-keys
  ;;     "n" '(:ignore t :wk "notes")
  ;;     ;; see org-roam and citar sections
  ;;     "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; ;; code
  ;; see 'flymake'
  ;; (zeds/leader-keys
  ;;     "c" '(:ignore t :wk "code"))

  ;; open
  (zeds/leader-keys
    "o" '(:ignore t :wk "open")
    "os" '(speedbar t :wk "speedbar")
    "op" '(elpaca-log t :wk "elpaca"))


  ;; ;; search
  ;; ;; see 'consult'
  ;; (zeds/leader-keys
  ;;     "s" '(:ignore t :wk "search"))

  ;; ;; templating
  ;; ;; see 'tempel'
  ;; (zeds/leader-keys
  ;;     "t" '(:ignore t :wk "template")
  )

;; END KEY BINDINGS - GENERAL.EL


;; WHICH-KEY
(use-package which-key
  :ensure t
  :after evil
  :demand t
  :init
  (setq which-key-idle-delay 0.3)
  (which-key-mode)
  :config
  (which-key-setup-minibuffer))
;; END WHICH-KEY


;; AUTOCOMPLETION - VERTICO
;; Enable vertico
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :demand t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :demand t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; END VERTICO

;; CONSULT
(use-package consult
  :ensure t
  :demand t
  :general
  (zeds/leader-keys
   "bb" '(consult-buffer :wk "consult buffer")
   "Bb" '(consult-bookmark :wk "consult bookmark")
   "ht" '(consult-theme :wk "consult theme")
   "sr" '(consult-ripgrep :wk "consult rg")
   "sg" '(consult-grep :wk "consult grep")
   "sG" '(consult-git-grep :wk "consult git grep")
   "sf" '(consult-find :wk "consult find")
   "sF" '(consult-locate :wk "consult locate")
   "sl" '(consult-line :wk "consult line")
   "sy" '(consult-yank-from-kill-ring :wk "consult yank from kill ring")
   "i" '(consult-imenu :wk "consult imenu"))
  :config
  ;; use project.el to retrieve the project root
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format))
;; END CONSULT

(use-package electric
  :demand t
  :ensure nil
  :init
  (electric-pair-mode 1) ;; automatically insert closing parens
  (electric-indent-mode 1)
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package ediff
  :demand t
  :ensure nil
  )

;; HELP
(use-package helpful
  :ensure t
  :demand t
  :general
  (zeds/leader-keys
   "hc" '(helpful-command :wk "helpful command")
   "hf" '(helpful-callable :wk "helpful callable")
   "hh" '(helpful-at-point :wk "helpful at point")
   "hF" '(helpful-function :wk "helpful function")
   "hv" '(helpful-variable :wk "helpful variable")
   "hk" '(helpful-key :wk "helpful key")))
;; END HELP

;; EGLOT LSP
(use-package eglot
  :ensure nil
  :init (setq completion-category-overrides '((eglot (styles orderless))))
  :commands eglot
  :hook ((rust-mode
	      python-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
)
;; (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server" "--lsp"))))
;; END EGLOT

;; PYTHON
(defvar zeds/pyenv--version nil)

(defun zeds/python-pyenv-mode-set-auto-h ()
  "Set pyenv-mode version from buffer-local variable."
  (when (eq major-mode 'python-mode)
    (when (not (local-variable-p 'zeds/pyenv--version))
      (make-local-variable 'zeds/pyenv--version)
      (setq zeds/pyenv--version (zeds/python-pyenv-read-version-from-file)))
    (if zeds/pyenv--version
        (pyenv-mode-set zeds/pyenv--version)
      (pyenv-mode-unset))))

(defun zeds/python-pyenv-read-version-from-file ()
  "Read pyenv version from .python-version file."
  (when-let (root-path (projectile-locate-dominating-file default-directory ".python-version"))
    (let* ((file-path (expand-file-name ".python-version" root-path))
           (version
            (with-temp-buffer
              (insert-file-contents-literally file-path)
              (string-trim (buffer-string)))))
      (if (member version (pyenv-mode-versions))
          version  ;; return.
        (message "pyenv: version `%s' is not installed (set by `%s')."
                 version file-path)))))

(use-package python
  :ensure nil
  :hook (python-mode . (lambda ()
		    (setq-default indent-tabs-mode t)
		    (setq-default tab-width 4)
		    (setq-default py-indent-tabs-mode t)))
  )
(use-package pyenv-mode
  :after python
  :ensure t
  :config
  (when (executable-find "pyenv")
    (pyenv-mode +1)
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
  (add-hook 'python-mode-local-vars-hook #'zeds/python-pyenv-mode-set-auto-h)
  (add-hook 'doom-switch-buffer-hook #'zeds/python-pyenv-mode-set-auto-h))

;; RUST
(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :config (setq rustic-lsp-client 'eglot))

;; WEB
(use-package js2-mode
  :ensure t)


(use-package project
  :ensure nil
  :general
  ;; assign built-in project.el bindings a new prefix
  (zeds/leader-keys "p" '(:keymap project-prefix-map :wk "project")))

(use-package dired
  :ensure nil
  :general
  (zeds/leader-keys
   "dd" '(dired :wk "dired") ;; open dired (in a directory)
   "dj" '(dired-jump :wk "dired jump")) ;; open direct in the current directory
  ;; ranger like navigation
  (:keymaps 'dired-mode-map
   :states 'normal
   "h" 'dired-up-directory
   "q" 'kill-current-buffer
   "l" 'dired-find-file)
  :hook
  (dired-mode . dired-hide-details-mode))

;; toggle subtree visibility with 'TAB'
;; makes dired a much more pleasant file manager
(use-package dired-subtree
  :ensure t
  :demand t)

(use-package lispy
  :ensure t
  :general
  (:keymaps 'lispy-mode-map
            "TAB" 'indent-for-tab-command) ;; necessary for 'corfu'
  :hook
  (reb-lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (racket-mode . lispy-mode)
  (fennel-mode . lispy-mode))

(use-package lispyville
  :ensure t
  :hook (lispy-mode . lispyville-mode)
  :general
  (:keymaps 'lispyville-mode-map
            "TAB" 'indent-for-tab-command) ;; necessary for 'corfu'
  ;; the following is necessary to retain tab completion in lispy mode
  :config
  ;; TODO play around with keythemes
  (lispyville-set-key-theme '(operators c-w additional)))


;; MARKDOWN
(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . olivetti-mode))
  (markdown-mode . variable-pitch-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-header-scaling t))
:
;; Better Completion
(use-package corfu
  :ensure t
  :demand t
  :hook
  (eval-expression-minibuffer-setup . corfu-mode)
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)  ;; allows cycling through candidates
  (corfu-auto nil) ;; disables auto-completion
  :bind
  :general
  (:keymaps 'corfu-map
            "SPC" 'corfu-insert-separator)) ;; for compatibility with orderless

(general-unbind
  :ensure t
  :states '(insert)
  "C-k" ;; this was interfering with corfu completion
  :states '(normal)
  "C-;")

(use-package magit
  :ensure t
  :general
  (zeds/leader-keys
   "g" '(:ignore t :wk "git")
   "gg" '(magit-status :wk "status")))

(provide 'init)
;;; init.el ends here
