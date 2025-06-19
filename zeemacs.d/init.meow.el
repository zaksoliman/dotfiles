;;; init.el --- Zak's customs emacs config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Zak Soliman
;;
;; Author: Zak Soliman <zakaria.soliman1@gmail.com>
;; Maintainer: Zak Soliman <zakaria.soliman1@gmail.com>
;; Created: August 23, 2024
;; Modified: August 23, 2024
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  My Awesome Minimal Emacs config
;;
;;; Code:

;;; PRELUDE
(when (version< emacs-version "29")
  (error "This requires Emacs 29 and above!"))

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))
  )
(setopt package-install-upgrade-built-in t)
;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)
;; Show help buffer after startup
(add-hook 'after-init-hook 'help-quick)

;;; VARIABLES
(defvar zeds/library-path "~/Documents/Library of Alexandria/"
  "Directory where my documents collection lives.")
(defvar zeds/notes-path "~/Documents/Library of Alexandria/notes/"
  "General Notes.")
(defvar zeds/roam-notes-path (concat zeds/notes-path "org-roam/zettels/")
  "Org-Roam Zettlekasten")
(defvar zeds/roam-dailies-path (concat zeds/notes-path "org-roam/dailies/")
  "Journal entries.")
(defvar zeds/org-path (concat zeds/notes-path "org/")
  "Org path.")

;;; FUNCTION DEFINITIONS
(defun zeds/eglot-python-workspace-config (server)
  ;; Default values in accordance with
  ;; https://github.com/python-lsp/python-lsp-server/blob/v1.10.0/CONFIGURATION.md
  ;; (or commit 2a5a953).  A value of null means we do not set a value and
  ;; therefore use the plugin's default value.

  ;; The recommended format for the `eglot-workspace-configuration' variable
  ;; value is a property list (plist for short):
  ;;
  ;; (:server plist…)
  ;;
  ;; Here :server identifies a particular language server and plist is the
  ;; corresponding keyword-value property list of one or more parameter
  ;; settings for that server, serialized by Eglot as a JSON object.
  ;; plist may be arbitrarily complex, generally containing other
  ;; keyword-value property sublists corresponding to JSON subobjects.

  ;; The JSON values are represented by Emacs Lisp values as follows:

  ;; JSON                         | Emacs Lisp
  ;; ------------------------------------------------
  ;; :true i.e. true              | t
  ;; :false i.e. false            | :json-false
  ;; :null i.e. null              | nil
  ;; :[] i.e. [] the empty array  | []*
  ;; :{} i.e. {} the empty object | eglot-{}**

  ;; * Lisp array elements should not be comma separated as they are in a
  ;; JSON array.
  ;; ** Must be evaluated via a backquote or `list'
  ;; e.g. `(:pylsp (:plugins (:jedi (:env_vars ,eglot-{})))) or
  ;;       (list :pylsp (list :plugins (list :jedi (list :env_vars eglot-{}))))
  (let ((venv-path (string-trim (shell-command-to-string "pyenv prefix"))))
    `(:pylsp  
      (:plugins (
                 :pylsp_mypy (
                              :enabled t
                              :ignore_missing_imports :json-false)
                 :jedi (:environment ,venv-path)
                 :jedi_completion (
                                   :cache_for ["pandas" "numpy" "tensorflow" "matplotlib"] ; string array: ["pandas", "numpy", "tensorflow", "matplotlib"] (default)
                                   :eager :json-false ; boolean: true or false (default)
                                   :enabled t ; boolean: true (default) or false
                                   :fuzzy :json-false ; boolean: true or false (default)
                                   :include_class_objects :json-false ; boolean: true or false (default)
                                   :include_function_objects :json-false ; boolean: true or false (default)
                                   :include_params t ; boolean: true (default) or false
                                   :resolve_at_most 25)
                 :jedi_definition
                 (:enabled t ; boolean: true (default) or false
                           :follow_builtin_definitions t ; boolean: true (default) or false
                           :follow_builtin_imports t ; boolean: true (default) or false
                           :follow_imports t) ; boolean: true (default) or false
                 :jedit_hover
                 (:enabled t) ; boolean: true (default) or false
                 :jedi_references
                 (:enabled t) ; boolean: true (default) or false
                 :jedi_signature_help
                 (:enabled t) ; boolean: true (default) or false
                 :jedi_symbols
                 (:all_scopes t ; boolean: true (default) or false
                              :enabled t ; boolean: true (default) or false
                              :include_import_symbols t) ; boolean: true (default) or false
                 :ruff (
                        :enabled t
                        :line_length 88
                        :cache_config t))))))

(defun zeds/open-init-dir ()
  "Opens my emacs config directory."
  (interactive)
  (dired "~/dotfiles/zeemacs.d"))

(defun zeds/open-init-file ()
  "Open emacs init.el file."
  (interactive)
  (find-file "~/dotfiles/zeemacs.d/init.el"))

(defun zeds/reload-init ()
  (interactive)
  (load-file "~/dotfiles/zeemacs.d/init.el"))

(defun zeds/window-maximize-buffer (&optional arg)
  "Close other windows to focus on this one."
  (interactive "P")
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

(defun zeds/macos-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "open %s"
                          (if app-name
                              (format "-a %s '%s'" (shell-quote-argument app-name) path)
                            (format "'%s'" path)))))
    (message "Running: %s" command)
    (shell-command command)))

(defun zeds/setup-prog-modes ()
  (display-line-numbers-mode 1)
  (hs-minor-mode 1)
  (hl-line-mode 1)
  (hs-minor-mode 1))

(defun zeds/setup-text-modes ()
  (display-line-numbers-mode -1)
  (hl-line-mode 1))


;;; CONFIGURE EMACS
(use-package emacs
  :demand t
  :hook
  ((prog-mode . zeds/setup-prog-modes)
   (text-mode . zeds/setup-text-modes)
   (vterm-mode . zeds/setup-text-modes))
  :init
  ;; BASICS
  (setq user-full-name "Zak Soliman"
        user-mail-address "zakaria.soliman1@gmail.com"
        enable-recursive-minibuffers t
        show-trailing-whitespace t
        sentence-end-double-space nil
        frame-inhibit-implied-resize t ;; useless for a tiling window manager
        global-auto-revert-non-file-buffers t ;; Revert Dired and other buffers:
        create-lockfiles nil
        delete-by-moving-to-trash t ;; use trash-cli rather than rm when deleting files.
        mouse-wheel-progressive-speed nil
        scroll-conservatively 101
        display-line-numbers-type 'relative
        ;; Don't persist a custom file
        custom-file (make-temp-file "") ; use a temp file as a placeholder
        custom-safe-themes t ; mark all themes as safe, since we can't persist now
        enable-local-variables :all     ; fix =defvar= warnings
        load-prefer-newer t
        ;; Enable tab completion (see `corfu`)
        tab-always-indent 'complete
        initial-scratch-message nil
        ring-bell-function 'ignore
        ;; Save existing clipboard text into the kill ring before replacing it.
        save-interprogram-paste-before-kill t
        ;; Prompts should go in the minibuffer, not in a GUI.
        use-dialog-box nil
        ;; Fix undo in commands affecting the mark.
        mark-even-if-inactive nil
        ;; Let C-k delete the whole line.
        kill-whole-line t
        ;; search should be case-sensitive by default
        case-fold-search nil
        ;; no need to prompt for the read command _every_ time
        compilation-read-command nil
        ;; scroll to first error
        compilation-scroll-output 'first-error
        ;; accept 'y' or 'n' instead of yes/no
        ;; the documentation advises against setting this variable
        ;; the documentation can get bent imo
        use-short-answers t
        ;; unicode ellipses are better
        truncate-string-ellipsis "…"
        ;; when I say to quit, I mean quit
        confirm-kill-processes nil
        browse-url-firefox-program "/Applications/Firefox Developer Edition.app/Contents/MacOS/firefox"
        browse-url-browser-function #'browse-url-firefox
        treesit-font-lock-level 4
        )

  ;; Modes I want by default
  (column-number-mode 1)
  ;; (global-display-line-numbers-mode 1)
  (global-visual-line-mode 1)
  (show-paren-mode 1)
  ;; Persist history over Emacs restarts.
  (scroll-bar-mode -1)            ;; disables scrollbar
  (pixel-scroll-precision-mode 1) ;; enable smooth scrolling
  (global-auto-revert-mode 1)

  (add-to-list 'auto-mode-alist '("\\.hql\\'" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(rustic-mode . rust-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-mode . c-ts-mode))

  (setq-default fill-column 80)
  ;; TABS
  (setq-default tab-width 2
                indent-tabs-mode nil)
  ;; indent-line-function 'insert-tab
  ;; (setq indent-tabs-mode nil
  ;; 			  ;; Enable indentation+completion using the TAB key.
  ;; 			  ;; `completion-at-point' is often bound to M-TAB.
  ;; 			;; tab-always-indent 'complete
  ;; 			)

  ;; COMPLETION
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; ALIASES
  (defalias 'yes-or-no-p 'y-or-n-p) ;; life is too short

  ;; BACKUP
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil
        backup-by-copying t
        version-control t
        delete-old-versions t
        ;; keep backup and save files in a dedicated directory
        backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))


  ;; TEXT
  (set-charset-priority 'unicode) ;; utf8 everywhere
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8
        default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; GLOBAL KEY BINDINGS
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything


  ;; Hide commands in M-x which don't work in the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package saveplace
  :init (save-place-mode 1))

(use-package savehist
  :init (savehist-mode 1))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "private/tmp")
  (recentf-mode))

;;; STYLE
;; Modeline
(use-package mood-line
  :ensure t
  ;; Enable mood-line
  :config
  (mood-line-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t       ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
                                        ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-city-lights t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode 1))

;;; ICONS
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

;;; WHICH-KEY
(use-package which-key
  :ensure t
  :demand t
  :init
  (setq which-key-idle-delay 0.3)
  (which-key-mode)
  :config
  (which-key-setup-minibuffer))

;;; Meow
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
    :demand t
    :ensure t
    :config
    (meow-setup)
    (meow-global-mode 1))

;;; AVY
(use-package avy
    :demand t
    :ensure t
  :init
  (defun zeds/avy-action-insert-newline (pt)
    (save-excursion
      (goto-char pt)
      (newline))
    (select-window
     (cdr
      (ring-ref avy-ring 0))))
  (defun zeds/avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0))))
  (defun zeds/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t) ;; adds an avy action for embark
  ;; :general
  ;; (general-def '(normal motion)
  ;;   "s" 'evil-avy-goto-char-timer
  ;;   "f" 'evil-avy-goto-char-in-line
  ;;   "gl" 'evil-avy-goto-line ;; this rules
  ;;   ";" 'avy-resume)
 :config
  (setf (alist-get ?. avy-dispatch-alist) 'zeds/avy-action-embark ;; embark integration
        (alist-get ?i avy-dispatch-alist) 'zeds/avy-action-insert-newline
        (alist-get ?K avy-dispatch-alist) 'zeds/avy-action-kill-whole-line)
  ) ;; kill lines with avy

;;; COMPLETION - VERTICO/CONSULT
;; Enable vertico
(use-package vertico
  :demand t
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  ;; :general
  ;; (:keymaps 'vertico-map
  ;;           ;; keybindings to cycle through vertico results.
  ;;           "C-j" 'vertico-next
  ;;           "C-k" 'vertico-previous
  ;;           "C-f" 'vertico-exit
  ;;           "<backspace>" 'vertico-directory-delete-char
  ;;           "C-<backspace>" 'vertico-directory-delete-word
  ;;           "C-w" 'vertico-directory-delete-word
  ;;           "RET" 'vertico-directory-enter)
)

;; (use-package vertico-directory
;;   :after vertico
;;   ;; More convenient directory navigation commands
;;   :bind (:map vertico-map
;;               ("RET" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   ;; Tidy shadowed file names
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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

;;; CONSULT
(use-package consult
  :ensure t
  :demand t
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

;;; HIGHLIGHT TODOs
(use-package hl-todo
  :ensure t
  :demand t
  :init
  (global-hl-todo-mode))

;;; ORG-MODE
(use-package org
  :demand t
  :init
  ;; edit settings
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t ;; special navigation behaviour in headlines
        org-insert-heading-respect-content t)

  ;; styling, hide markup, etc.
  (setq org-hide-emphasis-markers t
        org-src-fontify-natively t ;; fontify source blocks natively
        org-highlight-latex-and-related '(native) ;; fontify latex blocks natively
        ;; org-pretty-entities t
        org-ellipsis "…")

  ;; agenda styling
  (setq org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")

  ;; todo setup
  (setq org-todo-keywords
        ;; it's extremely useful to distinguish between short-term goals and long-term projects
        '((sequence "TODO(t)" "SOMEDAY(s)" "|" "DONE(d)")
          (sequence "TO-READ(r)" "READING(R)" "|" "HAVE-READ(d)")
          (sequence "PROJ(p)" "|" "COMPLETED(c)")))

  (setq org-adapt-indentation nil) ;; interacts poorly with 'evil-open-below'

  :custom
  (org-agenda-files '("~/notes/todo.org" "~/notes/teaching.org" "~/notes/projects.org"))
  ;; (org-cite-global-bibliography (list zeds/global-bib-file))
  ;; handle citations using citar
  ;; (org-cite-insert-processor 'citar)
  ;; (org-cite-follow-processor 'citar)
  ;; (org-cite-activate-processor 'citar)
  :hook
  ;; (org-mode . olivetti-mode)
  ;; (org-mode . variable-pitch-mode)
  (org-mode . (lambda () (electric-indent-local-mode -1)
                (display-line-numbers-mode -1))) ;; disable electric indentation

  :config
  (add-to-list 'org-latex-packages-alist '("" "braket" t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (emacs-lisp . t)
     (awk . t)))
  ;; set up org paths
  (setq org-directory zeds/org-path)
  (setq org-default-notes-file (concat org-directory "/notes.org")))

;;; ORG-ROAM
(use-package org-roam
  :ensure t
  :demand t
  :init
  (setq org-roam-directory zeds/roam-notes-path
        org-roam-dailies-directory zeds/roam-dailies-path)
  :config
  ;; org-roam-buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  (setq org-roam-capture-templates
        '(("m" "Main" plain
           "%?"
           :if-new(file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+filetags: :draft:\n\n\n* Content\n* Related Nodes")
           :unnarrowed t)
          ("w" "Work" plain
           "%?"
           :if-new (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :draft:\n")
           :unnarrowed t)
          ("r" "Reference" plain
           "%?"
           :if-new (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :draft:\n")
           :unnarrowed t)
          ("a" "Article" plain
           "%?"
           :if-new (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :draft:\n")
           :unnarrowed t)
          ("b" "Book Notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :draft:\n")
           :unnarrowed t)
          ("p" "Project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :project:draft:\n")
           :unnarrowed t)))

  ;; get tags to show up in 'org-roam-node-find':
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:60}"
                (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-completion-everywhere t ;; roam completion anywhere
        org-roam-db-location (concat org-roam-directory "/.database/org-roam.db"))

  (unless (< emacs-major-version 29)
    (setq org-roam-database-connector 'sqlite-builtin))
  (org-roam-db-autosync-mode) ;; ensures that org-roam is available on startup


  ;; dailies config
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))


(use-package ediff
  :demand t
  )

;;; HELP (helpful)
(use-package helpful
  :ensure t
  :demand t)

;;; PROGRAMMING STUFF :CODE
;;; EGLOT LSP
(use-package eglot
  :demand t
  :init
  (setq completion-category-overrides '((eglot (styles orderless))))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; (setq eglot-events-buffer-size 0)
  (setq eglot-autoshutdown t)
  (setq eglot-workspace-configuration #'zeds/eglot-python-workspace-config)
  ;;  :commands eglot
  ;;  :bind (:map eglot-mode-map
  ;; ;;             ("<f6>" . eglot-format-buffer))
  )

;;; PYTHON
;; (dir-locals-set-class-variables 'unwritable-directory
;;                                 (zeds/eglot-python-workspace-config))

;; (dir-locals-set-directory-class
;;    "/Users/zakaria/Projects/BIT/" 'unwritable-directory)

(defun zeds/pyenv-mode-versions ()
  "List installed python versions."
  (let ((versions (shell-command-to-string "pyenv versions --bare")))
    (cons "system" (split-string versions))))

(defvar zeds/pyenv--version nil)

(defun zeds/python-pyenv-read-version-from-file ()
  "Read pyenv version from .python-version file."
  (when-let (root-path (locate-dominating-file default-directory ".python-version"))
    (let* ((file-path (expand-file-name ".python-version" root-path))
           (version
            (with-temp-buffer
              (insert-file-contents-literally file-path)
              (string-trim (buffer-string)))))
      (if (member version (zeds/pyenv-mode-versions))
          version ;; return.
        (message "pyenv: version `%s' is not installed (set by `%s')."
                 version file-path)))))

(defun zeds/python-pyenv-mode-set-auto-h ()
  "Set pyenv-mode version from buffer-local variable."
  (when (memq major-mode '(python-ts-mode python-mode))
    (when (not (local-variable-p 'zeds/pyenv--version))
      (make-local-variable 'zeds/pyenv--version)
      (setq-local zeds/pyenv--version (zeds/python-pyenv-read-version-from-file)))
    (if zeds/pyenv--version
        (progn
          (pyenv-mode-set zeds/pyenv--version)
          ;; (setq-default eglot-workspace-configuration (zeds/eglot-python-workspace-config))

          ;; (setq-default eglot-workspace-configuration (zeds/eglot-python-workspace-config))
          ;; (eglot-ensure)
          )
      (pyenv-mode-unset))))

(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode . (lambda ()
                             (setq-local indent-tabs-mode nil)
                             (setq-local python-indent-offset 4)
                             (setq-local py-indent-tabs-mode t)
                             (setq-local tab-width 4)
                             ;; (eglot-ensure)
                             )))
  )

(use-package pyenv-mode
    :ensure t
    :config
    (when (executable-find "pyenv")
      (pyenv-mode +1)
      (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
    :hook (python-ts-mode . (lambda ()
                              (zeds/python-pyenv-mode-set-auto-h)
                              (eglot-ensure))))

;;; RUST
(use-package rust-mode
  :ensure t
  :after (eglot)
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-mode-treesitter-derive t)
  :hook ((rust-mode . (lambda ()
                        (setq indent-tabs-mode nil)
                        (eglot-ensure)
			                  (eglot-inlay-hints-mode -1))))
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))


;;; WEB
(use-package js2-mode
  :ensure t)

;;; LISP
(use-package elisp-mode
  :init
  (setq lisp-indent-function 'common-lisp-indent-function))

;;; TOOLING
;;; VTERM
(use-package vterm
  :ensure t
  :config
  (defun turn-off-chrome ()
    (hl-line-mode nil)
    (display-line-numbers-mode nil))
  :hook (vterm-mode . turn-off-chrome))

(use-package vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'project))

;;; RegEX
(use-package re-builder
  :config (setq reb-re-syntax 'rx))

;;; SEARCHING
;; (use-package deadgrep
;;   :ensure t
;;   :demand t
;;   :general
;;   (zeds/leader-keys
;;    "sd" '(deadgrep :wk "deadgrep")))

;;; HTTP SERVER
(use-package simple-httpd
  :ensure t
  :commands httpd-serve-directory)

;;; ESHELL
(use-package eshell)

;;; COLORS
(use-package rainbow-mode :ensure t)

;;; SNIPPETS
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets")
  (yas-global-mode 1))

;;; CHECKERS
;;; Flymake over flycheck
(use-package flymake
 ;; depends on consult
  :hook
  (TeX-mode . flymake-mode) ;; this is now working
  (emacs-lisp-mode . flymake-mode)
  (prog-mode . flymake-mode)
  ;; :init
  ;; (flymake-no-changes-timeout nil)
 )

(use-package recursion-indicator
  :ensure t
  :demand t
  :config
  (recursion-indicator-mode))

;;; PROJECT.EL
(use-package project)

;;; DIRED
(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode))

;; toggle subtree visibility with 'TAB'
;; makes dired a much more pleasant file manager
(use-package dired-subtree
  :ensure t
  :demand t)

;;; MARKDOWN
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

;;; MAGIT
(use-package magit
  :ensure t
  )

;;; PDF SUPPORT
(use-package pdf-tools
  :ensure t
  :defer t
  :hook (TeX-after-compilation-finished . TeX-revert-document-buffer)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; (require 'pdf-tools)
  (require 'pdf-view)
  (require 'pdf-misc)
  (require 'pdf-occur)
  (require 'pdf-util)
  (require 'pdf-annot)
  (require 'pdf-info)
  (require 'pdf-isearch)
  (require 'pdf-history)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-sync)
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  :init
  (setq pdf-view-restore-filename "~/.pdf-view-restore"))

;;; JINX
(use-package jinx
  :ensure t
  :demand t
  ;; :init
  ;; (setenv "PKG_CONFIG_PATH" (concat "/opt/homebrew/opt/glib/lib/pkgconfig/" (getenv "PKG_CONFIG_PATH")))
  :hook (emacs-startup . global-jinx-mode))

;;; Co-Pilot (copilot)
;; Based on: https://tony-zorman.com/posts/package-vc-install.html
(cl-defun zeds/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))


(use-package copilot
    :init (zeds/vc-install :fetcher "github" :repo "copilot-emacs/copilot.el")
    ;; :general (:keymaps 'copilot-completion-map
    ;;         ;; keybindings to cycle through vertico results.
    ;;         "C-<tab>" 'copilot-accept-completion
    ;;         )
    )

(provide 'init)
;;; init.el ends here


;;; COMPLETION corfu -- Better Completion
;; (use-package corfu
;;   :ensure t
;;   :demand t
;;   :hook
;;   (eval-expression-minibuffer-setup . corfu-mode)
;;   ;; ((prog-mode . corfu-mode)
;;   :init
;;   (global-corfu-mode)
;;   :custom
;;   (corfu-cycle t)  ;; allows cycling through candidates
;;   ;; (corfu-auto nil) ;; disables auto-completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
;;   ;; (:keymaps 'corfu-map
;;   ;;           "SPC" 'corfu-insert-separator)
;;   ;;     ;; for compatibility with orderless
;;   )

;;; keys unbound
;; (general-unbind
;;   :ensure t
;;   :states '(insert)
;;   "C-k" ;; this was interfering with corfu completion
;;   :states '(normal)
;;   "C-;")

;;; cape
;; (use-package cape
;;   :ensure t
;;   :demand t
;;   ;; bindings for dedicated completion commands
;;   ;; :general
;;   ;; ("M-p p" 'completion-at-point ;; capf
;;   ;;  "M-p t" 'complete-tag        ;; etags
;;   ;;  "M-p d" 'cape-dabbrev        ;; dabbrev
;;   ;;  "M-p h" 'cape-history
;;   ;;  "M-p f" 'cape-file
;;   ;;  "M-p k" 'cape-keyword
;;   ;;  "M-p s" 'cape-symbol
;;   ;;  "M-p a" 'cape-abbrev
;;   ;;  "M-p i" 'cape-ispell
;;   ;;  "M-p l" 'cape-line
;;   ;;  "M-p w" 'cape-dict
;;   ;;  "M-p \\" 'cape-tex
;;   ;;  "M-p &" 'cape-sgml
;;   ;;  "M-p r" 'cape-rfc1345)
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev))
