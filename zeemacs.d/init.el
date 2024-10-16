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
;;
;;; Code:

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
;; END VARIABLES
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

(defun zeds/macos-reveal-in-finder ()
  (interactive)
  (zeds/macos-open-with "Finder" default-directory))

;; ;;;###autoload (autoload '+macos/reveal-project-in-finder "os/macos/autoload" nil t)
;; (zeds/macos--open-with reveal-project-in-finder "Finder"
;;                    (or (doom-project-root) default-directory))
;; END FUNCTION DEFINITIONS

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
;; END EMACS DEFAULTS

(use-package saveplace
  :init (save-place-mode 1))

(use-package savehist
  :init (savehist-mode 1))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "private/tmp")
  (recentf-mode))

;;; THEME
;; https://protesilaos.com/emacs/standard-themes
;; (use-package ef-themes
;;   :ensure t
;;   :demand t
;;   :config
;;   (load-theme 'ef-winter)
;;   )

;; Modeline
(use-package mood-line
  :ensure t
  ;; Enable mood-line
  :config
  (mood-line-mode))
;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))
;;  ;; Use pretty Fira Code-compatible glyphs
;;  :custom
;;  (mood-line-glyph-alist mood-line-glyphs-fira-code))

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

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

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

;; END UI/DX

;;; EVIL
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t ;; For evil-collection
        evil-want-keybinding nil ;; For evil-collection
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-width 2
        evil-respect-visual-line-mode t ;; respect visual lines
        evil-search-module 'isearch ;; use emacs' built-in search functionality.
        evil-want-C-u-scroll t      ;; allow scroll up with 'C-u'
        evil-want-C-d-scroll t      ;; allow scroll down with 'C-d'
        evil-want-C-i-jump nil ;; hopefully this will fix weird tab behaviour
        evil-undo-system 'undo-redo)

  (evil-mode)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; buffers in which I want to immediately start typing should be in 'insert' state by default.
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'magit-diff-mode 'insert)
  (evil-set-initial-state 'Info-mode 'insert)
                                        ; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">") 'zeds/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'zeds/evil-shift-left-visual)

  (defun zeds/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun zeds/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore)))

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  ;; (setq evil-collection-mode-list '(dashboard dired ibuffer pdf magit compilation))
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
         (org-mode . (lambda () (push '(?$ . ("\\(" . "\\)")) evil-surround-pairs-alist))))

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

;;; KEY BINDINGS - GENERAL.EL
(use-package general
  :ensure t
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
    "wm" '(:ignore t  :wk "maximize")
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
    "fs" '(save-buffer :wk "save file")
    "fr" '(recentf :wk "recent files"))

  ;; buffer
  ;; see 'bufler' and 'popper'
  (zeds/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch buffer") ;; gets overridden by consult
    "bk" '(kill-this-buffer :wk "kill this buffer")
    "br" '(revert-buffer :wk "reload buffer")
    "bp" '(previous-buffer :wk "previous buffer")
    "bn" '(next-buffer :wk "next buffer")
    "bx" '(scratch-buffer :wk "Switch to scratch buffer"))

  ;; universal argument
  (zeds/leader-keys
    "u" '(universal-argument :wk "universal prefix"))

  ;; ;; notes
  ;; ;; see 'citar' and 'org-roam'
  (zeds/leader-keys
    "n" '(:ignore t :wk "notes")
    ;; see org-roam and citar sections
    "na" '(org-todo-list :wk "agenda todos")) ;; agenda

  ;; ;; code
  ;; see 'flymake'
  (zeds/leader-keys
    "c" '(:ignore t :wk "code"))

  ;; open
  (zeds/leader-keys
    "o" '(:ignore t :wk "open")
    "of" '(zeds/macos-reveal-in-finder :wk "open in finder")
    "os" '(speedbar t :wk "speedbar")
    "op" '(elpaca-log t :wk "elpaca"))


  ;; ;; search
  ;; ;; see 'consult'
  (zeds/leader-keys
    "s" '(:ignore t :wk "search"))

  ;; ;;Emacs Admin
  (zeds/leader-keys
    "e" '(:ignore t :wk "Emacs Admin")
    "er" '(zeds/reload-init :wk "Reload configs")
    "ei" '(zeds/open-init-file :wk "Open init file")
    "ed" '(zeds/open-init-dir :wk "Open config directory"))
  ;; ;; templating
  ;; ;; see 'tempel'
  ;; (zeds/leader-keys
  ;;     "t" '(:ignore t :wk "template")
  )

;; END KEY BINDINGS - GENERAL.EL


;;; WHICH-KEY
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

;;; AVY
(use-package avy
  :demand t
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
  :general
  (general-def '(normal motion)
    "s" 'evil-avy-goto-char-timer
    "f" 'evil-avy-goto-char-in-line
    "gl" 'evil-avy-goto-line ;; this rules
    ";" 'avy-resume)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'zeds/avy-action-embark ;; embark integration
        (alist-get ?i avy-dispatch-alist) 'zeds/avy-action-insert-newline
        (alist-get ?K avy-dispatch-alist) 'zeds/avy-action-kill-whole-line)) ;; kill lines with avy
;; END AVY


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
  :general
  (:keymaps 'vertico-map
            ;; keybindings to cycle through vertico results.
            "C-j" 'vertico-next
            "C-k" 'vertico-previous
            "C-f" 'vertico-exit
            "<backspace>" 'vertico-directory-delete-char
            "C-<backspace>" 'vertico-directory-delete-word
            "C-w" 'vertico-directory-delete-word
            "RET" 'vertico-directory-enter)
  (:keymaps 'minibuffer-local-map
            "M-h" 'backward-kill-word))

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
;; END VERTICO

;;; corfu -- Better Completion
(use-package corfu
  :ensure t
  :demand t
  :hook
  (eval-expression-minibuffer-setup . corfu-mode)
  ;; ((prog-mode . corfu-mode)
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)  ;; allows cycling through candidates
  ;; (corfu-auto nil) ;; disables auto-completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind
  :general
  (:keymaps 'corfu-map
            "SPC" 'corfu-insert-separator)) ;; for compatibility with orderless

;;; keys unbound
(general-unbind
  :ensure t
  :states '(insert)
  "C-k" ;; this was interfering with corfu completion
  :states '(normal)
  "C-;")

;;; cape
(use-package cape
  :ensure t
  :demand t
  ;; bindings for dedicated completion commands
  :general
  ("M-p p" 'completion-at-point ;; capf
   "M-p t" 'complete-tag        ;; etags
   "M-p d" 'cape-dabbrev        ;; dabbrev
   "M-p h" 'cape-history
   "M-p f" 'cape-file
   "M-p k" 'cape-keyword
   "M-p s" 'cape-symbol
   "M-p a" 'cape-abbrev
   "M-p i" 'cape-ispell
   "M-p l" 'cape-line
   "M-p w" 'cape-dict
   "M-p \\" 'cape-tex
   "M-p &" 'cape-sgml
   "M-p r" 'cape-rfc1345)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; company reftex
(use-package company-reftex
  :ensure t
  :after cape
  :init
  (defun reftex-setup-capf ()
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-reftex-labels)))
  :hook
  (LaTeX-mode . reftex-setup-capf))

;;; CONSULT
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
    "i" '(consult-imenu :wk "consult imenu")
    "so" '(consult-outline :wk "consult outline"))
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

;; EMBARK
(use-package embark
  :ensure t
  :demand t
  :general
  (zeds/leader-keys
    "." 'embark-act)    ;; easily accessible 'embark-act' binding.
  ("C-." 'embark-act)  ;; overlaps with evil-repeat
  ("C-;" 'embark-dwim) ;; overlaps with IEdit
  (:keymaps 'vertico-map
            "C-." 'embark-act) ;; embark on completion candidates
  (:keymaps 'embark-heading-map
            "l" 'org-id-store-link)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t                ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; END EMBARK

;;; AFFE
(use-package affe
  :ensure t
  :demand t
  :after orderless
  :general
  (zeds/leader-keys
    "sa" '(affe-grep :wk "affe grep")
    "sw" '(affe-find :wk "affe find"))
  :init
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))
;;  END AFFE

;;;  OLIVETTI Make writing prose nicer
;; Might remove later
(use-package olivetti
  :ensure t
  :demand t
  :init
  (setq olivetti-body-width 80)
  (setq olivetti-style 'fancy)
  (setq olivetti-minimum-body-width 50))
;; END OLIVETTY

;;; HIGHLIGHT TODOs
(use-package hl-todo
  :ensure t
  :demand t
  :init
  (global-hl-todo-mode))
;; END HIGHLIGHT TODOs

;;; ORG MODE CONFIG
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
        org-pretty-entities t
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
  ;; :general
  (zeds/local-leader-keys
    :keymaps 'org-mode-map
    "a" '(org-archive-subtree :wk "archive")
    "c" '(org-cite-insert :wk "insert citation")
    "l" '(:ignore t :wk "link")
    "ll" '(org-insert-link t :wk "link")
    "lp" '(org-latex-preview t :wk "prev latex")
    "h" '(consult-org-heading :wk "consult heading")
    "d" '(org-cut-special :wk "org cut special")
    "y" '(org-copy-special :wk "org copy special")
    "p" '(org-paste-special :wk "org paste special")
    "b" '(:keymap org-babel-map :wk "babel")
    "t" '(org-todo :wk "todo")
    "s" '(org-insert-structure-template :wk "template")
    "e" '(org-edit-special :wk "edit")
    "i" '(:ignore t :wk "insert")
    "ih" '(org-insert-heading :wk "insert heading")
    "is" '(org-insert-subheading :wk "insert heading")
    "f" '(org-footnote-action :wk "footnote action")
    ">" '(org-demote-subtree :wk "demote subtree")
    "<" '(org-promote-subtree :wk "demote subtree"))
  (:keymaps 'org-agenda-mode-map
            "j" '(org-agenda-next-line)
            "h" '(org-agenda-previous-line))

  :hook
  ;; (org-mode . olivetti-mode)
  (org-mode . variable-pitch-mode)
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

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (setq evil-org-retain-visual-state-on-shift t)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; (use-package org-auctex
;; :ensure (:type git :host github :repo
;;                  "karthink/org-auctex")
;; :hook (org-mode . org-auctex-mode))

(use-package org-transclusion
  :ensure t
  :after org
  :general
  (zeds/leader-keys
    "nt" '(org-transclusion-mode :wk "transclusion mode")))

(use-package org-appear
  :ensure t
  :after org
  :init (setq org-appear-trigger 'manual)
  :hook (org-mode . (lambda ()
                      (add-hook 'evil-insert-state-entry-hook
                                #'org-appear-manual-start
                                nil t)
                      (add-hook 'evil-insert-state-exit-hook
                                #'org-appear-manual-stop
                                nil t))))
(use-package org-cliplink
  :after org
  :general
  (zeds/local-leader-keys
    :keymaps 'org-mode-map
    "lc" '(org-cliplink :wk "cliplink")))

(use-package org-modern
  :ensure t
  :after org
  :config (global-org-modern-mode))

;;; ORG ROAM
(use-package org-roam
  :ensure t
  :demand t
  :general
  (zeds/leader-keys
    "nr" '(:ignore t :wk "roam")
    "nri" '(org-roam-node-insert t :wk "insert node")
    "nrt" '(org-roam-buffer-toggle t :wk "roam buffer toggle")
    "nrc" '(org-roam-capture t :wk "roam capture")
    "nrf" '(org-roam-node-find :wk "find node")
    "nrd" '(:ignore t :wk "dailies")
    "nrdt" '(org-roam-dailies-goto-today :wk "today")
    "nrdt" '(org-roam-dailies-goto-yesterday :wk "today")
    "nrdT" '(org-roam-dailies-goto-tomorrow :wk "today")
    "nrdd" '(org-roam-dailies-goto-date :wk "goto date"))
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

;;;  LATEX
(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t                ; parse on load
        reftex-plug-into-AUCTeX t
        TeX-auto-save t                 ; parse on save
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-electric-sub-and-superscript t
        TeX-engine 'luatex ;; use lualatex by default
        TeX-save-query nil
        TeX-electric-math (cons "\\(" "\\)")) ;; '$' inserts an in-line equation '\(...\)'

  (add-hook 'TeX-mode-hook #'reftex-mode)
  (add-hook 'TeX-mode-hook #'olivetti-mode)
  (add-hook 'TeX-mode-hook #'turn-on-auto-fill)
  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook #'outline-minor-mode)
  :general
  (zeds/local-leader-keys
    :keymaps 'LaTeX-mode-map
    ;; "TAB" 'TeX-complete-symbol ;; FIXME let's 'TAB' do autocompletion (but it's kind of useless to be honest)
    "=" '(reftex-toc :wk "reftex toc")
    "(" '(reftex-latex :wk "reftex label")
    ")" '(reftex-reference :wk "reftex ref")
    "m" '(LaTeX-macro :wk "insert macro")
    "s" '(LaTeX-section :wk "insert section header")
    "e" '(LaTeX-environment :wk "insert environment")
    "p" '(preview-at-point :wk "preview at point")
    "f" '(TeX-font :wk "font")
    "c" '(TeX-command-run-all :wk "compile")))

(use-package evil-tex
  :ensure t
  :after general
  :hook (LaTeX-mode . evil-tex-mode)
  :general
  (:keymaps 'evil-tex-mode-map
            "M-]" 'evil-tex-brace-movement)
  :config
  (unbind-key "M-n" 'evil-tex-mode-map)) ;; interfering with jinx

;;; ELECTRIC
(use-package electric
  :demand t
  :init
  (electric-pair-mode 1) ;; automatically insert closing parens
  ;; (electric-indent-mode 1)
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

(use-package ediff
  :demand t
  )

;;; HELP (helpful)
(use-package helpful
  :ensure t
  :demand t
  :general
  (zeds/leader-keys
    "hc" '(helpful-command :wk "describe command")
    "hf" '(helpful-callable :wk "describe callable")
    "hh" '(helpful-at-point :wk "describe at point")
    "hF" '(helpful-function :wk "describe function")
    "hv" '(helpful-variable :wk "describe variable")
    "hk" '(helpful-key :wk "describe key")))

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
  :general
  (zeds/leader-keys
    "c?" '(eldoc :wk "docstring")
    "cr" '(eglot-rename :wk "rename symbol")
    "ca" '(eglot-code-actions :wk "code actions")
    "cfd" '(xref-find-definitions-other-window :wk "find definitions")
    "cfr" '(xref-find-references :wk "find references"))
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


;; (use-package python
;;   :hook
;;   ((python-mode . (lambda ()
;;                        (setq-local indent-tabs-mode t)
;;                        (setq-local tab-width 4)
;;                        (setq-local py-indent-tabs-mode t)))
;;    (python-mode . eglot-ensure))
;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))


(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode . (lambda ()
                             (setq-local indent-tabs-mode nil)
                             (setq-local python-indent-offset 4)
                             (setq-local py-indent-tabs-mode t)
                             ;; (eglot-ensure)
                             )))
  :config
  (add-hook 'window-selection-change-functions (lambda (w)
                                                 (message "Changed window")
                                                 (message w)))
  )

;; (use-package pet
;;   :ensure t
;;   :hook ((python-ts-mode . (lambda ()
;;                              (setq-local python-shell-interpreter (pet-executable-find "python")
;;                                          python-shell-virtualenv-root (pet-virtualenv-root)
;;                                          )
;;                              (eglot-ensure)))))

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
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  :general
  (zeds/local-leader-keys
    :keymap eglot-mode-map
    "h" '(eglot-inlay-hints-mode :wk "Display eglot hints"))
  )


;;; WEB
(use-package js2-mode
  :ensure t)

;;; LISP
(use-package elisp-mode
  :init
  (setq lisp-indent-function 'common-lisp-indent-function))

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
  (vterm-toggle-scope 'project)
  :general
  (zeds/leader-keys
    "ot" '(vterm-toggle :wk "Open vterm")
    "oT" '(vterm :wk "Open vterm in window")
    ))

;;; RegEX
(use-package re-builder
  :general (zeds/leader-keys
             "se" '(regexp-builder :wk "regex builder"))
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
(use-package eshell
  :general
  (zeds/leader-keys
    "oe" '(eshell :wk "eshell")))

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
  :general
  (zeds/leader-keys
    :keymaps 'flymake-mode-map
    "cf" '(consult-flymake :wk "consult flymake") ;; depends on consult
    "cc" '(flymake-mode :wk "toggle flymake")) ;; depends on consult
  :hook
  (TeX-mode . flymake-mode) ;; this is now working
  (emacs-lisp-mode . flymake-mode)
  (prog-mode . flymake-mode)
  ;; :init
  ;; (flymake-no-changes-timeout nil)
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error))

(use-package recursion-indicator
  :ensure t
  :demand t
  :config
  (recursion-indicator-mode))

;;; PROJECT.EL
(use-package project
  :general
  ;; assign built-in project.el bindings a new prefix
  (zeds/leader-keys "p" '(:keymap project-prefix-map :wk "project")))
;;; DIRED
(use-package dired
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
  :general
  (zeds/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "status")))

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
  :hook (emacs-startup . global-jinx-mode)
  :general
  ("M-$" 'jinx-correct
   "C-M-$" 'jinx-languages))

(provide 'init)
;;; init.el ends here
