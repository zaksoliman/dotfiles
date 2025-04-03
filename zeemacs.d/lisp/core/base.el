;;; core/base.el --- Base Emacs Configs -*- lexical-binding: t -*-

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
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))


  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode)
              (compilation-auto-jump-to-error t)
              (compilation-scroll-output 'first-error))))

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


(provide 'core/base)
;;; core/base.el ends here
