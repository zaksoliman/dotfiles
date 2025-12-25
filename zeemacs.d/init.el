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

;; (setq package-check-signature nil)

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)
;; Show help buffer after startup
(add-hook 'after-init-hook 'help-quick)

 (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") 

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/langs" user-emacs-directory))
;; ;; (add-to-list 'load-path (expand-file-name "lisp/tools" user-emacs-directory))

(require 'package-setup)
(require 'variables)
(require 'functions)
(require 'base)
(require 'rust)
(require 'python-lang)
;; (require 'ai)


;; (use-package direnv
;;   :ensure t
;;   :config
;;   (direnv-mode))


;;; EVIL
(mapc #'define-prefix-command
      '(evil-application-map
        evil-buffer-map
        evil-error-map
        evil-file-map
        evil-git-map
        evil-global-leader-map
        evil-insert-map
        evil-jump-map
        evil-option-map
        evil-search-map
        evil-code-map
        evil-notes-map
        evil-help-map
        evil-dir-map
        evil-project-map
        evil-tab-map))

(defvar zeeds-project-map
(let ((map (make-sparse-keymap)))
  (set-keymap-parent map project-prefix-map)
  map)
  "Repeating map for balanced expressions.")

(defun zeds/evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun zeds/evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))


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

  (evil-set-leader 'normal (kbd ",") t)
  (evil-set-leader 'motion (kbd ",") t)
  (evil-set-leader 'visual (kbd ",") t)

  :bind
  (:map evil-motion-state-map
    ("SPC" . evil-global-leader-map)
    ("M-SPC" . evil-global-leader-map)
    :map evil-insert-state-map
    ("M-SPC" . evil-global-leader-map)
    :map evil-global-leader-map
    ("SPC" . execute-extended-command)
    ("a" . evil-application-map)
    ("b" . evil-buffer-map)
    ("c" . evil-code-map)
    ("f" . evil-file-map)
    ("g" . evil-git-map)
    ("j" . evil-jump-map)
    ("n" . evil-notes-map)
    ("o" . evil-option-map)
    ("p" . evil-project-map)
    ("s" . evil-search-map)
    ("u" . universal-argument)
    ("w" . evil-window-map)
    ("h" . evil-help-map)
    ("d" . evil-dir-map)
    :map evil-buffer-map
    ("b" . switch-to-buffer)
    ("p" . previous-buffer)
    ("n" . next-buffer)
    ("B" . bury-buffer)
    ("k" . kill-current-buffer)
    ("e" . eval-buffer)
    ("m" . view-echo-area-messages)
    ("s" . scratch-buffer)
    ("x" . kill-buffer-and-window)
    :map evil-window-map
    :prefix "m"
    :prefix-map maxize-window-prefix-map
    ("m" . zeds/window-maximize-buffer)
    ("s" . zeds/window-maximize-horizontally)
    ("v" . zeds/window-maximize-vertically)
    :map evil-file-map
    ("a" . write-file)
    ("c" . copy-file)
    ("f" . find-file)
    ("i" . insert-file)
    ("n" . fileloop-continue)
    ("r" . recentf)
    ("R" . zeds/reload-init)
    ("i" . zeds/open-init-file)
    ;; ("R" . miciah/rename-file-and-buffer)
    ("s" . save-buffer)
    :map evil-jump-map
    ("f" . find-function)
    ("v" . find-variable)
    :map evil-option-map
    ("f" . display-fill-column-indicator-mode)
    ("s" . window-toggle-side-windows)
    ("l" . toggle-truncate-lines)
    ("n" . display-line-numbers-mode)
    ("d" . toggle-debug-on-error)
    ("D" . toggle-debug-on-quit)
    ("T" . miciah/toggle-themes)
    :map evil-replace-state-map
    ("M-SPC" . evil-global-leader-map)
    :map evil-search-map
    ("c" . evil-ex-nohighlight)))

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :ensure t
  :after evil
  :custom (evil-collection-key-blacklist '("SPC"))
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collectio n-mode-list '(pdf pdf-view)) ;; evilify pdf-view
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

;;; WHICH-KEY
(use-package which-key
  :ensure t
  :after evil
  :demand t
  :init
  (setq which-key-idle-delay 0.3)
  :config
  (which-key-mode)
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
  :bind (
         :map evil-normal-state-map
              ("s" . evil-avy-goto-char-timer)
              ("f" . evil-avy-goto-char-in-line)
              ("gl" . evil-avy-goto-line)
              (";" . avy-resume)
        :map evil-motion-state-map
              ("f" . evil-avy-goto-char-in-line)
              ("gl" . evil-avy-goto-line)
              (";" . avy-resume))
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
  ;;:general
  ;; (:keymaps 'vertico-map
  ;;           ;; keybindings to cycle through vertico results.
  ;;           "C-j" 'vertico-next
  ;;           "C-k" 'vertico-previous
  ;;           "C-f" 'vertico-exit
  ;;           "<backspace>" 'vertico-directory-delete-char
  ;;           "C-<backspace>" 'vertico-directory-delete-word
  ;;           "C-w" 'vertico-directory-delete-word
  ;;           "RET" 'vertico-directory-enter)
  ;; (:keymaps 'minibuffer-local-map
  ;;           "M-h" 'backward-kill-word)
  )

(use-package vertico-directory
  :after vertico
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
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
  ) ;; for compatibility with orderless

;;; keys unbound
;; (general-unbind
;;   :ensure t
;;   :states '(insert)
;;   "C-k" ;; this was interfering with corfu completion
;;   :states '(normal)
;;   "C-;")

;;; cape
(use-package cape
  :ensure t
  :demand t
  ;; bindings for dedicated completion commands
  ;; :general
  ;; ("M-p p" 'completion-at-point ;; capf
  ;;  "M-p t" 'complete-tag        ;; etags
  ;;  "M-p d" 'cape-dabbrev        ;; dabbrev
  ;;  "M-p h" 'cape-history
  ;;  "M-p f" 'cape-file
  ;;  "M-p k" 'cape-keyword
  ;;  "M-p s" 'cape-symbol
  ;;  "M-p a" 'cape-abbrev
  ;;  "M-p i" 'cape-ispell
  ;;  "M-p l" 'cape-line
  ;;  "M-p w" 'cape-dict
  ;;  "M-p \\" 'cape-tex
  ;;  "M-p &" 'cape-sgml
  ;;  "M-p r" 'cape-rfc1345)
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
  :bind (:map evil-buffer-map
              ("b" . consult-buffer)
              :map evil-search-map
              ("o" . consult-outline)
              ("r" . consult-ripgrep)
              ("g" . consult-grep)
              ("G" . consult-git-grep)
              ("f" . consult-find)
              ("F" . consult-locate)
              ("l" . consult-line)
              ("y" . consult-yank-from-kill-ring)
              ("i" . consult-imenu)
              :map evil-help-map
              ("t" . consult-theme))
  :config
  ; use project.el to retrieve the project root
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
  ;; :general
  ;; (zeds/leader-keys
  ;;   "." 'embark-act)    ;; easily accessible 'embark-act' binding.
  ;; ("C-." 'embark-act)  ;; overlaps with evil-repeat
  ;; ("C-;" 'embark-dwim) ;; overlaps with IEdit
  ;; (:keymaps 'vertico-map
  ;;           "C-." 'embark-act) ;; embark on completion candidates
  ;; (:keymaps 'embark-heading-map
  ;;           "l" 'org-id-store-link)
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
;; (use-package affe
;;   :ensure t
;;   :demand t
;;   :after orderless
;;   ;; :general
;;   ;; (zeds/leader-keys
;;   ;;   "sa" '(affe-grep :wk "affe grep")
;;   ;;   "sw" '(affe-find :wk "affe find"))
;;   :init
;;   (defun affe-orderless-regexp-compiler (input _type _ignorecase)
;;     (setq input (orderless-pattern-compiler input))
;;     (cons input (lambda (str) (orderless--highlight input str))))
;;   :config
;;   (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

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


;;; ORG MODE CONFIG
(use-package org
  :demand t
  :init
  ;; edit settings
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t ;; special navigation behaviour in headlines
        org-insert-heading-respect-content t
        org-return-follows-link  t)

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
  ;; :general
  ;; (zeds/local-leader-keys
  ;;   :keymaps 'org-mode-map
  ;;   "a" '(org-archive-subtree :wk "archive")
  ;;   "c" '(org-cite-insert :wk "insert citation")
  ;;   "l" '(:ignore t :wk "link")
  ;;   "ll" '(org-insert-link t :wk "link")
  ;;   "lp" '(org-latex-preview t :wk "prev latex")
  ;;   "h" '(consult-org-heading :wk "consult heading")
  ;;   "d" '(org-cut-special :wk "org cut special")
  ;;   "y" '(org-copy-special :wk "org copy special")
  ;;   "p" '(org-paste-special :wk "org paste special")
  ;;   "b" '(:keymap org-babel-map :wk "babel")
  ;;   "t" '(org-todo :wk "todo")
  ;;   "s" '(org-insert-structure-template :wk "template")
  ;;   "e" '(org-edit-special :wk "edit")
  ;;   "i" '(:ignore t :wk "insert")
  ;;   "ih" '(org-insert-heading :wk "insert heading")
  ;;   "is" '(org-insert-subheading :wk "insert heading")
  ;;   "f" '(org-footnote-action :wk "footnote action")
  ;;   ">" '(org-demote-subtree :wk "demote subtree")
  ;;   "<" '(org-promote-subtree :wk "demote subtree"))
  ;; (:keymaps 'org-agenda-mode-map
  ;;           "j" '(org-agenda-next-line)
  ;;           "h" '(org-agenda-previous-line))

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
  ;; :general
  ;; (zeds/leader-keys
  ;;   "nt" '(org-transclusion-mode :wk "transclusion mode"))
  )

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

;; (use-package org-cliplink
;;   :after org
;;   ;;:general
;;   ;; (zeds/local-leader-keys
;;   ;;   :keymaps 'org-mode-map
;;   ;;   "lc" '(org-cliplink :wk "cliplink"))
;;   )

(use-package org-modern
  :ensure t
  :after org
  :config (global-org-modern-mode))

;;; ORG ROAM
(use-package org-roam
  :ensure t
  :demand t
  :bind (:map evil-notes-map
              :prefix "r"
              :prefix-map org-roam-map
              ("i" . org-roam-node-insert)
              ("t" . org-roam-buffer-toggle)
              ("c" . org-roam-capture)
              ("f" . org-roam-node-find)
              ("t" . org-roam-tag-add)
              ("T" . org-roam-tag-remove)
              :prefix "d"
              :prefix-map org-roam-daily-map
              ("t" . org-roam-dailies-goto-today)
              ("y" . org-roam-dailies-goto-yesterday)
              ("T" . org-roam-dailies-goto-tomorrow)
              ("d" . org-roam-dailies-goto-date)
              )
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
                              "#+title: ${title}\n#+filetags: :osedea:draft:\n")
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
  ;; :general
  ;; (zeds/local-leader-keys
  ;;   :keymaps 'LaTeX-mode-map
  ;;   ;; "TAB" 'TeX-complete-symbol ;; FIXME let's 'TAB' do autocompletion (but it's kind of useless to be honest)
  ;;   "=" '(reftex-toc :wk "reftex toc")
  ;;   "(" '(reftex-latex :wk "reftex label")
  ;;   ")" '(reftex-reference :wk "reftex ref")
  ;;   "m" '(LaTeX-macro :wk "insert macro")
  ;;   "s" '(LaTeX-section :wk "insert section header")
  ;;   "e" '(LaTeX-environment :wk "insert environment")
  ;;   "p" '(preview-at-point :wk "preview at point")
  ;;   "f" '(TeX-font :wk "font")
  ;;   "c" '(TeX-command-run-all :wk "compile"))
  )

(use-package evil-tex
  :ensure t
  :after general
  :hook (LaTeX-mode . evil-tex-mode)
  ;; :general
  ;; (:keymaps 'evil-tex-mode-map
  ;;           "M-]" 'evil-tex-brace-movement)
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
  :bind (:map evil-help-map
              ("c" . helpful-command)
              ("f" . helpful-callable)
              ("h" . helpful-at-point)
              ("F" . helpful-function)
              ("v" . helpful-variable)
              ("m" . helpful-mode)
              ("M" . helpful-macro)
              ("s" . helpful-symbol)
              ("K" . helful-kill-buffer)
              ("k" . helpful-key))
  )

;;; PROGRAMMING STUFF :CODE
;;; EGLOT LSP
(use-package eglot
  :demand t
  :init
  (setq completion-category-overrides '((eglot (styles orderless))))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0)
  (setq eglot-autoshutdown t)
  ;; (setq eglot-workspace-configuration #'zeds/eglot-python-workspace-config)
   :commands eglot
  :bind (:map eglot-mode-map
              ("<f6>" . eglot-format-buffer)
              :map evil-code-map
              ("?" . eldoc)
              ("r" . eglot-rename)
              ("a" . eglot-code-actions)
              ("sd" . xref-find-definitions-other-window)
              ("sr" . xref-find-references)))


;;; OCAML
;; (use-package tuareg
;;     :ensure t
;;     :init
;;  (add-hook 'tuareg-mode-hook
;;             (lambda() (setq tuareg-mode-name "🐫")))
;;  (add-hook 'tuareg-mode-hook
;;             (lambda()
;;               (setq-local comment-style 'multi-line)
;;               (setq-local comment-continue "   ")))
;; (add-hook 'tuareg-mode-hook
;;             (lambda()
;;               (when (functionp 'prettify-symbols-mode)
;;                 (prettify-symbols-mode))))
;; )

;; (use-package merlin
;;     :ensure t
;;     :init
;;     (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
;;       (when (and opam-share (file-directory-p opam-share))
;;         ;; Register Merlin
;;         (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;         (autoload 'merlin-mode "merlin" nil t nil)
;;         ;; Automatically start it in OCaml buffers
;;         (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;         ;; Use opam switch to lookup ocamlmerlin binary
;;         (setq merlin-command 'opam)
;;         ;; To easily change opam switches within a given Emacs session, you can
;;         ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
;;         ;; and use one of its "OPSW" menus.
;;         ))
;;     )

;; ;;; C LANG
(use-package c-ts-mode
    :ensure nil
    :after (eglot)
    :hook ((c-ts-mode . (lambda ()
                          (eglot-ensure))))
    :custom
      (c-offsets-alist '((inline-open           . 0)
                        (brace-list-open       . 0)
                        (inextern-lang         . 0)
                        (statement-case-open   . 4)
                        (access-label          . -)
                        (case-label            . 0)
                        (member-init-intro     . +)
                        (topmost-intro         . 0)
                        (inlambda              . 0) ;; better indentation for lambda
                        (innamespace           . 0) ;; no indentation after namespace
                         (arglist-cont-nonempty . +)))
      :bind (:map evil-code-map
                  ("c" . compile))
    :config
    (setq c-default-style "linux"
          c-basic-offset 4)
    ;; Based on: https://github.com/MaskRay/ccls/issues/191#issuecomment-606339964
    (add-to-list 'eglot-server-programs
                 '(c-ts-mode . ("ccls"
                                "--init"
                                "{\"clang\":
                                    {
                                    \"extraArgs\":[
                                      \"-isystem/usr/local/include\",
                                      \"-isystem/Library/Developer/CommandLineTools/usr/lib/clang/16.0.0/include\",
                                      \"-isystem/Library/Developer/CommandLineTools/usr/include\",
                                      \"-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include\",
                                      \"-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks\"
                                    ],
                                    \"resourceDir\":\"/Library/Developer/CommandLineTools/usr/lib/clang/16.0.0\"}}"))))

(use-package rmsbolt
    :ensure t
    :after (c-ts-mode)
    :bind (:map evil-code-map
                ("D" . rmsbolt)
                ))




;;; WEB
;; (use-package web-mode
;;   :mode (("\\.html?\\'" . web-mode)
;;          ("\\.css\\'"   . web-mode)
;;          ("\\.jsx?\\'"  . web-mode)
;;          ("\\.tsx?\\'"  . web-mode)
;;          ("\\.json\\'"  . web-mode))
;;   :config
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;;; JavaScript
(use-package js2-mode
    :ensure t
    :after (eglot)
    :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode)
         ("\\.ts\\'" . js2-mode)
         ("\\.tsx\\'" . js2-mode))
    :hook ((js2-mode . (lambda()
                         (eglot-ensure))))
    :config
    ;; js2-mode has its own syntax checker which can conflict with flymake/eglot.
    ;; We disable it to let the LSP and ESLint handle all diagnostics.
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (add-to-list 'eglot-server-programs '(js2-mode . ("typescript-language-server" "--stdio")))
    )

(use-package tsx-ts-mode
  :ensure nil
  :mode ("\\.tsx\\'" . tsx-ts-mode)
    :hook ((tsx-ts-mode . (lambda()
                         (eglot-ensure))))

  )


;;; LISP
(use-package elisp-mode
  :init
  (setq lisp-indent-function 'common-lisp-indent-function))

(use-package lispy
  :ensure t
  :bind (:map lispy-mode-map
              ("<TAB>" . indent-for-tab-command))
  ;; necessary for 'corfu'
  :hook
  (reb-lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (racket-mode . lispy-mode)
  (fennel-mode . lispy-mode))

(use-package lispyville
  :ensure t
  :hook (lispy-mode . lispyville-mode)
  :bind (:map lispyville-mode-map
              ("<TAB>" . indent-for-tab-command))
  ;; necessary for 'corfu'
  ;; the following is necessary to retain tab completion in lispy mode
  :config
  ;; TODO play around with key themes
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
  :bind (:map evil-application-map
              ("t" . vterm-toggle)
              ("T" . vterm))
  )

;;; RegEX
(use-package re-builder
  ;; :general (zeds/leader-keys
             ;; "se" '(regexp-builder :wk "regex builder"))
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
  ;; :general
  ;; (zeds/leader-keys
  ;;   "oe" '(eshell :wk "eshell"))
    )

;;; COLORS
(use-package rainbow-mode :ensure t)

;;; SNIPPETS
(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
          ("TAB" . nil)
          ("<tab>" . nil))
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.config/emacs/snippets")
  (yas-global-mode 1))

;;; CHECKERS
;;; Builtin Flymake over flycheck
(use-package flymake
  :ensure t
  :bind (:map evil-code-map
              ("f" . consult-flymake))
  ;; depends on consult
  :hook
  (TeX-mode . flymake-mode)
  (emacs-lisp-mode . flymake-mode)
  (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 1)
  ;; :general
  ;; (general-nmap "] !" 'flymake-goto-next-error)
  ;; (general-nmap "[ !" 'flymake-goto-prev-error)
  )

(use-package recursion-indicator
  :ensure t
  :demand t
  :config
  (recursion-indicator-mode))

;;; PROJECT.EL
(use-package project
    :init
    (set-keymap-parent evil-project-map project-prefix-map)
    )

;;; DIRED
(use-package dired
    :bind (:map evil-dir-map
           ("d" . dired)
           ("j" . dired-jump)
           :map dired-mode-map
           ("<normal-state> h" . dired-up-directory)
           ("<normal-state> q" . kill-current-buffer)
           ("<normal-state> l" . dirend-find-file))
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

(use-package nov
    :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'olivetti-mode)
  (add-hook 'nov-mode-hook 'variable-pitch-mode))

;;; MAGIT
(use-package magit
    :ensure t
    :bind (:map evil-git-map ("g" . magit-status))
  )

;;; PDF SUPPORT
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  ;; Automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t
        pdf-view-use-scaling t)
  (add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)
  ;; Use isearch instead of swiper
 ;; (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(with-eval-after-load 'pdf-view
  (evil-define-key 'normal pdf-view-mode-map
    "j" 'pdf-view-next-line-or-next-page
    "k" 'pdf-view-previous-line-or-previous-page
    "gg" 'pdf-view-first-page
    "G" 'pdf-view-last-page
    "h" 'image-backward-hscroll
    "l" 'image-forward-hscroll))

(defun zeds/copy-buffer-file-path ()
  "Copies the full file path of the current buffer to the kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "Copied file path: %s" buffer-file-name)))

;; (use-package pdf-tools
;;   :ensure t
;;   :hook (TeX-after-compilation-finished . TeX-revert-document-buffer)
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;  :config
;;    (pdf-tools-install)
;;    (require 'pdf-info)
;;   (require 'pdf-view)
;;   (require 'pdf-misc)
;;   (require 'pdf-occur)
;;   (require 'pdf-util)
;;   (require 'pdf-annot)
;;   (require 'pdf-isearch)
;;   (require 'pdf-history)
;;   (require 'pdf-links)
;;   (require 'pdf-outline)
;;   (require 'pdf-sync)
;;   (setq pdf-view-use-scaling t)
;;   (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))


;; (use-package pdf-view-restore
;;   :ensure t
;;   :after pdf-tools
;;   :config
;;   (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
;;   :init
;;   (setq pdf-view-restore-filename "~/.pdf-view-restore"))

;;; JINX
;; (use-package jinx
;;   :ensure t
;;   :demand t
;;   ;; :init
;;   ;; (setenv "PKG_CONFIG_PATH" (concat "/opt/homebrew/opt/glib/lib/pkgconfig/" (getenv "PKG_CONFIG_PATH")))
;;   :hook (emacs-startup . global-jinx-mode)
;;   ;; :general
;;   ;; ("M-$" 'jinx-correct
;;   ;;  "C-M-$" 'jinx-languages)
;;   )


;;; Benchmarking
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; ;; Emacs startup profiler to identify slow-loading packages
;; (use-package esup
;;   :ensure t
;;   :commands (esup))

;; Jinja2 support
(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" . jinja2-mode)
  :hook (jinja2-mode . (lambda ()
                        (setq-local comment-start "{# ")
                        (setq-local comment-end " #"))))


(provide 'init)
;;; init.el ends here
