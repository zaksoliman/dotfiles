;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:


(maybe-require-package 'org-roam)
(setq org-roam-directory "~/Documents/notes/org-roam/zettels")
(setq org-roam-dailies-directory "~/Documents/notes/org-roam/daily")
(setq org-roam-db-location "~/Documents/notes/org-roam/db/org-roam.db")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))


(add-hook 'after-init-hook 'org-roam-mode)

(provide 'init-org)
;;; init-org.el ends here



;;(when *is-a-mac*
;;  (maybe-require-package 'grab-mac-link))
;;
;;(maybe-require-package 'org-cliplink)
;;
;;(define-key global-map (kbd "C-c l") 'org-store-link)
;;(define-key global-map (kbd "C-c a") 'org-agenda)
;;
;;(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
;;  "A keymap for handy global access to org helpers, particularly clocking.")
;;
;;(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
;;(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
;;(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
;;(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
;;(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


; ;; Various preferences
; (setq org-log-done t
;       org-edit-timestamp-down-means-later t
;       org-hide-emphasis-markers t
;       org-catch-invisible-edits 'show
;       org-export-coding-system 'utf-8
;       org-fast-tag-selection-single-key 'expert
;       org-html-validation-link nil
;       org-export-kill-product-buffer-when-displayed t
;       org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html


; (maybe-require-package 'writeroom-mode)
; (define-minor-mode prose-mode
;   "Set up a buffer for prose editing.
; This enables or modifies a number of settings so that the
; experience of editing prose is a little more like that of a
; typical word processor."
;   nil " Prose" nil
;   (if prose-mode
;       (progn
;         (when (fboundp 'writeroom-mode)
;           (writeroom-mode 1))
;         (setq truncate-lines nil)
;         (setq word-wrap t)
;         (setq cursor-type 'bar)
;         (when (eq major-mode 'org)
;           (kill-local-variable 'buffer-face-mode-face))
;         (buffer-face-mode 1)
;         ;;(delete-selection-mode 1)
;         (setq-local blink-cursor-interval 0.6)
;         (setq-local show-trailing-whitespace nil)
;         (setq-local line-spacing 0.2)
;         (setq-local electric-pair-mode nil)
;         (ignore-errors (flyspell-mode 1))
;         (visual-line-mode 1))
;     (kill-local-variable 'truncate-lines)
;     (kill-local-variable 'word-wrap)
;     (kill-local-variable 'cursor-type)
;     (kill-local-variable 'blink-cursor-interval)
;     (kill-local-variable 'show-trailing-whitespace)
;     (kill-local-variable 'line-spacing)
;     (kill-local-variable 'electric-pair-mode)
;     (buffer-face-mode -1)
;     ;; (delete-selection-mode -1)
;     (flyspell-mode -1)
;     (visual-line-mode -1)
;     (when (fboundp 'writeroom-mode)
;       (writeroom-mode 0))))
; 
; ;;(add-hook 'org-mode-hook 'buffer-face-mode)
; (setq org-support-shift-select t)
; 
; ;;; To-do settings
; 
; (setq org-todo-keywords
;       (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
;               (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
;               (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
;       org-todo-repeat-to-state "NEXT")
; 
; (setq org-todo-keyword-faces
;       (quote (("NEXT" :inherit warning)
;               ("PROJECT" :inherit font-lock-string-face))))
; 
; ;;; Archiving
; 
; (setq org-archive-mark-done nil)
; (setq org-archive-location "%s_archive::* Archive")
; 
; 
; (require-package 'org-pomodoro)
; (setq org-pomodoro-keep-killed-pomodoro-time t)
; (with-eval-after-load 'org-agenda
;   (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))
; 
; 
; (with-eval-after-load 'org
;   (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
;   (when *is-a-mac*
;     (define-key org-mode-map (kbd "M-h") nil)
;     (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))
; 
; (with-eval-after-load 'org
;   (org-babel-do-load-languages
;    'org-babel-load-languages
;    `((R . t)
;      (ditaa . t)
;      (dot . t)
;      (emacs-lisp . t)
;      (gnuplot . t)
;      (haskell . nil)
;      (latex . t)
;      (ledger . t)
;      (ocaml . nil)
;      (octave . t)
;      (plantuml . t)
;      (python . t)
;      (ruby . t)
;      (screen . nil)
;      (,(if (locate-library "ob-sh") 'sh 'shell) . t)
;      (sql . t)
;      (sqlite . t))))


