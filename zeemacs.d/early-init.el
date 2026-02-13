;;; early-init.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Zak Soliman
;;
;; Author: Zak Soliman <zakaria.soliman1@gmail.com>
;; Maintainer: Zak Soliman <zakaria.soliman1@gmail.com>
;; Created: August 23, 2024
;; Modified: August 23, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/zakaria/early-init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;      Early init
;;
;;
;;; Code:

;; Startup speed, annoyance suppression

;; (setq use-package-always-defer t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold (* 16 1024 1024))

;; (setq gc-cons-threshold 10000000)

;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq byte-compile-warnings '(not obsolete))

(setq warning-suppress-log-types '((comp) (bytecomp)))

 ;; less noise when compiling elisp
(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-splash-screen t) ;; no thanks

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)

(menu-bar-mode -1)   ; disables menubar

(tool-bar-mode -1)   ; All these tools are in the menu-bar anyway

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq default-frame-alist '(
                            ;; (fullscreen . maximized)

                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            ;; (background-color . "#000000")
                            (foreground-color . "#ffffff")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;;use-file-dialog nil ;; don't use system file dialog
;;tab-bar-new-button-show nil ;; don't show new tab button
;;tab-bar-close-button-show nil ;; don't show tab close button
;;tab-line-close-button-show nil) ;; don't show tab close button
;; FONTS
;; This sets the default font on all graphical frames created after restarting Emacs.
(add-to-list 'default-frame-alist '(font . "Fira Code-12"))

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)
;; Uncomment the following line if line spacing needs adjusting.
;; (setq-default line-spacing 0.12)

(provide 'early-init)
;;; early-init.el ends here
