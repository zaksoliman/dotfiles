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
;; minimal UI

(setq inhibit-splash-screen t) ;; no thanks
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
