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

(setq package-enable-at-startup nil)

;; minimal UI
(menu-bar-mode -1) ;; disables menubar
(tool-bar-mode -1) ;; disables toolbar
(scroll-bar-mode -1) ;; disables scrollbar
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling
(recentf-mode 1)
(global-auto-revert-mode 1)

(setq inhibit-splash-screen t) ;; no thanks
        ;;use-file-dialog nil ;; don't use system file dialog
        ;;tab-bar-new-button-show nil ;; don't show new tab button
        ;;tab-bar-close-button-show nil ;; don't show tab close button
        ;;tab-line-close-button-show nil) ;; don't show tab close button

(provide 'early-init)
;;; early-init.el ends here
