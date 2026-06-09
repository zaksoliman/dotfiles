;;; core/keybinds.el --- Leader-key skeleton -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; The "skeleton & injection" pattern.  This file defines the leader-key
;; hierarchy as empty (or generic-only) keymaps -- the menu structure -- with
;; no dependency on any external package.  Individual packages inject their own
;; bindings into these maps from their own `use-package' blocks, e.g.
;;
;;     (use-package magit
;;       :bind (:map zeds/git-map ("g" . magit-status)))
;;
;; so a key is only bound when its package is actually present.
;;
;;; Code:

;;; Sub-maps (generic, package-agnostic bindings live here directly)

(defvar-keymap zeds/application-map :doc "Applications.")

(defvar-keymap zeds/buffer-map
  :doc "Buffers."
  "b" #'switch-to-buffer
  "p" #'previous-buffer
  "n" #'next-buffer
  "B" #'bury-buffer
  "k" #'kill-current-buffer
  "e" #'eval-buffer
  "m" #'view-echo-area-messages
  "s" #'scratch-buffer
  "x" #'kill-buffer-and-window)

(defvar-keymap zeds/code-map :doc "Code / LSP.")

(defvar-keymap zeds/dir-map :doc "Directories.")

(defvar-keymap zeds/file-map
  :doc "Files."
  "a" #'write-file
  "c" #'copy-file
  "f" #'find-file
  "n" #'fileloop-continue
  "r" #'recentf
  "R" #'zeds/reload-init
  "i" #'zeds/open-init-file
  "s" #'save-buffer)

(defvar-keymap zeds/git-map :doc "Git.")

(defvar-keymap zeds/help-map :doc "Help.")

(defvar-keymap zeds/jump-map
  :doc "Jump."
  "f" #'find-function
  "v" #'find-variable)

(defvar-keymap zeds/notes-map :doc "Notes.")

(defvar-keymap zeds/option-map
  :doc "Toggles."
  "f" #'display-fill-column-indicator-mode
  "s" #'window-toggle-side-windows
  "l" #'toggle-truncate-lines
  "n" #'display-line-numbers-mode
  "d" #'toggle-debug-on-error
  "D" #'toggle-debug-on-quit)

(defvar-keymap zeds/project-map :doc "Project.")

(defvar-keymap zeds/search-map
  :doc "Search."
  "c" #'evil-ex-nohighlight)

(defvar-keymap zeds/window-maximize-map
  :doc "Maximize windows."
  "m" #'zeds/window-maximize-buffer
  "s" #'zeds/window-maximize-horizontally
  "v" #'zeds/window-maximize-vertically)

;;; Leader root

(defvar-keymap zeds/leader-map
  :doc "Main leader map."
  "SPC" #'execute-extended-command
  "u" #'universal-argument
  "a" zeds/application-map
  "b" zeds/buffer-map
  "c" zeds/code-map
  "d" zeds/dir-map
  "f" zeds/file-map
  "g" zeds/git-map
  "h" zeds/help-map
  "j" zeds/jump-map
  "n" zeds/notes-map
  "o" zeds/option-map
  "p" zeds/project-map
  "s" zeds/search-map)

;;; Attach to evil once it is available

(with-eval-after-load 'evil
  ;; Window commands reuse evil's own window map (SPC w ...).
  (keymap-set zeds/leader-map "w" evil-window-map)
  (keymap-set evil-window-map "m" zeds/window-maximize-map)
  ;; Reach the leader from normal/motion (SPC) and from anywhere (M-SPC).
  (keymap-set evil-motion-state-map "SPC" zeds/leader-map)
  (keymap-set evil-motion-state-map "M-SPC" zeds/leader-map)
  (keymap-set evil-insert-state-map "M-SPC" zeds/leader-map)
  (keymap-set evil-replace-state-map "M-SPC" zeds/leader-map))

;;; which-key hints

(with-eval-after-load 'which-key
  ;; Leader root labels
  (which-key-add-keymap-based-replacements zeds/leader-map
    "SPC" "M-x"
    "u"   "universal arg"
    "a"   "applications"
    "b"   "buffers"
    "c"   "code/LSP"
    "d"   "directories"
    "f"   "files"
    "g"   "git"
    "h"   "help"
    "j"   "jump"
    "n"   "notes"
    "o"   "options/toggles"
    "p"   "project"
    "s"   "search"
    "w"   "windows")

  ;; Buffer map
  (which-key-add-keymap-based-replacements zeds/buffer-map
    "b" "switch buffer"
    "p" "prev buffer"
    "n" "next buffer"
    "B" "bury buffer"
    "k" "kill buffer"
    "e" "eval buffer"
    "m" "messages"
    "s" "scratch"
    "x" "kill buf+win")

  ;; File map
  (which-key-add-keymap-based-replacements zeds/file-map
    "a" "save as…"
    "c" "copy file"
    "f" "find file"
    "n" "next match"
    "r" "recent files"
    "R" "reload init"
    "i" "open init"
    "s" "save file")

  ;; Jump map
  (which-key-add-keymap-based-replacements zeds/jump-map
    "f" "find function"
    "v" "find variable")

  ;; Option/toggle map
  (which-key-add-keymap-based-replacements zeds/option-map
    "f" "fill-column indicator"
    "s" "side windows"
    "l" "truncate lines"
    "n" "line numbers"
    "d" "debug on error"
    "D" "debug on quit")

  ;; Search map
  (which-key-add-keymap-based-replacements zeds/search-map
    "c" "clear highlights")

  ;; Window maximize map
  (which-key-add-keymap-based-replacements zeds/window-maximize-map
    "m" "maximize buffer"
    "s" "maximize horiz"
    "v" "maximize vert"))

(provide 'keybinds)
;;; keybinds.el ends here
