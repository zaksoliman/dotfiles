;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zak Soliman"
      user-mail-address "zakaria.soliman1@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-zenburn)
;; (after! doom-themes
;;   (load-theme 'doom-nano-dark t)
;;   (setq doom-theme 'doom-nano-dark))

(setq projectile-project-search-path '("~/Projects"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/notes/")
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)
(add-hook! 'org-mode-hook
  (setq left-margin-width 5))
;; Make it more like a WYSIWYG editor
(after! org (setq org-hide-emphasis-markers t))

(use-package! org-roam
  :init
  (setq org-roam-directory "~/Documents/notes/org-roam/zettels")
  (setq org-roam-dailies-directory "~/Documents/notes/org-roam/dailies")
  :custom
  (org-roam-graph-executable "neato")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("m" "Main" plain
      "%?"
      :if-new(file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+filetags: :draft:\n")
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
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${doom-hierarchy:40} " (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-db-autosync-enable)
  )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (setq rustic-lsp-server 'rust-analyzer)
(use-package! circe
  :config
  (setq circe-default-nick "DangleWaggle"
        circe-default-user "DangleWaggle"
        circe-default-realname "DangleWaggle"))


;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; Python Formatter
(use-package! python-black
  :demand t
  :after python
  :config
  (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
  (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
  (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)
  )

;;;; LSP
(when (or (modulep! :checkers syntax +flymake)
          (not (modulep! :checkers syntax)))
  (setq lsp-diagnostics-provider :flymake))
(after! lsp-mode
  (setq
   lsp-log-io nil
   lsp-auto-guess-root t
   lsp-progress-via-spinner t
   lsp-enable-file-watchers nil
   lsp-idle-delay 0.01
   lsp-completion-enable-additional-text-edit t

   lsp-signature-render-documentation t
   lsp-signature-auto-activate '(:on-trigger-char :on-server-request :after-completion)
   lsp-signature-doc-lines 10

   lsp-eldoc-enable-hover t
   lsp-eldoc-render-all t
   lsp-headerline-breadcrumb-enable nil
   lsp-modeline-code-actions-segments '(count icon name)

   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-links nil

   lsp-lens-enable t))

(when (modulep! :completion company)
  (setq +lsp-company-backends '(company-capf :with company-yasnippet)))

(after! lsp-ui
  (setq
   ;; Sideline
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-sideline-show-symbol nil
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-diagnostics nil
   ;; Peek
   lsp-ui-peek-enable nil
   ;; Doc
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-delay 0.51
   lsp-ui-doc-max-width 50
   lsp-ui-doc-max-height 30
   lsp-ui-doc-include-signature t
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-header t))


;;;; rust
(after! rustic
  (set-formatter! 'rustic-mode #'rustic-cargo-fmt))

(map! (:map rustic-mode-map
       :localleader
       :desc "Toggle LSP hints" "h" #'lsp-rust-analyzer-inlay-hints-mode))

(setq rustic-lsp-server 'rust-analyzer
      lsp-rust-server 'rust-analyzer)

(set-popup-rule!
  "^\\*rust"
  :slot -2
  :size 0.45
  :side 'right
  :autosave t
  :quit 'current
  :ttl nil
  :modeline t)

(after! lsp-rust
  (setq lsp-rust-analyzer-lru-capacity 100
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-reborrow-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t
        lsp-rust-analyzer-cargo-watch-enable t
        lsp-rust-analyzer-cargo-run-build-scripts t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; TODO: upstream those
  ;; (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
  ;;  (-let* (((&plist :value) contents)
  ;;          (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
  ;;          (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
  ;;                         (-third-item groups)
  ;;                       (car groups)))
  ;;          (sig (--> sig_group
  ;;                    (--drop-while (s-equals? "```rust" it) it)
  ;;                    (--take-while (not (s-equals? "```" it)) it)
  ;;                    (--map (s-trim it) it)
  ;;                    (s-join " " it))))
  ;;    (lsp--render-element (concat "```rust\n" sig "\n```"))))

  (advice-add #'lsp-hover :after (lambda () (setq lsp--hover-saved-bounds nil))))

;; EVIL stuff!
;; (let ((alternatives '("doom-emacs-bw-light.svg")
;;                     ))
;;   (setq fancy-splash-image
;;         (concat doom-user-dir "splash/"
;;                 (nth (random (length alternatives)) alternatives))))

;; (after! evil-surround
;;   (let ((pairs '((?g "$" . "$")
;;                  (?h "(" . ")")
;;                  (?j "[" . "]")
;;                  (?k "{" . "}")
;;                  (?l "<" . ">")
;;                  (?' "'" . "'")
;;                  (?\" "\"" . "\""))))
;;     (prependq! evil-surround-pairs-alist pairs)
;;     (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))

;; Visual niceness
;;
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq doom-font (font-spec :family "Iosevka Slab" :size 13)
     ;;doom-variable-pitch-font (font-spec :family "ETBembo" :size 18)
     doom-variable-pitch-font (font-spec :family "Alegreya" :size 15))


;; Harpoon config
;; You can use this hydra menu that have all the commands
(map! :n "C-SPC" 'harpoon-quick-menu-hydra)
(map! :leader "j a" 'harpoon-add-file)
(map! :leader "j c" 'harpoon-clear)
(map! :leader "j f" 'harpoon-toggle-file)
(map! :leader "j h" 'harpoon-toggle-quick-menu)
(map! :leader "1" 'harpoon-go-to-1)
(map! :leader "2" 'harpoon-go-to-2)
(map! :leader "3" 'harpoon-go-to-3)
(map! :leader "4" 'harpoon-go-to-4)
(map! :leader "5" 'harpoon-go-to-5)
(map! :leader "6" 'harpoon-go-to-6)
(map! :leader "7" 'harpoon-go-to-7)
(map! :leader "8" 'harpoon-go-to-8)
(map! :leader "9" 'harpoon-go-to-9)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(after! hl-todo
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"       . "#FF7B00")
          ("FIXME"      . "#FF0000")
          ("DEBUG"      . "#A020F0")
          ("GOTCHA"     . "#FF4500")
          ("STUB"       . "#1E90FF")
          ("SECTION"    . "#007BFF")
          ("NOTE"       . "#33FFDA")
          ("REVIEW"     . "#1E90FF")
          ("DEPRECATED" . "#1E90FF"))))

;; Dired
(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
;; (setq dired-open-extensions '(("gif" . "sxiv")
;;                               ("jpg" . "sxiv")
;;                               ("png" . "sxiv")
;;                               ("mkv" . "mpv")
;;                               ("mp4" . "mpv")))

;; (evil-define-key 'normal peep-dired-mode-map
;;   (kbd "j") 'peep-dired-next-file
;;   (kbd "k") 'peep-dired-prev-file)
;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")
