;;; langs/rust.el --- Base Emacs Configs -*- lexical-binding: t -*-

;;; RUST
(use-package rust-mode
  :ensure t
  :after (eglot)
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-mode-treesitter-derive t)
(setq compilation-error-regexp-alist-alist
      (cons '(rustc "^\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([0-9]+\\):\\([0-9]+\\) \\(?:[Ee]rror\\|\\([Ww]arning\\)\\):" 1 (2 . 4) (3 . 5) (6))
        compilation-error-regexp-alist-alist))
  :hook ((rust-mode . (lambda ()
                        (setq indent-tabs-mode nil)
                        (eglot-ensure)
			                  (eglot-inlay-hints-mode -1))))
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (evil-define-key 'normal eglot-mode-map (kbd "<localleader>h") 'eglot-inlay-hints-mode)
  )

(provide 'rust)
;;; rust.el ends here
