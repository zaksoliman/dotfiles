;;; langs/rust.el --- Base Emacs Configs -*- lexical-binding: t -*-

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
  (evil-define-key 'normal eglot-mode-map (kbd "<localleader>h") 'eglot-inlay-hints-mode)
  )

(provide 'langs/rust)
;;; rust.el ends here
