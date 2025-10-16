;;; core/ai.el --- A.I. Assistant Emacs Configs -*- lexical-binding: t -*-

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :init
    (setq aidermacs-backend 'vterm)
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  (setenv "OPENAI_API_KEY" (exec-path-from-shell-getenv "OPENAI_API_KEY"))
  (setenv "OPENAI_API_BASE" (exec-path-from-shell-getenv "OPENAI_API_BASE"))

  :custom
  ; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "openai/gemini-2.5-pro"))


;;; Co-Pilot (copilot)
(use-package copilot
  ;; :init (zeds/vc-install :fetcher "github" :repo "copilot-emacs/copilot.el")
  :ensure t
  :after evil
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-l" . copilot-accept-completion)
              ("C-j" . copilot-next-completion)
              ("C-k" . copilot-previous-completion)
              ("M-l" . copilot-accept-completion-by-line))
  :init
    (setq copilot-backend 'copilot-node) ;; Use the NodeJS backend for better performance
    (setq copilot-indent-offset-warning-disable t)
    ;; (setq copilot-node-command "/opt/homebrew/bin/node") ;; If the command is not in your exec-path
  :config
    (setq copilot-completion-at-point-functions '(copilot-completion-at-point))
    (add-to-list 'copilot-indentation-alist '(js-json-mode . 2))
    (add-to-list 'copilot-indentation-alist '(prog-mode . 4))
    (add-to-list 'copilot-indentation-alist '(org-mode . 4))
    (add-to-list 'copilot-indentation-alist '(text-mode . 4))
    (add-to-list 'copilot-indentation-alist '(closure-mode . 4))
    (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 4)))

(use-package copilot-chat
  ;; :init (zeds/vc-install :fetcher "github" :repo "chep/copilot-chat.el")
  :ensure t
  :after copilot
  :bind (:map global-map
              ("C-c C-y" . copilot-chat-yank)
              ("C-c C-m" . copilot-chat-yank-pop)
              ("C-c C-M-y" . (lambda () (interactive) (copilot-chat-yank-pop -1))))
  :config (setq copilot-chat-model "claude-3.7-sonnet-thought"))


(provide 'ai)
;;; core/ai.el ends here
