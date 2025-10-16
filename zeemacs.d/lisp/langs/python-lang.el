;;; langs/python-lang.el --- Base Emacs Configs -*- lexical-binding: t -*-


(use-package uv
  :init
  (zeds/vc-install :fetcher "github" :repo "johannes-mueller/uv.el"))


;;; PYTHON
;; (dir-locals-set-class-variables 'unwritable-directory
;;                                 (zeds/eglot-python-workspace-config))

;; (dir-locals-set-directory-class
;;    "/Users/zakaria/Projects/BIT/" 'unwritable-directory)

(defun zeds/pyenv-mode-versions ()
  "List installed python versions."
  (let ((versions (shell-command-to-string "pyenv versions --bare")))
    (cons "system" (split-string versions))))

(defvar zeds/pyenv--version nil)

(defun zeds/python-pyenv-read-version-from-file ()
  "Read pyenv version from .python-version file."
  (when-let (root-path (locate-dominating-file default-directory ".python-version"))
    (let* ((file-path (expand-file-name ".python-version" root-path))
           (version
            (with-temp-buffer
              (insert-file-contents-literally file-path)
              (string-trim (buffer-string)))))
      (if (member version (zeds/pyenv-mode-versions))
          version ;; return.
        (message "pyenv: version `%s' is not installed (set by `%s')."
                 version file-path)))))

(defun zeds/python-pyenv-mode-set-auto-h ()
  "Set pyenv-mode version from buffer-local variable."
  (when (memq major-mode '(python-ts-mode python-mode))
    (when (not (local-variable-p 'zeds/pyenv--version))
      (make-local-variable 'zeds/pyenv--version)
      (setq-local zeds/pyenv--version (zeds/python-pyenv-read-version-from-file)))
    (if zeds/pyenv--version
        (progn
          (pyenv-mode-set zeds/pyenv--version)
          ;; (setq-default eglot-workspace-configuration (zeds/eglot-python-workspace-config))

          ;; (setq-default eglot-workspace-configuration (zeds/eglot-python-workspace-config))
          ;; (eglot-ensure)
          )
      (pyenv-mode-unset))))

(defun zeds/activate-project-venv ()
  "Activate pyenv or pyvenv virtualenv for the current project."
  (when-let ((proj (project-current)))
    (let* ((root (project-root proj))
           (venv (expand-file-name ".venv" root))
           (pyenv-version-file (expand-file-name ".python-version" root)))
      (cond
       ((file-exists-p venv)
        (pyvenv-activate venv))
       ((file-exists-p pyenv-version-file)
        (let ((version (string-trim
                        (with-temp-buffer
                          (insert-file-contents pyenv-version-file)
                          (buffer-string)))))
          (pyenv-mode-set version)))))))
;; (use-package python
;;   :hook
;;   ((python-mode . (lambda ()
;;                        (setq-local indent-tabs-mode t)
;;                        (setq-local tab-width 4)
;;                        (setq-local py-indent-tabs-mode t)))
;;    (python-mode . eglot-ensure))
;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))


(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter "ipython"
  :hook ((python-ts-mode . (lambda ()
                             (setq-local indent-tabs-mode nil)
                             (setq-local python-indent-offset 4)
                             (setq-local py-indent-tabs-mode t)
                             (setq-local tab-width 4)
                             (eglot-ensure)
                             )))
  :config
  (add-to-list
 'eglot-server-programs
 `((python-ts-mode) . ("uv" "tool" "run" "--from" "python-lsp-server[all]" "pylsp"))))


;; (use-package pyenv-mode
;;     :ensure t
;;     :config
;;     (when (executable-find "pyenv")
;;       (pyenv-mode +1)
;;       (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
;;     :hook (python-ts-mode . (lambda ()
;;                               (zeds/python-pyenv-mode-set-auto-h)
;;                               (eglot-ensure))))


(provide 'python-lang)
;;; python.el ends here
