;;; langs/python-lang.el --- Python development config -*- lexical-binding: t -*-

(use-package uv
 :init
 (zeds/vc-install :fetcher "github" :repo "johannes-mueller/uv.el"))

;;; Helper functions for virtual environment detection

(defun zeds/python-find-venv-path ()
"Find the virtual environment path for the current project.
Checks in order:
1. UV workspace (.venv in project root with uv.lock)
2. Legacy .venv directory
3. pyenv virtualenv from .python-version
Returns the path to the Python executable or nil."
 (when-let ((proj (project-current)))
   (let* ((root (project-root proj))
          (venv-dir (expand-file-name ".venv" root))
          (uv-lock (expand-file-name "uv.lock" root))
          (pyproject (expand-file-name "pyproject.toml" root))
          (python-version-file (expand-file-name ".python-version" root)))
     (cond
      ;; UV project (has uv.lock or pyproject.toml with .venv)
      ((and (file-directory-p venv-dir)
            (or (file-exists-p uv-lock)
                (file-exists-p pyproject)))
       (zeds/python-venv-executable venv-dir))
      ;; Legacy .venv
      ((file-directory-p venv-dir)
       (zeds/python-venv-executable venv-dir))
      ;; pyenv with .python-version
      ((file-exists-p python-version-file)
       (zeds/python-pyenv-executable python-version-file))
      ;; Fallback
      (t nil)))))

(defun zeds/python-venv-executable (venv-dir)
"Return the Python executable path for VENV-DIR."
 (let ((bin-dir (if (eq system-type 'windows-nt) "Scripts" "bin")))
   (expand-file-name "python" (expand-file-name bin-dir venv-dir))))

(defun zeds/python-pyenv-executable (version-file)
 "Return the Python executable for the version specified in VERSION-FILE."
 (let* ((version (string-trim
                  (with-temp-buffer
                    (insert-file-contents version-file)
                    (buffer-string))))
        (pyenv-root (or (getenv "PYENV_ROOT")
                        (expand-file-name "~/.pyenv")))
        (python-path (expand-file-name
                      (format "versions/%s/bin/python" version)
                      pyenv-root)))
   (when (file-executable-p python-path)
     python-path)))

(defun zeds/python-venv-path ()
 "Return the virtual environment directory for the current project."
 (when-let ((proj (project-current)))
   (let* ((root (project-root proj))
          (venv-dir (expand-file-name ".venv" root)))
     (when (file-directory-p venv-dir)
       venv-dir))))

;;; Eglot configuration for pyright

(defun zeds/eglot-python-workspace-config ()
 "Generate eglot workspace configuration for Python/Pyright."
 (let ((venv-path (zeds/python-venv-path))
       (python-path (zeds/python-find-venv-path)))
   `(:python
     (:pythonPath ,python-path
      :venvPath ,(when venv-path (file-name-directory venv-path))
      :venv ,(when venv-path (file-name-nondirectory venv-path)))
     :pyright
     (:disableOrganizeImports :json-false))))

(defun zeds/eglot-python-server-program ()
 "Return the appropriate server program for the current project.
Uses 'uv run' for uv projects, otherwise direct pyright-langserver."
 (let* ((proj (project-current))
        (root (when proj (project-root proj)))
        (uv-lock (when root (expand-file-name "uv.lock" root)))
        (pyproject (when root (expand-file-name "pyproject.toml" root))))
   (cond
    ;; UV project
    ((and root (or (file-exists-p uv-lock)
                   (and (file-exists-p pyproject)
                        (zeds/is-uv-project-p pyproject))))
     '("uv" "run" "pyright-langserver" "--stdio"))
    ;; Fallback to system pyright
    (t
     '("pyright-langserver" "--stdio")))))

(defun zeds/is-uv-project-p (pyproject-path)
 "Check if PYPROJECT-PATH indicates a uv-managed project."
 (when (file-exists-p pyproject-path)
   (with-temp-buffer
     (insert-file-contents pyproject-path)
     (or (search-forward "[tool.uv]" nil t)
         (search-forward "uv.lock" nil t)
         ;; Check if managed by uv (has uv in build system or dependencies)
         (goto-char (point-min))
         (search-forward "uv" nil t)))))

;;; Pyvenv integration for shell/REPL support

(defun zeds/python-activate-venv ()
 "Activate the appropriate virtual environment for the current buffer."
 (interactive)
 (when-let ((venv-path (zeds/python-venv-path)))
   (pyvenv-activate venv-path)
   (message "Activated venv: %s" venv-path)))

;;; Main python configuration

(use-package python-ts-mode
 :ensure nil
 :mode ("\\.py\\'" . python-ts-mode)
 :hook ((python-ts-mode . zeds/python-setup))
 :init
 (defun zeds/python-setup ()
   "Setup Python development environment for current buffer."
   (setq-local indent-tabs-mode nil)
   (setq-local python-indent-offset 4)
   (setq-local tab-width 4)
   ;; Activate venv for this buffer (affects shell commands, etc.)
   (zeds/python-activate-venv)
   ;; Start eglot
   (eglot-ensure))
 :config
 ;; Use a function to dynamically determine server program
 (setq-default eglot-server-programs
               (cons '((python-mode python-ts-mode) . zeds/eglot-python-contact)
                     (assq-delete-all 'python-mode
                                      (assq-delete-all 'python-ts-mode
                                                       eglot-server-programs)))))

(defun zeds/eglot-python-contact (_interactive)
 "Return eglot contact for Python.
Dynamically determines whether to use uv or direct pyright."
 (zeds/eglot-python-server-program))

;; Configure eglot workspace settings per-project
(setq-default eglot-workspace-configuration #'zeds/eglot-workspace-config-fn)

(defun zeds/eglot-workspace-config-fn (_server)
 "Return workspace configuration based on current project."
 (when (derived-mode-p 'python-ts-mode 'python-mode)
   (zeds/eglot-python-workspace-config)))

;;; Optional: pyvenv for virtual environment management

(use-package pyvenv
 :ensure t
 :commands (pyvenv-activate pyvenv-deactivate)
 :config
 ;; Restart python inferior process when venv changes
 (add-hook 'pyvenv-post-activate-hooks
           (lambda ()
             (when (bound-and-true-p python-shell-virtualenv-root)
               (setq python-shell-virtualenv-root pyvenv-virtual-env)))))

(provide 'python-lang)
;;; python-lang.el ends here
