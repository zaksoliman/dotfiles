;;; core/functions.el --- Utility Functions -*- lexical-binding: t -*-

(defun zeds/open-init-dir ()
  "Opens my emacs config directory."
  (interactive)
  (dired "~/dotfiles/zeemacs.d"))

(defun zeds/open-init-file ()
  "Open emacs init.el file."
  (interactive)
  (find-file "~/dotfiles/zeemacs.d/init.el"))

(defun zeds/reload-init ()
  (interactive)
  (load-file "~/dotfiles/zeemacs.d/init.el"))

(defun zeds/window-maximize-buffer (&optional arg)
  "Close other windows to focus on this one."
  (interactive "P")
  (delete-other-windows))

(defun zeds/window-maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-up)) (delete-window))
    (while (ignore-errors (windmove-down)) (delete-window))))

(defun zeds/window-maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (ignore-errors (windmove-left)) (delete-window))
    (while (ignore-errors (windmove-right)) (delete-window))))

(defun zeds/macos-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "open %s"
                          (if app-name
                              (format "-a %s '%s'" (shell-quote-argument app-name) path)
                            (format "'%s'" path)))))
    (message "Running: %s" command)
    (shell-command command)))

(defun zeds/macos-reveal-in-finder ()
  (interactive)
  (zeds/macos-open-with "Finder" default-directory))

;; ;;;###autoload (autoload '+macos/reveal-project-in-finder "os/macos/autoload" nil t)
;; (zeds/macos--open-with reveal-project-in-finder "Finder"
;;                    (or (doom-project-root) default-directory))
(defun zeds/setup-prog-modes ()
  (display-line-numbers-mode 1)
  (hs-minor-mode 1)
  (hl-line-mode 1)
  (hs-minor-mode 1))

(defun zeds/setup-text-modes ()
  (display-line-numbers-mode -1)
  (hl-line-mode 1))

(defun zeds/eglot-python-workspace-config (server)
  ;; Default values in accordance with
  ;; https://github.com/python-lsp/python-lsp-server/blob/v1.10.0/CONFIGURATION.md
  ;; (or commit 2a5a953).  A value of null means we do not set a value and
  ;; therefore use the plugin's default value.

  ;; The recommended format for the `eglot-workspace-configuration' variable
  ;; value is a property list (plist for short):
  ;;
  ;; (:server plist…)
  ;;
  ;; Here :server identifies a particular language server and plist is the
  ;; corresponding keyword-value property list of one or more parameter
  ;; settings for that server, serialized by Eglot as a JSON object.
  ;; plist may be arbitrarily complex, generally containing other
  ;; keyword-value property sublists corresponding to JSON subobjects.

  ;; The JSON values are represented by Emacs Lisp values as follows:

  ;; JSON                         | Emacs Lisp
  ;; ------------------------------------------------
  ;; :true i.e. true              | t
  ;; :false i.e. false            | :json-false
  ;; :null i.e. null              | nil
  ;; :[] i.e. [] the empty array  | []*
  ;; :{} i.e. {} the empty object | eglot-{}**

  ;; * Lisp array elements should not be comma separated as they are in a
  ;; JSON array.
  ;; ** Must be evaluated via a backquote or `list'
  ;; e.g. `(:pylsp (:plugins (:jedi (:env_vars ,eglot-{})))) or
  ;;       (list :pylsp (list :plugins (list :jedi (list :env_vars eglot-{}))))
  (let ((venv-path (string-trim (shell-command-to-string "pyenv prefix"))))
    `(:pylsp
      (:plugins (
                 :pylsp_mypy (
                              :enabled t
                              :ignore_missing_imports :json-false)
                 :jedi (:environment ,venv-path)
                 :jedi_completion (
                                   :cache_for ["pandas" "numpy" "tensorflow" "matplotlib"] ; string array: ["pandas", "numpy", "tensorflow", "matplotlib"] (default)
                                   :eager :json-false ; boolean: true or false (default)
                                   :enabled t ; boolean: true (default) or false
                                   :fuzzy :json-false ; boolean: true or false (default)
                                   :include_class_objects :json-false ; boolean: true or false (default)
                                   :include_function_objects :json-false ; boolean: true or false (default)
                                   :include_params t ; boolean: true (default) or false
                                   :resolve_at_most 25)
                 :jedi_definition
                 (:enabled t ; boolean: true (default) or false
                           :follow_builtin_definitions t ; boolean: true (default) or false
                           :follow_builtin_imports t ; boolean: true (default) or false
                           :follow_imports t) ; boolean: true (default) or false
                 :jedit_hover
                 (:enabled t) ; boolean: true (default) or false
                 :jedi_references
                 (:enabled t) ; boolean: true (default) or false
                 :jedi_signature_help
                 (:enabled t) ; boolean: true (default) or false
                 :jedi_symbols
                 (:all_scopes t ; boolean: true (default) or false
                              :enabled t ; boolean: true (default) or false
                              :include_import_symbols t) ; boolean: true (default) or false
                 :pylint (:enabled :json-false)
                 :ruff
                  (:enabled t)
                 ))
      ;; :ccls (:initializationOptions (:clang (:extraArgs ["-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"])))

      )))


(provide 'functions)
