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

(provide 'core/functions)
