;;; fix-autoloads.el --- Regenerate autoloads
(require 'package)
(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(message "Scanning %s for packages..." package-user-dir)

(let ((files (directory-files package-user-dir t "^[^.]")))
  (dolist (file files)
    (when (file-directory-p file)
      (let ((pkg-desc (package-load-descriptor file)))
        (if pkg-desc
            (progn
              (message "Generating autoloads for %s..." (package-desc-name pkg-desc))
              (package-generate-autoloads (package-desc-name pkg-desc) file))
          (message "Skipping %s (no package descriptor)" file))))))

(message "Done regenerating autoloads.")
