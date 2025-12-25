;;; repro.el --- Debug script
(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(message "Package user dir: %s" package-user-dir)
(message "Package activated list: %s" package-activated-list)

(condition-case err
    (require 'vertico)
  (error (message "Failed to require vertico: %s" err)))

(if (featurep 'vertico)
    (message "Vertico loaded successfully")
  (message "Vertico NOT loaded"))

(condition-case err
    (require 'evil-collection)
  (error (message "Failed to require evil-collection: %s" err)))
