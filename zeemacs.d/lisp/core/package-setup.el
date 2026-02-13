;;; core/package-setup.el --- Package management setup -*- lexical-binding: t -*-

(setq package-archives
	  '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
		("MELPA"        . "https://melpa.org/packages/")
		("ORG"          . "https://orgmode.org/elpa/")
		("MELPA Stable" . "https://stable.melpa.org/packages/")
		("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
	  package-archive-priorities
	  '(("GNU ELPA"     . 20)
		("MELPA"        . 15)
		("ORG"          . 10)
		("MELPA Stable" . 5)
		("nongnu"       . 0)))

(package-initialize)
(setopt package-install-upgrade-built-in t)

(unless package-archive-contents
  (condition-case err
      (package-refresh-contents)
    (error
     (message "Failed to refresh package contents: %s" err)
     (setq package-archive-contents nil))))

(cl-defun zeds/vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
  This is a thin wrapper around `package-vc-install' in order to
  make non-interactive usage more ergonomic.  Takes the following
  named arguments:

  - FETCHER the remote where to get the package (e.g., \"gitlab\").
    If omitted, this defaults to \"github\".

  - REPO should be the name of the repository (e.g.,
    \"slotThe/arXiv-citation\".

  - NAME, REV, and BACKEND are as in `package-vc-install' (which
    see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))
  )

(provide 'package-setup)
;;; package-setup.el ends here
