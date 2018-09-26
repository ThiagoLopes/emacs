;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;;------------------------------------------------------------------------------

;; Configs
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(require 'base)
(require 'base-utils)
(require 'base-keybindings)
(require 'base-packages)
(require 'base-themes)
(require 'base-exec-path)
(require 'lang-javascript)
(require 'lang-python)
(require 'lang-html)
(require 'lang-go)

;;Print welcome message
(cl-concatenate 'string
                ";; Welcome to ｅｍａｃｓ"
                ";; Today's date: "
                (format-time-string "%B %d %Y")
                (concat "\n\n;; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
(get-buffer-create (current-buffer)))

;;; init.el ends here
