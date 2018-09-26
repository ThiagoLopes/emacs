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
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require-package 'use-package)
(eval-when-compile
  (require 'use-package)
  (use-package auto-compile
    :config (auto-compile-on-load-mode)))

(setq use-package-always-ensure t)
;;------------------------------------------------------------------------------
;; Add base(s)
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(require 'base)
(require 'base-packages)
(require 'base-utils)
(require 'base-keybindings)
(require 'base-themes)

(require 'langs)
;;; init.el ends here
