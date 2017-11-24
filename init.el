;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

(setq user-full-name "Thiago Lopes")
(setq user-mail-address "thiagolopes@protonmail.com")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base)
(require 'base-packages)
;; (require 'base-theme)
;; (require 'base-extensions)
;; (require 'base-functions)
;; (require 'base-global-keys)

;; (require 'lang-python)

;;; init.el ends here
