;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; The rest of the init file.

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;------------------------------------------------------------------------------

;;Record startup timestamp
(defvar super-emacs/invokation-time
(current-time))

(setq user-full-name "Thiago Lopes"
      user-mail-address "thiagolopes@protonmail.com")

(add-to-list 'load-path (concat user-emacs-directory "elisp"))
;; (add-to-list 'load-path ".orgmode/")


(require 'base)
(require 'base-keybindings)
(require 'base-packages)
(require 'base-themes)
;; (require 'lang-javascript)
(require 'lang-python)
(require 'lang-html)

;;Print welcome message
(princ (cl-concatenate 'string
                       "01000101 01101101 01100001 01100011 01110011\n\n\n"
                       "Startup completed in "
                       (number-to-string (cadr (time-subtract (current-time)
                                                              super-emacs/invokation-time)))
                       " seconds\n\n"
                       "Welcome to ｅｍａｃｓ  安ェ殴ど依挨虞!\n\n"
                       "Today's date: "
                       (format-time-string "%B %d %Y"))
(get-buffer-create (current-buffer)))

;;; init.el ends here
